open Printf
open Merlin_t
open Utils
open Fuzzy_search

module Log = Common.Log.Make(struct let prefix = "NAMEDB" end)
let _ =
  Log.set_print_timestamp true;
  Log.set_verbosity `DEBUG

type mod_entry = {
  filename : string;
  modname : string;
  mutable timestamp : float;
  mutable values : Merlin_t.entry list;
}

type db = {
  mutable table : mod_entry list;
  mutable timestamp : float;
}

let mx_name_db = Mutex.create()
let mx_count_threads = Mutex.create()

let count db = db.table |> List.fold_left (fun sum x -> sum + List.length x.values) 0

let count_merlin_threads = ref 0

let event_database_initialized = Manual_reset_event.create None

let database : db option ref = ref None

let get_project_source_filenames project =
  File_util.readtree (Project.path_src project)
  |> List.map (fun dir ->
      File_util.ls ~dir ~pattern:"*.ml" |> List.map (Filename.concat dir))
  |> List.concat

let filter pattern db =
  let compare = Utils.Memo.fast ~f:(fun (a, b) -> FuzzyLetters.compare ~min_score:0.85 `Greedy a b) in
  Mutex.protect mx_name_db begin fun () ->
    db.table
    |> List.map (fun entry ->
        entry.values
        |> List.filter_map begin fun val_entry ->
          let score, paths = compare (pattern, val_entry.name) in
          if score > 0. then Some (score, val_entry) else None
        end)
    |> List.concat
    |> List.sort (fun (a, _) (b, _) -> Stdlib.compare b a)
  end

let normalize_stdlib_name modname =
  if String.starts_with ~prefix:"Stdlib." modname
  then Str.string_after modname 7
  else modname

let rec add_outline db entry modname outline =
  let name = normalize_stdlib_name modname in
  let temp = ref [ { kind = "Module"; name; desc = ""; info = "" } ] in
  outline
  |> List.iter begin fun item ->
    let name = modname ^ "." ^ item.ol_name in
    if item.ol_kind = "Module" then begin
      let value = { kind = item.ol_kind; name = item.ol_name; desc = ""; info = "" } in
      temp := value :: !temp;
      add_outline db entry name item.ol_children;
      add_outline db entry item.ol_name item.ol_children;
    end else begin
      let value = { kind = item.ol_kind; name; desc = ""; info = "" } in
      temp := value :: !temp
    end
  end;
  Mutex.protect mx_name_db (fun () -> entry.values <- List.rev_append !temp entry.values)

let max_merlin_processes = 8
let merlin_process = Semaphore.Counting.make max_merlin_processes

let rec add_compl db modname compl count_lib_values =
  let name = normalize_stdlib_name modname in
  let entry = { filename=""; modname=name; timestamp=0.0; values=[] } in
  entry.values <- { name; kind = "Module"; desc = ""; info = "" } :: entry.values;
  let name_prefix = if name = "Stdlib" then "" else name ^ "." in
  compl
  |> List.iter begin fun (item : Merlin_j.entry) ->
    if item.kind = "Module" then
      complete_prefix db (sprintf "%s.%s" modname item.name) count_lib_values;
    entry.values <- { item with name = name_prefix ^ item.name } :: entry.values
  end;
  entry

and complete_prefix db modname count_lib_values =
  let finally ?ex () =
    Mutex.protect mx_count_threads (fun () -> decr count_merlin_threads);
    ex |> Option.iter (fun ex -> Log.println `ERROR "%s" (Printexc.to_string ex));
    if !count_merlin_threads <= 0 then begin
      Manual_reset_event.set event_database_initialized (Some true)
    end
  in
  try
    Mutex.protect mx_count_threads (fun () -> incr count_merlin_threads);
    Semaphore.Counting.acquire merlin_process;
    Merlin.complete_prefix ~position:(1,1) ~prefix:(modname ^ ".") ~filename:"dummy" ~buffer:""
    |> Async.start_with_continuation ~name:(sprintf "%s-compl-%s" __MODULE__ modname) begin function
    | Merlin.Ok compl ->
        begin
          try
            Semaphore.Counting.release merlin_process;
            let mod_entry = add_compl db modname compl.entries count_lib_values in
            Mutex.protect mx_name_db begin fun () ->
              mod_entry.timestamp <- Unix.gettimeofday();
              db.table <- mod_entry :: db.table;
              count_lib_values := !count_lib_values + List.length mod_entry.values
            end;
            finally()
          with ex -> finally ~ex ()
        end
    | Merlin.Error _ | Merlin.Failure _ -> finally()
    end
  with ex -> finally ~ex ()

let update ?(is_init=false) db filename =
  if is_init then Thread.delay 0.075
  else
    Mutex.protect mx_name_db (fun () ->
        db.table <- db.table |> List.filter (fun me -> me.filename <> filename));
  if Sys.file_exists filename then begin
    let modname =
      filename |> Filename.basename |> Filename.remove_extension |> String.capitalize_ascii in
    let mod_entry = { filename; modname; timestamp=Unix.gettimeofday(); values=[] } in
    Mutex.protect mx_name_db (fun () -> db.table <- mod_entry :: db.table);
    Merlin.outline
      ~filename
      ~buffer:(File_util.read filename |> Buffer.contents)
    |> Async.start_with_continuation ~name:(sprintf "%s-outline-%s" __MODULE__ modname) begin function
    | Merlin.Ok outline ->
        add_outline db mod_entry modname outline;
        Mutex.protect mx_name_db (fun () -> mod_entry.timestamp <- Unix.gettimeofday());
    | Merlin.Failure _ | Merlin.Error _ -> ()
    end;
  end

let init ?(cont=ignore) project =
  let time_start = Unix.gettimeofday() in
  let db = { table=[]; timestamp=time_start } in
  database := Some db;
  let count_lib_values = ref 0 in
  get_project_source_filenames project |> List.iter (update ~is_init:true db);
  Async.create ~name:(sprintf "%s-wait-event-finished" __MODULE__) begin fun () ->
    Manual_reset_event.wait event_database_initialized |> ignore;
    Log.println `DEBUG "init complete (%d entries, %.0fs)" (count db) (Unix.gettimeofday() -. time_start);
    cont db
  end |> Async.start;
  Merlin.list_modules ()
  |> Async.start_with_continuation ~name:(sprintf "%s-modules" __MODULE__) begin fun x ->
    begin
      match x with
      | Merlin.Ok modules ->
          "Str" :: "Unix" :: "Thread" :: "Stdlib" :: modules
          |> List.iter begin fun modname ->
            match List.find_opt (fun me -> me.modname = modname) db.table with
            | None -> complete_prefix db modname count_lib_values
            | _ -> ()
          end
      | Merlin.Failure msg | Merlin.Error msg ->
          Log.println `ERROR "%s" msg
    end
  end

let update_all ?(cont=ignore) project =
  match !database with
  | Some db ->
      Thread.create begin fun () ->
        get_project_source_filenames project
        |> List.iter begin fun filename ->
          match db.table |> List.find_opt (fun x -> x.filename = filename) with
          | Some dbf when (Unix.stat filename).Unix.st_mtime <= dbf.timestamp -> ()
          | _ -> update db filename
        end;
        cont db
      end () |> ignore;
  | _ ->
      let message = "Loading completion..." in
      Activity.add Activity.Other message;
      project |> init ~cont:begin fun db ->
        Activity.remove message;
        cont db
      end;
      Activity.wrap ~delay:750 Activity.Other ignore ()

module Cache = struct

  let create_filename ?(dump=false) project =
    let cache_dir = Project.path_cache project in
    Utils.mkdir_p cache_dir;
    cache_dir // (sprintf "names%s" (if dump then ".dump" else ""));;

  let dump project =
    match !database with
    | Some db ->
        Mutex.protect mx_name_db begin fun () ->
          let filename = create_filename ~dump:true project in
          let chan = open_out_bin filename in
          let finally () = close_out chan in
          try
            let count_mod = ref 0 in
            let count_val = ref 0 in
            db.table
            |> List.sort compare
            |> List.iter begin fun entry ->
              Printf.fprintf chan "%s (%s)\n%!" entry.modname (if entry.filename <> "" then entry.filename else "N/A");
              incr count_mod;
              entry.values
              |> List.iter begin fun value ->
                incr count_val;
                Printf.fprintf chan "  %s %s : %s\n%!" value.kind value.name value.desc;
              end;
              Printf.fprintf chan "\n%!";
            end;
            finally();
            Log.println `DEBUG "database dumped, %d modules, %d values, %d merlin threads running. See file %s"
              !count_mod !count_val !count_merlin_threads filename;
          with ex ->
            finally();
            raise ex
        end;
    | _ -> ();;

  let reset ~project =
    let name = sprintf "Loading names..." in
    Log.println `DEBUG "%s -- %s" __FUNCTION__ name;
    Async.create ~name:(sprintf "%s-init" __MODULE__) begin fun () ->
      GtkThread.async (Activity.add Activity.Symbol) name;
      init project ~cont:(fun _ -> GtkThread.async Activity.remove name)
    end
    |> Async.start;;

  let save ~project =
    match !database with
    | Some table ->
        Mutex.protect mx_name_db begin fun () ->
          let filename = create_filename project in
          let chan = open_out_bin filename in
          let finally () = close_out chan in
          try
            let cache = Oe.Dump (Oe.magic, table) in
            output_value chan cache;
            Log.println `DEBUG "Saved %d entries to %s" (count table) filename;
            finally();
          with ex -> begin
              finally();
              raise ex
            end
        end;
    | _ -> ();;

  let load ~project =
    let filename = create_filename project in
    if Sys.file_exists filename then begin
      Mutex.protect mx_name_db begin fun () ->
        let chan = open_in_bin filename in
        let finally () = close_in chan in
        try
          let (db : db) = Oe.open_dump (input_value chan) in
          database := Some db;
          Log.println `DEBUG "Loaded %d entries from %s" (count db) filename;
          finally();
        with
        | Oe.Bad_magic_number ->
            finally();
            reset ~project
        | ex ->
            finally();
            raise ex
      end
    end else (reset ~project);;

end
