module ColorOps = Color

let initialized : (int * (Margin_diff.local * Margin_diff.global)) list ref = ref []

type base = HEAD | Stage [@@deriving show]
let base : base ref = ref HEAD

let update_models page diffs =
  let open Odiff in
  let diffs = List.sort begin fun a b ->
      match a with
      | Delete _ -> (match b with Delete _ -> 0 | Add _ | Change _  -> 1)
      | Add _ | Change _ -> (match b with Delete _ -> -1 | Add _ | Change _  -> 0)
    end diffs
  in
  begin
    match List.assoc_opt page#get_oid !initialized with
    | Some (local, global) ->
        local#set_diffs diffs;
        global#set_diffs diffs
    | _ -> ()
  end

let diff ?(ignore_whitespace=true) page continue_with =
  match Utils.filename_relative (Filename.dirname (Sys.getcwd())) page#get_filename with
  | Some filename ->
      let open Printf in
      let term = match !base with HEAD -> "HEAD" | Stage -> "" in
      let w_opt = if ignore_whitespace then " -w" else "" in
      let text = page#buffer#get_text ?start:None ?stop:None ?slice:None ?visible:None () in
      (* Suppress errors to handle files not yet tracked in HEAD *)
      let cmd = sprintf "bash -c 'diff%s <(git show %s:%s 2>/dev/null) -'" w_opt term filename in
      let ic, oc = Unix.open_process cmd in
      output_string oc text;
      close_out oc;
      Async.create ~name:__FUNCTION__ begin fun () ->
        try
          let diffs = Odiff.from_channel ic in
          GtkThread.sync begin fun () ->
            continue_with diffs;
            ignore (Unix.close_process (ic, oc));
          end ()
        with ex ->
          Printf.eprintf "%s\n%s\nFile being parsed: %s\n%s\n%!" __LOC__
            (Printexc.to_string ex)
            filename
            (Printexc.get_backtrace());
      end
      |> Async.start
  | _ -> ()

let try_diff ?(force=false) page =
  let margins = List.assoc_opt page#get_oid !initialized in
  let is_changed = Option.fold ~none:true ~some:(fun (ml, mg) ->
      ml#is_changed_after_last_diff || mg#is_changed_after_last_diff) margins
  in
  if (is_changed || force) && page#view#visible then begin
    diff page begin fun diffs ->
      try
        diffs |> update_models page;
        Option.iter begin fun (ml, mg) ->
          ml#sync_diff_time();
          mg#sync_diff_time();
          page#margin_manager#build();
          if force then GtkBase.Widget.queue_draw page#as_widget
        end margins;
      with Gpointer.Null as ex -> ()
    end
  end

let to_buffer (buffer : GText.buffer) ignore_whitespace filename1 filename2 =
  Global_diff_gtext.insert buffer ignore_whitespace filename1 filename2

let init_page page =
  match List.assoc_opt page#get_oid !initialized with
  | None ->
      page#view#event#connect#after#focus_in ~callback:(fun _ ->
          Gmisclib.Idle.add ~prio:300 (fun () -> try_diff ~force:true page);
          false) |> ignore;
      let margin_local = new Margin_diff.local page#view in
      let margin_global = new Margin_diff.global page#view in
      page#margin_manager#add (margin_local :> Margin.margin);
      page#margin_manager#add (margin_global :> Margin.margin);
      page#statusbar#button_switch_diff_term#connect#clicked ~callback:begin fun () ->
        base := (match !base with HEAD -> Stage | Stage -> HEAD);
        try_diff ~force:true page;
      end |> ignore;
      initialized := (page#get_oid, (margin_local, margin_global)) :: !initialized;
      (*try_diff ~force:true page;*)
      Gmisclib.Idle.add page#margin_manager#build;
  | _ -> ()

let init_editor editor =
  editor#connect#add_page ~callback:init_page |> ignore;
  editor#connect#remove_page ~callback:begin fun page ->
    begin
      match !initialized |> List.assoc_opt page#get_oid with
      | Some (local, global) ->
          page#margin_manager#remove (local :> Margin.margin);
          page#margin_manager#remove (global :> Margin.margin)
      | _ -> ()
    end;
    initialized := List.filter (fun (oid, _) -> oid <> page#get_oid) (!initialized);
  end |> ignore;
  (* Timeout *)
  let id_timeout_diff = ref None in
  let create_timeout_diff () =
    Option.iter Gmisclib.Timeout.remove !id_timeout_diff;
    id_timeout_diff := None;
    let callback () =
      try
        editor#with_current_page (fun page ->
            if page#view#has_focus then (try_diff page));
        true
      with ex ->
        Printf.eprintf "%s\n%s\n%s\n%!"
          __LOC__ (Printexc.to_string ex) (Printexc.get_backtrace());
        true
    in
    id_timeout_diff := Some (Gmisclib.Timeout.add __FUNCTION__ ~ms:1000 ~callback);
  in
  let main _ =
    create_timeout_diff();
    let connect_focus_change () =
      GWindow.toplevel editor#coerce
      |> Option.iter begin fun w ->
        w#event#connect#focus_in ~callback:begin fun _ ->
          create_timeout_diff();
          false
        end |> ignore;
        w#event#connect#focus_out ~callback:begin fun _ ->
          Gaux.may !id_timeout_diff ~f:Gmisclib.Timeout.remove;
          id_timeout_diff := None;
          false
        end |> ignore;
      end
    in
    match GWindow.toplevel editor#coerce with
    | Some _ -> connect_focus_change ()
    | _ -> editor#misc#connect#realize ~callback:connect_focus_change |> ignore;
  in
  begin
    try main ()
    with Gpointer.Null -> editor#misc#connect#map ~callback:main |> ignore;
  end
