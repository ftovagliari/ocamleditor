open Printf

module Log = Common.Log.Make(struct let prefix = "OCP-INDEX" end)
let _ =
  Log.set_print_timestamp true;
  Log.set_verbosity `ERROR

let (//) = Filename.concat
let [@inline] (!!) none some = Option.fold ~none ~some
let (~~) opt f = f opt

type context = string * int option * int option

let execute_async command ?(context:context option) ~full_open ident =
  let context =
    context |> !! "" (fun (fn, line, col) ->
        line |> !! (sprintf "%s" fn) (fun ln ->
            col |> !! (sprintf "%s:%d" fn ln) (sprintf "%s:%d,%d" fn ln)))
  in
  let context = if context = "" then "" else "--context=" ^ context in
  let full_open = match full_open with [] -> "" | _ -> String.concat "," full_open |> sprintf "-F %s" in
  let build = "--build=." in
  let i_dirs = "-I ." in
  let arguments = [| command; context; full_open; build; i_dirs; ident |] in
  let cmd_line = String.concat " " ("ocp-index":: (arguments |> Array.to_list)) in
  let channels = Unix.open_process_full cmd_line (Unix.environment ()) in
  Async.create ~name:(sprintf "`%s`" cmd_line) (fun () -> channels)

let fullname_async ~context ident =
  execute_async "print" ~context ~full_open:[] ident
  |> Async.map ~name:"fullname_async" begin fun ((ic, _, _) as channels) ->
    let pid = Unix.process_full_pid channels in
    let first_line =
      try Some (String.trim (input_line ic))
      with End_of_file ->
        (try Unix.close_process_full channels |> ignore;
         with Unix.Unix_error(Unix.ECHILD, _ , _) -> ());
        None
    in
    (try Unix.waitpid [] pid |> ignore with Unix.Unix_error(Unix.ECHILD, _, _) -> ());
    first_line |> Option.map (fun x -> String.sub x 0 (String.index x ' '))
  end
