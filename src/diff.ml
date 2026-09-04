module ColorOps = Color

let initialized : (int * (Margin_diff.local * Margin_diff.global)) list ref = ref []

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

let compare_with_head page continue_with =
  match Utils.filename_relative (Filename.dirname (Sys.getcwd())) page#get_filename with
  | Some filename ->
      let open Printf in
      let buf = Buffer.create 1024 in
      Spawn.async "git" [| "show"; sprintf "HEAD:%s" filename |]
        ~process_in:(Spawn.loop (fun ic -> Buffer.add_string buf (input_line ic); Buffer.add_char buf '\n'))
        ~continue_with:begin fun _ ->
          GtkThread.sync begin fun () ->
            let text = page#buffer#get_text ?start:None ?stop:None ?slice:None ?visible:None () in
            try continue_with (Odiff.strings_diffs (Buffer.contents buf) text)
            with ex ->
              Printf.eprintf "%s, %s (%s)\n%!" __LOC__ (Printexc.to_string ex) filename;
          end ()
        end |> ignore
  | _ -> ()

let try_compare ?(force=false) page =
  let margins = List.assoc_opt page#get_oid !initialized in
  let is_changed = Option.fold ~none:true ~some:(fun (ml, mg) ->
      ml#is_changed_after_last_diff || mg#is_changed_after_last_diff) margins
  in
  if (is_changed || force) && page#view#visible then begin
    compare_with_head page begin fun diffs ->
      try
        diffs |> update_models page;
        Option.iter (fun (ml, mg) -> ml#sync_diff_time(); mg#sync_diff_time()) margins
      with Gpointer.Null as ex -> ()
      (*Printf.eprintf "%s\n%s\n%s\n%!" __LOC__ (Printexc.to_string ex) (Printexc.get_backtrace());*)
    end
  end

let to_buffer (buffer : GText.buffer) ignore_whitespace filename1 filename2 =
  Global_diff_gtext.insert buffer ignore_whitespace filename1 filename2

let init_page page =
  match List.assoc_opt page#get_oid !initialized with
  | None ->
      page#view#event#connect#focus_in ~callback:(fun _ -> try_compare ~force:true page; false) |> ignore;
      let margin_local = new Margin_diff.local page#view in
      let margin_global = new Margin_diff.global page#view in
      page#margin_manager#add (margin_local :> Margin.margin);
      page#margin_manager#add (margin_global :> Margin.margin);
      initialized := (page#get_oid, (margin_local, margin_global)) :: !initialized;
      try_compare ~force:true page;
      Gmisclib.Idle.add page#view#build_gutter
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
    Option.iter GMain.Timeout.remove !id_timeout_diff;
    id_timeout_diff := None;
    let callback () =
      try
        editor#with_current_page begin fun page ->
          if page#view#has_focus then (try_compare page);
        end;
        true
      with ex ->
        Printf.eprintf "%s\n%s\n%s\n%!"
          __LOC__ (Printexc.to_string ex) (Printexc.get_backtrace());
        true
    in
    id_timeout_diff := Some (GMain.Timeout.add ~ms:1000 ~callback);
  in
  let main _ =
    create_timeout_diff();
    Gaux.may (GWindow.toplevel editor#coerce) ~f:begin fun (w : GWindow.window) ->
      w#event#connect#focus_in ~callback:begin fun _ ->
        create_timeout_diff();
        false
      end |> ignore;
      w#event#connect#focus_out ~callback:begin fun _ ->
        Gaux.may !id_timeout_diff ~f:GMain.Timeout.remove;
        id_timeout_diff := None;
        false
      end |> ignore;
    end;
  in
  begin
    try main ()
    with Gpointer.Null -> editor#misc#connect#map ~callback:main |> ignore;
  end
