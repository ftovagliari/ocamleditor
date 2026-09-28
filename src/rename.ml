module Log = Common.Log.Make(struct let prefix = "RENAME" end)
let _ =
  Log.set_print_timestamp true;
  Log.set_verbosity `ERROR

let do_rename page renaming_positions new_name cont =
  Log.println `DEBUG "---------------";
  page#buffer#undo#begin_block ~name:"renaming";
  let count =
    Async.await renaming_positions
    |> List.fold_left begin fun count (token, m1, m2, is_use) ->
      let start = page#buffer#get_iter_at_mark m1 in
      let stop = page#buffer#get_iter_at_mark m2 in
      match token with
      | `Label when is_use ->
          Log.println `DEBUG "%d `Label USE" start#offset;
          page#buffer#insert ?iter:(Some stop) ?tag_names:None ?tags:None (":" ^ new_name) |> ignore;
          count + 1
      | `Label ->
          Log.println `DEBUG "%d `Label DEF" start#offset;
          page#buffer#delete ~start ~stop |> ignore;
          page#buffer#insert ?iter:(Some start) ?tag_names:None ?tags:None new_name |> ignore;
          count + 1
      | `Record_label_semi ->
          Log.println `DEBUG "%d `Record_label_semi" start#offset;
          page#buffer#insert ?iter:(Some stop) ?tag_names:None ?tags:None (" = " ^ new_name) |> ignore;
          count + 1
      | `Lident ->
          Log.println `DEBUG "%d `Lident %s" start#offset (if is_use then "USE" else "DEF");
          page#buffer#delete ~start ~stop |> ignore;
          page#buffer#insert ?iter:(Some start) ?tag_names:None ?tags:None new_name |> ignore;
          count + 1
      | `Record_label_equal
      | `Dot_lident
      | `Uident ->
          assert false
      | `None ->
          Log.println `DEBUG "%d `None %s" start#offset (if is_use then "USE" else "DEF");
          page#buffer#delete ~start ~stop |> ignore;
          page#buffer#insert ?iter:(Some start) ?tag_names:None ?tags:None new_name |> ignore;
          count + 1
    end 0
  in
  cont();
  page#buffer#undo#end_block ();
  count

let get_renaming_positions page =
  let text = page#buffer#get_text ?start:None ?stop:None ?slice:None ?visible:None () in
  let exception Rename_not_supported in
  try
    page#mark_occurrences_manager#refs
    |> List.map begin fun (m1, m2) ->
      let start = page#buffer#get_iter_at_mark m1 in
      let pos = start#offset in
      match Lex.for_renaming text pos with
      | `Record_label_equal | `Dot_lident | `Uident -> raise Rename_not_supported
      | token -> token, m1, m2
    end
  with Rename_not_supported -> []

let get_window_position page renaming_positions =
  let iter = page#buffer#get_iter `INSERT in
  match
    renaming_positions
    |> List.find_opt begin fun (_, m1, m2) ->
      let start = page#buffer#get_iter_at_mark m1 in
      let stop = page#buffer#get_iter_at_mark m2 in
      start#compare iter <= 0 && iter#compare stop <= 0
    end
  with
  | Some (_, start, stop) ->
      let start = page#buffer#get_iter_at_mark start in
      let stop = page#buffer#get_iter_at_mark stop in
      let len = stop#offset - start#offset in
      start#forward_chars (len / 2)
  | _ -> iter

let re_ocaml_ident = Str.regexp "[_a-z][a-zA-Z0-9_']*"

let rename editor =
  match editor#get_page `ACTIVE with
  | Some page ->
      begin
        match get_renaming_positions page with
        | [] -> editor#status_message "Renaming is not supported here."
        | ((_, m1, m2) :: _) as renaming_positions ->
            let old_name =
              let start = Some (page#buffer#get_iter_at_mark m1) in
              let stop = Some (page#buffer#get_iter_at_mark m2) in
              page#buffer#get_text ?start ?stop ?slice:None ?visible:None ()
            in
            let vbox = GPack.vbox ~spacing:0 ~border_width:0 () in
            let hbox = GPack.hbox ~spacing:5 ~border_width:5 ~packing:vbox#add () in
            let entry = GEdit.entry ~text:old_name ~has_frame:false
                ~width_chars:(String.length old_name + 10) ~packing:hbox#add () in
            let spinner = GMisc.spinner ~active:false ~packing:(hbox#pack ~expand:false) () in
            let iter = get_window_position page renaming_positions in
            let renaming_positions =
              let text = page#buffer#get_text ?start:None ?stop:None ?slice:None ?visible:None () in
              let start = page#buffer#get_iter_at_mark m1 in
              Async.create begin fun () ->
                spinner#start();
                let result =
                  renaming_positions |> List.map begin fun (t, m1, m2) ->
                    let is_use =
                      match [@warning "-4"] Definition.locate ~filename:page#get_filename ~text ~iter:start with
                      | Merlin.Ok (Some _) -> true
                      | _ -> false
                    in
                    t, m1, m2, is_use
                  end in
                spinner#stop();
                result
              end
              |> Async.start_as_task
            in
            let popover = Gtk_util.popover_at_iter ~view:page#view#as_gtext_view vbox#coerce in
            popover.Gtk_util.popup iter;
            entry#misc#grab_focus();
            let pref = Preferences.preferences#get in
            let open Settings_t in
            entry#misc#modify_font_by_name pref.editor_base_font;
            entry#select_region ~start:0 ~stop:(String.length old_name);
            entry#connect#activate ~callback:begin fun ev ->
              spinner#start();
              let new_name = entry#text in
              entry#set_editable false;
              (* TODO Support prefix and infix symbols *)
              let cont () =
                popover.Gtk_util.popdown();
                Gmisclib.Timeout.add __FUNCTION__ ~ms:1000 ~callback:(fun () -> popover.Gtk_util.destroy();false) |> ignore;
              in
              try
                if Str.string_match re_ocaml_ident new_name 0 then begin
                  let count = do_rename page renaming_positions new_name cont in
                  Printf.ksprintf editor#status_message "%d occurrences have been renamed." count;
                end else
                  Printf.ksprintf editor#status_message "%S is not a valid identifier." new_name;
              with ex ->
                Printf.eprintf "%s\n%s\n%s\n%!" __LOC__ (Printexc.to_string ex) (Printexc.get_backtrace());
                popover.Gtk_util.destroy();
            end |> ignore;
      end
  | _ -> ()
