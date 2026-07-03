module Log = Common.Log.Make(struct let prefix = "RENAME" end)
let _ =
  Log.set_print_timestamp true;
  Log.set_verbosity `ERROR

let do_rename page renaming_positions new_name =
  Log.println `DEBUG "---------------";
  let text = page#buffer#get_text ?start:None ?stop:None ?slice:None ?visible:None () in
  page#buffer#undo#begin_block ~name:"renaming";
  let count =
    renaming_positions
    |> List.fold_left begin fun count (token, m1, m2) ->
      let start = page#buffer#get_iter_at_mark m1 in
      let stop = page#buffer#get_iter_at_mark m2 in
      let is_use =
        match [@warning "-4"] Definition.locate ~filename:page#get_filename ~text ~iter:start with
        | Merlin.Ok (Some _) -> true
        | _ -> false
      in
      match token with
      | `Label when is_use ->
          Log.println `DEBUG "%d `Label USE" start#offset;
          page#buffer#insert_interactive ?iter:(Some stop) ?default_editable:None (":" ^ new_name) |> ignore;
          count + 1
      | `Label ->
          Log.println `DEBUG "%d `Label DEF" start#offset;
          page#buffer#delete_interactive ~start ~stop ?default_editable:None ();
          page#buffer#insert_interactive ?iter:(Some start) ?default_editable:None new_name |> ignore;
          count + 1
      | `Record_label_semi ->
          Log.println `DEBUG "%d `Record_label_semi" start#offset;
          page#buffer#insert_interactive ?iter:(Some stop) ?default_editable:None (" = " ^ new_name) |> ignore;
          count + 1
      | `Lident ->
          Log.println `DEBUG "%d `Lident %s" start#offset (if is_use then "USE" else "DEF");
          page#buffer#delete_interactive ~start ~stop ?default_editable:None ();
          page#buffer#insert_interactive ?iter:(Some start) ?default_editable:None new_name |> ignore;
          count + 1
      | `Record_label_equal
      | `Dot_lident
      | `Uident ->
          assert false
      | `None ->
          Log.println `DEBUG "%d `None %s" start#offset (if is_use then "USE" else "DEF");
          page#buffer#delete_interactive ~start ~stop ?default_editable:None ();
          page#buffer#insert_interactive ?iter:(Some start) ?default_editable:None new_name |> ignore;
          count + 1
    end 0
  in
  page#buffer#undo#end_block ();
  count

let get_renaming_positions page =
  let text = page#buffer#get_text ?start:None ?stop:None ?slice:None ?visible:None () in
  let exception Rename_not_supported in
  try
    page#view#mark_occurrences_manager#refs
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
  let position_with_cursor =
    match
      renaming_positions
      |> List.find_opt begin fun (_, m1, m2) ->
        let start = page#buffer#get_iter_at_mark m1 in
        let stop = page#buffer#get_iter_at_mark m2 in
        start#compare iter <= 0 && iter#compare stop <= 0
      end
    with
    | Some (_, start, _) -> page#buffer#get_iter_at_mark start
    | _ -> iter
  in
  let rect = page#view#get_iter_location position_with_cursor in
  let x, y = page#view#buffer_to_window_coords ~tag:`WIDGET
      ~x:(Gdk.Rectangle.x rect) ~y:(Gdk.Rectangle.y rect) in
  let pX, pY = Gdk.Window.get_pointer_location (Gdk.Window.root_parent ()) in
  let win = (match page#view#get_window `WIDGET with None -> assert false | Some w -> w) in
  let px, py = Gdk.Window.get_pointer_location win in
  let x = pX - px + x in
  let _, lh = page#view#get_line_yrange iter in
  let y = pY - py + y + lh in
  x, y

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
            let entry = GEdit.entry ~text:old_name ~has_frame:false ~width_chars:(String.length old_name + 10) () in
            let x, y = get_window_position page renaming_positions in
            let window = Gtk_util.window entry#coerce ~modal:true ~border_width:1 ~x ~y () in
            let pref = Preferences.preferences#get in
            let open Settings_t in
            entry#misc#modify_font_by_name pref.editor_base_font;
            entry#select_region ~start:0 ~stop:(String.length old_name);
            entry#event#connect#key_press ~callback:begin fun ev ->
              if GdkEvent.Key.keyval ev = GdkKeysyms._Return then begin
                let new_name = entry#text in
                (* TODO Support prefix and infix symbols *)
                if Str.string_match re_ocaml_ident new_name 0 then begin
                  let count = do_rename page renaming_positions new_name in
                  Printf.ksprintf editor#status_message "%d occurrences have been renamed." count;
                end else
                  Printf.ksprintf editor#status_message "%S is not a valid identifier." new_name;
                window#destroy();
                true
              end else false
            end |> ignore;
            window#move ~x ~y
      end
  | _ -> ()
