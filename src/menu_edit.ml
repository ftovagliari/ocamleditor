(** "Edit" menu, defined as a set of {!Command}s plus a declarative description of the menu. *)

open Utils
open GdkKeysyms

(** Commands of the Edit menu. Availability is evaluated when a command is invoked (see {!Command}). *)
let register ~browser =
  let editor = browser#editor in
  let on_page f = editor#with_current_page f in
  let check_page f = match editor#get_page `ACTIVE with Some page -> f page | None -> false in
  let has_page () = check_page (fun _ -> true) in
  let has_selection () = check_page (fun page -> page#buffer#has_selection) in
  let is_ocaml_source () =
    check_page begin fun page ->
      let name = page#get_filename in
      name ^^^ ".ml" || name ^^^ ".mli"
    end
  in
  let ctrl = [`CONTROL] and ctrl_shift = [`CONTROL; `SHIFT] in
  let reg id label ?accels ?enabled run = ignore (Command.register ~id:("edit." ^ id) ~label ?accels ?enabled run) in
  reg "undo" "Undo" ~accels:[ctrl, _z]
    ~enabled:(fun () -> check_page (fun page -> page#buffer#undo#can_undo))
    (fun () -> on_page (fun page -> page#undo()));
  reg "redo" "Redo" ~accels:[ctrl, _y; ctrl_shift, _z]
    ~enabled:(fun () -> check_page (fun page -> page#buffer#undo#can_redo))
    (fun () -> on_page (fun page -> page#redo()));
  reg "cut" "Cut" ~accels:[ctrl, _x] ~enabled:has_selection (fun () -> on_page (fun page -> page#cut ()));
  reg "copy" "Copy" ~accels:[ctrl, _c] ~enabled:has_selection (fun () -> on_page (fun page -> page#copy ()));
  reg "paste" "Paste" ~accels:[ctrl, _v] ~enabled:has_page (fun () -> on_page (fun page -> page#paste ()));
  reg "delete" "Delete" ~enabled:has_selection (fun () -> on_page (fun page -> page#delete ()));
  reg "select_all" "Select All" ~accels:[ctrl, _a] ~enabled:has_page begin fun () ->
    on_page (fun page -> ignore (page#buffer#select_range page#buffer#start_iter page#buffer#end_iter))
  end;
  reg "select_word" "Select Word" ~accels:[ctrl, _w] ~enabled:has_page begin fun () ->
    on_page (fun page -> ignore (page#ocaml_view#obuffer#select_ocaml_word ?pat:None ()))
  end;
  reg "select_expression" "Select Enclosing Expression" ~accels:[ctrl, _d] begin fun () ->
    on_page (fun page -> page#ocaml_view#select_enclosing_expr ?iter:None ())
  end;
  reg "select_delimiter" "Select to Matching Delimiter" ~accels:[ctrl_shift, _d]
    ~enabled:(fun () -> check_page (fun page -> page#view#current_matching_tag_bounds <> []))
    begin fun () ->
      on_page (fun page -> ignore (page#view#select_to_matching_delim ?include_delim:None ()))
    end;
  reg "comment" "Comment Block" ~accels:[ctrl, _slash] ~enabled:is_ocaml_source begin fun () ->
    on_page (fun page -> page#ocaml_view#toggle_comment false)
  end;
  reg "select_comment" "Select Comment"
    ~accels:[ctrl_shift, _slash; [`CONTROL; `MOD1], _slash] (* the first one doesn't work on some layouts *)
    ~enabled:is_ocaml_source
    (fun () -> on_page (fun page -> page#ocaml_view#toggle_comment true));
  reg "toggle_case" "Convert To Uppercase/Lowercase" ~accels:[ctrl, _u] ~enabled:has_selection begin fun () ->
    on_page (fun page -> page#buffer#toggle_case ())
  end;
  reg "indent_more" "Increase Selection Indent" ~accels:[[`SHIFT; `MOD1], _Right] ~enabled:has_selection begin fun () ->
    on_page (fun page -> page#buffer#indent ?decrease:(Some false) ())
  end;
  reg "indent_less" "Decrease Line/Selection Indent" ~accels:[[`SHIFT; `MOD1], _Left] ~enabled:has_page begin fun () ->
    on_page (fun page -> page#buffer#indent ?decrease:(Some true) ())
  end;
  let ocp_indent bounds page = ignore (Ocp_indent.indent ~project:editor#project ~view:page#view bounds) in
  reg "indent" "Indent Line/Selection" ~accels:[[], _Tab] ~enabled:has_page (fun () -> on_page (ocp_indent `SELECTION));
  reg "indent_all" "Indent All" ~enabled:has_page (fun () -> on_page (ocp_indent `ALL));
  reg "templates" "Templates..." ~accels:[ctrl, _j] ~enabled:is_ocaml_source begin fun () ->
    browser#with_current_project begin fun project ->
      on_page (fun page -> Templ.popup project page#ocaml_view)
    end
  end;
  reg "rename" "Rename Symbol" ~accels:[[], _F2] (fun () -> Rename.rename editor);
  reg "completion" "Completion" ~accels:[ctrl, _space] begin fun () ->
    browser#with_current_project begin fun project ->
      on_page (fun page -> ignore (Completion.create ~project ~page ()))
    end
  end;
  reg "completion_experimental" "Completion (experimental)" ~accels:[ctrl_shift, _space] ~enabled:is_ocaml_source begin fun () ->
    browser#with_current_project begin fun project ->
      on_page (fun page -> ignore (Completion.create ~use_names:true ~project ~page ()))
    end
  end;
  reg "quick_info" "Show Quick Info at Cursor" ~accels:[ctrl, _i] ~enabled:is_ocaml_source browser#quick_info_at_cursor;
  reg "eval_in_toplevel" "Eval in Toplevel" ~accels:[[], _F8] ~enabled:is_ocaml_source begin fun () ->
    on_page (fun page -> page#ocaml_view#obuffer#send_to_shell ())
  end;
  is_ocaml_source

(** Creates the Edit menu. Returns the menubar entry and its menu, like the other menus. *)
let create ~browser ~group =
  let is_ocaml_source = register ~browser in
  let quick_info_on_hover menu =
    let item = GMenu.check_menu_item
        ~label:"Enable On-Mouse-Hover"
        ~active:Preferences.preferences#get.Settings_j.editor_quick_info_enabled
        ~packing:menu#add ()
    in
    ignore (item#connect#toggled ~callback:begin fun () ->
        Preferences.preferences#get.Settings_j.editor_quick_info_enabled <- item#active;
        Preferences.save()
      end)
  in
  let open Menu_builder in
  let header, menu = Menu_builder.create ~group ~label:"Edit" [
      item "edit.undo"; item "edit.redo";
      separator;
      item "edit.cut"; item "edit.copy"; item "edit.paste"; item "edit.delete"; item "edit.select_all";
      separator;
      item "edit.select_word"; item "edit.select_expression"; item "edit.select_delimiter";
      item "edit.comment"; item "edit.select_comment"; item "edit.toggle_case";
      item "edit.indent_more"; item "edit.indent_less"; item "edit.indent"; item "edit.indent_all";
      item "edit.templates"; item "edit.rename"; item "edit.completion"; item "edit.completion_experimental";
      sub ~enabled:is_ocaml_source "Quick Info" [item "edit.quick_info"; custom quick_info_on_hover];
      separator;
      item "edit.eval_in_toplevel";
    ]
  in
  header, menu
