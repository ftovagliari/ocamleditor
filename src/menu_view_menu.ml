(** "View" menu. (The helpers of [Menu_view] are used by the editor, which is compiled before the
    modules this menu depends on, hence the separate module.) *)

open Utils
open GdkKeysyms

(** Commands of the View menu. The toggles carry their state ([checked]), which the menu shows as a
    check mark each time it opens: nobody has to keep the items in sync with the actual state. *)
let register ~browser =
  let editor = browser#editor in
  let has_page () = editor#get_page `ACTIVE <> None in
  let is_ocaml_source () =
    match editor#get_page `ACTIVE with
    | Some page -> let name = page#get_filename in name ^^^ ".ml" || name ^^^ ".mli"
    | None -> false
  in
  let folding_enabled () = Preferences.preferences#get.Settings_j.editor_code_folding_enabled in
  let ctrl_alt = [`CONTROL; `MOD1] in
  let reg id label ?accels ?enabled ?checked run =
    ignore (Command.register ~id:("view." ^ id) ~label ?accels ?enabled ?checked run)
  in
  reg "toolbar" "Toolbar" ~checked:(fun () -> browser#toolbar_visible#get)
    (fun () -> browser#toolbar_visible#set (not browser#toolbar_visible#get));
  reg "tabbar" "Tabbar" ~accels:[ctrl_alt, _b] ~checked:(fun () -> editor#show_tabs)
    (fun () -> browser#tabbar_visible#set (not editor#show_tabs));
  reg "outline" "Outline" ~accels:[ctrl_alt, _z] ~checked:(fun () -> editor#show_outline)
    (fun () -> browser#outline_visible#set (not editor#show_outline));
  reg "messages" "Messages" ~accels:[ctrl_alt, _m] ~checked:(fun () -> (Messages.vmessages())#visible)
    (fun () -> let m = Messages.vmessages() in m#set_visible (not m#visible));
  reg "hmessages" "Messages (right pane)" ~checked:(fun () -> (Messages.hmessages())#visible)
    (fun () -> let m = Messages.hmessages() in m#set_visible (not m#visible));
  reg "compact_mode" "Compact Mode" (fun () -> browser#set_maximized_view `FIRST);
  reg "fullscreen" "Fullscreen" ~accels:[[], _F11] (fun () -> browser#set_maximized_view `SECOND);
  reg "restore_layout" "Restore Default Layout [Ctrl+K Esc]" (fun () -> browser#set_maximized_view `NONE);
  reg "remove_messages" "Remove All Messages" ~accels:[[`CONTROL; `SHIFT], _m]
    (fun () -> ignore ((Messages.vmessages())#remove_all_tabs()));
  reg "code_folding" "Enable Code Folding" ~accels:[[`CONTROL; `SHIFT], _F1] ~enabled:is_ocaml_source
    ~checked:folding_enabled begin fun () ->
    Preferences.preferences#get.Settings_j.editor_code_folding_enabled <- not (folding_enabled ());
    Preferences.save()
  end;
  reg "fold_toggle_current" "Toggle Current Fold" ~accels:[[`CONTROL], _minus]
    ~enabled:(fun () -> is_ocaml_source () && folding_enabled ())
    (fun () -> editor#with_current_page Margin_fold.toggle_current_fold);
  reg "fold_collapse_definitions" "Collapse to Definitions [Ctrl+K 0]" ~enabled:is_ocaml_source begin fun () ->
    Gmisclib.Idle.add ~prio:300 (fun () -> editor#with_current_page Margin_fold.collapse_to_definitions)
  end;
  reg "fold_expand_all" "Expand All Folds [Ctrl+K J]"
    ~enabled:(fun () -> is_ocaml_source () && folding_enabled ())
    (fun () -> editor#with_current_page Margin_fold.expand_all);
  reg "whitespace" "Show Whitespace Characters" ~enabled:has_page
    ~checked:(fun () -> editor#show_whitespace_chars) (Editor_menu.show_whitespace_characters_toggled ~editor);
  reg "word_wrap" "Toggle Word-Wrap" ~enabled:has_page
    ~checked:(fun () -> editor#word_wrap) (Editor_menu.toggle_word_wrap_toggled ~editor);
  reg "dependency_graph" "Dependency Graph"
    ~enabled:begin fun () ->
      match editor#get_page `ACTIVE with
      | Some page -> Menu_view.get_switch_view_sensitive editor#project page
      | None -> false
    end
    (fun () -> editor#with_current_page (fun page -> page#button_dep_graph#clicked ()));
  reg "revision_history" "Revision History" ~enabled:has_page
    (fun () -> editor#with_current_page (fun page -> page#show_revision_history ()));
  is_ocaml_source

(** Creates the View menu. *)
let create ~browser ~group =
  let is_ocaml_source = register ~browser in
  Menu_builder.create ~group ~label:"View" Menu_builder.[
      item "view.toolbar"; item "view.tabbar"; item "view.outline"; item "view.messages"; item "view.hmessages";
      sub "Appearance" [item "view.compact_mode"; item "view.fullscreen"; item "view.restore_layout"];
      separator;
      item "view.remove_messages";
      separator;
      sub ~enabled:is_ocaml_source "Code Folding" [
        item "view.code_folding"; item "view.fold_toggle_current";
        item "view.fold_collapse_definitions"; item "view.fold_expand_all";
      ];
      item "view.whitespace"; item "view.word_wrap";
      separator;
      item "view.dependency_graph"; item "view.revision_history";
    ]
