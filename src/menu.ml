(*

  OCamlEditor
  Copyright (C) 2010-2014 Francesco Tovagliari

  This file is part of OCamlEditor.

  OCamlEditor is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  OCamlEditor is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.

*)

open Printf
open GdkKeysyms
open Utils
open Menu_types
open Preferences

(** set_label *)
let set_label item text = item#misc#set_property "label" (`STRING (Some text));;

(** edit *)
let edit ~browser ~group ~flags
    ~get_menu_item_undo
    ~get_menu_item_redo items =
  let editor = browser#editor in
  let edit = GMenu.menu_item ~label:"Edit" () in
  let menu = GMenu.menu ~packing:edit#set_submenu () in
  let cursor = Gdk.Cursor.create `ARROW in
  menu#event#connect#motion_notify  ~callback:begin fun _ ->
    Gdk.Window.set_cursor menu#misc#window cursor;
    false;
  end |> ignore;
  (* Undo *)
  let undo = GMenu.image_menu_item ~label:"Undo" ~packing:menu#add () in
  undo#set_image (Icons.create (??? Icons.undo_16))#coerce;
  ignore (undo#connect#activate ~callback:(fun () -> editor#with_current_page (fun page -> page#undo())));
  undo#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._z ~flags;
  get_menu_item_undo := (fun () -> undo);
  (* Redo *)
  let redo = GMenu.image_menu_item ~label:"Redo" ~packing:menu#add () in
  redo#set_image (Icons.create (??? Icons.redo_16))#coerce;
  ignore (redo#connect#activate ~callback:(fun () -> editor#with_current_page (fun page -> page#redo())));
  redo#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._y ~flags;
  redo#add_accelerator ~group ~modi:[`CONTROL; `SHIFT] GdkKeysyms._z ~flags;
  get_menu_item_redo := (fun () -> redo);
  (* Cut & Paste... *)
  let _ = GMenu.separator_item ~packing:menu#add () in
  let cut = GMenu.image_menu_item ~label:"Cut" ~packing:menu#add () in
  cut#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._x ~flags;
  ignore (cut#connect#activate ~callback:(fun () -> editor#with_current_page (fun page -> page#cut ())));
  let copy = GMenu.image_menu_item ~label:"Copy" ~packing:menu#add () in
  copy#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._c ~flags;
  ignore (copy#connect#activate ~callback:(fun () -> editor#with_current_page (fun page -> page#copy ())));
  let paste = GMenu.image_menu_item ~label:"Paste" ~packing:menu#add () in
  paste#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._v ~flags;
  ignore (paste#connect#activate ~callback:(fun () -> editor#with_current_page (fun page -> page#paste ())));
  let delete = GMenu.image_menu_item ~label:"Delete" ~packing:menu#add () in
  ignore (delete#connect#activate ~callback:(fun () -> editor#with_current_page (fun page -> page#delete ())));
  let select_all = GMenu.image_menu_item ~label:"Select All" ~packing:menu#add () in
  (*select_all#set_image (GMisc.image ~stock:`SELECT_ALL ~icon_size:`MENU ())#coerce;*)
  ignore (select_all#connect#activate ~callback:(fun () ->
      editor#with_current_page (fun page ->
          ignore(page#buffer#select_range page#buffer#start_iter page#buffer#end_iter))));
  select_all#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._a ~flags;
  (* Select Word *)
  let _ = GMenu.separator_item ~packing:menu#add () in
  let select_word = GMenu.menu_item ~label:"Select Word" ~packing:menu#add () in
  ignore (select_word#connect#activate ~callback:(fun () ->
      editor#with_current_page (fun page ->
          ignore (page#ocaml_view#obuffer#select_ocaml_word ?pat:None ()))));
  select_word#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._w ~flags;
  (* Select Enclosing Expressions *)
  let select_expr = GMenu.menu_item ~label:"Select Enclosing Expression" ~packing:menu#add () in
  select_expr#connect#activate ~callback:(fun () ->
      editor#with_current_page (fun page -> page#ocaml_view#select_enclosing_expr ?iter:None ())
    ) |> ignore;
  select_expr#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._d ~flags;
  (* Select to Matching Delimiter *)
  let select_par_expr = GMenu.menu_item ~label:"Select to Matching Delimiter" ~packing:menu#add () in
  ignore (select_par_expr#connect#activate ~callback:(fun () ->
      editor#with_current_page (fun page ->
          ignore (page#view#matching_delim_goto ?select:(Some true) ?strict:None ()))));
  select_par_expr#add_accelerator ~group ~modi:[`CONTROL; `SHIFT] GdkKeysyms._d ~flags;
  (* Comment/Uncomment *)
  let comment = GMenu.menu_item ~label:"Comment Block" ~packing:menu#add () in
  comment#connect#activate ~callback:(fun () ->
      editor#with_current_page (fun page -> page#ocaml_view#toggle_comment false)) |> ignore;
  comment#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._slash ~flags;
  let select_comment = GMenu.menu_item ~label:"Select Comment" ~packing:menu#add () in
  select_comment#connect#activate ~callback:(fun () ->
      editor#with_current_page (fun page -> page#ocaml_view#toggle_comment true)) |> ignore;
  select_comment#add_accelerator ~group ~modi:[`CONTROL; `SHIFT] GdkKeysyms._slash ~flags; (* Doesn't work *)
  select_comment#add_accelerator ~group ~modi:[`CONTROL; `MOD1] GdkKeysyms._slash ~flags;
  (* Change To Uppercase/Lowercase *)
  let toggle_case = GMenu.menu_item ~label:"Convert To Uppercase/Lowercase" ~packing:menu#add () in
  ignore (toggle_case#connect#activate ~callback:(fun () ->
      editor#with_current_page (fun page -> page#buffer#toggle_case ())));
  toggle_case#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._u ~flags;
  (* Increase Selection Indent *)
  let increase_selection_indent = GMenu.image_menu_item ~label:"Increase Selection Indent" ~packing:menu#add () in
  increase_selection_indent#set_image (GMisc.image ~stock:`INDENT ~icon_size:`MENU ())#coerce;
  ignore (increase_selection_indent#connect#activate ~callback:(fun () ->
      editor#with_current_page (fun page -> page#buffer#indent ?decrease:(Some false) ())));
  increase_selection_indent#add_accelerator ~group ~modi:[`SHIFT;`MOD1] GdkKeysyms._Right ~flags;
  (* Decrease Selection Indent *)
  let decrease_selection_indent = GMenu.image_menu_item ~label:"Decrease Line/Selection Indent" ~packing:menu#add () in
  decrease_selection_indent#set_image (GMisc.image ~stock:`UNINDENT ~icon_size:`MENU ())#coerce;
  ignore (decrease_selection_indent#connect#activate ~callback:(fun () ->
      editor#with_current_page (fun page -> page#buffer#indent ?decrease:(Some true) ())));
  decrease_selection_indent#add_accelerator ~group ~modi:[`SHIFT;`MOD1] GdkKeysyms._Left ~flags;
  (* Indent Selection (ocp-indent) *)
  let indent_selection = ref None in
  let indent_all = ref None in
  let item = GMenu.image_menu_item ~label:"Indent Line/Selection" ~packing:menu#add () in
  let ocp_indent bounds page = ignore (Ocp_indent.indent ~project: editor#project ~view:page#view bounds) in
  indent_selection := Some item;
  (*item#set_image (GMisc.image ~stock:`INDENT ~icon_size:`MENU ())#coerce;*)
  ignore (item#connect#activate ~callback:(fun () -> editor#with_current_page (ocp_indent `SELECTION)));
  item#add_accelerator ~group ~modi:[] GdkKeysyms._Tab ~flags;
  let item = GMenu.image_menu_item ~label:"Indent All" ~packing:menu#add () in
  indent_all := Some item;
  (*item#set_image (GMisc.image ~stock:`INDENT ~icon_size:`MENU ())#coerce;*)
  ignore (item#connect#activate ~callback:(fun () -> editor#with_current_page (ocp_indent `ALL)));

  (* Templates *)
  let templates = GMenu.menu_item ~label:"Templates..." ~packing:menu#add () in
  ignore (templates#connect#activate ~callback:(fun () ->
      browser#with_current_project (fun project ->
          editor#with_current_page (fun page -> Templ.popup project page#ocaml_view))));
  templates#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._j ~flags;
  (* Rename *)
  let rename = GMenu.image_menu_item ~label:"Rename Symbol" ~packing:menu#add () in
  rename#add_accelerator ~group ~modi:[] GdkKeysyms._F2 ~flags;
  rename#connect#activate ~callback:(fun () -> Rename.rename editor) |> ignore;
  (* Completion *)
  let complet = GMenu.menu_item ~label:"Completion" ~packing:menu#add () in
  ignore (complet#connect#activate ~callback:begin fun () ->
      browser#with_current_project begin fun project ->
        editor#with_current_page (fun page -> Complete_prefix.create_window ~project ~page |> ignore)
      end
    end);
  complet#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._space ~flags;
  (* Quick Info *)
  let quick_info = GMenu.menu_item ~label:"Quick Info" ~packing:menu#add () in
  let quick_info_menu = GMenu.menu ~packing:quick_info#set_submenu () in
  let quick_info_at_cursor = GMenu.menu_item ~label:"Show Quick Info at Cursor" ~packing:quick_info_menu#add () in
  ignore (quick_info_at_cursor#connect#activate ~callback:browser#quick_info_at_cursor);
  quick_info_at_cursor#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._i ~flags;
  let quick_info_mouse = GMenu.check_menu_item
      ~label:"Enable On-Mouse-Hover"
      ~active:Preferences.preferences#get.Settings_j.editor_quick_info_enabled
      ~packing:quick_info_menu#add ()
  in
  ignore (quick_info_mouse#connect#toggled ~callback:(fun () ->
      Preferences.preferences#get.Settings_j.editor_quick_info_enabled <- quick_info_mouse#active;
      Preferences.save()));
  (** Show documentation *)
  let show_doc_at_cursor = GMenu.image_menu_item ~label:"Show Documentation" ~packing:menu#add () in
  show_doc_at_cursor#connect#activate ~callback:editor#show_doc_at_cursor |> ignore;
  show_doc_at_cursor#add_accelerator ~group ~modi:[] GdkKeysyms._F1 ~flags;
  let _ = GMenu.separator_item ~packing:menu#add () in
  (** Eval in Toplevel *)
  let to_shell = GMenu.image_menu_item ~label:"Eval in Toplevel" ~packing:menu#add () in
  to_shell#set_image (Icons.create (??? Icons.toplevel))#coerce;
  ignore (to_shell#connect#activate ~callback:(fun () ->
      editor#with_current_page (fun page -> page#ocaml_view#obuffer#send_to_shell ())));
  to_shell#add_accelerator ~group ~modi:[] GdkKeysyms._F8 ~flags;
  (**  *)
  ignore (edit#misc#connect#state_changed ~callback:begin fun _ ->
      let has_current_page = editor#get_page `ACTIVE <> None in
      if not has_current_page then begin
        undo#misc#set_sensitive false;
        redo#misc#set_sensitive false;
      end;
      List.iter (function Some i -> i#misc#set_sensitive has_current_page | _ -> ()) [
        Some (cut :> GMenu.menu_item);
        Some (paste :> GMenu.menu_item);
        Some (select_word :> GMenu.menu_item);
        Some (select_par_expr :> GMenu.menu_item);
        Some (comment :> GMenu.menu_item);
        Some (select_comment :> GMenu.menu_item);
        Some (toggle_case :> GMenu.menu_item);
        Some (templates :> GMenu.menu_item);
        Some (complet :> GMenu.menu_item);
        Some (quick_info :> GMenu.menu_item);
        Some (to_shell :> GMenu.menu_item);
        Some (select_all :> GMenu.menu_item);
        Some (increase_selection_indent :> GMenu.menu_item);
        Some (decrease_selection_indent :> GMenu.menu_item);
        (match !indent_selection with Some item -> Some (item :> GMenu.menu_item) | _ -> None);
        (match !indent_all with Some item -> Some (item :> GMenu.menu_item) | _ -> None);
      ];
      editor#with_current_page begin fun page ->
        let name = page#get_filename in
        let sensitive = name ^^^ ".ml" || name ^^^ ".mli" in
        List.iter (fun i -> i#misc#set_sensitive sensitive) [
          (comment :> GMenu.menu_item);
          (select_comment :> GMenu.menu_item);
          (templates :> GMenu.menu_item);
          (complet :> GMenu.menu_item);
          (quick_info :> GMenu.menu_item);
          (to_shell :> GMenu.menu_item);
        ];
        List.iter (fun x -> x#misc#set_sensitive page#buffer#has_selection)
          [cut#coerce; copy#coerce; delete#coerce; toggle_case#coerce; increase_selection_indent#coerce];
        let has_tag_delim = page#view#current_matching_tag_bounds <> [] in
        select_par_expr#misc#set_sensitive has_tag_delim;
      end;
    end);
  edit, menu
;;

(** search *)
let search ~browser ~group ~flags items =
  let editor = browser#editor in
  let search_item = GMenu.menu_item ~label:"Search" () in
  let menu = GMenu.menu ~packing:search_item#set_submenu () in
  let cursor = Gdk.Cursor.create `ARROW in
  menu#event#connect#motion_notify  ~callback:begin fun _ ->
    Gdk.Window.set_cursor menu#misc#window cursor;
    false;
  end |> ignore;
  (** Find and Replace *)
  let find_repl = GMenu.image_menu_item ~label:"Find and Replace" ~packing:menu#add () in
  find_repl#set_image (GMisc.image ~pixbuf:(??? Icons.find_replace) (*~stock:`FIND_AND_REPLACE*) ~icon_size:`MENU ())#coerce;
  ignore (find_repl#connect#activate ~callback:(fun () ->
      Menu_search.find_replace ?find_all:None ?search_word_at_cursor:None editor));
  (*find_repl#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._f ~flags;*)
  (** Find Next *)
  let find_next = GMenu.image_menu_item ~label:"Find Next" ~packing:menu#add () in
  ignore (find_next#connect#activate ~callback:(fun () -> Menu_search.find_next editor));
  find_next#add_accelerator ~group GdkKeysyms._F3 ~flags;
  (** Find Previous *)
  let find_prev = GMenu.image_menu_item ~label:"Find Previous" ~packing:menu#add () in
  ignore (find_prev#connect#activate ~callback:(fun () -> Menu_search.find_prev editor));
  find_prev#add_accelerator ~group ~modi:[`SHIFT] GdkKeysyms._F3 ~flags;
  (** Search Again *)
  let search_again = GMenu.image_menu_item ~label:"Search Again" ~packing:menu#add () in
  search_again#set_image (GMisc.image ~pixbuf:(??? Icons.search_again_16) ())#coerce;
  ignore (search_again#connect#activate ~callback:(fun () -> Menu_search.search_again editor));
  search_again#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._F3 ~flags;
  (** Find All *)
  let find_all = GMenu.image_menu_item ~label:"Find All" ~packing:menu#add () in
  find_all#set_image (GMisc.image ~pixbuf:(??? Icons.search_results_16) ())#coerce;
  ignore (find_all#connect#activate ~callback:(fun () ->
      Menu_search.find_replace ?find_all:(Some true) ?search_word_at_cursor:(Some true) editor));
  find_all#add_accelerator ~group ~modi:[`MOD1] GdkKeysyms._f ~flags;
  (** Search Incremental *)
  let i_search = GMenu.image_menu_item ~label:"Search Incremental" ~packing:menu#add () in
  (*i_search#set_image (GMisc.image ~stock:`FIND ~icon_size:`MENU ())#coerce;*)
  let modi, key = [(`CONTROL : Gdk.Tags.modifier)], GdkKeysyms._f in
  let full_find : unit -> unit = fun () -> Menu_search.find_replace ?find_all:None ?search_word_at_cursor:None editor in
  i_search#connect#activate ~callback:begin fun () ->
    editor#i_search ?full_find:(Some (modi, key, full_find)) ()
  end |> ignore;
  i_search#add_accelerator ~group ~modi key ~flags;
  (** Find/Replace in Path *)
  let find_in_path = GMenu.image_menu_item ~label:"Find/Replace in Path" ~packing:menu#add () in
  ignore (find_in_path#connect#activate ~callback:begin fun () ->
      Menu_search.find_replace ~find_in_buffer:false ?search_word_at_cursor:(Some true) editor;
    end);
  find_in_path#add_accelerator ~group ~modi:[`CONTROL;`SHIFT] GdkKeysyms._f ~flags;
  find_in_path#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._p ~flags;
  (** Clear find/replace history *)
  let clear_find_history = GMenu.image_menu_item ~label:"Clear Find/Replace History" ~packing:menu#add () in
  ignore (clear_find_history#connect#activate ~callback:Find_text.clear_history);
  let _ = GMenu.separator_item ~packing:menu#add () in
  (** Go to Line *)
  let goto_line = GMenu.image_menu_item
      (*~image:(GMisc.image ~stock:`JUMP_TO ~icon_size:`MENU ())*)
      ~label:"Go to Line..." ~packing:menu#add () in
  ignore (goto_line#connect#activate ~callback:(fun () ->
      editor#with_current_page (fun page -> page#ocaml_view#goto ())));
  goto_line#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._g ~flags;
  (** Find definition *)
  let find_definition = GMenu.image_menu_item
      ~label:"Find Definition" ~packing:menu#add () in
  find_definition#set_image (GMisc.image ~pixbuf:(??? Icons.definition) ())#coerce;
  ignore (find_definition#connect#activate ~callback:(fun () ->
      editor#with_current_page (fun page ->
          ignore (editor#scroll_to_definition ~page ~iter:(page#buffer#get_iter `INSERT)))));
  find_definition#add_accelerator ~group ~modi:[] GdkKeysyms._F12 ~flags;
  find_definition#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._Return ~flags;
  (* Find references *)
  let find_references = GMenu.image_menu_item ~label:"Find References" ~packing:menu#add () in
  find_references#set_image (GMisc.image ~pixbuf:(??? Icons.references) ())#coerce;
  find_references#add_accelerator ~group ~modi:[`SHIFT] GdkKeysyms._F12 ~flags;
  find_references#add_accelerator ~group ~modi:[`CONTROL; `SHIFT] GdkKeysyms._Return ~flags;
  ignore (find_references#connect#activate ~callback:(fun () -> Menu_search.find_definition_references editor));
  (*  *)
  ignore (search_item#misc#connect#state_changed ~callback:begin fun state ->
      if state = `NORMAL then begin
        Menu_search.update_items_visibility
          (*~label_find_used_components
            ~find_used_components*)
          ~find_definition
          ~find_references
          editor
      end
    end);
  (** Find file *)
  let dialog_find_file = GMenu.menu_item ~label:"Find File..." ~packing:menu#add () in
  ignore (dialog_find_file#connect#activate ~callback:(fun () -> browser#dialog_find_file ?all:(Some true) ()));
  dialog_find_file#add_accelerator ~group ~modi:[`CONTROL; `SHIFT] GdkKeysyms._L ~flags;
  let _ = GMenu.separator_item ~packing:menu#add () in
  (** Bookmarks *)
  let bookmarks = GMenu.image_menu_item
      ~image:(GMisc.image ~pixbuf:(??? Icons.bB) ~icon_size:`MENU ())
      ~label:"Bookmarks" ~packing:menu#add () in
  let bookmark_menu = GMenu.menu ~packing:bookmarks#set_submenu () in
  let _  = GMenu.tearoff_item ~packing:bookmark_menu#add () in
  Gmisclib.Idle.add begin fun () ->
    List.iter2 begin fun num key ->
      let mi = GMenu.menu_item ~label:(sprintf "Goto Bookmark %d" num) ~packing:bookmark_menu#add () in
      ignore (mi#connect#activate ~callback:(fun () -> editor#bookmark_goto ~num));
      mi#add_accelerator ~group ~modi:[`CONTROL] key ~flags;
    end [1; 2; 3; 4; 5; 6; 7; 8; 9; 0] [_1; _2; _3; _4; _5; _6; _7; _8; _9; _0];
    let _ = GMenu.separator_item ~packing:bookmark_menu#add () in
    List.iter2 begin fun num key ->
      let mi = GMenu.menu_item ~label:(sprintf "Bookmark %d" num) ~packing:bookmark_menu#add () in
      ignore (mi#connect#activate ~callback:(fun () -> editor#bookmark_create ~num ?where:None ?callback:None ()));
      mi#add_accelerator ~group ~modi:[`MOD1; `CONTROL] key ~flags;
    end [1; 2; 3; 4; 5; 6; 7; 8; 9; 0] [_1; _2; _3; _4; _5; _6; _7; _8; _9; _0];
  end;
  (** Callback *)
  ignore (search_item#misc#connect#state_changed ~callback:begin fun _ ->
      let items = [
        (find_repl :> GMenu.menu_item);
        (find_next :> GMenu.menu_item);
        (find_prev :> GMenu.menu_item);
        (search_again :> GMenu.menu_item);
        (find_all :> GMenu.menu_item);
        (i_search :> GMenu.menu_item);
        (goto_line :> GMenu.menu_item);
      ] in
      let has_current_page = editor#get_page `ACTIVE <> None in
      List.iter (fun i -> i#misc#set_sensitive has_current_page) items;
    end);
  search_item, menu
;;

(** view *)
let view ~browser ~group ~flags
    ~menu_item_view_menubar
    ~menu_item_view_toolbar
    ~menu_item_view_tabbar
    ~menu_item_view_outline
    ~menu_item_view_messages ~menu_item_view_hmessages items =
  let editor = browser#editor in
  let view = GMenu.menu_item ~label:"View" () in
  let menu = GMenu.menu ~packing:view#set_submenu () in
  let cursor = Gdk.Cursor.create `ARROW in
  menu#event#connect#motion_notify  ~callback:begin fun _ ->
    Gdk.Window.set_cursor menu#misc#window cursor;
    false;
  end |> ignore;
  (*begin
    let item = GMenu.check_menu_item ~label:"Menubar" ~active:browser#menubar_visible ~packing:menu#add () in
    item#add_accelerator ~group ~modi:[`CONTROL; `MOD1] GdkKeysyms._n ~flags;
    let sign = item#connect#activate ~callback:(fun () -> browser#set_menubar_visible (not browser#menubar_visible)) in
    menu_item_view_menubar := (item, sign) :: !menu_item_view_menubar;
    end;*)
  begin
    let item = GMenu.check_menu_item ~label:"Toolbar" ~active:browser#toolbar_visible#get ~packing:menu#add () in
    let sign = item#connect#activate ~callback:(fun () -> browser#toolbar_visible#set (not browser#toolbar_visible#get)) in
    menu_item_view_toolbar := (item, sign) :: !menu_item_view_toolbar;
  end;
  begin
    let item = GMenu.check_menu_item ~label:"Tabbar" ~active:editor#show_tabs ~packing:menu#add () in
    item#add_accelerator ~group ~modi:[`CONTROL; `MOD1] GdkKeysyms._b ~flags;
    let sign = item#connect#activate ~callback:(fun () -> browser#tabbar_visible#set (not editor#show_tabs)) in
    menu_item_view_tabbar := (item, sign) :: !menu_item_view_tabbar;
  end;
  begin
    let item = GMenu.check_menu_item ~label:"Outline" ~active:editor#show_outline ~packing:menu#add () in
    item#add_accelerator ~group ~modi:[`CONTROL; `MOD1] GdkKeysyms._z ~flags;
    let sign = item#connect#activate ~callback:(fun () -> browser#outline_visible#set (not editor#show_outline)) in
    menu_item_view_outline := (item, sign) :: !menu_item_view_outline;
  end;
  begin
    let item = GMenu.check_menu_item ~label:"Messages" ~active:Messages.vmessages#visible ~packing:menu#add () in
    item#add_accelerator ~group ~modi:[`CONTROL; `MOD1] GdkKeysyms._m ~flags;
    let sign = item#connect#activate ~callback:(fun () -> Messages.vmessages#set_visible (not Messages.vmessages#visible)) in
    menu_item_view_messages := (item, sign) :: !menu_item_view_messages;
  end;
  begin
    let item = GMenu.check_menu_item ~label:"Messages (right pane)" ~active:Messages.hmessages#visible ~packing:menu#add () in
    let sign = item#connect#activate ~callback:(fun () -> Messages.hmessages#set_visible (not Messages.hmessages#visible)) in
    menu_item_view_hmessages := (item, sign) :: !menu_item_view_hmessages;
  end;
  (* Appearance *)
  let appearance = GMenu.image_menu_item ~label:"Appearance" ~packing:menu#add () in
  let appearance_menu = GMenu.menu ~packing:appearance#set_submenu () in
  let compact_mode = GMenu.image_menu_item ~label:"Compact Mode" ~packing:appearance_menu#add () in
  let fullscreen = GMenu.image_menu_item ~label:"Fullscreen" ~packing:appearance_menu#add () in
  let resore_default_layout = GMenu.image_menu_item ~label:"Restore Default Layout [Ctrl+K Esc]" ~packing:appearance_menu#add () in
  let _ = compact_mode#connect#activate ~callback:(fun () -> browser#set_maximized_view `FIRST) in
  let _ = fullscreen#connect#activate ~callback:(fun () -> browser#set_maximized_view `SECOND) in
  let _ = resore_default_layout#connect#activate ~callback:(fun () -> browser#set_maximized_view `NONE) in
  fullscreen#add_accelerator ~group ~modi:[] GdkKeysyms._F11 ~flags;
  fullscreen#add_accelerator ~group ~modi:[`CONTROL; `MOD1] GdkKeysyms._period ~flags;
  compact_mode#add_accelerator ~group ~modi:[`CONTROL; `MOD1] GdkKeysyms._comma ~flags;
  (** Remove All Messages *)
  let _ = GMenu.separator_item ~packing:menu#add () in
  let messages_remove = GMenu.menu_item ~label:"Remove All Messages" ~packing:menu#add () in
  ignore (messages_remove#connect#activate ~callback:(fun () -> ignore (Messages.vmessages#remove_all_tabs())));
  messages_remove#add_accelerator ~group ~modi:[`CONTROL; `SHIFT] GdkKeysyms._m ~flags;
  (** Code Folding *)
  let _ = GMenu.separator_item ~packing:menu#add () in
  let code_folding = GMenu.menu_item ~label:"Code Folding" ~packing:menu#add () in
  let code_folding_menu = GMenu.menu ~packing:code_folding#set_submenu () in
  let enable_code_folding = GMenu.check_menu_item ~label:"Enable Code Folding" ~packing:code_folding_menu#add () in
  enable_code_folding#add_accelerator ~group ~modi:[`CONTROL; `SHIFT] GdkKeysyms._F1 ~flags;
  ignore (enable_code_folding#connect#after#toggled ~callback:begin fun () ->
      Menu_view.toggle_code_folding ~enable_code_folding editor
    end);
  let collapse_enclosing = GMenu.menu_item ~label:"Toggle Current Fold" ~packing:code_folding_menu#add () in
  collapse_enclosing#connect#activate ~callback:(fun () ->
      editor#with_current_page (fun page -> (* TODO *) ())) |> ignore;
  collapse_enclosing#add_accelerator ~group ~modi:[`CONTROL;] GdkKeysyms._minus ~flags;
  (* Collapse to Definitions *)
  let collapse_definitions = GMenu.menu_item ~label:"Collapse to Definitions [Ctrl+K 0]" ~packing:code_folding_menu#add () in
  collapse_definitions#connect#activate ~callback:(fun () ->
      editor#with_current_page Margin_fold.collapse_to_definitions) |> ignore;
  (* Expand All folds *)
  let unfold_all = GMenu.menu_item ~label:"Expand All Folds [Ctrl+K J]" ~packing:code_folding_menu#add () in
  unfold_all#connect#activate ~callback:(fun () -> editor#with_current_page Margin_fold.expand_all) |> ignore;
  (** Select in Structure Pane *)
  let select_in_outline = GMenu.image_menu_item
      ~image:(GMisc.image ~pixbuf:(??? Icons.select_in_structure) ~icon_size:`MENU ())
      ~label:"Select in Structure Pane" ~packing:menu#add () in
  (** Show Whitespace Characters *)
  let show_whitespace_chars = GMenu.check_menu_item
      ~active:editor#show_whitespace_chars
      ~label:"Show Whitespace Characters" ~packing:menu#add () in
  let signal_show_whitespace_chars = show_whitespace_chars#connect#toggled
      ~callback:(Editor_menu.show_whitespace_characters_toggled ~editor) in
  (** Toggle Word-Wrap *)
  let toggle_word_wrap = GMenu.check_menu_item ~label:"Toggle Word-Wrap" ~packing:menu#add () in
  let signal_toggle_wrod_wrap = toggle_word_wrap#connect#toggled
      ~callback:(Editor_menu.toggle_word_wrap_toggled ~editor) in
  (**  *)
  let _ = GMenu.separator_item ~packing:menu#add () in
  let switch_viewer = GMenu.menu_item ~label:"Dependency Graph" ~packing:menu#add () in
  ignore (switch_viewer#connect#activate ~callback:begin fun () ->
      editor#with_current_page (fun page -> page#button_dep_graph#clicked ())
    end);
  let rev_history = GMenu.image_menu_item
      ~image:(GMisc.image ~pixbuf:(??? Icons.history) ())#coerce
      ~label:"Revision History" ~packing:menu#add ()
  in
  rev_history#connect#activate ~callback:begin fun () ->
    editor#with_current_page (fun page -> page#show_revision_history ())
  end |> ignore;
  let git_diff = GMenu.image_menu_item
      ~label:"Show statistics (Git)" ~packing:menu#add ()
  in
  git_diff#connect#activate ~callback:begin fun () ->
    Git.diff_stat (Git.show_diff_stat None)
  end |> ignore;
  (** Callback *)
  ignore (view#misc#connect#state_changed ~callback:begin fun _ ->
      Menu_view.update_labels
        ~code_folding
        ~select_in_outline
        ~enable_code_folding
        ~collapse_enclosing
        ~unfold_all
        ~show_whitespace_chars
        ~signal_show_whitespace_chars
        ~toggle_word_wrap
        ~signal_toggle_wrod_wrap
        ~switch_viewer ~rev_history
        editor
    end);
  (*  *)
  view, menu
;;

(** tools *)
let tools ~browser ~group ~flags items =
  let editor = browser#editor in
  let tools = GMenu.menu_item ~label:"Tools" () in
  let menu = GMenu.menu ~packing:tools#set_submenu () in
  let cursor = Gdk.Cursor.create `ARROW in
  menu#event#connect#motion_notify  ~callback:begin fun _ ->
    Gdk.Window.set_cursor menu#misc#window cursor;
    false;
  end |> ignore;
  let preferences = GMenu.image_menu_item ~label:"Preferences" ~packing:menu#add () in
  preferences#connect#activate ~callback:(fun () -> Preferences_tool.create ~editor ()) |> ignore;
  let _ = GMenu.separator_item ~packing:menu#add () in
  let toplevel = GMenu.menu_item ~label:"OCaml Toplevel" ~packing:menu#add () in
  ignore (toplevel#connect#activate ~callback:browser#shell);
  let module_browser = GMenu.menu_item ~label:"Module Browser" ~packing:menu#add () in
  let dialog_external_tools = GMenu.menu_item ~label:"External Tools" ~packing:menu#add () in
  ignore (dialog_external_tools#connect#activate ~callback:browser#dialog_external_tools);
  let _ = GMenu.separator_item ~packing:menu#add () in
  let et_items = ref [] in
  let callback _ =
    browser#with_current_project begin fun project ->
      List.iter menu#remove !et_items;
      let _, _, translate_macro =
        External_tools.get_macros
          ~get_editor:(fun () -> editor)
          ~get_current_project:(fun () -> project) ()
      in
      et_items := External_tools.inject (External_tools.read()) menu translate_macro;
      toplevel#misc#set_sensitive true;
      module_browser#misc#set_sensitive true;
      dialog_external_tools#misc#set_sensitive true;
    end;
  in
  callback();
  ignore (tools#misc#connect#state_changed ~callback);
  tools, menu
;;

(** window *)
let window ~browser ~group ~flags
    ~get_menu_item_nav_history_backward
    ~get_menu_item_nav_history_forward
    ~get_menu_item_nav_history_last
    items =
  let editor = browser#editor in
  let window = GMenu.menu_item ~label:"Window" () in
  let menu = items.window in
  let cursor = Gdk.Cursor.create `ARROW in
  menu#event#connect#motion_notify  ~callback:begin fun _ ->
    Gdk.Window.set_cursor menu#misc#window cursor;
    false;
  end |> ignore;
  window#set_submenu menu;
  (** window_switch *)
  let window_switch = GMenu.menu_item ~label:"Switch..." ~packing:menu#append () in
  ignore (window_switch#connect#activate ~callback:editor#dialog_file_select);
  (*window_switch#misc#set_sensitive (List.length !items > 0);*)
  let _ = GMenu.separator_item ~packing:menu#add () in
  (** Navigation Backward *)
  let backward = GMenu.image_menu_item ~label:"Back" ~packing:menu#append () in
  let forward = GMenu.image_menu_item ~label:"Forward" ~packing:menu#append () in
  get_menu_item_nav_history_backward := (fun () -> backward);
  get_menu_item_nav_history_forward := (fun () -> forward);
  backward#set_image (GMisc.image ~pixbuf:(??? Icons.go_back) ~icon_size:`MENU ())#coerce;
  let backward_menu = GMenu.menu ~packing:backward#set_submenu () in
  let prev = GMenu.menu_item ~label:"Previous Location" ~packing:backward_menu#add () in
  prev#add_accelerator ~group ~modi:[`MOD1] GdkKeysyms._Left ~flags;
  ignore (prev#connect#activate ~callback:(fun () ->
      browser#goto_location `PREV; browser#set_menu_item_nav_history_sensitive()));
  let _ = GMenu.separator_item ~packing:backward_menu#add () in
  browser#create_menu_history `BACK ~menu:backward_menu;
  (** Navigation Forward *)
  forward#set_image (GMisc.image ~pixbuf:(??? Icons.go_forward) ~icon_size:`MENU ())#coerce;
  let forward_menu = GMenu.menu ~packing:forward#set_submenu () in
  let next = GMenu.menu_item ~label:"Next Location" ~packing:forward_menu#add () in
  next#add_accelerator ~group ~modi:[`MOD1] GdkKeysyms._Right ~flags;
  ignore (next#connect#activate ~callback:(fun () ->
      browser#goto_location `NEXT; browser#set_menu_item_nav_history_sensitive()));
  let _ = GMenu.separator_item ~packing:forward_menu#add () in
  browser#create_menu_history `FORWARD ~menu:forward_menu;
  (** Last Edit Location *)
  let last_edit_location = GMenu.image_menu_item ~label:"Last Edit Location" ~packing:menu#append () in
  last_edit_location#add_accelerator ~group ~modi:[`MOD1] GdkKeysyms._End ~flags;
  last_edit_location#add_accelerator ~group ~modi:[`MOD1] GdkKeysyms._KP_End ~flags;
  last_edit_location#set_image (GMisc.image ~pixbuf:(??? Icons.goto_last) ~icon_size:`MENU ())#coerce;
  ignore (last_edit_location#connect#activate ~callback:(fun () -> browser#goto_location `LAST));
  get_menu_item_nav_history_last := (fun () -> last_edit_location);
  (** Clear Location History *)
  let clear_location_history = GMenu.image_menu_item ~label:"Clear Location History" () in
  menu#append (clear_location_history :> GMenu.menu_item);
  ignore (clear_location_history#connect#activate ~callback:begin fun () ->
      Location_history.clear editor#location_history;
      browser#set_menu_item_nav_history_sensitive()
    end);
  browser#set_menu_item_nav_history_sensitive();
  (**  *)
  ignore (window#misc#connect#state_changed ~callback:begin fun _ ->
      window_switch#misc#set_sensitive (List.length items.window_pages > 0);
      clear_location_history#misc#set_sensitive (Location_history.length editor#location_history > 0);
      editor#with_current_page begin fun page ->
        let current_page_oid = page#misc#get_oid in
        items.window_signal_locked <- true;
        List.iter (fun (oid, item) -> item#set_active (oid = current_page_oid)) items.window_pages;
        items.window_signal_locked <- false;
      end;
    end);
  window, menu
;;

(** help *)
let help ~browser ~group ~flags items =
  let editor = browser#editor in
  let help = GMenu.menu_item ~label:"Help" () in
  let menu = GMenu.menu ~packing:help#set_submenu () in
  let cursor = Gdk.Cursor.create `ARROW in
  menu#event#connect#motion_notify  ~callback:begin fun _ ->
    Gdk.Window.set_cursor menu#misc#window cursor;
    false;
  end |> ignore;
  let key_assist = GMenu.menu_item ~label:"Key Assist" ~packing:menu#add () in
  ignore (key_assist#connect#activate ~callback:Key_assist.window);
  let _ = GMenu.separator_item ~packing:menu#add () in
  let gc_compact = GMenu.menu_item ~label:"Force Garbage Collection" ~packing:menu#add () in
  ignore (gc_compact#connect#activate ~callback:Gc.compact);
  let clear_cache = GMenu.menu_item ~label:"Clear Editor Cache" ~packing:menu#add () in
  ignore (clear_cache#connect#activate ~callback:editor#clear_cache);
  (*if try int_of_string (List.assoc "debug" App_config.application_param) >= 1 with Not_found -> false then begin*)
  let crono = GMenu.menu_item ~label:"Print Debug Info" ~packing:menu#add () in
  crono#add_accelerator ~group ~modi:[`CONTROL; `MOD1] GdkKeysyms._apostrophe ~flags;
  ignore (crono#connect#activate ~callback:(Print_debug_info.print ~editor));
  (*end;*)
  let system_properties = GMenu.menu_item ~label:"System Properties" ~packing:menu#add () in
  ignore (system_properties#connect#activate ~callback:Menu_help.system_properties);
  let _ = GMenu.separator_item ~packing:menu#add () in
  let about = GMenu.menu_item ~label:(sprintf "About %s" About.program_name) ~packing:menu#add () in
  ignore (about#connect#activate ~callback:Menu_help.about);
  help, menu
;;

(** create *)
let create ~browser ~group
    ~get_menu_item_undo
    ~get_menu_item_redo
    ~get_menu_item_nav_history_backward
    ~get_menu_item_nav_history_forward
    ~get_menu_item_nav_history_last
    ~menu_item_view_menubar
    ~menu_item_view_toolbar
    ~menu_item_view_tabbar
    ~menu_item_view_outline
    ~menu_item_view_messages
    ~menu_item_view_hmessages
    () =
  let flags = [`VISIBLE; `LOCKED] in
  let items = {
    menus                         = [||];
    menu_items                    = [];
    file_rename                   = GMenu.menu_item ~label:"Rename" ();
    file_recent_select            = GMenu.menu_item ~label:"Select File..." ();
    file_recent_clear             = GMenu.menu_item ~label:"Clear File History" ();
    file_recent_sep               = GMenu.separator_item ();
    file_switch                   = GMenu.menu_item ~label:"Switch to Implementation/Interface" ();
    file_close                    = GMenu.image_menu_item ();
    file_close_all                = GMenu.menu_item ~label:"Close All" ();
    file_revert                   = GMenu.image_menu_item ~label:"Revert" (*~stock:`REVERT_TO_SAVED*) ();
    file_delete                   = GMenu.image_menu_item ~image:(Icons.create (??? Icons.delete_16)) ();
    window                        = GMenu.menu ();
    window_radio_group            = None;
    window_pages                  = [];
    window_signal_locked          = false;
    window_n_childs               = 1;
    project                       = GMenu.menu ();
    project_history               = [];
    project_history_signal_locked = false;
  } in
  items.file_revert#set_image (GMisc.image ~pixbuf:(??? Icons.revert_to_saved_16) ())#coerce;
  items.menus <- [|
    Menu_file.file ~browser ~group ~flags items;
    edit ~browser ~group ~flags
      ~get_menu_item_undo
      ~get_menu_item_redo
      items;
    search ~browser ~group ~flags items;
    (view ~browser ~group ~flags
       ~menu_item_view_menubar
       ~menu_item_view_toolbar
       ~menu_item_view_tabbar
       ~menu_item_view_outline
       ~menu_item_view_messages
       ~menu_item_view_hmessages items);
    Menu_project.project ~browser ~group ~flags items;
    tools ~browser ~group ~flags items;
    window ~browser ~group ~flags
      ~get_menu_item_nav_history_backward
      ~get_menu_item_nav_history_forward
      ~get_menu_item_nav_history_last
      items;
    help ~browser ~group ~flags items;
  |];
  items.menu_items <- Array.to_list (Array.map (fun (x, _) -> x) items.menus);
  List.iter (fun i -> i#activate()) items.menu_items;
  items


