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
open Find_text
open Preferences

(** create *)
let create ~project ~editor ?(buffer : GText.buffer option) ?widget
    ?(search_word_at_cursor=false)
    ?(find_all=false) () =
  let buffer_has_selection =
    match buffer with Some buffer -> buffer#has_selection | _ -> false
  in
  let search_in_path = buffer = None in
  let title = if search_in_path then "Find/Replace in Path" else "Find/Replace Text" in
  let dialog = GWindow.window ~title ~icon:(??? Icons.oe) ~width:600 ~position:`CENTER
      ~type_hint:`DIALOG ~border_width:8 ~modal:true ~show:false () in
  let key = if search_in_path then "dialog-find-text-path" else "dialog-find-text-buffer" in
  Gmisclib.Window.GeometryMemo.add ~key ~window:dialog Preferences.geometry_memo;
  let _ = dialog#set_skip_taskbar_hint true in
  let _ = dialog#set_skip_pager_hint true in
  let hbox = GPack.hbox ~spacing:13 ~packing:dialog#add () in
  let vbox = GPack.vbox ~spacing:13 ~packing:hbox#add () in
  let table = GPack.table ~row_spacings:5 ~col_spacings:5 ~packing:(vbox#pack ~expand:false) () in
  let label = GMisc.label ~text:"Text to find: " ~xalign:0.0 ~packing:(table#attach ~top:0 ~left:0) () in
  let entry_find = GEdit.combo_box_entry ~wrap_width:3
      ~model:status.h_find.model ~text_column:status.h_find.column
      ~focus_on_click:false ~packing:(table#attach ~top:0 ~left:1 ~expand:`X) () in
  entry_find#entry#misc#modify_font_by_name "monospace";
  let label_repl = GMisc.label ~text:"Replace with: " ~xalign:0.0 ~packing:(table#attach ~top:1 ~left:0) () in
  let entry_repl = GEdit.combo_box_entry ~focus_on_click:false ~wrap_width:3
      ~model:status.h_repl.model ~text_column:status.h_repl.column
      ~packing:(table#attach ~top:1 ~left:1 ~expand:`X) () in
  entry_repl#entry#misc#modify_font_by_name "monospace";
  (** check_use_regexp *)
  let table = GPack.table ~row_spacings:5 ~col_spacings:5 ~packing:(vbox#pack ~expand:false) () in
  let check_use_regexp = GButton.check_button
      ~active:status.use_regexp
      ~packing:(table#attach ~top:0 ~left:0 ~expand:`X) () in
  let _ = check_use_regexp#add (GMisc.label ~markup:"Use regular expressions (<tt>Str</tt> syntax)" ())#coerce in
  (** check_case_sensitive *)
  let check_case_sensitive = GButton.check_button ~label:"Case sensitive"
      ~active:status.case_sensitive
      ~packing:(table#attach ~top:1 ~left:0 ~expand:`X) () in
  (** check_search_backward *)
  let check_search_backward = GButton.check_button ~label:"Search backward"
      ~show:(not search_in_path)
      ~active:(status.direction = Backward)
      ~packing:(table#attach ~top:0 ~left:1 ~expand:`X) () in
  (** check_selected_text_only *)
  let check_selected_text_only = GButton.check_button ~label:"Selected text only"
      ~packing:(table#attach ~top:1 ~left:1 ~expand:`X)
      ~show:(not search_in_path && buffer_has_selection) () in
  (* check_match_whole_word *)
  let check_match_whole_word = GButton.check_button ~label:"Match whole word"
      ~active:status.match_whole_word
      ~packing:(table#attach ~top:2 ~left:0 ~expand:`X) () in
  (** Search path *)
  let frame = GBin.frame ~label:" Search path " ~packing:(vbox#pack ~expand:false) ~show:search_in_path () in
  let pbox = GPack.vbox ~spacing:5 ~border_width:5 ~packing:frame#add () in
  let box = GPack.hbox ~spacing:5 ~packing:pbox#add () in
  (** radio_specified_path *)
  let radio_specified_path = GButton.radio_button ~label:"Specified directory:" ~active:false
      ~packing:(box#pack ~expand:false) () in
  let entry_specified_path = GEdit.combo_box_entry
      ~focus_on_click:false
      ~model:status.h_path.model ~text_column:status.h_path.column
      ~packing:(box#pack ~expand:true) () in
  let _ = entry_specified_path#entry#set_editable false in
  let label = GMisc.label ~markup:(sprintf "<big>%s</big>" (Convert.to_utf8 "  ...  ")) () in
  let button_path = GButton.button ~packing:(box#pack ~expand:false) () in
  button_path#add label#coerce;
  ignore (button_path#connect#clicked ~callback:begin fun () ->
      let dialog = GWindow.file_chooser_dialog ~action:`SELECT_FOLDER ~position:`CENTER ~title:"Select folder..." () in
      dialog#set_current_folder (Project.path_src project) |> ignore;
      dialog#add_button_stock `OK `OK;
      dialog#add_button_stock `CANCEL `CANCEL;
      match dialog#run () with
      | `OK ->
          Gaux.may dialog#filename ~f:(fun name -> entry_specified_path#entry#set_text name);
          dialog#destroy()
      | _ -> dialog#destroy()
    end);
  let group = radio_specified_path#group in
  (** radio_project_src *)
  let radio_project_src = GButton.radio_button ~label:"Project source path" ~active:false ~group ~packing:pbox#add () in
  (** radio_only_open_files *)
  let radio_only_open_files = GButton.radio_button ~label:"Only open files" ~active:false ~group ~packing:pbox#add () in
  (** Filename Pattern *)
  let box = GPack.hbox ~packing:vbox#pack ~spacing:21 ~show:search_in_path () in
  (** check_recursive *)
  let check_recursive = GButton.check_button
      ~label:"Include subdirectories"
      ~active:status.recursive
      ~packing:box#pack () in
  let pbox = GPack.hbox ~packing:box#add () in
  let check_pattern = GButton.check_button
      ~label:"Restrict to: "
      ~active:(status.pattern <> None)
      ~packing:pbox#pack () in
  let entry_pattern = GEdit.combo_box_entry
      ~focus_on_click:false
      ~model:status.h_pattern.model ~text_column:status.h_pattern.column
      ~packing:pbox#add () in
  entry_pattern#entry#set_text begin
    match status.h_pattern.model#get_iter_first with
    | None -> ""
    | Some row -> status.h_pattern.model#get ~row ~column:status.h_pattern.column
  end;
  let enable_entry_pattern () =
    if check_pattern#active && check_pattern#misc#get_flag `SENSITIVE then begin
      entry_pattern#misc#set_sensitive true;
      entry_pattern#entry#misc#grab_focus()
    end else begin
      entry_pattern#misc#set_sensitive false;
    end
  in
  ignore (check_pattern#connect#toggled ~callback:enable_entry_pattern);
  (**  *)
  ignore (radio_specified_path#connect#toggled ~callback:begin fun () ->
      entry_specified_path#misc#set_sensitive radio_specified_path#active;
      button_path#misc#set_sensitive radio_specified_path#active;
    end);
  ignore (radio_only_open_files#connect#toggled ~callback:begin fun () ->
      check_recursive#misc#set_sensitive (not radio_only_open_files#active);
      check_pattern#misc#set_sensitive (not radio_only_open_files#active);
      enable_entry_pattern();
    end);
  let callback () =
    let path = match status.h_path.model#get_iter_first with
      | None -> ""
      | Some row -> status.h_path.model#get ~row ~column:status.h_path.column
    in
    entry_specified_path#entry#set_text path;
  in
  ignore (radio_project_src#connect#pressed ~callback);
  ignore (radio_specified_path#connect#pressed ~callback);
  ignore (radio_only_open_files#connect#pressed ~callback);
  begin
    match status.path with
    | Project_source -> radio_project_src#set_active true; callback()
    | Only_open_files -> radio_only_open_files#set_active true; callback();
    | Specified path -> radio_specified_path#set_active true; callback()
  end;
  (* Button box *)
  let bbox = GPack.button_box ~spacing:5 `VERTICAL ~layout:`START ~packing:(hbox#pack ~expand:false) () in
  let button_find = GButton.button ~label:"Find" ~packing:bbox#add () in
  let button_find_all = GButton.button ~label:"Find All" ~packing:bbox#add ~show:(buffer <> None) () in
  let button_repl = GButton.button ~label:"Replace..." ~packing:bbox#add () in
  let button_cancel = GButton.button ~label:"Cancel" ~packing:bbox#add () in
  bbox#set_child_secondary button_cancel#coerce true;
  button_find_all#misc#set_tooltip_text "Ctrl+Return";
  begin
    try
      entry_find#entry#set_text (status.h_find.model#get
                                   ~row:(status.h_find.model#get_iter (GTree.Path.create [0]))
                                   ~column:status.h_find.column);
    with Failure _ -> ()
  end;
  (* Default values in entries *)
  let text =
    if search_word_at_cursor then begin
      match editor#get_page `ACTIVE
      with None -> "" | Some page ->
        let start, stop = page#buffer#select_word ?iter:None ?pat:(Some Ocaml_word_bound.regexp) ?select:(Some false) ?search:None () in
        page#buffer#get_text ?start:(Some start) ?stop:(Some stop) ?slice:None ?visible:None ()
    end else ""
  in
  let text =
    match editor#get_page (`ACTIVE)
    with Some page when page#buffer#has_selection -> page#buffer#selection_text () | _ -> text
  in
  let is_multiline = String.contains text '\n' in
  check_selected_text_only#set_active ((*text <> ""*)is_multiline);
  let selected_text_bounds =
    begin
      if text <> "" then begin
        match buffer with None -> None | Some buffer ->
          let i1, i2 = buffer#selection_bounds in
          Some (if i1#compare i2 <= 0 then (i1, i2) else (i2, i1))
      end else None
    end;
  in
  if String.length text > 0 && not is_multiline then begin
    entry_find#entry#set_text text;
  end;
  entry_find#entry#misc#grab_focus();
  if entry_pattern#entry#text = "" then (entry_pattern#entry#set_text "*.{ml,mli,mll,mly,txt}");
  let callback () =
    let not_empty = String.length entry_find#entry#text > 0 in
    button_find#misc#set_sensitive not_empty;
    button_repl#misc#set_sensitive not_empty;
    button_find_all#misc#set_sensitive not_empty;
  in
  entry_find#entry#connect#changed ~callback |> ignore;
  ignore (entry_find#entry#event#connect#key_press ~callback:begin fun ev ->
      if GdkEvent.Key.keyval ev = GdkKeysyms._Tab then (entry_repl#entry#misc#grab_focus(); true)
      else false
    end);
  callback();
  (*  *)
  let widget = match widget with
    | None -> new Find_text_output.widget ~dialog ?buffer ~editor ()
    | Some x -> x
  in
  let set_options () =
    update_status
      ~project
      ~text_find:entry_find#entry#text
      ~text_repl:entry_repl#entry#text
      ~use_regexp:check_use_regexp#active
      ~case_sensitive:check_case_sensitive#active
      ~match_whole_word:check_match_whole_word#active
      ~direction:(if check_search_backward#active then Backward else Forward)
      ~path:begin
        if radio_specified_path#active then Specified entry_specified_path#entry#text
        else if radio_project_src#active then Project_source
        else if radio_only_open_files#active then Only_open_files
        else assert false
      end
      ~recursive:check_recursive#active
      ~pattern:(if check_pattern#active then (Some entry_pattern#entry#text) else None) ();
    let new_text_find = new GUtil.variable status.text_find#get in
    widget#set_options {status with text_find = new_text_find};
    widget#set_selected_text_bounds (if check_selected_text_only#active then selected_text_bounds else None);
  in
  let callback ?all () =
    set_options();
    dialog#misc#hide();
    ignore (Thread.create (fun () -> widget#find ?all ()) ());
  in
  button_find_all#connect#clicked ~callback:(fun () -> callback ~all:true ()) |> ignore;
  button_find#connect#clicked ~callback:begin fun () ->
    match buffer with
    | None -> button_find_all#clicked()
    | _ ->
        callback ~all:false ();
        dialog#destroy();
  end |> ignore;
  button_repl#connect#clicked ~callback:begin fun () ->
    set_options();
    dialog#misc#hide();
    widget#find();
    widget#replace();
  end |> ignore;
  button_cancel#connect#clicked ~callback:begin fun () ->
    if widget#misc#parent <> None then (dialog#misc#hide ()) else (dialog#destroy ());
  end |> ignore;
  dialog#event#connect#key_press ~callback:begin fun ev ->
    if GdkEvent.Key.keyval ev = GdkKeysyms._Escape then begin
      button_cancel#clicked();
      true;
    end else if GdkEvent.Key.keyval ev = GdkKeysyms._Return && GdkEvent.Key.state ev = [`CONTROL] then begin
      if String.length entry_find#entry#text > 0 then begin
        button_find_all#clicked();
        true;
      end else false;
    end else if GdkEvent.Key.keyval ev = GdkKeysyms._Return then begin
      if String.length entry_find#entry#text > 0 then begin
        button_find#clicked();
        true;
      end else false;
    end else false;
  end |> ignore;
  if find_all then (button_find_all#clicked());
  (*  let callback () =
      button_find#misc#set_sensitive (not radio_specified_path#active || String.length entry_specified_path#entry#text > 0)
      in
      radio_specified_path#connect#pressed ~callback;
      radio_project_src#connect#pressed ~callback;
      callback();*)
  dialog, widget
