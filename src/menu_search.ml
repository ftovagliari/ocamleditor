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
open Preferences

(** find_replace *)
let find_replace
    ?(find_all=false)
    ?(find_in_buffer=true)
    ?(search_word_at_cursor=(Preferences.preferences#get.editor_search_word_at_cursor))
    editor =
  editor#with_current_page begin fun page ->
    let buffer = if find_in_buffer then Some page#view#buffer else None in
    let dialog, page = Find_text_dialog.create ?buffer ~editor
        ~project:editor#project ~find_all ~search_word_at_cursor ()
    in
    Gaux.may (GWindow.toplevel editor) ~f:(fun tl -> dialog#set_transient_for tl#as_window);
    let hbox = GPack.hbox ~spacing:1 () in
    let _ = Gtk_util.label_icon ~packing:hbox#pack Icon_set.search_find_all in
    let label = GMisc.label ~packing:hbox#pack () in
    ignore (page#connect#search_started ~callback:begin fun () ->
        try
          if page#misc#parent = None then
            (ignore ((Messages.vmessages())#append_page ~label_widget:hbox#coerce page#as_page));
          label#set_text page#text_to_find;
          page#set_title page#text_to_find;
          page#present ();
        with ex -> Printf.eprintf "File \"menu_search.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());
      end);
    ignore (page#connect#search_finished ~callback:begin fun () ->
        page#is_working#set false;
        page#present();
      end);
    if not find_all then (dialog#show())
  end

(** find_prev *)
let find_prev editor =
  try
    editor#with_current_page (fun page ->
        Find_text_in_buffer.find Find_text.Backward
          ~view:page#view ~canceled:(fun () -> false))
  with Find_text.No_current_regexp -> ()

(** find_next *)
let find_next editor =
  editor#with_current_page begin fun page ->
    try
      Find_text_in_buffer.find Find_text.Forward
        ~view:page#view ~canceled:(fun () -> false)
    with Find_text.No_current_regexp -> find_replace editor
  end

(** search_again *)
let search_again editor =
  try
    editor#with_current_page begin fun page ->
      Find_text_in_buffer.find Find_text.status.Find_text.direction
        ~view:page#view ~canceled:(fun () -> false)
    end
  with Find_text.No_current_regexp -> find_replace editor

(** set_has_definition

    The answer comes from Merlin in a background thread: [set_sensitive] is called later, from the
    GTK main loop. *)
let set_has_definition_with page set_sensitive =
  Definition.locate_async
    ~filename:page#get_filename
    ~text:(page#buffer#get_text ?start:None ?stop:None ?slice:None ?visible:None ())
    ~iter:(page#buffer#get_iter `INSERT)
  |> Async.start_with_continuation begin function
  | Merlin.Ok def -> Gmisclib.Idle.add ~prio:100 (fun () -> set_sensitive (def <> None))
  | Merlin.Failure _ -> ()
  | Merlin.Error _ -> ()
  end

let set_has_definition page item = set_has_definition_with page item#misc#set_sensitive

(** set_has_references *)
let set_has_references_with page set_sensitive =
  let filename : string = page#get_filename in
  let buffer : Ocaml_text.buffer = page#buffer in
  let text = buffer#get_text () in
  let iter = buffer#get_iter `INSERT in
  Definition.references ~filename ~text ~iter
  |> Async.start_with_continuation
    (fun ranges -> Gmisclib.Idle.add ~prio:100 (fun () -> set_sensitive (ranges <> [])))

let set_has_references page item = set_has_references_with page item#misc#set_sensitive

(** create_search_results_pane *)
let create_search_results_pane ~icon ~editor ~page =
  let widget = new Search_results.widget ~editor () in
  widget#button_new_search#misc#set_sensitive false;
  let hbox = GPack.hbox ~spacing:1 () in
  let _ = Gtk_util.label_icon ~packing:hbox#pack icon in
  let label = GMisc.label ~packing:hbox#pack () in
  let iter = page#buffer#get_iter `INSERT in
  let mark = page#buffer#create_mark ?name:None ?left_gravity:None iter in
  widget#misc#connect#destroy ~callback:(fun () -> page#buffer#delete_mark (`MARK mark)) |> ignore;
  widget#connect#after#search_started ~callback:begin fun () ->
    if widget#misc#parent = None then
      (Messages.vmessages())#append_page ~label_widget:hbox#coerce widget#as_page |> ignore;
    widget#present ();
  end |> ignore;
  widget, mark, label

(** find_definition_references *)
let find_definition_references editor =
  editor#with_current_page begin fun page ->
    let widget, mark, label = create_search_results_pane ~icon:Icon_set.find_references ~editor ~page in
    widget#set_title "TITLE";
    widget#connect#search_started ~callback:begin fun () ->
      let filename : string = page#get_filename in
      let buffer : Ocaml_text.buffer = page#buffer in
      let iter = buffer#get_iter `INSERT in
      let text = buffer#get_text () in
      let wstart, wstop = buffer#select_word ~pat:Ocaml_word_bound.regexp ~select:false () in
      let ident = buffer#get_text ~start:wstart ~stop:wstop () in
      let open Merlin_j in
      let def_range =
        match Definition.locate ~filename ~text ~iter with
        | Merlin.Ok (Some range) ->
            Some
              (Some Icon_set.edit,
               { range with
                 start = { range.start with line = range.start.line + 1};
                 stop = { range.stop with line = range.stop.line + 1 } })
        | Merlin.Ok None
        | Merlin.Failure _
        | Merlin.Error _ -> None
      in
      Definition.references ~filename ~text ~iter
      |> Async.start_with_continuation begin fun ranges ->
        let ranges = ranges |> List.map (fun r -> (None : string option), r) in
        let ranges =
          match def_range with
          | Some (_, dr) ->
              let is_def_range (_, r) = r.start.line = dr.start.line && r.start.col = dr.start.col && r.stop.line = dr.stop.line in
              ranges |> List.map (fun ((_, r) as r') -> if is_def_range r' then Some Icon_set.edit, r else r')
          | _ -> (* Already at definition point: definition range is returned but we don't know what it is *)
              ranges
        in
        ranges
        |> Utils.ListExt.group_by (fun (_, r) -> r.file)
        |> List.map begin fun (file, ranges) ->
          let filename = match file with Some x -> x | _ -> filename in
          let timestamp = (Unix.stat filename).Unix.st_mtime in
          { Search_results.filename;
            real_filename = filename;
            locations = Search_results.Offset ranges;
            timestamp };
        end
        |> List.sort compare
        |> widget#set_results;
        label#set_text ident;
        ksprintf widget#label_message#set_label
          "References to <tt>%s</tt>" (Glib.Markup.escape_text ident);
        widget#next_file()
      end
    end |> ignore;
    widget#start_search();
  end;;

(** update_items_visibility *)
let update_items_visibility
    (*~label_find_used_components
      ~find_used_components*)
    ~find_definition
    ~find_references
    editor =
  Gmisclib.Idle.add ~prio:100 begin fun () ->
    (*set_has_used_components editor label_find_used_components find_used_components;*)
    editor#with_current_page begin fun page ->
      set_has_definition page find_definition;
      set_has_references page find_references;
    end
  end
;;

(** {1 "Search" menu} *)

let register ~browser =
  let open GdkKeysyms in
  let editor = browser#editor in
  let has_page () = editor#get_page `ACTIVE <> None in
  let ctrl = [`CONTROL] and ctrl_shift = [`CONTROL; `SHIFT] in
  let reg id label ?accels ?enabled ?on_menu_open run =
    ignore (Command.register ~id:("search." ^ id) ~label ?accels ?enabled ?on_menu_open run)
  in
  reg "find_replace" "Find and Replace" ~enabled:has_page (fun () -> find_replace editor);
  reg "find_next" "Find Next" ~accels:[[], _F3] ~enabled:has_page (fun () -> find_next editor);
  reg "find_prev" "Find Previous" ~accels:[[`SHIFT], _F3] ~enabled:has_page (fun () -> find_prev editor);
  reg "search_again" "Search Again" ~accels:[ctrl, _F3] ~enabled:has_page (fun () -> search_again editor);
  reg "find_all" "Find All" ~accels:[[`MOD1], _f] ~enabled:has_page begin fun () ->
    find_replace ~find_all:true ~search_word_at_cursor:true editor
  end;
  (* Incremental search: a second invocation falls back to the full dialog. *)
  let is_activated = ref None in
  ignore (editor#connect#switch_page ~callback:(fun _ -> !is_activated |> Option.iter (fun c -> c#destroy())));
  reg "incremental" "Search Incremental" ~accels:[ctrl, _f] ~enabled:has_page begin fun () ->
    match !is_activated with
    | Some child ->
        child#destroy();
        is_activated := None;
        find_replace editor
    | _ ->
        is_activated := editor#i_search ();
        !is_activated |> Option.iter begin fun child ->
          ignore (child#misc#connect#destroy ~callback:(fun () -> is_activated := None))
        end
  end;
  reg "find_in_path" "Find/Replace in Path" ~accels:[ctrl_shift, _f; ctrl, _p] begin fun () ->
    find_replace ~find_in_buffer:false ~search_word_at_cursor:true editor
  end;
  reg "clear_history" "Clear Find/Replace History" Find_text.clear_history;
  reg "goto_line" "Go to Line..." ~accels:[ctrl, _g] ~enabled:has_page begin fun () ->
    editor#with_current_page (fun page -> page#ocaml_view#goto ())
  end;
  (* The availability of definition/references is only known to Merlin (slow, asynchronous): it is
     shown as a hint while the menu is open, but never blocks the shortcut. *)
  reg "find_definition" "Find Definition" ~accels:[[], _F12] ~enabled:has_page
    ~on_menu_open:begin fun set_sensitive ->
      editor#with_current_page (fun page -> set_has_definition_with page set_sensitive)
    end
    begin fun () ->
      editor#with_current_page (fun page ->
          ignore (editor#scroll_to_definition ~page ~iter:(page#buffer#get_iter `INSERT)))
    end;
  reg "find_references" "Find References" ~accels:[[`SHIFT], _F12] ~enabled:has_page
    ~on_menu_open:begin fun set_sensitive ->
      editor#with_current_page (fun page -> set_has_references_with page set_sensitive)
    end
    (fun () -> find_definition_references editor);
  reg "find_file" "Find File..." ~accels:[ctrl_shift, _L] (fun () -> browser#dialog_find_file ?all:(Some true) ());
  List.iter2 begin fun num key ->
    reg (sprintf "goto_bookmark_%d" num) (sprintf "Goto Bookmark %d" num) ~accels:[ctrl, key]
      (fun () -> editor#bookmark_goto ~num);
    reg (sprintf "set_bookmark_%d" num) (sprintf "Bookmark %d" num) ~accels:[[`MOD1; `CONTROL], key]
      (fun () -> editor#bookmark_create ~num ?where:None ())
  end [1; 2; 3; 4; 5; 6; 7; 8; 9; 0] [_1; _2; _3; _4; _5; _6; _7; _8; _9; _0]

(** Creates the Search menu. *)
let create ~browser ~group =
  register ~browser;
  let nums = [1; 2; 3; 4; 5; 6; 7; 8; 9; 0] in
  let bookmarks =
    List.map (fun n -> Menu_builder.item (sprintf "search.goto_bookmark_%d" n)) nums
    @ [Menu_builder.separator]
    @ List.map (fun n -> Menu_builder.item (sprintf "search.set_bookmark_%d" n)) nums
  in
  Menu_builder.create ~group ~label:"Search" Menu_builder.[
      item "search.find_replace"; item "search.find_next"; item "search.find_prev";
      item "search.search_again"; item "search.find_all"; item "search.incremental";
      item "search.find_in_path"; item "search.clear_history";
      separator;
      item "search.goto_line"; item "search.find_definition"; item "search.find_references";
      item "search.find_file";
      separator;
      sub "Bookmarks" bookmarks;
    ]
