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
    let _ = GMisc.image ~pixbuf:(??? Icons.search_results_16) ~packing:hbox#pack () in
    let label = GMisc.label ~packing:hbox#pack () in
    ignore (page#connect#search_started ~callback:begin fun () ->
        try
          if page#misc#parent = None then
            (ignore (Messages.vmessages#append_page ~label_widget:hbox#coerce page#as_page));
          label#set_text page#text_to_find;
          page#set_title page#text_to_find;
          if page#icon = None then page#set_icon (Some (??? Icons.search_results_16));
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

(** set_has_definition *)
let set_has_definition editor item =
  editor#with_current_page begin fun page ->
    let def =
      Definition.locate
        ~filename:page#get_filename
        ~text:(page#buffer#get_text ?start:None ?stop:None ?slice:None ?visible:None ())
        ~iter: (page#buffer#get_iter `INSERT)
    in
    match def with
    | Merlin.Ok def -> item#misc#set_sensitive (def <> None);
    | Merlin.Failure _ -> ()
    | Merlin.Error _ -> ()
  end

(** set_has_references *)
let set_has_references editor item =
  editor#with_current_page begin fun page ->
    let filename : string = page#get_filename in
    let buffer : Ocaml_text.buffer = page#buffer in
    let text = buffer#get_text () in
    let iter = buffer#get_iter `INSERT in
    let line = iter#line + 1 in
    let col = iter#line_offset in
    Definition.references ~filename ~text ~iter
    |> Async.start_with_continuation
      (fun ranges -> item#misc#set_sensitive (ranges <> []))
  end

(** create_search_results_pane *)
let create_search_results_pane ~pixbuf ~editor ~page =
  let widget = new Search_results.widget ~editor () in
  widget#button_new_search#misc#set_sensitive false;
  let hbox = GPack.hbox ~spacing:1 () in
  let _ = GMisc.image ~pixbuf ~packing:hbox#pack () in
  let label = GMisc.label ~packing:hbox#pack () in
  let iter = page#buffer#get_iter `INSERT in
  let mark = page#buffer#create_mark ?name:None ?left_gravity:None iter in
  widget#misc#connect#destroy ~callback:(fun () -> page#buffer#delete_mark (`MARK mark)) |> ignore;
  widget#connect#after#search_started ~callback:begin fun () ->
    if widget#misc#parent = None then
      Messages.vmessages#append_page ~label_widget:hbox#coerce widget#as_page |> ignore;
    widget#present ();
  end |> ignore;
  widget, mark, label

(** find_definition_references *)
let find_definition_references editor =
  editor#with_current_page begin fun page ->
    let widget, mark, label = create_search_results_pane ~pixbuf:(??? Icons.references) ~editor ~page in
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
              (Some (??? Icons.edit),
               { range with
                 start = { range.start with line = range.start.line + 1};
                 stop = { range.stop with line = range.stop.line + 1 } })
        | Merlin.Ok None
        | Merlin.Failure _
        | Merlin.Error _ -> None
      in
      Definition.references ~filename ~text ~iter
      |> Async.start_with_continuation begin fun ranges ->
        let ranges = ranges |> List.map (fun r -> (None : GdkPixbuf.pixbuf option), r) in
        let ranges =
          match def_range with
          | Some (_, dr) ->
              let is_def_range (_, r) = r.start.line = dr.start.line && r.start.col = dr.start.col && r.stop.line = dr.stop.line in
              ranges |> List.map (fun ((_, r) as r') -> if is_def_range r' then Some (??? Icons.edit), r else r')
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
        if widget#icon = None then widget#set_icon (Some (??? Icons.references));
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
    set_has_definition editor find_definition;
    set_has_references editor find_references;
  end
;;

