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
open Utils
open Templates
open Template

type t = Templates.t list

exception Error of string * string

(** load_custom *)
let load_custom kind =
  let load filename scope =
    spec := List.filter (fun (sc, _, _, _) -> sc <> scope) !spec;
    if Sys.file_exists filename then begin
      try
        Dynlink.allow_unsafe_modules true;
        Dynlink.loadfile (Dynlink.adapt_filename filename);
        List.iter begin fun (key, descr, f) ->
          let f (view : Ocaml_text.view) =
            let start, stop = view#buffer#selection_bounds in
            let selection = view#buffer#get_text ~start ~stop () in
            let text_out = f selection in
            view#buffer#delete ~start ~stop;
            view#buffer#insert text_out;
          in
          spec := (scope, key, descr, Action f) :: !spec;
          spec := Templates.sort !spec;
        end !Template.table;
        Template.table := [];
      with Dynlink.Error error ->
        raise (Error (sprintf
                        "Cannot load custom template from file:\n\n%s" filename, (Dynlink.error_message error)));
    end
  in
  match kind with
  | `project project ->
      let filename = project.Prj.root // Oe_config.template_project_filename in
      load filename Project;
  | `user ->
      let filename = Preferences.preferences#get.editor_custom_templ_filename in
      if filename <> "" then load filename User
;;

(** apply *)
let apply ~project (view : Ocaml_text.view) (templ : Templates.t) =
  view#tbuffer#undo#begin_block ~name:"Templates.apply";
  let buffer = view#obuffer in
  let start, stop = buffer#selection_bounds in
  let bol = start#set_line_index 0 in
  let mark_begin = buffer#create_mark(* ~name:(Gtk_util.create_mark_name "Templ.apply1")*) bol in
  let bol = bol#forward_find_char ((<>) 32) in
  let base = bol#line_index in
  let selection = buffer#get_text ~start ~stop () in
  let indent_width = ref 0 in
  let mark_i = ref None in
  let mark_s = ref None in
  let insert_block text = function
    | `ALIGN n ->
        let indent = (Alignment.mk_spaces (base + n)) in
        let text = Str.global_replace (Utils.regexp "\n\\(.\\)") ("\n" ^ indent ^ "\\1") text in
        let text = indent ^ text in
        buffer#insert text
    | `INDENT ->
        let indent = (Alignment.mk_spaces !indent_width) in
        let text = Str.global_replace (Utils.regexp "\n\\(.\\)") ("\n" ^ indent ^ "\\1") text in
        let text = indent ^ text in
        buffer#insert text
    | `NONE -> buffer#insert text
  in
  (* Parse template *)
  begin
    match templ with
    | Templ templ ->
        ignore (buffer#delete_selection ());
        List.iter begin function
        | I ->
            Gaux.may !mark_i ~f:(fun mark -> buffer#delete_mark (`MARK mark));
            mark_i := Some (buffer#create_mark(* ~name:(Gtk_util.create_mark_name "Templ.apply2")*) (buffer#get_iter `INSERT));
        | S ->
            Gaux.may !mark_s ~f:(fun mark -> buffer#delete_mark (`MARK mark));
            mark_s := Some (buffer#create_mark(* ~name:(Gtk_util.create_mark_name "Templ.apply3")*) (buffer#get_iter `INSERT));
        | NL -> buffer#insert "\n";
        | IN -> indent_width := !indent_width + view#obuffer#tab_width;
        | OUT -> indent_width := !indent_width - view#obuffer#tab_width;
        | T text -> buffer#insert text
        | T0 text -> insert_block text (`ALIGN 0);
        | TI text -> insert_block text (`ALIGN !indent_width);
        | SELECTION ->
            let is_block = String.contains selection '\n' in
            if is_block then begin
              insert_block selection `INDENT;
            end else begin
              buffer#insert selection;
            end
        | SELECTION_TRIM -> buffer#insert (String.trim selection)
        | SELECTION_OPT def ->
            let selection = String.trim selection in
            buffer#insert (if selection = "" then def else selection)
        | CURRENT_FILENAME ->
            Gaux.may view#obuffer#file ~f:(fun file -> buffer#insert file#basename)
        | CURRENT_LINE -> buffer#insert (string_of_int ((buffer#get_iter `INSERT)#line + 1))
        | DESCRIPTION -> buffer#insert project.Prj.description
        end templ;
    | Action func -> func view
  end;
  let mark_end = buffer#create_mark(* ~name:(Gtk_util.create_mark_name "Templ.apply4")*) (buffer#get_iter `INSERT) in
  (* Place cursor *)
  (match !mark_i, !mark_s with
   | None, None -> ()
   | (Some mark), None -> buffer#place_cursor ~where:(buffer#get_iter_at_mark (`MARK mark));
   | None, (Some mark) -> buffer#place_cursor ~where:(buffer#get_iter_at_mark (`MARK mark));
   | (Some mi), (Some ms) -> buffer#select_range (buffer#get_iter_at_mark (`MARK mi)) (buffer#get_iter_at_mark (`MARK ms)));
  view#tbuffer#undo#end_block();
  Gaux.may !mark_i ~f:(fun mark -> buffer#delete_mark (`MARK mark));
  Gaux.may !mark_s ~f:(fun mark -> buffer#delete_mark (`MARK mark));
  (* Indent block *)
  let start = buffer#get_iter_at_mark (`MARK mark_begin) in
  let start = start#set_line_index 0 in
  let stop = buffer#get_iter_at_mark (`MARK mark_end) in
  let stop = stop#forward_line#set_line_index 0 in
  ignore (Ocp_indent.indent ~project ~view (`BOUNDS (start, stop)));
  (* Colorize *)
  let remove_marks () =
    buffer#delete_mark (`MARK mark_begin);
    buffer#delete_mark (`MARK mark_end);
  in
  if buffer#lexical_enabled then begin
    let start, stop =
      match view#current_matching_tag_bounds with
      | [_,d; a,_] ->
          (buffer#get_iter_at_mark (`MARK a)), (buffer#get_iter_at_mark (`MARK d))
      | _ -> (buffer#get_iter_at_mark (`MARK mark_begin)), ((buffer#get_iter_at_mark (`MARK mark_end)))
    in
    Lexical.tag view#buffer ~start ~stop;
    (*Lexical.tag view#buffer
      ~start:(buffer#get_iter_at_mark (`MARK mark_begin))
      ~stop:(buffer#get_iter_at_mark (`MARK mark_end));*)
  end;
  remove_marks();;

(** widget *)
class widget ~project ~(view : Ocaml_text.view) ?packing ()=
  let vbox = GPack.vbox ~spacing:5 ~border_width:0 ?packing () in
  let cols = new GTree.column_list in
  let col_key  = cols#add Gobject.Data.string in
  let col_name  = cols#add Gobject.Data.string in
  let col_descr  = cols#add Gobject.Data.string in
  let model = GTree.list_store cols in
  let renderer = GTree.cell_renderer_text [(*`CELL_BACKGROUND Preferences.preferences#get.Preferences.pref_bg_color_popup*)] in
  let vc_name = GTree.view_column ~renderer:(renderer, ["markup", col_name]) ~title:"Name" () in
  let vc_descr = GTree.view_column ~renderer:(renderer, ["markup", col_descr]) ~title:"Description" () in
  let sw = GBin.scrolled_window ~shadow_type:`NONE ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:vbox#add () in
  let lview = GTree.view ~model ~headers_visible:true ~reorderable:false (*~width:800 ~height:200 *)~packing:sw#add () in
  let _ = lview#append_column vc_name in
  let _ = lview#append_column vc_descr in
  let _ = lview#set_headers_visible false in
  let popover = Gtk_util.popover_at_iter ~width:700 ~height:200 ~view:view#as_gtext_view vbox#coerce in
  object (self)
    inherit GObj.widget vbox#as_widget

    initializer
      (*lview#misc#modify_base [`NORMAL, `NAME Preferences.preferences#get.Preferences.pref_bg_color_popup];*)
      let font_name = Preferences.preferences#get.editor_base_font in
      let family =
        String.sub font_name 0 (Option.value (String.rindex_opt font_name ' ') ~default:(String.length font_name))
      in
      List.iter begin fun (_, name, descr, templ) ->
        let row = model#append () in
        model#set ~row ~column:col_key name;
        model#set ~row ~column:col_name
          (sprintf {|<b><span face="%s" size="small">%s</span></b>|} family (Glib.Markup.escape_text name));
        model#set ~row ~column:col_descr (
          sprintf {|<span face="%s" size="small">%s</span>|} family (Glib.Markup.escape_text descr));
      end !Templates.spec;
      lview#connect#row_activated ~callback:begin fun path _ ->
        let row = model#get_iter path in
        let name = model#get ~row ~column:col_key in
        try
          let _, _, _, templ = List.find (fun (_, x, _, _) -> name = x) !Templates.spec in
          apply ~project view templ;
          view#misc#grab_focus();
          popover.Gtk_util.popdown();
          GMain.Timeout.add ~ms:1000 ~callback:(fun () -> popover.Gtk_util.destroy(); false) |> ignore;
        with Not_found -> ()
      end |> ignore;

    method show () =
      popover.Gtk_util.popup (view#buffer#get_iter `INSERT);
      Gmisclib.Idle.add lview#misc#grab_focus;
  end

(** popup *)
let popup project (view : Ocaml_text.view) =
  let widget = new widget ~project ~view () in
  widget#show()
