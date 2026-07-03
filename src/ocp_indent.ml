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
open Option_syntax

let ( // ) = Filename.concat

let file_exists files =
  let exception File_found of string in
  try
    List.iter (fun file -> if Sys.file_exists file then raise (File_found file)) files;
    None
  with File_found file -> Some file

let find_ocp_indent_config' project =
  let root = Prj.(project.root) in
  let+ config_file = file_exists [ root // "src" // ".ocp-indent"; root // ".ocp-indent" ] in
  try
    Some (Printexc.print File_util.read config_file |> Buffer.contents)
  with _ -> None

let find_ocp_indent_config = Utils.Memo.fast ~f:find_ocp_indent_config'

let indent_config ~project ~pref ?(syntaxes=[]) () =
  Approx_lexer.disable_extensions ();
  List.iter
    (fun syntax -> try Approx_lexer.enable_extension syntax
      with IndentExtend.Syntax_not_found name ->
        Format.eprintf "Warning: unknown syntax extension %S@." name)
    syntaxes;
  match find_ocp_indent_config project with
  | Some file_config -> IndentConfig.(update_from_string default file_config)
  | None ->
      let config = Settings_t.(pref.editor_indent_config) in
      match String.trim config with
      | "" -> IndentConfig.default
      | editor_config  -> IndentConfig.(update_from_string default editor_config)

let collect (n : int) offsets = n :: offsets

let output ~project ~pref ?syntaxes start stop = IndentPrinter.{
    debug = false;
    config = indent_config ~project ~pref ?syntaxes ();
    in_lines = (fun n -> n >= start && n <= stop);
    indent_empty = false;
    adaptive = false;
    kind = Numeric collect
  }

(** forward_non_blank *)
let forward_non_blank iter =
  let is_dirty = ref false in
  let rec f it =
    let ch = it#char in
    is_dirty := !is_dirty && ch = 9;
    if ch <> 32 && ch <> 9 && ch <> 13 then it
    else f it#forward_char
  in
  f iter, !is_dirty

let ensure_last_newline (buffer : GText.buffer) =
  if buffer#end_iter#line_offset > 0 then begin
    let insert_mark = buffer#create_mark @@ buffer#get_iter_at_mark `INSERT in
    buffer#insert ~iter: buffer#end_iter "\n";
    buffer#place_cursor ~where: (buffer#get_iter_at_mark @@ `MARK insert_mark)
  end

let contents (buffer : GText.buffer) =
  let start, stop = buffer#start_iter, buffer#end_iter in
  buffer#get_text ~start ~stop ()

(** indent *)
let indent ~project ~view ?syntaxes bounds =
  let pref = Preferences.preferences#get in
  let buffer = view#tbuffer in
  let indent () =
    let start, stop =
      match bounds with
      | `SELECTION -> buffer#selection_bounds
      | `BOUNDS iters -> iters
      | `ALL -> ensure_last_newline buffer#as_gtext_buffer;
          buffer#start_iter, buffer#end_iter
    in
    let start, stop = if start#compare stop > 0 then stop, start else start, stop in
    let stop = if not (stop#equal start) then stop#backward_line#forward_to_line_end else stop in
    let start_line, stop_line = start#line + 1, stop#line + 1 in

    let contents = contents buffer#as_gtext_buffer in
    let ns = Nstream.of_string contents in
    let offsets = IndentPrinter.proceed (output ~project ~pref ?syntaxes start_line stop_line) ns IndentBlock.empty [] in
    let lines = List.rev offsets in

    buffer#undo#begin_block ~name:"ocp-indent";
    buffer#block_signal_handlers();
    let start_line = start#line in
    let start_line_index = start#line_index in
    List.iteri begin fun i spaces ->
      let start = buffer#get_iter (`LINE (start_line + i)) in
      let stop, is_dirty = forward_non_blank start in
      let existing = stop#line_index - start#line_index in
      if existing < spaces && not is_dirty then begin
        let start = buffer#get_iter (`LINE (start_line + i)) in
        buffer#insert ?iter:(Some start) ?tag_names:None ?tags:None (Alignment.mk_spaces (spaces - existing));
      end else if existing > spaces && not is_dirty then begin
        buffer#delete ~start ~stop:(start#set_line_index (existing - spaces));
      end else if (*existing <> spaces ||*) is_dirty then begin
        buffer#delete ~start ~stop;
        let start = buffer#get_iter (`LINE (start_line + i)) in
        buffer#insert ?iter:(Some start) ?tag_names:None ?tags:None (Alignment.mk_spaces spaces);
      end
    end lines;

    if bounds = `SELECTION && buffer#has_selection then begin
      let m = if (buffer#get_iter `INSERT)#compare (buffer#get_iter `SEL_BOUND) < 0
        then `INSERT else `SEL_BOUND in
      buffer#move_mark m ~where:(buffer#get_iter_at_char ?line:(Some start_line) start_line_index);
    end;
    buffer#unblock_signal_handlers();
    buffer#undo#end_block ();
    view#draw_current_line_background ?force:(Some true) (buffer#get_iter `INSERT);
    true
  in
  if bounds = `SELECTION && not buffer#has_selection then begin
    let ins = buffer#get_iter `INSERT in
    let iter = ins#set_line_index 0 in
    let stop = if iter#ends_line then iter else iter#forward_to_line_end in
    let line = iter#get_text ~stop in
    if String.trim line = "" then
      (if pref.editor_indent_empty_line then indent() else false)
    else if ins#ends_line then false else indent ()
  end else indent ()

let indent_for_extension ~project ~view ~extension bounds =
  match extension with
  | ".ml" | ".mli" -> ignore @@ indent ~project ~view bounds
  (*| ".mll" -> ignore @@ indent ~project ~view ~syntaxes: ["mll"] bounds*)
  | _ -> ()
