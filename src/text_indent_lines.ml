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


open Text_util

let scan_indent view (start : GText.iter) (stop : GText.iter) =
  let data = ref [] in
  let iter = ref (start#set_line_index 0) in
  let stop = stop#set_line_index 0 in
  while !iter#compare stop < 0 do
    if List.mem !iter#char blanks then (iter := !iter#forward_find_char not_blank);
    let indent = !iter#line_index / view#tbuffer#tab_width in
    data := (!iter#line, indent) :: !data;
    iter := !iter#forward_line;
  done;
  !data;;

let use_glow = false
let style = `ON_OFF_DASH
let width = 1
let color_hl_fg = `NAME (if Preferences.preferences#get.theme_is_dark then "#ffffff" else "#000000")
let color_hl_bg = `NAME "#e0f0ff"
let color_hl_bg1 = `NAME "#fff0e0"

(** draw_indent_lines *)
let draw_indent_lines view (drawable : GDraw.drawable) start stop y0 =
  let left_margin = view#left_margin + 1 in (* +1 per evitare di coprire la linea del cursore *)
  let buffer = view#tbuffer in
  let current_iter : GText.iter = buffer#get_iter `INSERT in
  let y_current_line, _ = view#get_line_yrange current_iter#forward_line in
  let y_current_line = y_current_line - y0 in
  let indents = scan_indent view start stop in
  let count = ref buffer#line_count in
  let prev = ref 0 in
  let lines = ref [] in
  let add_segment ll x y1 y2 =
    match List.assoc_opt x !ll with
    | Some segs ->
        begin
          match !segs with
          | (y3, y4) :: tl when y2 = y3 ->
              segs := (y1, y4) :: tl;
          | seg -> segs := (y1, y2) :: seg;
        end
    | _ -> (ll := (x, ref [y1, y2]) :: !ll)
  in
  let hline = ref 0 in
  let hadjust = match view#hadjustment with Some adj -> int_of_float adj#value - left_margin | _ -> 0 in
  let draw line indent =
    let iter = buffer#get_iter (`LINE line) in
    let y1, h1 = view#get_line_yrange iter in
    let y1 = y1 - y0 in
    let y2 = y1 + h1 in
    hline := max h1 !hline;
    for i = 1 to indent - 1 do
      let x = view#approx_char_width * (i * buffer#tab_width) - hadjust in
      add_segment lines x y1 y2;
    done;
  in
  List.iter begin fun (line, indent) ->
    if !count > line then begin
      while !count > line do
        draw !count !prev;
        decr count;
      done
    end;
    draw line indent;
    prev := indent;
    decr count;
  end indents;
  (*hline := !hline * 2;*)
  (* Draw lines *)
  let current_block = ref true in
  let mm = 2 * buffer#tab_width * view#approx_char_width in

  let draw_highlight x y1 y2 =
    if use_glow then begin
      drawable#set_foreground color_hl_bg;
      drawable#set_line_attributes ~width:3 ~style:`SOLID ();
      drawable#line ~x ~y:y1 ~x ~y:y2;
    end;
    drawable#set_foreground color_hl_fg;
    drawable#set_line_attributes ~width ~style ();
    current_block := false;
  in
  Gdk.GC.set_dashes drawable#gc ~offset:1 Oe_config.on_off_dashes;
  List.iter begin fun (x, xlines) ->
    List.iter begin fun (y1, y2) ->
      if y2 - y1 > !hline then begin
        if (x - left_margin) mod mm = 0 then begin
          if !current_block && y1 <= y_current_line && y_current_line <= y2 then begin
            draw_highlight x y1 y2;
          end else begin
            if use_glow then begin
              drawable#set_foreground color_hl_bg1(*view#options#base_color*);
              drawable#set_line_attributes ~width:3 ~style:`SOLID ();
              drawable#line ~x ~y:y1 ~x ~y:y2;
            end;
            drawable#set_foreground view#options#indent_lines_color_dashed;
            drawable#set_line_attributes ~width ~style ();
          end;
          drawable#line ~x ~y:y1 ~x ~y:y2;
        end else begin
          if y2 - y1 > !hline then begin
            if !current_block && y1 <= y_current_line && y_current_line <= y2 then begin
              draw_highlight x y1 y2;
            end else begin
              if use_glow then begin
                drawable#set_foreground color_hl_bg1(*view#options#base_color*);
                drawable#set_line_attributes ~width:3 ~style:`SOLID ();
                drawable#line ~x ~y:y1 ~x ~y:y2;
              end;
              drawable#set_foreground view#options#indent_lines_color_solid;
              drawable#set_line_attributes ~width ~style:`SOLID ();
            end;
            drawable#line ~x ~y:y1 ~x ~y:y2;
          end;
        end
      end
    end !xlines
  end !lines;

  (*  Gdk.GC.set_dashes drawable#gc ~offset:1 [1; 5];
      List.iter begin fun (x, xlines) ->
      List.iter begin fun (y1, y2) ->
        if y2 - y1 > !hline then begin
          if !first && y1 <= cly && cly <= y2 then begin
            drawable#set_foreground (`NAME "#ff0000");
            drawable#set_line_attributes ~width:2 ~style:`SOLID ();
            first := false;
          end else begin
            drawable#set_foreground view#options#base_color;
            drawable#set_line_attributes ~width:2 ~style:`SOLID ();
            drawable#line ~x ~y:y1 ~x ~y:y2;
            drawable#set_foreground view#options#indent_lines_color_solid;
            drawable#set_line_attributes ~width:1 ~style:`ON_OFF_DASH ();
          end;
          drawable#line ~x ~y:y1 ~x ~y:y2
        end
      end !xlines
      end lines2;*)

