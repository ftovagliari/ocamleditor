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
module ColorOps = Color
open Preferences
open Settings_t

let apply (view : Text.view) pref =
  view#set_left_margin (pref.editor_left_margin + Oe_config.current_line_width);
  view#set_current_line_border_x1 (view#left_margin - (max 1 (Oe_config.current_line_width / 2)) - 1);
  view#set_current_line_border_x2 (view#left_margin - Oe_config.current_line_width / 2 + Oe_config.current_line_border_adjust + 1);
  ksprintf GtkMain.Rc.parse_string {|
style "s1" {
  GtkTextView::cursor_aspect_ratio = %.1f
}
class "GtkTextView" style "s1"
|} pref.editor_cursor_aspect_ratio;
  let above, below = pref.editor_pixels_lines in
  view#set_pixels_above_lines above;
  view#set_pixels_below_lines below;
  let color = if pref.editor_bg_color_theme
    then `COLOR (view#misc#style#base `NORMAL)
    else (`NAME pref.editor_bg_color_user)
  in
  view#mark_occurrences_manager#mark();
  view#options#set_mark_occurrences
    (pref.editor_mark_occurrences_enabled,
     pref.editor_mark_occurrences_under_cursor,
     ?? (pref.editor_mark_occurrences_bg_color));
  view#mark_occurrences_manager#mark();
  let show_indent_lines, indent_lines_color_s, pref_editor_indent_lines_color_d = pref.editor_indent_lines in
  view#options#set_show_indent_lines show_indent_lines;
  view#options#set_indent_lines_color_solid (`NAME ?? indent_lines_color_s);
  view#options#set_indent_lines_color_dashed (`NAME ?? pref_editor_indent_lines_color_d);
  view#options#set_show_line_numbers pref.editor_show_line_numbers;
  view#options#set_line_numbers_font pref.editor_base_font;
  view#modify_font pref.editor_base_font;
  view#options#set_word_wrap pref.editor_wrap;
  view#options#set_show_dot_leaders pref.editor_dot_leaders;
  view#options#set_current_line_border_enabled pref.editor_current_line_border;
  view#options#set_text_color (ColorOps.name_of_gdk (Preferences.editor_tag_color "lident"));
  let default_bg_color =
    if pref.editor_bg_color_theme then begin
      let color = (?? (Preferences.default_values.editor_bg_color_user)) in
      view#misc#modify_base [`NORMAL, `NAME color];
      view#misc#modify_bg [`NORMAL, `NAME color];
      color;
    end else begin
      let color = (*`NAME*) (?? (pref.editor_bg_color_user)) in
      view#misc#modify_base [`NORMAL, `NAME color];
      view#misc#modify_bg [`NORMAL, `NAME color];
      color;
    end;
  in
  view#options#set_base_color default_bg_color;
  let editor_tags = pref.editor_tags in
  if pref.editor_highlight_current_line then begin
    view#options#set_highlight_current_line
      (Some (match (List.find_opt (fun t -> t.name = "highlight_current_line") editor_tags)
             with Some t -> (?? (t.color)), (?? (t.bg_color)) | _ -> assert false));
  end else (view#options#set_highlight_current_line None);
  view#tbuffer#set_tab_width pref.editor_tab_width;
  view#tbuffer#set_tab_spaces pref.editor_tab_spaces;
  view#options#set_smart_home (pref.editor_smart_keys_home = 0);
  view#options#set_smart_end (pref.editor_smart_keys_end = 1);
  if pref.editor_right_margin_visible then begin
    view#options#set_visible_right_margin
      (Some (pref.editor_right_margin, `NAME ?? (pref.editor_right_margin_color)))
  end else (view#options#set_visible_right_margin None);
  match List.find_opt (fun t -> t.name = "selection") editor_tags with
  | Some t ->
      let bg_color = if t.bg_default then view#options#text_color else (`NAME ?? (t.bg_color)) in
      view#misc#modify_base [`SELECTED, bg_color; `ACTIVE, bg_color];
      let fg_color = if t.bg_default then `NAME default_bg_color else `NAME ?? (t.color) in
      view#misc#modify_text [`SELECTED, fg_color; `ACTIVE, fg_color];
  | _ -> assert false


