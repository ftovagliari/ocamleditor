(*

  OCamlEditor
  Copyright (C) 2010-2012 Francesco Tovagliari

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


let apply (view : Text.view) pref =
  view#set_left_margin pref.Preferences.pref_editor_left_margin;
  let above, below = pref.Preferences.pref_editor_pixels_lines in
  view#set_pixels_above_lines above;
  view#set_pixels_below_lines below;
  let color = if snd pref.Preferences.pref_bg_color
    then `COLOR (view#misc#style#base `NORMAL)
    else (`NAME (fst pref.Preferences.pref_bg_color))
  in
  view#mark_occurrences_manager#mark();
  view#options#set_mark_occurrences pref.Preferences.pref_editor_mark_occurrences;
  view#mark_occurrences_manager#mark();
  view#options#set_show_indent_lines pref.Preferences.pref_editor_indent_lines;
  view#options#set_indent_lines_color_solid (`NAME pref.Preferences.pref_editor_indent_lines_color_s);
  view#options#set_indent_lines_color_dashed (`NAME pref.Preferences.pref_editor_indent_lines_color_d);
  view#options#set_show_line_numbers pref.Preferences.pref_show_line_numbers;
  view#options#set_line_numbers_font pref.Preferences.pref_base_font;
  view#modify_font pref.Preferences.pref_base_font;
  view#options#set_word_wrap pref.Preferences.pref_editor_wrap;
  view#options#set_show_dot_leaders pref.Preferences.pref_editor_dot_leaders;
  view#options#set_current_line_border_enabled pref.Preferences.pref_editor_current_line_border;
  view#options#set_text_color (Color.name_of_gdk (Preferences.tag_color "lident"));
  view#options#set_base_color begin
    if snd pref.Preferences.pref_bg_color then begin
      (* "Use theme color" option removed *)
      let color = (*`NAME*) (fst ((Preferences.create_defaults()).Preferences.pref_bg_color)) in
      (*view#misc#modify_bg [`NORMAL, (Oe_config.gutter_color_bg color)];*)
      view#misc#modify_base [`NORMAL, `NAME color];
      color;
    end else begin
      let color = (*`NAME*) (fst pref.Preferences.pref_bg_color) in
      view#misc#modify_base [`NORMAL, `NAME color];
      color;
    end;
  end;
  if pref.Preferences.pref_highlight_current_line then begin
    view#options#set_highlight_current_line
      (Some (match (List.assoc "highlight_current_line" pref.Preferences.pref_tags)
        with ((`NAME c), _, _, _, _) -> c | _ -> assert false));
  end else (view#options#set_highlight_current_line None);
  view#tbuffer#set_tab_width pref.Preferences.pref_editor_tab_width;
  view#tbuffer#set_tab_spaces pref.Preferences.pref_editor_tab_spaces;
  view#options#set_smart_home (pref.Preferences.pref_smart_keys_home = 0);
  view#options#set_smart_end (pref.Preferences.pref_smart_keys_end = 1);
  if pref.Preferences.pref_right_margin_visible then begin
    view#options#set_visible_right_margin (Some
      (pref.Preferences.pref_right_margin, `NAME pref.Preferences.pref_right_margin_color))
  end else (view#options#set_visible_right_margin None);


