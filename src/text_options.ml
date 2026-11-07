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


open GUtil

class options () =
  object (self)
    val mutable text_color : GDraw.color = `BLACK
    val mutable base_color : GDraw.color = `WHITE
    val mutable indent_lines_color_solid : GDraw.color = `BLACK
    val mutable indent_lines_color_dashed : GDraw.color = `BLACK
    val mutable current_line_highlight : (string * string) option = None
    val mutable current_line_border_enabled = true;
    val mutable current_line_border_color : GDraw.color = `NAME "#d0d0d0"
    val mutable current_line_bg_color : GDraw.color = `NAME "#f0f0f0"
    val mutable show_line_numbers = true
    val mutable show_indent_lines = true
    val mutable show_dot_leaders = true
    val mutable show_whitespace_chars = false
    val mutable show_markers = true
    val mutable smart_home = true;
    val mutable smart_end = true;
    val mutable word_wrap = false
    val mutable mark_occurrences : (bool * bool * string) = false, false , ""
    val mutable visible_right_margin : (int * GDraw.color) option = None;
    val mutable line_numbers_font = ""
    val mutable mark_occurrences_changed = new mark_occurrences_changed ()
    val mutable line_numbers_changed = new line_numbers_changed ()
    val mutable current_line_highlight_changed = new current_line_highlight_changed ()
    val mutable line_numbers_font_changed = new line_numbers_font_changed ()
    val mutable show_markers_changed = new show_markers_changed ()
    val mutable word_wrap_changed = new word_wrap_changed ()

    initializer
      ignore (self#connect#line_numbers_changed ~callback:(fun x -> show_line_numbers <- x));
      ignore (self#connect#line_numbers_font_changed ~callback:(fun x -> line_numbers_font <- x));
      ignore (self#connect#current_line_highlight_changed ~callback:(fun x -> current_line_highlight <- x));
      ignore (self#connect#show_markers_changed ~callback:(fun x -> show_markers <- x));
      ignore (self#connect#word_wrap_changed ~callback:(fun x -> word_wrap <- x));
      ignore (self#connect#mark_occurrences_changed ~callback:(fun x -> mark_occurrences <- x));

    method set_base_color x = base_color <- `NAME x
    method base_color = base_color

    method text_color = text_color
    method set_text_color color = text_color <- `NAME color;

    method set_indent_lines_color_solid x = indent_lines_color_solid <- x
    method indent_lines_color_solid = indent_lines_color_solid

    method set_indent_lines_color_dashed x = indent_lines_color_dashed <- x
    method indent_lines_color_dashed = indent_lines_color_dashed

    method set_current_line_highlight = current_line_highlight_changed#call
    method current_line_highlight = current_line_highlight

    method current_line_border_enabled = current_line_border_enabled
    method set_current_line_border_enabled x = current_line_border_enabled <- x

    method current_line_border_color = current_line_border_color
    method set_current_line_border_color x = current_line_border_color <- x

    method set_current_line_bg_color x = current_line_bg_color <- x
    method current_line_bg_color = current_line_bg_color

    method set_show_line_numbers = line_numbers_changed#call;
    method show_line_numbers = show_line_numbers

    method set_show_indent_lines x = show_indent_lines <- x
    method show_indent_lines = show_indent_lines

    method smart_home = smart_home
    method set_smart_home x = smart_home <- x

    method smart_end = smart_end
    method set_smart_end x = smart_end <- x

    method word_wrap = word_wrap
    method set_word_wrap = word_wrap_changed#call

    method show_whitespace_chars = show_whitespace_chars
    method set_show_whitespace_chars x = show_whitespace_chars <- x

    method set_show_markers = show_markers_changed#call
    method show_markers = show_markers

    method show_dot_leaders = show_dot_leaders
    method set_show_dot_leaders x = show_dot_leaders <- x

    method mark_occurrences = mark_occurrences
    method set_mark_occurrences = mark_occurrences_changed#call

    method set_line_numbers_font = line_numbers_font_changed#call
    method line_numbers_font = line_numbers_font

    method visible_right_margin = visible_right_margin
    method set_visible_right_margin x = visible_right_margin <- x

    method connect = new signals
      ~line_numbers_changed
      ~line_numbers_font_changed
      ~current_line_highlight_changed
      ~show_markers_changed
      ~word_wrap_changed
      ~mark_occurrences_changed
  end

(** Signals *)
and line_numbers_changed () = object inherit [bool] signal () end
and line_numbers_font_changed () = object inherit [string] signal () end
and current_line_highlight_changed () = object inherit [(string * string) option] signal () end
and show_markers_changed () = object inherit [bool] signal () end
and word_wrap_changed () = object inherit [bool] signal () end
and mark_occurrences_changed () = object inherit [bool * bool * string] signal () end

and signals
    ~line_numbers_changed
    ~line_numbers_font_changed
    ~current_line_highlight_changed
    ~show_markers_changed
    ~word_wrap_changed
    ~mark_occurrences_changed =
  object
    inherit ml_signals [
        line_numbers_changed#disconnect;
        line_numbers_font_changed#disconnect;
        current_line_highlight_changed#disconnect;
        show_markers_changed#disconnect;
        word_wrap_changed#disconnect;
        mark_occurrences_changed#disconnect
      ]
    method line_numbers_changed = line_numbers_changed#connect ~after
    method line_numbers_font_changed = line_numbers_font_changed#connect ~after
    method current_line_highlight_changed = current_line_highlight_changed#connect ~after
    method show_markers_changed = show_markers_changed#connect ~after
    method word_wrap_changed = word_wrap_changed#connect ~after
    method mark_occurrences_changed = mark_occurrences_changed#connect ~after
  end
