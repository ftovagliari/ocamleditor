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


module ColorOps = Color
open Pref_page
open Preferences
open Settings_t

(** pref_editor_display *)
class pref_editor_display title ?packing () =
  let vbox                         = GPack.vbox ~spacing ?packing () in
  let align                        = create_align ~title:"Settings" ~vbox () in
  let box                          = GPack.vbox ~spacing:row_spacings ~packing:align#add () in
  let check_show_line_numbers      = GButton.check_button ~label:"Show line numbers" ~packing:box#pack () in
  let clb_vbox                     = GPack.vbox ~spacing:3 ~packing:box#pack () in
  let check_highlight_current_line = GButton.check_button ~label:"Highlight current line" ~packing:clb_vbox#pack () in
  let clb_align                    = GBin.alignment ~padding:(0, 0, indent, 0) ~packing:clb_vbox#add () in
  let check_current_line_border    = GButton.check_button ~label:"Draw border" ~packing:clb_align#add () in
  let il_vbox                      = GPack.vbox ~spacing:3 ~packing:box#pack () in
  let check_indent_lines           = GButton.check_button ~label:"Show indentation guidelines" ~packing:il_vbox#pack () in
  let il_align                     = GBin.alignment ~padding:(0, 0, indent, 0) ~packing:il_vbox#add () in
  let il_hbox                      = GPack.hbox ~spacing:5 ~packing:il_align#add () in
  let _                            = GMisc.label ~text:"Solid lines color:" ~packing:il_hbox#pack () in
  let il_button_solid              = GButton.color_button ~packing:(il_hbox#pack ~fill:false) () in
  let _                            = il_button_solid#set_relief `NONE in
  let _                            = GMisc.label ~text:"Dashed lines color:" ~packing:il_hbox#pack () in
  let il_button_dashed             = GButton.color_button ~packing:(il_hbox#pack ~fill:false) () in
  let _                            = il_button_dashed#set_relief `NONE in
  let check_show_dot_leaders       = GButton.check_button ~label:"Show dot leaders" ~packing:box#pack () in
  let check_code_folding           = GButton.check_button ~label:"Enable code folding" ~packing:box#pack () in
  let check_global_gutter          = GButton.check_button ~label:"Show global gutter" ~packing:box#pack () in
  let rm_vbox                      = GPack.vbox ~spacing:5 ~packing:box#pack () in
  let hbox                         = GPack.hbox ~spacing:5 ~packing:rm_vbox#pack () in
  let adjustment                   = GData.adjustment ~lower:0. ~upper:300. ~step_incr:1. ~page_size:0. () in
  let check_right_margin           = GButton.check_button ~active:false ~label:"Visible right margin at column:" ~packing:hbox#pack () in
  let entry_right_margin           = GEdit.spin_button ~numeric:true ~digits:0 ~rate:1.0 ~adjustment ~packing:hbox#pack () in
  let rm_align                     = GBin.alignment ~padding:(0, 0, indent, 0) ~packing:rm_vbox#add () in
  let rm_hbox                      = GPack.hbox ~spacing:5 ~packing:rm_align#add () in
  let _                            = GMisc.label ~text:"Color:" ~packing:rm_hbox#pack () in
  let rm_button                    = GButton.color_button ~packing:(rm_hbox#pack ~fill:false) () in
  let _                            = rm_button#set_relief `NONE in
  let mo_vbox                      = GPack.vbox ~spacing:5 ~packing:box#pack () in
  let check_mark_occurrences       = GButton.check_button ~active:false ~label:"Highlight all occurrences of selected text" ~packing:mo_vbox#pack () in
  let mo_align                     = GBin.alignment ~padding:(0, 0, indent, 0) ~packing:mo_vbox#add () in
  let mo_vbox                      = GPack.vbox ~spacing:5 ~packing:mo_align#add () in
  let check_mo_uc                  = GButton.check_button ~active:false ~label:"Highlight related keywords under cursor" ~packing:mo_vbox#pack () in
  let mo_cbox                      = GPack.hbox ~spacing:5 ~packing:mo_vbox#add () in
  let _                            = GMisc.label ~text:"Color:" ~packing:mo_cbox#pack () in
  let mo_button                    = GButton.color_button ~packing:(mo_cbox#pack ~fill:false) () in
  let _                            = mo_button#set_relief `NONE in
  let check_thick_caret            = GButton.check_button ~label:"Enable thick caret" ~packing:box#pack () in
  object (self)
    inherit page title vbox

    initializer
      let signal_id = Preferences.preferences#connect#changed ~callback:self#read in
      self#misc#connect#destroy ~callback:(fun () -> Preferences.preferences#connect#disconnect signal_id) |> ignore;
      ignore (check_right_margin#connect#toggled ~callback:begin fun () ->
          entry_right_margin#misc#set_sensitive check_right_margin#active;
        end);
      ignore (check_mark_occurrences#connect#toggled ~callback:begin fun () ->
          mo_vbox#misc#set_sensitive check_mark_occurrences#active
        end);
      ignore (check_highlight_current_line#connect#toggled ~callback:begin fun () ->
          check_current_line_border#misc#set_sensitive check_highlight_current_line#active
        end);
      ignore (check_indent_lines#connect#toggled ~callback:begin fun () ->
          il_align#misc#set_sensitive check_indent_lines#active
        end);
      ignore (check_right_margin#connect#toggled ~callback:begin fun () ->
          rm_align#misc#set_sensitive check_right_margin#active
        end);

    method write pref =
      pref.editor_cursor_aspect_ratio <- (if check_thick_caret#active then 0.1 else 0.0);
      pref.editor_highlight_current_line <- check_highlight_current_line#active;
      pref.editor_show_line_numbers <- check_show_line_numbers#active;
      let _, s, d = pref.editor_indent_lines in
      let solid_color = il_button_solid#color |> ColorOps.name_of_gdk in
      let dashed_color = il_button_dashed#color |> ColorOps.name_of_gdk in
      pref.editor_indent_lines <-
        check_indent_lines#active, Preferences.Color.new_themed_color solid_color s,
        Preferences.Color.new_themed_color dashed_color d;
      let color = rm_button#color |> ColorOps.name_of_gdk in
      pref.editor_right_margin_visible <- check_right_margin#active;
      Preferences.Color.set_themed_color pref.editor_right_margin_color color;
      pref.editor_right_margin <- entry_right_margin#value_as_int;
      pref.editor_code_folding_enabled <- check_code_folding#active;
      pref.editor_show_global_gutter <- check_global_gutter#active;
      let color = mo_button#color |> ColorOps.name_of_gdk in
      pref.editor_mark_occurrences_enabled <- check_mark_occurrences#active;
      pref.editor_mark_occurrences_under_cursor <- check_mo_uc#active;
      Preferences.Color.set_themed_color pref.editor_mark_occurrences_bg_color color;
      pref.editor_dot_leaders <- check_show_dot_leaders#active;
      pref.editor_current_line_border <- check_current_line_border#active;

    method read pref =
      check_thick_caret#set_active (pref.editor_cursor_aspect_ratio > 0.0);
      check_highlight_current_line#set_active pref.editor_highlight_current_line;
      check_show_line_numbers#set_active pref.editor_show_line_numbers;
      let enabled, color_s, color_d = pref.editor_indent_lines in
      check_indent_lines#set_active (not enabled);
      check_indent_lines#set_active enabled;
      let enabled = pref.editor_right_margin_visible in
      check_right_margin#set_active (not enabled);
      check_right_margin#set_active enabled;
      entry_right_margin#set_value (float pref.editor_right_margin);
      (`NAME ??(pref.editor_right_margin_color)) |> GDraw.color |> rm_button#set_color;
      check_code_folding#set_active (pref.editor_code_folding_enabled);
      check_global_gutter#set_active (pref.editor_show_global_gutter);
      check_mark_occurrences#set_active (not pref.editor_mark_occurrences_enabled);
      check_mark_occurrences#set_active (pref.editor_mark_occurrences_enabled);
      check_mo_uc#set_active pref.editor_mark_occurrences_under_cursor;
      (`NAME ??(pref.editor_mark_occurrences_bg_color)) |> GDraw.color |> mo_button#set_color;
      check_show_dot_leaders#set_active pref.editor_dot_leaders;
      check_current_line_border#set_active pref.editor_current_line_border;
      check_highlight_current_line#set_active (not pref.editor_highlight_current_line);
      check_highlight_current_line#set_active pref.editor_highlight_current_line;
      (`NAME ??color_s) |> GDraw.color |> il_button_solid#set_color;
      (`NAME ??color_d) |> GDraw.color |> il_button_dashed#set_color;
  end

