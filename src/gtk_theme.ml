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

(* Condensed font for the file list in the search results pane. None is default font. (`STRETCH `CONDENSED doesn't work) *)
let find_text_output_font_condensed : string option ref = ref None
let set_find_text_output_font_condensed context =
  find_text_output_font_condensed :=
    try Some (List.find (Gtk_util.try_font context) [(*"Arial Narrow"; *)"Arial"; "Helvetica"; "Sans"])
    with Not_found -> None

let get_style_outline (pref : Settings_t.settings) =
  let style_outline, apply_outline =
    let open Preferences in
    let base_color = ?? (pref.outline_color_nor_bg) in
    let even, odd =
      match pref.outline_color_alt_rows with
      | None -> base_color, base_color
      | Some x -> base_color, (ColorOps.name (ColorOps.set_value x (`NAME base_color)))
    in
    sprintf "
          style \"outline-treestyle\" {
            GtkTreeView::even-row-color = \"%s\"
            GtkTreeView::odd-row-color = \"%s\"
          }" even odd,
    "widget \"*.outline_treeview\" style \"outline-treestyle\"";
  in style_outline, apply_outline

let set_theme ?theme ~context () =
  let pref = Preferences.preferences#get in
  let style_smallbutton, apply_smallbutton = "\
      style \"oe_menubar\" {
        ythickness = 0
        GtkMenuBar::shadow-type = none
        GtkMenuBar::internal-padding = 0
      }
      style \"menubar-button\" {
        GtkButton::child-displacement-x = 1
        GtkButton::child-displacement-y = 1
        GtkButton::inner-border = { 0, 0, 0, 0 }
        xthickness = 0
        ythickness = 0
      }
      style \"menubar-button-arrow\" {
        GtkButton::child-displacement-x = 1
        GtkButton::child-displacement-y = 1
        GtkButton::inner-border = { 3, 3, 2, 2 }
        xthickness = 0
        ythickness = 0
      }
      style \"window-button\" {
        GtkButton::child-displacement-x = 0
        GtkButton::child-displacement-y = 0
        GtkButton::inner-border = { 6,6,6,6 }
        xthickness = 0
        ythickness = 0
      }
      style \"gitbutton\" {
        GtkButton::image-spacing = 8
        xthickness = 0
        ythickness = 0
      }
      style \"oe-statusbar\" {
        fg[NORMAL] = @selected_fg_color
      }
      style \"small-button\" {
        GtkButton::child-displacement-x = 0
        GtkButton::child-displacement-y = 0
        GtkButton::inner-border = { 0, 0, 0, 0 }
        xthickness = 0
        ythickness = 0
      }", "widget \"*.smallbutton\" style \"small-button\"
style \"oe-tooltip\"
{
  bg[NORMAL] = \"#FFE375\"
  fg[NORMAL] = \"#000000\"
}
widget_class \"*<GtkToolButton>*<GtkButton>\" style \"menubar-button\"
widget \"*.windowbutton\" style \"window-button\"
widget \"*.menubar_button_arrow\" style \"menubar-button-arrow\"
widget \"*.oe_menubar\" style:highest \"oe_menubar\"
#widget \"gtk-tooltip*\" style \"oe-tooltip\"
widget \"*.gitbutton\" style:highest \"gitbutton\"
widget \"*.statusbar.*\" style:highest \"oe-statusbar\"
                                               "
  in
  let style_targetlist, apply_targetlist =
    let open Preferences in
    match Oe_config.targetlist_alternating_row_colors with
    | None -> "", ""
    | Some x ->
        let base_color = (?? (pref.editor_bg_color_user)) in
        sprintf "
                                               style \"targetlist-treestyle\" {
            GtkTreeView::even-row-color = \"%s\"
            GtkTreeView::odd-row-color = \"%s\"
          }" base_color (ColorOps.name (ColorOps.set_value x (`NAME base_color))),
        "widget \"*.targetlist_treeview\" style \"targetlist-treestyle\""
  in
  let gtk_font_name =
    match String.trim pref.font with
    | "" ->
        begin
          try
            let family, size = List.find (fun (n, _) -> Gtk_util.try_font context n) ["Sans", 9] in
            let font_name = sprintf "%s %d" family size in
            pref.font <- font_name;
            Preferences.save();
            sprintf "gtk-font-name = \"%s\"" font_name;
          with Not_found -> ""
        end;
    | x -> sprintf "gtk-font-name = \"%s\"" x
  in
  let style_outline, apply_outline = get_style_outline pref in
  let rc =
    String.concat "\n" [
      style_smallbutton; style_outline; style_targetlist;
      apply_smallbutton; apply_outline; apply_targetlist;
      "gtk-button-images = 0";
      "gtk-double-click-time = 500";
      "gtk-double-click-distance = 10";
      "gtk-tooltip-timeout=100";
      gtk_font_name;
    ]
  in
  GtkMain.Rc.parse_string rc;
  set_find_text_output_font_condensed context
;;

