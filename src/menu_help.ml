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


open Utils
open Printf
open Preferences

(** system_properties *)
let system_properties () =
  let window = GWindow.window ~icon:(??? Logo.oe) ~title:"System Properties" ~position:`CENTER ~modal:true ~resizable:true ~show:false () in
  let text = Glib.Convert.locale_to_utf8 (System_properties.to_string ()) in
  let buffer = GText.buffer ~text () in
  let vbox = GPack.vbox ~spacing:0 ~border_width:0 ~packing:window#add () in
  let sw = GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~width:920 ~height:600 ~packing:vbox#add () in
  let view = GText.view ~buffer ~editable:false ~packing:sw#add () in
  view#misc#modify_font_by_name (Preferences.preferences#get.editor_base_font);
  view#set_left_margin 8;
  view#set_right_margin 8;
  view#set_pixels_above_lines 2;
  view#set_pixels_below_lines 2;
  view#set_cursor_visible false;
  let bbox = GPack.button_box `HORIZONTAL ~layout:`SPREAD ~border_width:8 ~packing:vbox#pack () in
  let button_close = GButton.button ~packing:bbox#pack () in
  button_close#set_image (Gtk_util.label_icon Icon_set.toolbar_close)#coerce;
  button_close#connect#clicked ~callback:window#destroy |> ignore;
  Gmisclib.Util.esc_destroy_window window;
  window#resize ~width:1 ~height:1;
  window#show()


(** about *)
let about () =
  (*let logo = GdkPixbuf.create ~width:1 ~height:1 () in*)
  let dialog = GWindow.about_dialog
      ~modal:true
      ~width:310
      ~position:`CENTER
      ~icon:(??? Logo.oe)
      ~name:About.program_name
      ~version:About.version
      ~copyright:About.copyright
      ~logo:(??? Logo.logo)
      ~license:{|OCamlEditor is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  OCamlEditor is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.|}
      ~show:false
      ()
  in
  (*let vbox = dialog#vbox in
    let image = GMisc.image ~file:"/home/frank/ocaml/ocamleditor/src/icons/dark/logo.gif" ~packing:vbox#add () in
    vbox#reorder_child image#coerce ~pos:1;*)
  match dialog#run() with _ -> dialog#destroy()

(** Commands of the Help menu. *)
let register ~browser =
  let editor = browser#editor in
  let reg id label ?accels run = ignore (Command.register ~id:("help." ^ id) ~label ?accels run) in
  reg "clear_cache" "Clear Editor Cache" editor#clear_cache;
  reg "print_debug_info" "Print Debug Info"
    ~accels:[[`CONTROL; `MOD1], GdkKeysyms._apostrophe] (Print_debug_info.print ~editor);
  reg "system_properties" "System Properties" system_properties;
  reg "about" (Printf.sprintf "About %s" About.program_name) about

(** Creates the Help menu. *)
let create ~browser ~group =
  register ~browser;
  Menu_builder.create ~group ~label:"Help" Menu_builder.[
      item "help.clear_cache"; item "help.print_debug_info"; item "help.system_properties";
      separator;
      item "help.about";
    ]
