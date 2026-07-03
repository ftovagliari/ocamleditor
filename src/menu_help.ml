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
  let window = GWindow.window ~icon:(??? Icons.oe) ~title:"System Properties" ~position:`CENTER ~modal:true ~resizable:false ~show:false () in
  let text = Glib.Convert.locale_to_utf8 (System_properties.to_string ()) in
  let buffer = GText.buffer ~text () in
  let vbox = GPack.vbox ~spacing:0 ~border_width:0 ~packing:window#add () in
  let sw = GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:vbox#add () in
  let view = GText.view ~buffer ~editable:false ~width:850 ~height:600 ~packing:sw#add () in
  view#misc#modify_font_by_name (Preferences.preferences#get.editor_base_font);
  view#set_left_margin 8;
  view#set_right_margin 8;
  view#set_pixels_above_lines 2;
  view#set_pixels_below_lines 2;
  view#set_cursor_visible false;
  let bbox = GPack.button_box `HORIZONTAL ~layout:`SPREAD ~border_width:8 ~packing:vbox#pack () in
  let button_close = GButton.button ~packing:bbox#pack () in
  button_close#set_image (Icons.create (??? Icons.close_16))#coerce;
  button_close#connect#clicked ~callback:window#destroy |> ignore;
  Gmisclib.Util.esc_destroy_window window;
  window#show();
  window#set_position `CENTER_ALWAYS


(** about *)
let about () =
  let version, _ =
    match About.version |> Str.split (Str.regexp "[~-]") with
    | version :: comments :: _ -> version, comments
    | _ -> "", ""
  in
  let dialog = GWindow.about_dialog
      ~modal:true
      ~width:310
      ~position:`CENTER
      ~icon:(??? Icons.oe)
      ~name:About.program_name
      ~version
      ~copyright:About.copyright
      ~logo:(??? Icons.logo)
      (*~comments*)
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
  let vbox = dialog#vbox in
  let modify_label ?(color="#808080") label =
    label#misc#modify_fg [`NORMAL, `NAME color];
    label#misc#modify_font_by_name "Sans 7";
  in
  let hbox = GPack.hbox ~packing:vbox#pack ~show:false () in
  (* Link website *)
  let link_button = GButton.button ~relief:`NONE ~packing:hbox#add () in
  let label = GMisc.label ~text:(sprintf "Visit the %s website" About.program_name) ~packing:link_button#add () in
  modify_label ~color:"#0000ff" label;
  link_button#set_focus_on_click false;
  ignore (link_button#connect#clicked ~callback:(fun () ->
      ksprintf open_url "%s?%s-%s-%s"
        About.website Sys.os_type About.version !About.build_id));

  (* Report a bug *)
  let link_button = GButton.button ~relief:`NONE ~packing:hbox#add () in
  let label = GMisc.label ~text:"Report an issue" ~packing:link_button#add () in
  modify_label ~color:"#0000ff" label;
  link_button#set_focus_on_click false;
  ignore (link_button#connect#clicked ~callback:(fun () -> open_url About.issues));
  match dialog#run() with _ -> dialog#destroy()





