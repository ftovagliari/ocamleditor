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
open GUtil

let cols          = new GTree.column_list
let col_toggle    = cols#add Gobject.Data.boolean
let col_name      = cols#add Gobject.Data.string
let col_descr     = cols#add Gobject.Data.string

let re_name_descr = Str.regexp "\\([^ ]+\\)[ \t\r]+\\(.+\\)"
let re_version    = Str.regexp "[ \t\r]+\\(.+\\)"

class widget ?packing () =
  let sw          = GBin.scrolled_window ~shadow_type:`IN ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ?packing () in
  let model       = GTree.list_store cols in
  let modelf      = GTree.model_filter model in
  let _           = modelf#set_visible_column col_toggle in
  let rend_toggle = GTree.cell_renderer_toggle [] in
  let rend_text   = GTree.cell_renderer_text [] in
  let vc_toggle   = GTree.view_column ~title:"" ~renderer:(rend_toggle, ["active", col_toggle]) () in
  let vc_name     = GTree.view_column ~title:"Name" ~renderer:(rend_text, ["text", col_name]) () in
  let vc_descr    = GTree.view_column ~title:"Description" ~renderer:(rend_text, ["markup", col_descr]) () in
  let view        = GTree.view ~model ~headers_visible:false ~reorderable:false ~enable_search:true ~packing:sw#add () in
  let _           = view#selection#set_mode `MULTIPLE in
  let _           = view#append_column vc_toggle in
  let _           = view#append_column vc_name in
  let _           = view#append_column vc_descr in
  let _           = view#set_search_column 1 in
  object (self)
    inherit GObj.widget sw#as_widget

    val changed = new changed()

    initializer
      vc_name#set_cell_data_func rend_text begin fun model row ->
        let checked = model#get ~row ~column:col_toggle in
        if checked && not self#is_current_model_filtered then begin
          let color = Color.name (Color.set_value 0.90 (`COLOR (view#misc#style#base `NORMAL))) in
          rend_text#set_properties [`CELL_BACKGROUND_SET true; `CELL_BACKGROUND color];
          rend_toggle#set_properties [`CELL_BACKGROUND_SET true; `CELL_BACKGROUND color];
        end else begin
          rend_text#set_properties [`CELL_BACKGROUND_SET false];
          rend_toggle#set_properties [`CELL_BACKGROUND_SET false];
        end
      end;
      self#load();
      ignore (rend_toggle#connect#toggled ~callback:begin fun path ->
          let path =
            if self#is_current_model_filtered then modelf#convert_path_to_child_path path else path
          in
          let row = model#get_iter path in
          let name = model#get ~row ~column:col_name in
          let checked = model#get ~row ~column:col_toggle in
          model#set ~row ~column:col_toggle (not checked);
          changed#call (name, checked);
        end);

    method private is_current_model_filtered =
      let current_model_oid = view#model#misc#get_oid in
      let modelf_oid = modelf#misc#get_oid in
      current_model_oid = modelf_oid

    method set_filter x =
      view#set_model (if x then Some modelf#coerce else Some model#coerce)

    method load () =
      let lines = Shell.get_command_output "ocamlfind list -describe" in
      let lines = List.fold_left begin fun (name_descr, acc) line ->
          match name_descr with
          | None ->
              if Str.string_match re_name_descr line 0 then begin
                let name = Str.matched_group 1 line in
                let descr = Str.matched_group 2 line in
                Some (name, descr), acc
              end else (None, acc)
          | Some (name, descr) ->
              if Str.string_match re_version line 0 then begin
                let version = Str.matched_group 1 line in
                None, (name, descr, version) :: acc
              end else None, (name, descr, "??") :: acc
        end (None, []) lines in
      let lines = List.rev (match lines with None, lines -> lines | _, lines -> lines)  in
      model#clear();
      List.iter begin fun (name, descr, version) ->
        let row = model#append () in
        model#set ~row ~column:col_toggle false;
        model#set ~row ~column:col_name name;
        model#set ~row ~column:col_descr (sprintf "%s\n<i><small>%s</small></i>" descr version);
      end lines

    method select_packages names =
      model#foreach begin fun _ row ->
        let name = model#get ~row ~column:col_name in
        model#set ~row ~column:col_toggle (List.mem name names);
        false
      end

    method get_selected_packages () =
      let result = ref [] in
      model#foreach begin fun _ row ->
        let checked = model#get ~row ~column:col_toggle in
        if checked then begin
          let name = model#get ~row ~column:col_name in
          result := name :: !result;
        end;
        false
      end;
      List.rev !result

    method connect = new signals ~changed
  end

and changed () = object inherit [string * bool] signal () end
and signals ~changed =
  object
    inherit ml_signals [changed#disconnect]
    method changed = changed#connect ~after
  end

let create = new widget

open Preferences
let dialog (parent : GObj.widget) () =
  (*let window = Gmisclib.Window.popup ~widget:parent () in*)
  let window = GWindow.window ~title:"Select Findlib packages" ~position:`CENTER ~modal:true ~show:false () in
  Gaux.may (GWindow.toplevel parent) ~f:(fun x -> window#set_transient_for x#as_window);
  let vbox = GPack.vbox ~border_width:5 ~spacing:5 ~packing:window#add () in
  let widget = create ~packing:vbox#add () in
  let hbox = GPack.hbox ~spacing:8 ~packing:vbox#pack () in
  let button_filter = GButton.check_button ~active:false ~label:"Show selected only" ~packing:hbox#pack () in
  button_filter#set_focus_on_click false;
  let bbox = GPack.button_box `HORIZONTAL ~layout:`END ~border_width:5 ~packing:hbox#add () in
  let button_close = GButton.button ~packing:bbox#pack () in
  button_close#set_image (Icons.create (??? Icons.close_16))#coerce;
  ignore (button_close#connect#clicked ~callback:window#destroy);
  ignore (button_filter#connect#toggled ~callback:(fun () -> widget#set_filter button_filter#active));
  window#resize ~width:parent#misc#allocation.Gtk.width ~height:400 ;
  ignore (window#event#connect#key_press ~callback:begin fun ev ->
      if GdkEvent.Key.keyval ev = GdkKeysyms._Escape then (window#destroy (); true)
      else false
    end);
  window#present();
  let x, y =
    let x0, y0 = Gdk.Window.get_pointer_location window#misc#window in
    let x, y = parent#misc#toplevel#misc#pointer in
    let alloc = parent#misc#allocation in
    let alloc_popup = window#misc#allocation in
    let x = x0 - x + alloc.Gtk.x in
    let y = y0 - y + alloc.Gtk.y + alloc.Gtk.height in
    let x, y =
      (if x + alloc_popup.Gtk.width > (Gdk.Screen.width()) then (Gdk.Screen.width() - alloc_popup.Gtk.width - 3) else x),
      (if y + alloc_popup.Gtk.height > (Gdk.Screen.height()) then (Gdk.Screen.height() - alloc_popup.Gtk.height - 3) else y);
    in x, y
  in
  window#move ~x ~y:(y + 3);
  widget, window;;
