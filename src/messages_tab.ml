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

let targets = [{ Gtk.target = "STRING"; flags = []; info = 0}]

class drag_handler = object
  method private beginning (_ : GObj.drag_context) = ()
  method private data_delete (_ : GObj.drag_context) = ()
  method private data_get (_ : GObj.drag_context) (_ : GObj.selection_context)
      ~(info : int) ~(time : int32) = ()
  method private data_received (_ : GObj.drag_context) ~(x : int) ~(y : int)
      (_ : GObj.selection_data) ~(info : int) ~(time : int32) = ()
  method private drop (_ : GObj.drag_context) ~(x : int) ~(y : int)
      ~(time : int32) = false
  method private ending (_ : GObj.drag_context) = ()
  method private leave (_ : GObj.drag_context) ~(time : int32) = ()
  method private motion (_ : GObj.drag_context) ~(x : int) ~(y : int)
      ~(time : int32) = false
end

open Preferences
class widget ~page ?label_widget ?(with_spinner=true) ?packing () =
  let ebox              = GBin.event_box ?packing () in
  let hbox              = GPack.hbox ~spacing:0 ~packing:ebox#add () in
  let button            = GButton.button ~relief:`NONE () in
  let spinner           = GMisc.image ~xpad:2 ~ypad:2 ~file:(Icon.get_themed_filename "spinner_16.gif") () in
  let image             = Icons.create (??? Icons.button_close_8) in
  let set_active =
    if with_spinner then fun active ->
      if active then begin
        (try hbox#remove (List.hd hbox#children) with _ -> ());
        hbox#pack spinner#coerce;
        hbox#reorder_child ~pos:0 spinner#coerce;
      end else begin
        (try hbox#remove (List.hd hbox#children) with _ -> ());
        hbox#pack button#coerce;
        hbox#reorder_child ~pos:0 button#coerce;
      end;
    else (fun _ -> ())
  in
  object (self)
    inherit GObj.widget ebox#as_widget
    inherit drag_handler

    initializer self#init ()
    method hbox = hbox
    method button = button

    method set_active = set_active

    method private data_get _ sel ~info ~time =
      if info = 0 then (ksprintf sel#return "%d" page#misc#get_oid)

    method private data_delete _ = ()

    method private init () =
      ebox#misc#set_property "visible-window" (`BOOL false);
      ebox#drag#source_set targets ~modi:[`BUTTON1 ] ~actions:[`MOVE ];
      (*ebox#drag#source_set_icon drag_icon;*)
      ebox#drag#connect#data_get ~callback:self#data_get;
      ebox#drag#connect#data_delete ~callback:self#data_delete;
      (*  *)
      ignore (page#is_working#connect#changed ~callback:self#set_active);
      (*  *)
      button#connect#after#clicked ~callback:page#destroy;
      button#set_image image#coerce;
      button#misc#set_can_focus false;
      button#misc#set_can_default false;
      button#set_image image#coerce;
      ignore (button#event#connect#enter_notify ~callback:begin fun _ ->
          image#set_pixbuf (??? Icons.button_close_hi_8);
          false
        end);
      ignore (button#event#connect#leave_notify ~callback:begin fun _ ->
          image#set_pixbuf (??? Icons.button_close_8);
          false
        end);
      if with_spinner then (hbox#pack spinner#coerce) else (hbox#pack button#coerce);
      page#set_button button;
      Gaux.may label_widget ~f:hbox#pack;
  end




