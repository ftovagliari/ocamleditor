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
[@@@warning "-48"]

open Printf

let info ?(title="") ?(message_type=`INFO) ~message widget =
  match GWindow.toplevel widget with
  | None -> ()
  | Some parent ->
      if message <> "" then begin
        let message = GWindow.message_dialog ~message ~title
            ~modal:true ~position:`CENTER ~parent ~type_hint:`DIALOG
            ~message_type ~buttons:(GWindow.Buttons.ok) () in
        message#set_transient_for parent#as_window;
        ignore(message#run());
        message#destroy()
      end

let message ?(title="") ~message message_type =
  let message = GWindow.message_dialog ~message ~title
      ~modal:true ~position:`CENTER ~type_hint:`DIALOG ~icon:(!Otherwidgets_config.app_icon())
      ~message_type ~buttons:(GWindow.Buttons.ok) () in
  ignore(message#run());
  message#destroy()

let display_exn ?parent ?title ?(message="") e =
  let display ?parent () =
    let message = sprintf "%s\n\n%s\n\n%s" message (Printexc.to_string e) (Printexc.get_backtrace()) in
    let message = String.trim message in
    let dialog = GWindow.message_dialog ~message ?title
        ~modal:true ~destroy_with_parent:false ~position:`CENTER ?parent
        ~message_type:`ERROR ~buttons:(GWindow.Buttons.ok) () in
    ignore(dialog#run());
    dialog#destroy()
  in
  match parent with
  | None -> display ()
  | Some widget ->
      begin
        match GWindow.toplevel widget with
        | None -> display ()
        | Some parent -> display ~parent ()
      end

let process_still_active ~name ~ok ~cancel () =
  let dialog = GWindow.message_dialog ~title:"Process is still active"
      ~message:(sprintf "Terminate process \"%s\"?" name)
      ~message_type:`QUESTION ~buttons:GWindow.Buttons.ok_cancel ~modal:true ~position:`CENTER () in
  begin match dialog#run () with
  | `OK -> ok()
  | `CANCEL ->
      dialog#destroy();
      cancel()
  | _ -> ()
  end;
  dialog#destroy();;

let confirm ?(title="") ?image ~message ~yes ~no ?(cancel=true) parent =
  let dialog = GWindow.dialog
      ~title
      ~urgency_hint:true
      ~position:`CENTER
      ~modal:true
      ~border_width:8
      ~icon:(!Otherwidgets_config.app_icon())
      () in
  let yes_text, yes_func = yes in
  let no_text, no_func = no in
  dialog#add_button yes_text `YES;
  dialog#add_button no_text `NO;
  if cancel then (dialog#add_button "Cancel" `CANCEL);
  dialog#set_default_response `YES;
  dialog#set_skip_taskbar_hint false;
  dialog#set_skip_pager_hint false;
  dialog#vbox#set_spacing 8;
  Gaux.may ~f:(fun x -> dialog#set_transient_for x#as_window) (GWindow.toplevel parent);
  let hbox = GPack.hbox ~spacing:8 ~packing:dialog#vbox#add () in
  Gaux.may image ~f:hbox#pack;
  hbox#pack (match image with
      | Some image -> image
      | _ -> (GMisc.image ~stock:`DIALOG_QUESTION ~icon_size:`DIALOG ())#coerce);
  let _ = GMisc.label ~xalign:0.0 ~text:message ~packing:hbox#add () in
  match dialog#run () with
  | `YES ->
      yes_func ();
      dialog#destroy();
      `YES
  | `NO ->
      no_func ();
      dialog#destroy();
      `NO
  | _ ->
      dialog#destroy();
      `CANCEL









