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


open Menu_types
open Utils

let recent_items = ref []

let file_recent_callback ~(file_recent_menu : GMenu.menu) editor =
  let count = ref 0 in
  try
    List.iter file_recent_menu#remove !recent_items;
    recent_items := [];
    List.iter begin fun filename ->
      incr count;
      let label = filename in
      let mi = GMenu.menu_item ~label ~packing:(file_recent_menu#insert ~pos:1) () in
      recent_items := mi :: !recent_items;
      ignore (mi#connect#activate ~callback:(fun () ->
          ignore (editor#open_file ~active:true ~scroll_offset:0 ~offset:0 ?remote:None filename)));
      if !count > 30 then (raise Exit)
    end (List.rev editor#file_history.File_history.content);
  with Exit -> ()

let get_file_switch_sensitive page =
  let name = page#get_filename in (name ^^^ ".ml" || name ^^^ ".mli")

open Preferences
let file ~browser ~group ~flags items =
  let editor = browser#editor in
  let file = GMenu.menu_item ~label:"File" () in
  let menu = GMenu.menu ~packing:file#set_submenu () in
  (* New Project *)
  let new_project = GMenu.menu_item ~label:"New Project..." ~packing:menu#add () in
  ignore (new_project#connect#activate ~callback:browser#dialog_project_new);
  (* New file *)
  let new_file = GMenu.image_menu_item ~image:(GMisc.image ~pixbuf:(??? Icons.new_file) (*~stock:`NEW*) ~icon_size:`MENU ())
      ~label:"New File..." ~packing:menu#add () in
  new_file#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._n ~flags;
  ignore (new_file#connect#activate ~callback:browser#dialog_file_new);
  (* Open Project *)
  let _ = GMenu.separator_item ~packing:menu#add () in
  let project_open = GMenu.image_menu_item
      ~label:"Open Project..." ~packing:menu#add () in
  ignore (project_open#connect#activate ~callback:browser#dialog_project_open);
  project_open#add_accelerator ~group ~modi:[`CONTROL;`SHIFT] GdkKeysyms._o ~flags;
  (* Open File *)
  let open_file = GMenu.image_menu_item ~image:(GMisc.image ~pixbuf:(??? Icons.open_file) (*~stock:`OPEN*) ~icon_size:`MENU ())
      ~label:"Open File..." ~packing:menu#add () in
  open_file#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._o ~flags;
  ignore (open_file#connect#activate ~callback:editor#dialog_file_open);
  let open_remote = GMenu.image_menu_item ~label:"Open Remote File..." ~packing:menu#add () in
  open_remote#connect#activate ~callback:begin fun () ->
    let title =
      match open_remote#misc#get_property "label" with
      | `STRING (Some x) -> x
      | _ -> ""
    in
    let window = GWindow.window
        ~resizable:false
        ~type_hint:`DIALOG
        ~allow_grow:false ~allow_shrink:false
        ~position:`CENTER ~border_width:8
        ~icon:(??? Icons.oe) ~title ~modal:true ~show:false ()
    in
    Gmisclib.Window.GeometryMemo.add ~key:"dialog-remote-edit" ~window Preferences.geometry_memo;
    let vbox = GPack.vbox ~spacing:8 ~packing:window#add () in
    let widget = new Remote.widget ~packing:vbox#add () in
    let _ = GMisc.separator `HORIZONTAL ~packing:vbox#pack () in
    let bbox = GPack.button_box `HORIZONTAL ~border_width:3 ~spacing:3 ~layout:`END ~packing:vbox#pack () in
    let button_ok = GButton.button ~stock:`OK ~packing:bbox#pack () in
    let button_cancel = GButton.button ~stock:`CANCEL ~packing:bbox#pack () in
    Gaux.may (GWindow.toplevel editor) ~f:(fun x -> window#set_transient_for x#as_window);
    ignore (button_cancel#connect#clicked ~callback:window#destroy);
    ignore (button_ok#connect#clicked ~callback:widget#apply);
    ignore (window#event#connect#key_press ~callback:begin fun ev ->
        let key = GdkEvent.Key.keyval ev in
        if key = GdkKeysyms._Escape then (button_cancel#clicked(); true)
        else false
      end);
    ignore (widget#connect#open_file ~callback:begin fun (remote, filename) ->
        ignore (editor#open_file ~active:true ~scroll_offset:0 ~offset:0 ?remote:(Some remote) filename);
        window#destroy();
      end);
    window#show();
  end |> ignore;

  (* Recent Files... *)
  let file_recent = GMenu.menu_item ~label:"Recent Files" ~packing:menu#add () in
  let file_recent_menu = GMenu.menu ~packing:file_recent#set_submenu () in
  (* file_recent_select *)
  file_recent_menu#add items.file_recent_select;
  ignore (items.file_recent_select#connect#activate ~callback:begin fun () ->
      browser#dialog_find_file ?all:(Some false) ()
    end);
  (* recent items *)
  ignore (file_recent#connect#activate ~callback:(fun () -> file_recent_callback ~file_recent_menu editor));
  (* file_recent_clear *)
  file_recent_menu#add items.file_recent_sep;
  file_recent_menu#add items.file_recent_clear;
  ignore (items.file_recent_clear#connect#activate ~callback:editor#file_history_clear);
  (* Switch Implementation/Interface *)
  menu#add items.file_switch;
  ignore (items.file_switch#connect#activate ~callback:(fun () -> editor#with_current_page editor#switch_mli_ml));
  items.file_switch#add_accelerator ~group ~modi:[`CONTROL; `SHIFT] GdkKeysyms._I ~flags;
  (* Save *)
  let _ = GMenu.separator_item ~packing:menu#add () in
  let save_file = GMenu.image_menu_item ~label:"Save" ~packing:menu#add () in
  save_file#set_image (GMisc.image ~pixbuf:(??? Icons.save_16) ())#coerce;
  save_file#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._s ~flags;
  ignore (save_file#connect#activate ~callback:begin fun () ->
      Gaux.may ~f:editor#save (editor#get_page `ACTIVE)
    end);
  (* Save as.. *)
  let save_as = GMenu.image_menu_item
      ~image:(GMisc.image ~pixbuf:(??? Icons.save_as_16) ())
      ~label:"Save As..." ~packing:menu#add () in
  save_as#add_accelerator ~group ~modi:[`CONTROL; `SHIFT] GdkKeysyms._s ~flags;
  ignore (save_as#connect#activate ~callback:begin fun () ->
      Gaux.may ~f:editor#dialog_save_as (editor#get_page `ACTIVE)
    end);
  (* Save All *)
  let save_all = GMenu.image_menu_item ~label:"Save All" ~packing:menu#add () in
  save_all#add_accelerator ~group ~modi:[`CONTROL; `SHIFT] GdkKeysyms._a ~flags;
  ignore (save_all#connect#activate ~callback:browser#save_all);
  save_all#set_image (GMisc.image ~pixbuf:(??? Icons.save_all_16) ())#coerce;
  (* Rename *)
  menu#add items.file_rename;
  ignore (items.file_rename#connect#activate ~callback:begin fun () ->
      editor#with_current_page begin fun current_page ->
        editor#dialog_rename current_page;
        browser#set_title ()
      end
    end);
  (* Close current *)
  let _ = GMenu.separator_item ~packing:menu#add () in
  menu#add (items.file_close :> GMenu.menu_item);
  items.file_close#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._F4 ~flags;
  ignore (items.file_close#connect#activate ~callback:begin fun () ->
      editor#with_current_page (fun p -> ignore (editor#dialog_confirm_close p))
    end);
  (* Close all except current *)
  menu#add items.file_close_all;
  ignore (items.file_close_all#connect#activate ~callback:begin fun () ->
      editor#with_current_page (fun p -> editor#close_all ?except:(Some p) ())
    end);
  (* Revert current *)
  menu#add (items.file_revert :> GMenu.menu_item);
  ignore (items.file_revert#connect#activate ~callback:begin fun () ->
      editor#with_current_page editor#revert
    end);
  (* Delete current *)
  let _ = GMenu.separator_item ~packing:menu#add () in
  menu#add (items.file_delete :> GMenu.menu_item);
  ignore (items.file_delete#connect#activate ~callback:editor#dialog_delete_current);
  (* Exit *)
  let _ = GMenu.separator_item ~packing:menu#add () in
  let quit = GMenu.image_menu_item ~label:"Exit" ~packing:menu#add () in
  quit#connect#activate ~callback:begin fun () ->
    let confirm =
      Dialog.confirm ~title:"Confirm exit" ~message:"Are you sure you want to close the editor?"
        ~yes:("Close", ignore) ~no:("Cancel", ignore) ~cancel:false browser#window
    in
    match confirm with `YES -> browser#exit editor () | _ -> ()
  end |> ignore;
  quit#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._q ~flags;
  (* callback *)
  ignore (file#misc#connect#state_changed ~callback:begin fun _ ->
      let page = editor#get_page `ACTIVE in
      let has_current_page = page <> None in
      List.iter (fun i -> i#misc#set_sensitive has_current_page) [
        (items.file_switch :> GMenu.menu_item);
        (save_file :> GMenu.menu_item);
        (save_as :> GMenu.menu_item);
        (save_all :> GMenu.menu_item);
        (items.file_rename :> GMenu.menu_item);
        (items.file_close :> GMenu.menu_item);
        (items.file_close_all :> GMenu.menu_item);
        (items.file_revert :> GMenu.menu_item);
        (items.file_delete :> GMenu.menu_item);
      ];
      Option.iter (fun page -> items.file_switch#misc#set_sensitive (get_file_switch_sensitive page)) page;
      let has_current_project = browser#current_project#get <> None in
      new_file#misc#set_sensitive has_current_project;
    end);
  file, menu;;

