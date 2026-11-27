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
          ignore (editor#open_file ~active:true ~offset:0 ?remote:None filename)));
      if !count > 30 then (raise Exit)
    end (List.rev editor#file_history.File_history.content);
  with Exit -> ()

let get_file_switch_sensitive page =
  let name = page#get_filename in (name ^^^ ".ml" || name ^^^ ".mli")

open Preferences

(** Commands of the File menu. *)
let register ~browser =
  let editor = browser#editor in
  let has_page () = editor#get_page `ACTIVE <> None in
  let has_project () = browser#current_project#get <> None in
  let has_history () = List.length editor#file_history.File_history.content > 0 in
  let is_ocaml_source () =
    match editor#get_page `ACTIVE with Some page -> get_file_switch_sensitive page | None -> false
  in
  (* Label that mentions the current file, e.g. "Close «file.ml»". *)
  let with_file_name fmt default () =
    match editor#get_page `ACTIVE with
    | Some page -> Printf.sprintf fmt (Filename.basename page#get_filename)
    | None -> default
  in
  let ctrl = [`CONTROL] and ctrl_shift = [`CONTROL; `SHIFT] in
  let reg id label ?label_fn ?accels ?enabled run =
    ignore (Command.register ~id:("file." ^ id) ~label ?label_fn ?accels ?enabled run)
  in
  reg "new_project" "New Project..." browser#dialog_project_new;
  reg "new_file" "New File..." ~accels:[ctrl, GdkKeysyms._n] ~enabled:has_project browser#dialog_file_new;
  reg "open_project" "Open Project..." ~accels:[ctrl_shift, GdkKeysyms._o] browser#dialog_project_open;
  reg "open_file" "Open File..." ~accels:[ctrl, GdkKeysyms._o] editor#dialog_file_open;
  reg "open_remote" "Open Remote File..." begin fun () ->
    let window = GWindow.window
        ~resizable:false
        ~type_hint:`DIALOG
        ~position:`CENTER ~border_width:8
        ~icon:(??? Logo.oe) ~title:"Open Remote File..." ~modal:true ~show:false ()
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
        ignore (editor#open_file ~active:true ~offset:0 ?remote:(Some remote) filename);
        window#destroy();
      end);
    window#show()
  end;
  reg "recent_select" "Select File..." ~enabled:has_history (fun () -> browser#dialog_find_file ?all:(Some false) ());
  reg "recent_clear" "Clear File History" ~enabled:has_history editor#file_history_clear;
  reg "switch" "Switch to Implementation/Interface" ~accels:[ctrl_shift, GdkKeysyms._I]
    ~enabled:is_ocaml_source (fun () -> editor#with_current_page editor#switch_mli_ml);
  reg "save" "Save" ~accels:[ctrl, GdkKeysyms._s] ~enabled:has_page begin fun () ->
    Gaux.may ~f:editor#save (editor#get_page `ACTIVE)
  end;
  reg "save_as" "Save As..." ~accels:[ctrl_shift, GdkKeysyms._s] ~enabled:has_page begin fun () ->
    Gaux.may ~f:editor#dialog_save_as (editor#get_page `ACTIVE)
  end;
  reg "save_all" "Save All" ~accels:[ctrl_shift, GdkKeysyms._a] ~enabled:has_page browser#save_all;
  reg "rename" "Rename" ~label_fn:(with_file_name "Rename \xC2\xAB%s\xC2\xBB" "Rename") ~enabled:has_page begin fun () ->
    editor#with_current_page begin fun current_page ->
      editor#dialog_rename current_page;
      browser#set_title ()
    end
  end;
  reg "close_project" "Close Project" ~enabled:has_project browser#project_close;
  reg "close" "Close" ~label_fn:(with_file_name "Close \xC2\xAB%s\xC2\xBB" "Close")
    ~accels:[ctrl, GdkKeysyms._F4] ~enabled:has_page begin fun () ->
    editor#with_current_page (fun p -> ignore (editor#dialog_confirm_close p))
  end;
  reg "close_all" "Close All" ~label_fn:(with_file_name "Close All Except \xC2\xAB%s\xC2\xBB" "Close All")
    ~enabled:has_page begin fun () ->
    editor#with_current_page (fun p -> editor#close_all ?except:(Some p) ())
  end;
  reg "revert" "Revert" ~label_fn:(with_file_name "Revert to Saved \xC2\xAB%s\xC2\xBB" "Revert")
    ~enabled:has_page (fun () -> editor#with_current_page editor#revert);
  reg "delete" "Delete" ~label_fn:(with_file_name "Delete \xC2\xAB%s\xC2\xBB" "Delete")
    ~enabled:has_page editor#dialog_delete_current;
  reg "exit" "Exit" ~accels:[ctrl, GdkKeysyms._q] begin fun () ->
    let confirm =
      Dialog.confirm ~title:"Confirm exit" ~message:"Are you sure you want to close the editor?"
        ~yes:("Close", ignore) ~no:("Cancel", ignore) ~cancel:false browser#window
    in
    match confirm with `YES -> browser#exit editor () | _ -> ()
  end

(** Creates the File menu. *)
let create ~browser ~group =
  register ~browser;
  let editor = browser#editor in
  (* The recent files are dynamic: they are rebuilt every time the menu opens. *)
  let recent =
    Menu_builder.dynamic_submenu ~group ~category:"File" ~label:"Recent Files"
      ~static:Menu_builder.[item "file.recent_select"; separator; item "file.recent_clear"]
      ~fill:(fun submenu -> file_recent_callback ~file_recent_menu:submenu editor; [])
  in
  Menu_builder.create ~group ~label:"File" Menu_builder.[
      item "file.new_project"; item "file.new_file";
      separator;
      item "file.open_project"; item "file.open_file"; item "file.open_remote";
      custom recent;
      item "file.switch";
      separator;
      item "file.save"; item "file.save_as"; item "file.save_all"; item "file.rename";
      separator;
      item "file.close_project";
      separator;
      item "file.close"; item "file.close_all"; item "file.revert";
      separator;
      item "file.delete";
      separator;
      item "file.exit";
    ]
