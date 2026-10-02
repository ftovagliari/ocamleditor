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
open Utils

(** file_select *)
let file_select ~editor () =
  let window = GWindow.window ~title:"Switch"
      ~width:600 ~height:400 ~modal:true
      ~position:`CENTER ~border_width:5 ~show:false () in
  Gmisclib.Window.GeometryMemo.add ~key:"dialog-switch-window" ~window Preferences.geometry_memo;
  Gaux.may (GWindow.toplevel editor) ~f:(fun x -> window#set_transient_for x#as_window);
  let vbox = GPack.vbox ~spacing:0 ~packing:window#add () in
  let cols = new GTree.column_list in
  let col_icon  = cols#add Gobject.Data.string in
  let col_name  = cols#add Gobject.Data.string in
  let col_path  = cols#add Gobject.Data.string in
  let model = GTree.list_store cols in
  let renderer = GTree.cell_renderer_text [] in
  let renderer_bold = GTree.cell_renderer_text [] in
  let renderer_icon = GTree.cell_renderer_pixbuf [] in
  renderer_bold#set_properties [`WEIGHT `BOLD];
  let vc_name = GTree.view_column ~title:"File" ~renderer:(renderer_bold, ["text", col_name]) () in
  let vc_path = GTree.view_column ~title:"Path" ~renderer:(renderer, ["text", col_path]) () in
  let vc_icon = GTree.view_column ~title:"" ~renderer:(renderer_icon, ["stock-id", col_icon]) () in
  let sw = GBin.scrolled_window ~shadow_type:`IN ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
      ~packing:vbox#add () in
  let view = GTree.view ~model:model ~headers_visible:false ~reorderable:true ~width:130 ~packing:sw#add () in
  view#append_column vc_icon |> ignore;
  view#append_column vc_name |> ignore;
  view#append_column vc_path |> ignore;
  view#selection#set_mode `MULTIPLE;
  view#set_search_column 1;
  List.iter begin fun p ->
    let row = model#append() in
    let _, _, label = p#tab_widget in
    model#set ~row ~column:col_name label#text;
    model#set ~row ~column:col_path p#get_filename;
    let opened, changed =
      match editor#get_page (`FILENAME p#get_filename) with
      | None -> false, false
      | Some page -> true, page#view#buffer#modified
    in
    model#set ~row ~column:col_icon (if changed then "gtk-floppy" else "");
  end editor#pages;
  let bbox = GPack.button_box `HORIZONTAL ~layout:`END ~border_width:8 ~spacing:8
      ~packing:(vbox#pack ~expand:false) () in
  let button_close = GButton.button ~label:"Close files" ~packing:bbox#add () in
  let button_cancel = GButton.button ~label:"Done" ~packing:bbox#add () in
  button_cancel#connect#clicked ~callback:window#destroy |> ignore;
  let activate () =
    List.iter begin fun path ->
      let row = model#get_iter path in
      let filename = model#get ~row ~column:col_path in
      ignore (editor#open_file ~active:true ~offset:0 ?remote:None filename)
    end view#selection#get_selected_rows;
    window#destroy()
  in
  ignore (view#connect#row_activated ~callback:(fun _ _ -> activate ()));
  ignore (button_close#connect#clicked ~callback:begin fun () ->
      let closing = ref [] in
      List.iter begin fun path ->
        let row = model#get_iter path in
        let filename = model#get ~row ~column:col_path in
        let page = editor#open_file ~active:true ~offset:0 ?remote:None filename in
        Gaux.may (editor#get_page `ACTIVE) ~f:(fun p -> ignore (editor#dialog_confirm_close p));
        closing := row :: !closing;
      end view#selection#get_selected_rows;
      ignore (List.map model#remove !closing)
    end);
  ignore (window#event#connect#key_release ~callback:begin fun ev ->
      let key = GdkEvent.Key.keyval ev in
      if key = GdkKeysyms._Escape then (window#destroy(); true)
      else begin
        window#present();
        false
      end
    end);
  editor#with_current_page begin fun page ->
    model#foreach begin fun _ row ->
      let filename = model#get ~row ~column:col_path in
      if filename = page#get_filename then begin
        view#selection#select_iter row;
        false
      end else false
    end;
    window#misc#set_can_focus true;
    window#misc#grab_focus();
    view#misc#grab_focus();
    window#present()
  end


(** confirm_close *)
let confirm_close ~editor (page : Editor_page.page) =
  if page#buffer#modified then begin
    let message = sprintf "File modified: \xC2\xAB%s\xC2\xBB. Do you wish to save changes?"
        (Filename.basename page#get_filename) in
    let response = Dialog.confirm
        ~title:"Close Modified File"
        ~message ~image:(GMisc.image ~stock:`SAVE ~icon_size:`DIALOG ())#coerce
        ~yes:("Save", begin fun () ->
            editor#save page;
            editor#close page;
          end)
        ~no:("Do Not Save", begin fun () ->
            editor#close page;
          end) page
    in response <> `CANCEL
  end else (editor#close page; true)


open Preferences

(** save_modified *)
let save_modified ~editor ~close ~callback pages =
  if pages <> [] then begin
    let dialog = GWindow.dialog ~position:`CENTER ~border_width:5
        ~icon:(??? Icons.oe) ~modal:true ~title:"Save Modified" () in
    let checklist = new Checklist.checklist
      ~packing:dialog#vbox#add
      (List.map (fun (x, p) -> x, p#get_filename) pages) in
    dialog#add_button_stock `OK `OK;
    dialog#add_button_stock `CANCEL `CANCEL;
    dialog#action_area#add checklist#button_all#coerce;
    dialog#action_area#add checklist#button_none#coerce;
    dialog#action_area#set_child_secondary checklist#button_all#coerce true;
    dialog#action_area#set_child_secondary checklist#button_none#coerce true;
    match dialog#run () with
    | `OK ->
        checklist#iter begin fun save filename ->
          let _, page = List.find (fun (_, p) -> p#get_filename = filename) pages in
          if save then (editor#save page) else (Autosave.delete ~filename ());
          if close then editor#close page;
        end;
        dialog#destroy();
        callback();
    | _ -> dialog#destroy()
  end else (callback())


(** file_open *)
let file_open ~editor () =
  let path = editor#project.Prj.root // Prj.default_dir_src in
  let filters = [
    ("Source files", ["*.ml*"; "README*"; "INSTALL*"; "META";
                      "ChangeLog"; "CHANGES"; "NEWS*"; "TODO*"; "BUGS*"; "CONTRIB*";
                      "Makefile*"; "*.sh"; "*.bat"; "*.cmd"]);
    ("All files", ["*"])]
  in
  let dialog = GWindow.file_chooser_dialog ~action:`OPEN ~width:600 ~height:600
      ~title:"Open file..." ~icon:(??? Icons.oe) ~position:`CENTER ~show:false () in
  List.iter begin fun (name, patterns) ->
    dialog#add_filter (GFile.filter ~name ~patterns ())
  end filters;
  dialog#add_select_button_stock `OK `OK;
  dialog#add_button_stock `CANCEL `CANCEL;
  dialog#set_current_folder path |> ignore;
  dialog#set_select_multiple true;
  match dialog#run () with
  | `OK ->
      List.iter (fun filename ->
          ignore (editor#open_file ~active:true ~offset:0 ?remote:None filename)) dialog#get_filenames;
      dialog#destroy()
  | _ -> dialog#destroy()

