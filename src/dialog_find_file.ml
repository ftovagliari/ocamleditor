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
open Utils
open Preferences

let filter =
  let include_filenames =
    List.map (!~~)
      ["README"; "INSTALL"; "NEWS"; "BUGS"; "CONTRIB"; "Makefile"; "TODO"; "AUTHORS"; "ChangeLog"; "META"; "ocamleditor_opam"]
  in
  fun filename ->
    filename ^^^ ".ml"  ||
    filename ^^^ ".mli" ||
    filename ^^^ ".mll" ||
    filename ^^^ ".mly" ||
    filename ^^^ ".bat" ||
    filename ^^^ ".cmd" ||
    filename ^^^ ".txt" ||
    filename ^^^ ".sh"  ||
    let len = String.length filename in
    List.exists begin fun re ->
      try ignore (Str.search_backward re filename len); true
      with Not_found -> false
    end include_filenames;;

let pixbuf_open_in_editor () = (??? Icons.button_close)

let help = "<small>Press \"<tt>Ctrl+Return</tt>\" to toggle open/close; press \"<tt>F3</tt>\" to move focus to the search entry; press \"<tt>Ctrl+Shift+L</tt>\" to show the list of files currently open in the editor.</small>";;

(** create *)
let create ?(all=true) ~(editor : Editor.editor) ~roots () =
  let title                = if all then "Find File" else "Select File" in
  let window               = GWindow.window ~title ~icon:(??? Icons.oe) ~height:500 ~modal:true ~decorated:false ~position:`CENTER_ALWAYS ~border_width:1 ~show:false () in
  let _                    = Gmisclib.Window.GeometryMemo.add ~key:"dialog-find-file" ~window Preferences.geometry_memo in
  let _                    = window#set_skip_taskbar_hint true in
  let _                    = window#set_skip_pager_hint true in
  let _                    = window#misc#modify_bg [`NORMAL, `COLOR (window#misc#style#bg `SELECTED)] in
  let ebox                 = GBin.event_box ~packing:window#add () in
  let _                    = window#set_focus_on_map true in
  let vbox                 = GPack.vbox ~spacing:5 ~border_width:5 ~packing:ebox#add () in
  (* Quick file chooser *)
  let source               =
    if all then begin
      ksprintf window#set_title "%s" title;
      `path (roots, editor#file_history.File_history.content)
    end else `filelist editor#file_history.File_history.content
  in
  let quick_file_chooser   = new Quick_file_chooser.widget ~source ~name:"" ~filter ~packing:vbox#add () in
  let _                    = GMisc.label ~xalign:0.0 ~width:600 ~line_wrap:true ~markup:help ~packing:vbox#pack () in
  (* Buttons *)
  let bbox                 = GPack.button_box `HORIZONTAL ~layout:`END ~border_width:8 ~spacing:8
      ~packing:(vbox#pack ~expand:false) () in
  let button_close         = GButton.button ~label:"Close Files" ~packing:bbox#add () in
  let _                    = button_close#set_focus_on_click false in
  let _                    = button_close#misc#set_tooltip_text "Ctrl+Return" in
  let button_open          = GButton.button ~label:"Open Files" ~packing:bbox#add () in
  let _                    = button_open#set_focus_on_click false in
  let _                    = button_open#misc#set_tooltip_text "Return" in
  let button_done          = GButton.button ~label:"Done" ~packing:bbox#add () in
  let button_clear_history = GButton.button ~label:"Clear File History" ~packing:bbox#add () in
  let _                    = button_clear_history#set_focus_on_click false in
  bbox#set_child_secondary button_clear_history#coerce true;
  (* Actions *)
  ignore (button_clear_history#connect#clicked ~callback:begin fun () ->
      File_history.clear editor#file_history;
    end);
  ignore (button_done#connect#clicked ~callback:(fun () -> window#misc#hide(); window#destroy()));
  let pixbuf = pixbuf_open_in_editor () in
  quick_file_chooser#set_default_choose_func begin fun ~filename ~has_cursor ->
    ignore (editor#open_file ~active:has_cursor ~scroll_offset:0 ~offset:0 ?remote:None filename);
    `set pixbuf;
  end;
  ignore (button_open#connect#clicked ~callback:begin fun () ->
      quick_file_chooser#activate();
    end);
  ignore (button_close#connect#clicked ~callback:begin fun () ->
      quick_file_chooser#activate ~f:begin fun ~filename ~has_cursor:_ ->
        Gaux.may (editor#get_page (`FILENAME filename)) ~f:editor#dialog_confirm_close;
        `clear
      end ();
    end);
  ignore (quick_file_chooser#view#connect#row_activated ~callback:begin fun _ _ ->
      quick_file_chooser#activate();
      window#destroy()
    end);
  window#event#connect#focus_out ~callback:begin fun _ ->
    Printf.printf "focus_out \n%!" ;
    window#misc#hide();
    window#destroy();
    true
  end |> ignore;
  let show_currently_opened () =
    quick_file_chooser#display (quick_file_chooser#get_paths_with_icon () |> List.map (fun x -> 0., x));
    Gmisclib.Idle.add ~prio:300 begin fun () ->
      editor#with_current_page begin fun page ->
        let filename = page#get_filename in
        match quick_file_chooser#get_path ~filename with
        | Some path ->
            begin
              try
                quick_file_chooser#select_path path;
                quick_file_chooser#set_cursor path;
              with Gpointer.Null -> ()
            end;
        | _ -> ()
      end
    end
  in
  begin
    match quick_file_chooser#source with
    | `path (hd :: _, _) ->
        let is_relative =  Utils.filename_relative hd in
        let len = String.length hd in
        quick_file_chooser#set_cell_data_func begin fun model row ->
          let dirname = model#get ~row ~column:Quick_file_chooser.col_path in
          let ld = String.length dirname in
          quick_file_chooser#renderer#set_properties [`WEIGHT
                                                        (if ld >= len && is_relative dirname <> None then `BOLD else `NORMAL)]
        end
    | _ -> ()
  end;
  (* Update icons *)
  let icon_func ~filename =
    let opened, changed =
      begin
        try
          match editor#get_page (`FILENAME filename) with
          | None -> false, false
          | Some page -> true, page#view#buffer#modified
        with Unix.Unix_error _ -> false, false
      end;
    in
    if changed then Some (??? Icons.save_14) else (if opened then Some (pixbuf_open_in_editor ()) else None)
  in
  let update_icons () =
    GtkThread2.sync quick_file_chooser#reset_icons ();
    let filenames = List.map (fun p -> p#get_filename) editor#pages in
    List.iter begin fun filename ->
      GtkThread2.sync begin fun () ->
        match quick_file_chooser#get_path ~filename with
        | Some path ->
            let row = quick_file_chooser#model#get_iter path in
            let pixbuf = icon_func ~filename in
            quick_file_chooser#set_icon ~row pixbuf;
        | _ -> ()
      end ();
    end filenames;
  in
  (* Escape *)
  ignore (window#event#connect#key_press ~callback:begin fun ev ->
      let state = GdkEvent.Key.state ev in
      let key = GdkEvent.Key.keyval ev in
      if key = GdkKeysyms._Escape then (window#destroy(); true)
      else if List.for_all (fun x -> List.mem x [`CONTROL; `SHIFT]) state && key = GdkKeysyms._L then begin
        show_currently_opened ();
        true
      end else if state = [`CONTROL] && key = GdkKeysyms._Return then begin
        quick_file_chooser#activate ~f:begin fun ~filename ~has_cursor ->
          match editor#get_page (`FILENAME filename) with
          | Some page ->
              let is_closed = editor#dialog_confirm_close page in
              if is_closed then `clear else `ignore
          | _ ->
              ignore (editor#open_file ~active:has_cursor ~scroll_offset:0 ~offset:0 ?remote:None filename);
              `set (pixbuf_open_in_editor ())
        end ();
        true
      end else if key = GdkKeysyms._Return then begin
        quick_file_chooser#activate();
        window#destroy();
        true
      end else false
    end);
  (* Present *)
  window#present();
  quick_file_chooser#update_model();
  update_icons ();
  show_currently_opened();
  window
;;



