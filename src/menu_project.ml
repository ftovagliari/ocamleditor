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
open Menu_types

let set_label item text = item#misc#set_property "label" (`STRING (Some text));;

let state_changed_callback
    ~project_clean_default_target
    ~project_compile_only
    ~project_build
    ~project_run
    ~project_comp_file
    ~project_script
    ~build_dep_item
    ~clean_item
    ~build_item
    ~run_item
    ~project_clean
    ~project_refresh
    ~project_targets
    ~dialog_project_properties
    ~sep1
    ~sep2
    ~sep3
    ~clean_menu
    ~build_menu
    ~build_dep_menu
    ~run_menu
    ~items
    ~group
    ~flags
    editor browser =
  browser#with_default_target begin fun target ->
    ksprintf (set_label project_clean_default_target) "Clean \xC2\xAB%s\xC2\xBB" target.Target.name;
    ksprintf (set_label project_compile_only) "Compile \xC2\xAB%s\xC2\xBB" target.Target.name;
    ksprintf (set_label project_build) "Build \xC2\xAB%s\xC2\xBB" target.Target.name;
  end;
  browser#with_default_runtime_config ~open_dialog:false begin fun rc ->
    ksprintf (set_label project_run) "Run \xC2\xAB%s\xC2\xBB" rc.Rconf.name;
  end;
  editor#with_current_page begin fun page ->
    let name = Filename.basename page#get_filename in
    if name ^^^ ".ml" || name ^^^ ".mli" then begin
      ksprintf (set_label project_comp_file) "Compile \xC2\xAB%s\xC2\xBB" name;
      project_comp_file#misc#set_sensitive true
    end else begin
      ksprintf (set_label project_comp_file) "Compile";
      project_comp_file#misc#set_sensitive false
    end;
  end;
  (*  *)
  let has_current_project = browser#current_project#get <> None in
  let has_default_target = match browser#current_project#get with Some x when Project.default_target x <> None -> true | _ -> false in
  project_clean_default_target#misc#set_sensitive has_default_target;
  project_clean_default_target#misc#set_property "sensitive" (`BOOL has_default_target);
  project_compile_only#misc#set_property "sensitive" (`BOOL has_default_target);
  project_build#misc#set_property "sensitive" (`BOOL has_default_target);
  project_run#misc#set_property "sensitive" (`BOOL has_default_target);
  project_script#misc#set_property "sensitive" (`BOOL has_default_target);
  build_dep_item#misc#set_property "sensitive" (`BOOL has_default_target);
  clean_item#misc#set_property "sensitive" (`BOOL has_default_target);
  build_item#misc#set_property "sensitive" (`BOOL has_default_target);
  run_item#misc#set_property "sensitive" (`BOOL has_default_target);
  project_clean#misc#set_property "sensitive" (`BOOL has_default_target);
  project_refresh#misc#set_property "sensitive" (`BOOL has_current_project);
  project_targets#misc#set_property "sensitive" (`BOOL has_current_project);
  dialog_project_properties#misc#set_property "sensitive" (`BOOL has_current_project);
  project_comp_file#misc#set_property "sensitive" (`BOOL has_current_project);
  sep1#misc#set_property "sensitive" (`BOOL has_default_target);
  sep2#misc#set_property "sensitive" (`BOOL has_default_target);
  sep3#misc#set_property "visible" (`BOOL (browser#project_history.File_history.content <> []));
  (*  *)
  Gmisclib.Idle.add (fun () -> List.iter clean_menu#remove clean_menu#children);
  Gmisclib.Idle.add (fun () -> List.iter build_menu#remove build_menu#children);
  Gmisclib.Idle.add (fun () -> List.iter build_dep_menu#remove build_dep_menu#children);
  Gmisclib.Idle.add (fun () -> List.iter run_menu#remove run_menu#children);
  browser#with_current_project begin fun project ->
    let current_project_filename = Project.filename project in
    List.iter  begin fun (filename, item) ->
      items.project_history_signal_locked <- true;
      item#set_active (filename = current_project_filename);
      items.project_history_signal_locked <- false;
    end items.project_history;
    List.iter begin fun tg ->
      Gmisclib.Idle.add begin fun () ->
        let item = GMenu.menu_item ~label:tg.Target.name ~packing:clean_menu#add () in
        ignore (item#connect#activate ~callback:begin fun () ->
            ignore (Task_console.exec ~editor `CLEAN tg)
          end);
      end;
    end project.Prj.targets;
    Gmisclib.Idle.add begin fun () ->
      let item_all = GMenu.menu_item ~label:"All targets" ~packing:build_menu#add () in
      let _ = item_all#connect#activate ~callback:(fun () -> browser#build_all project.Prj.targets) in
      item_all#add_accelerator ~group ~modi:[`CONTROL;`MOD1] GdkKeysyms._F10 ~flags;
      ignore (GMenu.separator_item ~packing:build_menu#add ());
    end;
    List.iter begin fun tg ->
      Gmisclib.Idle.add begin fun () ->
        let item = GMenu.menu_item ~label:tg.Target.name ~packing:build_menu#add () in
        ignore (item#connect#activate ~callback:begin fun () ->
            ignore (Task_console.exec ~editor `COMPILE tg)
          end);
        let item = GMenu.menu_item ~label:tg.Target.name ~packing:build_dep_menu#add () in
        ignore (item#connect#activate ~callback:begin fun () ->
            ignore (Task_console.exec ~editor ~with_deps:true `COMPILE tg)
          end);
      end
    end project.Prj.targets;
    List.iter begin fun rc ->
      Gmisclib.Idle.add begin fun () ->
        let item = GMenu.menu_item ~label:rc.Rconf.name ~packing:run_menu#add () in
        ignore (item#connect#activate ~callback:begin fun () ->
            try
              let bc = List.find (fun b -> b.Target.id = rc.Rconf.target_id) project.Prj.targets in
              ignore (Task_console.exec ~editor (`RCONF rc) bc)
            with Not_found -> ()
          end);
      end
    end project.Prj.executables;
  end

let project ~browser ~group ~flags items =
  let editor = browser#editor in
  let project = GMenu.menu_item ~label:"Project" () in
  let menu = items.project in
  let cursor = Gdk.Cursor.create `ARROW in
  ignore (menu#event#connect#expose ~callback:begin fun _ ->
      Gdk.Window.set_cursor menu#misc#window cursor;
      false
    end);
  project#set_submenu menu;

  (* Clean default target *)
  let open Preferences in
  let project_clean_default_target = GMenu.image_menu_item ~label:"Clean" ~packing:menu#add () in
  project_clean_default_target#set_image (Icons.create (??? Icons.clear_build_16))#coerce;
  project_clean_default_target#add_accelerator ~group ~modi:[`CONTROL;`SHIFT] GdkKeysyms._Delete ~flags;
  project_clean_default_target#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._F9 ~flags;
  ignore (project_clean_default_target#connect#activate ~callback:begin fun () ->
      browser#with_current_project (fun _ ->
          browser#with_default_target (fun target ->
              ignore (Task_console.exec ~editor `CLEAN target)))
    end);

  (* Compile *)
  let project_compile_only = GMenu.image_menu_item ~label:"Compile" ~packing:menu#add () in
  project_compile_only#set_image (Icons.create (??? Icons.compile_all_16))#coerce;
  project_compile_only#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._B ~flags;
  project_compile_only#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._F10 ~flags;
  ignore (project_compile_only#connect#activate ~callback:begin fun () ->
      browser#with_current_project (fun _ ->
          browser#with_default_target (fun target ->
              ignore (Task_console.exec ~editor `COMPILE_ONLY target)))
    end);

  (* Build *)
  let project_build = GMenu.image_menu_item ~label:"Build" ~packing:menu#add () in
  project_build#set_image (Icons.create (??? Icons.build_16))#coerce;
  project_build#add_accelerator ~group ~modi:[`CONTROL;`SHIFT] GdkKeysyms._B ~flags;
  ignore (project_build#connect#activate ~callback:begin fun () ->
      browser#with_current_project (fun _ ->
          browser#with_default_target (fun target ->
              ignore (Task_console.exec ~editor `COMPILE target)))
    end);

  (* Run current *)
  let project_run = GMenu.image_menu_item ~label:"Run" ~packing:menu#add () in
  project_run#set_image (Icons.create (??? Icons.start_16))#coerce;
  project_run#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._F11 ~flags;
  ignore (project_run#connect#activate ~callback:begin fun () ->
      browser#with_current_project (fun project ->
          browser#with_default_runtime_config ~open_dialog:true (fun rc ->
              let bc = List.find (fun b -> b.Target.id = rc.Rconf.target_id) project.Prj.targets in
              ignore (Task_console.exec ~editor (`RCONF rc) bc)))
    end);

  (* Clean... *)
  let clean_item = GMenu.image_menu_item ~label:"Clean..." ~packing:menu#add () in
  let clean_menu = GMenu.menu ~packing:clean_item#set_submenu () in

  (* Build... *)
  let build_item = GMenu.image_menu_item ~label:"Build..." ~packing:(menu#insert ~pos:5) () in
  let build_menu = GMenu.menu ~packing:build_item#set_submenu () in

  (* Build with dependencies... *)
  let build_dep_item = GMenu.image_menu_item ~label:"Build with dependencies..." ~packing:(menu#insert ~pos:6) () in
  let build_dep_menu = GMenu.menu ~packing:build_dep_item#set_submenu () in

  (* Run... *)
  let run_item = GMenu.image_menu_item ~label:"Run..." ~packing:menu#add () in
  let run_menu = GMenu.menu ~packing:run_item#set_submenu () in

  (* Clean Project *)
  let project_clean = GMenu.image_menu_item ~label:"Clean Project" ~packing:menu#add () in
  project_clean#add_accelerator ~group ~modi:[`CONTROL; `MOD1] GdkKeysyms._F9 ~flags;
  ignore (project_clean#connect#activate ~callback:browser#project_clean);
  let sep1 = GMenu.separator_item ~packing:menu#add () in

  (* Compile file *)
  let project_comp_file = GMenu.image_menu_item ~label:"Compile file" ~packing:menu#add () in
  project_comp_file#set_image (GMisc.image ~pixbuf:(??? Icons.compile_file_16) ())#coerce;
  ignore (project_comp_file#connect#activate ~callback:begin fun () ->
      browser#editor#with_current_page begin fun p ->
        if Preferences.preferences#get.editor_save_all_bef_comp then (editor#save_all());
        p#compile_buffer ?join:None ()
      end
    end);
  let sep2 = GMenu.separator_item ~packing:menu#add () in

  (* Project Properties *)
  let dialog_project_properties = GMenu.image_menu_item ~label:"Project Properties" ~packing:menu#add () in
  dialog_project_properties#set_image (Icons.create (??? Icons.project_16))#coerce;
  ignore (dialog_project_properties#connect#activate ~callback:(fun () ->
      browser#dialog_project_properties ?page_num:(Some 0) ?show:(Some true) ()));
  dialog_project_properties#add_accelerator ~group ~modi:[`CONTROL; `SHIFT] GdkKeysyms._P ~flags;

  (* Targets *)
  let project_targets = GMenu.image_menu_item ~image:(Icons.create (??? Icons.target_16))#coerce ~label:"Targets" ~packing:menu#add () in
  ignore (project_targets#connect#activate ~callback:(fun () ->
      browser#dialog_project_properties ?page_num:(Some 1) ?show:(Some true) ()));
  project_targets#add_accelerator ~group ~modi:[`MOD1] GdkKeysyms._Return ~flags;

  (* Generate build script *)
  let project_script = GMenu.image_menu_item ~label:"Generate Build Script" ~packing:menu#add () in
  ignore (project_script#connect#activate ~callback:(fun () ->
      browser#with_current_project begin fun project ->
        let dialog = Build_script_ui.window ~project () in
        Gaux.may (GWindow.toplevel editor) ~f:(fun w -> dialog#set_transient_for w#as_window);
      end));

  (* Project Refresh *)
  let project_refresh = GMenu.image_menu_item ~label:"Refresh" ~packing:menu#add () in
  project_refresh#set_image (GMisc.image ~pixbuf:(??? Icons.refresh16)(*~stock:`REFRESH*) ~icon_size:`MENU ())#coerce;
  ignore (project_refresh#connect#activate ~callback:browser#refresh);

  (* Project Clear Cache *)
  let project_clear_cache = GMenu.image_menu_item ~label:"Clear Cache" ~packing:menu#add () in
  ignore (project_clear_cache#connect#activate ~callback:browser#clear_cache);

  let sep3 = GMenu.separator_item ~packing:menu#add () in
  (* Callback *)
  ignore (project#misc#connect#state_changed ~callback:(fun _ -> state_changed_callback
                                                           ~project_clean_default_target
                                                           ~project_compile_only
                                                           ~project_build
                                                           ~project_run
                                                           ~project_comp_file
                                                           ~project_script
                                                           ~build_dep_item
                                                           ~clean_item
                                                           ~build_item
                                                           ~run_item
                                                           ~project_clean
                                                           ~project_refresh
                                                           ~project_targets
                                                           ~dialog_project_properties
                                                           ~sep1
                                                           ~sep2
                                                           ~sep3
                                                           ~clean_menu
                                                           ~build_menu
                                                           ~build_dep_menu
                                                           ~run_menu
                                                           ~items
                                                           ~group
                                                           ~flags
                                                           editor browser));
  project, menu

