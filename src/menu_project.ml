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

(** Commands of the Project menu. *)
let register ~browser =
  let open GdkKeysyms in
  let editor = browser#editor in
  let has_project () = browser#current_project#get <> None in
  let has_default_target () =
    match browser#current_project#get with Some x when Project.default_target x <> None -> true | _ -> false
  in
  let is_ocaml_source () =
    match editor#get_page `ACTIVE with
    | Some page -> let name = Filename.basename page#get_filename in name ^^^ ".ml" || name ^^^ ".mli"
    | None -> false
  in
  (* Labels that mention the default target / runtime configuration / current file. *)
  let with_target fmt default () =
    let label = ref default in
    browser#with_default_target (fun target -> label := sprintf fmt target.Target.name);
    !label
  in
  let with_runtime_config fmt default () =
    let label = ref default in
    browser#with_default_runtime_config ~open_dialog:false (fun rc -> label := sprintf fmt rc.Rconf.name);
    !label
  in
  let compile_file_label () =
    match editor#get_page `ACTIVE with
    | Some page when is_ocaml_source () -> sprintf "Compile \xC2\xAB%s\xC2\xBB" (Filename.basename page#get_filename)
    | _ -> "Compile"
  in
  let exec_on_default_target kind () =
    browser#with_current_project (fun _ ->
        browser#with_default_target (fun target -> ignore (Task_console.exec ~editor kind target)))
  in
  let ctrl = [`CONTROL] and ctrl_shift = [`CONTROL; `SHIFT] and ctrl_alt = [`CONTROL; `MOD1] in
  let reg id label ?label_fn ?accels ?enabled run =
    ignore (Command.register ~id:("project." ^ id) ~label ?label_fn ?accels ?enabled run)
  in
  reg "clean_default" "Clean" ~label_fn:(with_target "Clean \xC2\xAB%s\xC2\xBB" "Clean")
    ~accels:[ctrl_shift, _Delete; ctrl, _F9] ~enabled:has_default_target (exec_on_default_target `CLEAN);
  reg "compile_only" "Compile" ~label_fn:(with_target "Compile \xC2\xAB%s\xC2\xBB" "Compile")
    ~accels:[ctrl, _B; ctrl, _F10] ~enabled:has_default_target (exec_on_default_target `COMPILE_ONLY);
  reg "build" "Build" ~label_fn:(with_target "Build \xC2\xAB%s\xC2\xBB" "Build")
    ~accels:[ctrl_shift, _B] ~enabled:has_default_target (exec_on_default_target `COMPILE);
  reg "run" "Run" ~label_fn:(with_runtime_config "Run \xC2\xAB%s\xC2\xBB" "Run")
    ~accels:[[], _F5] ~enabled:has_default_target begin fun () ->
    browser#with_current_project (fun project ->
        browser#with_default_runtime_config ~open_dialog:true (fun rc ->
            let bc = List.find (fun b -> b.Target.id = rc.Rconf.target_id) project.Prj.targets in
            ignore (Task_console.exec ~editor (`RCONF rc) bc)))
  end;
  reg "build_all" "All targets" ~accels:[ctrl_alt, _F10] ~enabled:has_project begin fun () ->
    browser#with_current_project (fun project -> browser#build_all project.Prj.targets)
  end;
  reg "clean_project" "Clean Project" ~accels:[ctrl_alt, _F9] ~enabled:has_default_target browser#project_clean;
  reg "compile_file" "Compile file" ~label_fn:compile_file_label
    ~enabled:(fun () -> has_project () && is_ocaml_source ()) begin fun () ->
    editor#with_current_page begin fun p ->
      if Preferences.preferences#get.Settings_j.editor_save_all_bef_comp then (editor#save_all());
      p#compile_buffer ?join:None ()
    end
  end;
  reg "properties" "Project Properties" ~accels:[[`MOD1], _p] ~enabled:has_project begin fun () ->
    browser#dialog_project_properties ?page_num:(Some "general") ?show:(Some true) (())
  end;
  reg "targets" "Targets" ~accels:[[`MOD1], _Return] ~enabled:has_project begin fun () ->
    browser#dialog_project_properties ?page_num:(Some "targets") ?show:(Some true) (())
  end;
  reg "build_script" "Generate Build Script" ~enabled:has_default_target begin fun () ->
    browser#with_current_project begin fun project ->
      let dialog = Build_script_ui.window ~project () in
      Gaux.may (GWindow.toplevel editor) ~f:(fun w -> dialog#set_transient_for w#as_window)
    end
  end;
  reg "dune" "Generate dune Files" begin fun () ->
    browser#with_current_project begin fun project ->
      let summary =
        try
          let outcome = Project_dune.write project in
          let listing label = function
            | [] -> []
            | files -> [sprintf "%s:\n  %s" label (String.concat "\n  " files)]
          in
          let diagnostics =
            outcome.Project_dune.oc_diagnostics
            |> List.filter (fun d -> d.Project_dune.dg_severity <> `INFO)
            |> List.map begin fun d ->
              sprintf "  %s: %s%s"
                (Project_dune.string_of_severity d.Project_dune.dg_severity)
                (if d.Project_dune.dg_file <> "" then d.Project_dune.dg_file ^ ": " else "")
                d.Project_dune.dg_message
            end
          in
          let parts =
            listing "Updated" outcome.Project_dune.oc_changed
            @ listing "Removed" outcome.Project_dune.oc_removed
            @ (match diagnostics with
                | [] -> []
                | _ -> [sprintf "Not expressible in dune:\n%s" (String.concat "\n" diagnostics)])
          in
          match parts with
          | [] -> "The dune files are already up to date."
          | parts -> String.concat "\n\n" parts
        with ex -> sprintf "Cannot generate the dune files.\n\n%s" (Printexc.to_string ex)
      in
      Dialog.info ~title:"Generate dune Files" ~message:summary editor
    end
  end;
  reg "refresh" "Refresh" ~enabled:has_project browser#refresh;
  reg "clear_cache" "Clear Cache" browser#clear_cache;
  has_default_target

(** Creates the Project menu. [items.project] is the menu to fill: the browser appends to it the
    entries of the project history. *)
let create ~browser ~group ~items =
  let has_default_target = register ~browser in
  let editor = browser#editor in
  (* One entry per target / runtime configuration, rebuilt every time the menu opens. *)
  let per_target ~label ~enabled ~static ~targets ~exec =
    Menu_builder.dynamic_submenu ~group ~category:"Project" ~label ~enabled ~static ~fill:begin fun submenu ->
      match browser#current_project#get with
      | None -> []
      | Some project ->
          List.map begin fun x ->
            let item = GMenu.menu_item ~label:(fst x) ~packing:submenu#add () in
            ignore (item#connect#activate ~callback:(fun () -> exec project (snd x)));
            item
          end (targets project)
    end
  in
  let target_names project = List.map (fun tg -> tg.Target.name, tg) project.Prj.targets in
  let clean_menu =
    per_target ~label:"Clean..." ~enabled:has_default_target ~static:[]
      ~targets:target_names ~exec:(fun _ tg -> ignore (Task_console.exec ~editor `CLEAN tg))
  in
  let build_menu =
    per_target ~label:"Build..." ~enabled:has_default_target
      ~static:Menu_builder.[item "project.build_all"; separator]
      ~targets:target_names ~exec:(fun _ tg -> ignore (Task_console.exec ~editor `COMPILE tg))
  in
  let build_dep_menu =
    per_target ~label:"Build with dependencies..." ~enabled:has_default_target ~static:[]
      ~targets:target_names
      ~exec:(fun _ tg -> ignore (Task_console.exec ~editor ~with_deps:true `COMPILE tg))
  in
  let run_menu =
    per_target ~label:"Run..." ~enabled:has_default_target ~static:[]
      ~targets:(fun project -> List.map (fun rc -> rc.Rconf.name, rc) project.Prj.executables)
      ~exec:begin fun project rc ->
        try
          let bc = List.find (fun b -> b.Target.id = rc.Rconf.target_id) project.Prj.targets in
          ignore (Task_console.exec ~editor (`RCONF rc) bc)
        with Not_found -> ()
      end
  in
  (* Separator and check items of the project history (the latter are added by the browser). *)
  let history menu =
    let sep = GMenu.separator_item ~packing:menu#add () in
    ignore (menu#misc#connect#map ~callback:begin fun () ->
        if browser#project_history.File_history.content <> [] then sep#misc#show () else sep#misc#hide ();
        browser#with_current_project begin fun project ->
          let current_project_filename = Project.Path.fullname project in
          List.iter begin fun (filename, item) ->
            items.project_history_signal_locked <- true;
            item#set_active (filename = current_project_filename);
            items.project_history_signal_locked <- false;
          end items.project_history
        end
      end)
  in
  Menu_builder.create ~group ~label:"Project" ~menu:items.project Menu_builder.[
      item "project.clean_default"; item "project.compile_only"; item "project.build"; item "project.run";
      custom clean_menu; custom build_menu; custom build_dep_menu; custom run_menu;
      item "project.clean_project";
      separator;
      item "project.compile_file";
      separator;
      item "project.properties"; item "project.targets"; item "project.build_script"; item "project.dune";
      item "project.refresh"; item "project.clear_cache";
      custom history;
    ]
