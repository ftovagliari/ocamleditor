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


open GUtil
open Task
open Preferences

class view ?packing () =
  let changed = new changed () in
  let xalign = 0.0 in
  let vbox as etask_vbox = GPack.vbox ~spacing:8 ?packing () in
  let entry_name = GEdit.entry ~packing:vbox#pack () in
  let box = GPack.hbox ~spacing:5 ~packing:vbox#pack () in
  let check_always_project = GButton.check_button ~label:"Always run when building project" ~active:true ~packing:box#add () in
  let check_always_script = GButton.check_button ~label:"Always run when executing build script" ~active:true ~packing:box#add () in
  (* Phase *)
  let box = GPack.vbox ~packing:vbox#pack () in
  let _ = GMisc.label ~text:"Phase" ~xalign ~packing:box#add () in
  let phases = [Before_clean; Clean; After_clean; Before_compile; Compile; After_compile] in
  let strings = List.map Task.descr_of_phase phases in
  let combo_phase, _ = GEdit.combo_box_text ~strings ~active:0 ~packing:box#add () in
  let callback () = combo_phase#misc#set_sensitive (check_always_project#active || check_always_script#active) in
  let _ = check_always_project#connect#toggled ~callback in
  let _ = check_always_script#connect#toggled ~callback in
  (* Working dir *)
  let box = GPack.vbox ~packing:vbox#pack ~show:true () in
  let _ = GMisc.label ~markup:"Working directory <small>(relative to the project source path)</small>" ~xalign ~packing:box#add () in
  let box = GPack.hbox ~spacing:5 ~packing:box#pack () in
  let button_dir = GEdit.entry ~packing:box#add () in
  (* Program *)
  let box = GPack.vbox ~packing:vbox#pack () in
  let _ = GMisc.label ~text:"Command" ~xalign ~packing:box#add () in
  let hbox = GPack.hbox ~spacing:5 ~packing:box#pack () in
  let entry_cmd = GEdit.entry ~packing:hbox#add () in
  let button_cmd = GButton.button ~label:"  ...  " ~packing:hbox#pack () in
  let _ = button_cmd#connect#clicked ~callback:begin fun () ->
      let dialog = GWindow.file_chooser_dialog ~action:`OPEN ~position:`CENTER
          ~title:"Choose program..." ~icon:(??? Icons.oe)
          ~modal:true ~show:false () in
      dialog#add_select_button_stock `OK `OK;
      dialog#add_button_stock `CANCEL `CANCEL;
      dialog#add_filter (GFile.filter ~name:"All Files" ~patterns:["*"] ());
      dialog#add_filter (GFile.filter ~name:"OCaml Scripts" ~patterns:["*.ml"] ());
      dialog#add_filter (GFile.filter ~name:"Shell Scripts" ~patterns:["*.sh"; "*.cmd"; "*.bat"] ());
      dialog#add_filter (GFile.filter ~name:"Executables" ~patterns:["*.exe"] ());
      match dialog#run () with
      | `OK ->
          Gaux.may dialog#filename ~f:entry_cmd#set_text;
          dialog#destroy()
      | _ -> dialog#destroy()
    end in
  (* Command line arguments, rule outputs/dependencies and Environment Variables *)
  let entry_args, entry_env, entry_outputs, entry_deps = Args_env_widget.create_task vbox in
  object (self)
    inherit GObj.widget vbox#as_widget
    val mutable etask = None

    initializer
      ignore (entry_name#connect#changed
                ~callback:(self#update (fun etask -> etask.Task.et_name <- entry_name#text)));
      ignore (check_always_project#connect#toggled
                ~callback:(self#update (fun etask -> etask.Task.et_always_run_in_project <- check_always_project#active)));
      ignore (check_always_script#connect#toggled
                ~callback:(self#update (fun etask -> etask.Task.et_always_run_in_script <- check_always_script#active)));
      ignore (entry_cmd#connect#changed
                ~callback:(self#update (fun etask -> etask.Task.et_cmd <- entry_cmd#text)));
      ignore (entry_args#connect#changed
                ~callback:(self#update (fun etask -> etask.Task.et_args <- entry_args#entries)));
      ignore (entry_outputs#connect#changed
                ~callback:(self#update (fun etask -> etask.Task.et_outputs <- entry_outputs#entries)));
      ignore (entry_deps#connect#changed
                ~callback:(self#update (fun etask -> etask.Task.et_deps <- entry_deps#entries)));
      ignore (entry_env#connect#changed
                ~callback:(self#update (fun etask -> etask.Task.et_env <- entry_env#entries)));
      ignore (entry_env#connect#replace_changed
                ~callback:(fun is_replace -> self#update (fun etask -> etask.Task.et_env_replace <- is_replace) ()));
      ignore (combo_phase#connect#changed
                ~callback:(self#update (fun etask -> etask.Task.et_phase <- Some (List.nth phases combo_phase#active))));
      ignore (button_dir#connect#changed
                ~callback:(self#update (fun etask -> etask.Task.et_dir <- button_dir#text)));


    method set_task et =
      etask <- Some et;
      entry_name#set_text et.Task.et_name;
      check_always_project#set_active et.Task.et_always_run_in_project;
      check_always_script#set_active et.Task.et_always_run_in_script;
      entry_cmd#set_text et.Task.et_cmd;
      entry_args#set_entries et.Task.et_args;
      entry_outputs#set_entries et.Task.et_outputs;
      entry_deps#set_entries et.Task.et_deps;
      entry_env#set_entries et.Task.et_env;
      entry_env#set_replace et.Task.et_env_replace;
      button_dir#set_text et.Task.et_dir;
      self#misc#set_sensitive (not et.Task.et_readonly);
      match et.Task.et_phase with
      | Some ph -> combo_phase#set_active (Utils.ListExt.pos ph phases);
      | None -> combo_phase#set_active 0;

    method private update update_func () =
      Gaux.may etask ~f:update_func;

    method entry_name = entry_name

    method connect = new signals ~changed

  end

and changed () = object (self) inherit [unit] signal () as super end
and signals ~changed =
  object (self)
    inherit ml_signals [changed#disconnect]
    method changed = changed#connect ~after
  end
