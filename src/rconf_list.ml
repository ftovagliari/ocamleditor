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
open Preferences

(** view *)
class view ~target_list ~editor ~project ~page ?packing () =
  (* "editor#project" is different from "project" when "project" is a new project
     (i.e. you are in "New project" dialog window) *)
  let selection_changed = new selection_changed () in
  let vbox = GPack.vbox ~spacing:5 ?packing () in
  let cols = new GTree.column_list in
  let col_data  = cols#add Gobject.Data.caml in
  let col_name  = cols#add Gobject.Data.string in
  let col_default  = cols#add Gobject.Data.boolean in
  let model = GTree.list_store cols in
  let renderer = GTree.cell_renderer_text [] in
  let renderer_bool = GTree.cell_renderer_toggle [`ACTIVATABLE true] in
  let vc = GTree.view_column ~renderer:(renderer, ["markup", col_name]) ~title:"Name" () in
  let _ = vc#set_min_width 220 in
  let _ = vc#set_max_width 220 in
  let vc_default = GTree.view_column ~renderer:(renderer_bool, ["active", col_default]) ~title:"Default" () in
  let sw = GBin.scrolled_window ~shadow_type:`IN ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:vbox#add () in
  let view = GTree.view ~rules_hint:(Oe_config.targetlist_alternating_row_colors <> None) ~model:model ~headers_visible:true ~reorderable:false ~width:285(* ~height:385*) ~packing:sw#add () in
  let _  = view#misc#set_name "targetlist_treeview" in
  let _ = view#append_column vc in
  let _ = view#append_column vc_default in
  (* Buttons *)
  let bbox = GPack.button_box `HORIZONTAL (*~layout:`SPREAD*) ~packing:vbox#pack () in
  let b_add = GButton.button ~packing:bbox#add () in
  let _ = b_add#misc#set_tooltip_text "Create new run configuration" in
  let _ = b_add#set_image (GMisc.image  ~pixbuf:(??? Icons.new_file) (*~stock:`NEW*) ~icon_size:`BUTTON ())#coerce in
  let b_remove = GButton.button ~packing:bbox#add () in
  let _ = b_remove#misc#set_tooltip_text "Delete selected run configurations" in
  let _ = b_remove#set_image (Icons.create (??? Icons.delete_16))#coerce in
  let b_run = GButton.button ~packing:bbox#add () in
  let _ = b_run#misc#set_tooltip_text "Run" in
  let _ = b_run#set_image (Icons.create (??? Icons.start_16))#coerce in
  object (self)
    inherit GObj.widget vbox#as_widget

    method model = model
    method column_name = col_name

    method button_add = b_add

    method get path =
      let row = model#get_iter path in
      model#get ~row ~column:col_data

    method private to_list () =
      let lst = ref [] in
      model#foreach begin fun path row ->
        lst := (model#get ~row ~column:col_data) :: !lst;
        false
      end;
      List.rev !lst

    method get_rconfigs = self#to_list

    method current_path () =
      match List.rev view#selection#get_selected_rows with
      | path :: _ -> Some path
      | [] -> None

    method private current () =
      let path = self#current_path () in
      match path with None -> None | Some path -> Some (self#get path)

    method reset () =
      let current_selection =
        match view#selection#get_selected_rows with
        | path :: _ when project.Prj.executables <> [] -> Some path
        | _ -> None
      in
      model#clear();
      self#append project.Prj.executables;
      match current_selection with
      | Some path -> view#selection#select_path path
      | _ when project.Prj.executables = [] -> b_add#clicked()
      | _ -> ()

    method private append rcs =
      List.iter begin fun rconf ->
        GtkThread2.sync begin fun () ->
          let rconf = {rconf with Rconf.id = rconf.Rconf.id} in
          let row = model#append () in
          model#set ~row ~column:col_data rconf;
          model#set ~row ~column:col_name (self#markup rconf);
          model#set ~row ~column:col_default rconf.Rconf.default;
          view#selection#select_iter row;
        end ()
      end rcs;
      view#expand_all()

    method private markup rc =
      match List.find_opt (fun bc -> bc.Target.id = rc.Rconf.target_id) (target_list#get_targets()) with
      | Some target ->
          sprintf "<b>%s</b>\n<small><i>%s</i></small>" rc.Rconf.name target.Target.name
      | _ -> ""

    initializer
      ignore (view#selection#connect#changed ~callback:begin fun () ->
          if List.length view#selection#get_selected_rows = 0 then begin
            selection_changed#call None;
          end else begin
            match view#selection#get_selected_rows with
            | path :: _ ->
                let row = model#get_iter path in
                page#set (model#get ~row ~column:col_data);
            | [] -> assert false
          end
        end);
      let update_rconf_name () =
        match self#current_path () with
        | None -> ()
        | Some path ->
            let row = model#get_iter path in
            let rc = model#get ~row ~column:col_data in
            rc.Rconf.name <- page#entry_name#text;
            model#set ~row ~column:col_name (self#markup rc);
      in
      page#entry_name#connect#changed ~callback:update_rconf_name;
      page#combo_bc#connect#changed ~callback:update_rconf_name;
      (** b_add#connect#clicked *)
      let _ =
        ignore (b_add#connect#clicked ~callback:begin fun () ->
            let rconfigs = self#to_list() in
            let default = List.length rconfigs = 0 in
            let id = (List.fold_left (fun acc t -> max acc t.Rconf.id) (-1) rconfigs) + 1 in
            let name = sprintf "New Run Configuration %d" id in
            let targets = List.filter (fun bc -> bc.Target.target_type = Target.Executable && bc.Target.files <> "") (target_list#get_targets()) in
            let targets = List.sort (fun bc1 bc2 ->
                if bc1.Target.default then (-1)
                else if bc2.Target.default then 1
                else (Stdlib.compare bc1.Target.id bc2.Target.id)) targets in
            match targets with
            | bc :: _ ->
                let rc = Rconf.create ~name ~id ~target_id:bc.Target.id in
                rc.Rconf.default <- default;
                self#append [rc];
                page#set_targets();
                page#set_tasks();
                page#combo_bc#set_active 0;
                page#combo_task#set_active 2; (* 2 = build *)
                Gmisclib.Idle.add ~prio:300 page#entry_name#misc#grab_focus
            | [] -> ()
          end);
        (** b_remove#connect#clicked *)
        ignore (b_remove#connect#clicked ~callback:begin fun () ->
            try
              let last_path_index = (GTree.Path.get_indices (List.hd view#selection#get_selected_rows)).(0) in
              let paths = view#selection#get_selected_rows in
              let rows = List.map model#get_iter paths in
              List.iter (fun row -> ignore (model#remove row)) rows;
              let rconfigs = self#to_list() in
              let index = min last_path_index (List.length rconfigs - 1) in
              view#selection#select_path (GTree.Path.create [index])
            with Failure _ -> ()
          end);
        (** b_run#connect#clicked*)
        ignore (b_run#connect#clicked ~callback:begin fun () ->
            Gaux.may (self#current_path ()) ~f:begin fun path ->
              let row = model#get_iter path in
              let rc = model#get ~row ~column:col_data in
              let targets = (target_list#get_targets()) in
              match List.find_opt (fun b -> b.Target.id = rc.Rconf.target_id) targets with
              | Some bc -> ignore (Task_console.exec ~editor (`RCONF rc) bc)
              | _ -> ()
            end
          end);
        ignore (renderer_bool#connect#toggled ~callback:begin fun row ->
            let row = model#get_iter row in
            let active = model#get ~row ~column:col_default in
            if not active then begin
              model#foreach begin fun _ row ->
                (model#set ~row ~column:col_default false);
                let rconf = model#get ~row ~column:col_data in
                rconf.Rconf.default <- false;
                false
              end;
              model#set ~row ~column:col_default true;
              let rconf = model#get ~row ~column:col_data in
              rconf.Rconf.default <- true;
            end
          end);
        ignore (view#misc#connect#map ~callback:begin fun () ->
            b_add#misc#set_sensitive begin
              List.exists (fun bc -> bc.Target.target_type = Target.Executable && bc.Target.files <> "") (target_list#get_targets())
            end
          end);
        (** Remove all executable configurations linked to the targets removed. *)
        target_list#connect#removed ~callback:begin fun elements ->
          List.iter begin function
          | Target_list.Target bc ->
              let paths = ref [] in
              model#foreach begin fun path row ->
                let rc = self#get path in
                if rc.Rconf.target_id = bc.Target.id then (paths := path :: !paths);
                false
              end;
              let rows = List.map model#get_iter !paths in
              List.iter (fun row -> ignore (model#remove row)) rows;
          | _ -> ()
          end elements
        end
      in ()

    method connect = new signals ~selection_changed
  end

and selection_changed () = object (self) inherit [Gtk.tree_path option] signal () as super end
and signals ~selection_changed =
  object (self)
    inherit ml_signals [selection_changed#disconnect]
    method selection_changed = selection_changed#connect ~after
  end








