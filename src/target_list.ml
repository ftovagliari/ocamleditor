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
open Target
open Task
open GUtil
open Preferences

type item = Target of Target.t | ETask of Task.t

let cols              = new GTree.column_list
let col_data          = cols#add Gobject.Data.caml
let col_name          = cols#add Gobject.Data.string
let col_default       = cols#add Gobject.Data.boolean
let col_visible       = cols#add Gobject.Data.boolean

(** view *)
class view ~editor ~project ?packing () =
  (* "editor#project" is different from "project" when "project" is a new (i.e. you are in "New project" dialog window) *)
  let selection_changed = new selection_changed () in
  let removed           = new removed () in
  let add_target        = new add_target () in
  let add_etask         = new add_etask () in
  let vbox              = GPack.vbox ~spacing:5 ?packing () in
  let model             = GTree.tree_store cols in
  let modelf            = (*GTree.model_filter*) model in
  (*let _                 = modelf#set_visible_column col_visible in*)
  let renderer          = GTree.cell_renderer_text [`XPAD 5] in
  let renderer_pixbuf   = GTree.cell_renderer_pixbuf [] in
  let renderer_pixbuf2  = GTree.cell_renderer_pixbuf [`VISIBLE false; `PIXBUF (??? Icons.findlib); `XPAD 3;`XALIGN 0.0] in
  let renderer_default  = GTree.cell_renderer_toggle [`RADIO false; `ACTIVATABLE true; `WIDTH 50] in
  let vc                = GTree.view_column ~title:"Name" () in
  let _                 = vc#pack ~expand:false renderer_pixbuf in
  let _                 = vc#pack ~expand:true renderer in
  let _                 = vc#pack ~expand:false renderer_pixbuf2 in
  let _                 = vc#add_attribute renderer "text" col_name in
  let _                 = vc#set_min_width 300 in
  let _                 = vc#set_max_width 300 in
  let vc_default        = GTree.view_column ~title:"Default" () in
  let _                 = vc_default#pack ~expand:true renderer_default in
  let _                 = vc_default#add_attribute renderer_default "active" col_default in
  let _                 = vc_default#set_sizing `FIXED in
  let sw                = GBin.scrolled_window ~shadow_type:`IN ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:vbox#add () in
  let view              = GTree.view ~rules_hint:(Oe_config.targetlist_alternating_row_colors <> None) ~model:modelf ~headers_visible:false ~reorderable:true ~width:350 (*~height:385*) ~packing:sw#add () in
  let _                 = view#misc#set_name "targetlist_treeview" in
  let _                 = view#misc#set_property "enable-tree-lines" (`BOOL true) in
  let _                 = view#append_column vc in
  let _                 = view#append_column vc_default in
  let _                 = vc_default#set_visible true in
  let _                 = view#selection#set_mode `SINGLE in
  let tooltips          = GData.tooltips () in
  (* Buttons *)
  let bbox              = GPack.hbox ~spacing:3 ~packing:vbox#pack () in
  let b_new             = Gmisclib.Button.button_menu ~packing:bbox#pack () in
  let _                 = b_new#set_image  (GMisc.image  ~pixbuf:(??? Icons.new_file) (*~stock:`NEW*) ~icon_size:`MENU ())#coerce in
  let _                 = b_new#misc#set_tooltip_text "New..." in
  let _                 = GMisc.label ~text:"" ~packing:bbox#add () in
  let b_remove          = GButton.button ~packing:bbox#pack () in
  let _                 = tooltips#set_tip ~text:"Delete selected items" b_remove#coerce in
  let _                 = b_remove#set_image (Icons.create (??? Icons.delete_16))#coerce in
  let b_up              = GButton.button ~packing:bbox#pack () in
  let _                 = tooltips#set_tip ~text:"Move Up" b_up#coerce in
  let _                 = b_up#set_image (Icons.create (??? Icons.arrow_up))#coerce in
  let b_down            = GButton.button ~packing:bbox#pack () in
  let _                 = tooltips#set_tip ~text:"Move Down" b_down#coerce in
  let _                 = b_down#set_image (Icons.create (??? Icons.arrow_down))#coerce in
  let _                 = GMisc.label ~text:"" ~packing:bbox#add () in
  let b_clean           = GButton.button ~packing:bbox#pack () in
  let _                 = tooltips#set_tip ~text:"Clean" b_clean#coerce in
  let _                 = b_clean#set_image (Icons.create (??? Icons.clear_build_16))#coerce in
  let b_compile         = GButton.button ~packing:bbox#pack () in
  let _                 = tooltips#set_tip ~text:"Build" b_compile#coerce in
  let _                 = b_compile#set_image (Icons.create (??? Icons.build_16))#coerce in
  let b_run             = GButton.button ~packing:bbox#pack () in
  let _                 = tooltips#set_tip ~text:"Run external task/Install library" b_run#coerce in
  let _                 = b_run#set_image (GMisc.image ~xalign:0.5 (*~width:24*) ~pixbuf:(??? Icons.start_16) ())#coerce in
  object (self)
    inherit GObj.widget vbox#as_widget
    val mutable sign_row_collapsed = None
    val mutable sign_row_expanded = None

    initializer
      self#reset();
      ignore (view#selection#connect#changed ~callback:self#selection_changed);
      (*(* b_add#connect#clicked *)
        let _ = b_add#connect#clicked ~callback:(fun () -> ignore (self#add_target ())) in
        (* b_etask#connect#clicked *)
        let _ = b_etask#connect#clicked ~callback:self#add_external_task in
        (* b_duplicate#connect#clicked *)
        let _ = b_duplicate#connect#clicked ~callback:self#duplicate in*)
      (* b_new *)
      b_new#set_menu_only ();
      ignore (b_new#connect#show_menu ~callback:begin fun (label, menu) ->
          label := None;
          let item = GMenu.image_menu_item ~image:(GMisc.image ~pixbuf:(??? Icons.target_16) ())#coerce ~label:"Create new target" ~packing:menu#append () in
          ignore (item#connect#activate ~callback:(fun () -> ignore (self#add_target ())));
          let item = GMenu.image_menu_item ~image:(GMisc.image ~pixbuf:(??? Icons.etask_16) ())#coerce ~label:"Add external build task" ~packing:menu#append () in
          ignore (item#connect#activate ~callback:self#add_external_task);
          let item = GMenu.image_menu_item ~image:(GMisc.image ~stock:`COPY ())#coerce ~label:"Duplicate" ~packing:menu#append () in
          ignore (item#connect#activate ~callback:self#duplicate);
        end);
      (*b_clean#connect#clicked*)
      ignore (b_clean#connect#clicked ~callback:self#clean);
      (*b_compile#connect#clicked*)
      ignore (b_compile#connect#clicked ~callback:self#compile);
      (*b_run#connect#clicked*)
      ignore (b_run#connect#clicked ~callback:self#run);
      (* b_remove#connect#clicked *)
      ignore (b_remove#connect#clicked ~callback:self#remove);
      (**  *)
      ignore (b_up#connect#clicked ~callback:(fun () -> self#move `UP));
      ignore (b_down#connect#clicked ~callback:(fun () -> self#move `DOWN));
      (* Default check toggled *)
      ignore (renderer_default#connect#toggled ~callback:self#toggle_default);
      (* Description (byt, opt) *)
      vc#set_cell_data_func renderer self#cell_data_func;
      (* Show hide column "default" *)
      vc_default#set_cell_data_func renderer_default begin fun model row ->
        let id = model#get ~row ~column:col_data in
        renderer_default#set_properties [`VISIBLE (match id with Target tg -> (tg.target_type <> External && tg.visible) | ETask _ -> false)]
      end;
      view#drag#connect#drop ~callback:begin fun ctx ~x ~y ~time ->
        let is_valid_source =
          match view#selection#get_selected_rows with
          | path :: [] ->
              let row = model#get_iter path in
              begin
                match model#get ~row ~column:col_data with
                | Target tg -> tg.visible && (not tg.readonly)
                | ETask et -> et.et_visible && (not et.et_readonly)
              end;
          | _ -> true
        in
        if is_valid_source then begin
          Option.fold (view#get_path_at_pos ~x ~y) ~none:false ~some:(fun (path, _, _, _) ->
              let row = model#get_iter path in
              match model#get ~row ~column:col_data with
              | Target tg when tg.visible && not tg.readonly ->
                  Gmisclib.Idle.add begin fun () ->
                    if model#iter_has_child row then (GTree.Path.down path);
                    view#expand_to_path path;
                    view#selection#select_path path;
                  end;
                  false
              | Target _ | ETask _ -> true (* true aborts drop *);
            )
        end else true
      end |> ignore;
      let callback c row _ =
        match model#get ~row ~column:col_data with
        | Target tg -> tg.node_collapsed <- c;
        | ETask _ -> ()
      in
      sign_row_expanded <- Some (view#connect#row_expanded ~callback:(callback false));
      sign_row_collapsed <- Some (view#connect#row_collapsed ~callback:(callback true));

    method model = model
    method view = view
    method column_name = col_name
    method has_errors target = target.files = "";

    method get path =
      let row = model#get_iter path in
      model#get ~row ~column:col_data

    method get_targets () =
      let targets = ref [] in
      let with_parent row f =
        Option.iter begin fun row ->
          match model#get ~row ~column:col_data with
          | Target parent -> f parent
          | ETask _ -> assert false (* External Tasks can not have children *);
        end (model#iter_parent row);
      in
      model#foreach begin fun _ row ->
        begin
          try
            begin
              match model#get ~row ~column:col_data with
              | Target target ->
                  with_parent row begin fun parent ->
                    parent.sub_targets <-
                      target :: (List.filter (fun t -> t.id <> target.id) parent.sub_targets)
                  end;
                  targets := target :: !targets;
                  target.sub_targets <- [];
                  target.external_tasks <- [];
              | ETask task ->
                  with_parent row begin fun parent ->
                    parent.external_tasks <-
                      task :: (List.filter ((<>) task) parent.external_tasks)
                  end;
            end;
            false
          with ex -> Printf.eprintf "File \"target_list.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace()); false
        end;
      end;
      List.iter (fun tg -> tg.external_tasks <- List.rev tg.external_tasks) !targets;
      List.iter (fun tg -> tg.sub_targets <- List.rev tg.sub_targets) !targets;
      List.rev !targets

    method length = List.length (self#to_list())

    method private append ?parent targets =
      let count = ref 0 in
      Gaux.may sign_row_collapsed ~f:view#misc#handler_block;
      Gaux.may sign_row_expanded ~f:view#misc#handler_block;
      let rows =
        List.map begin fun target ->
          let exists = ref false in
          model#foreach begin fun _ row ->
            let item = model#get ~row ~column:col_data in
            begin
              match item with
              | Target tg -> if tg.id = target.id then exists := true;
              | ETask _ -> ()
            end;
            !exists
          end;
          if not !exists then begin
            GtkThread2.sync begin fun () ->
              let target = {target with id = target.id} in
              target.external_tasks <- List.map (fun et ->
                  {et with Task.et_name = et.Task.et_name}) target.external_tasks;
              let row = model#append ?parent () in
              incr count;
              model#set ~row ~column:col_data (Target target);
              model#set ~row ~column:col_name target.name;
              model#set ~row ~column:col_default target.default;
              model#set ~row ~column:col_visible target.visible;
              let children = List.fold_right begin fun tg acc ->
                  try (List.find (fun t -> t.id = tg.id) project.Prj.targets) :: acc with Not_found -> acc
                end target.Target.sub_targets []
              in
              GtkThread2.sync (fun () -> ignore (self#append ~parent:row children)) ();
              List.iter begin fun task ->
                GtkThread2.sync (fun () -> ignore (self#append_task ~parent:row ~task)) ()
              end target.external_tasks;
              (*view#selection#select_iter row*)
              if target.node_collapsed then view#collapse_row (model#get_path row);
              Some row
            end ()
          end else None
        end targets
      in
      Gaux.may sign_row_collapsed ~f:view#misc#handler_unblock;
      Gaux.may sign_row_expanded ~f:view#misc#handler_unblock;
      List.filter_map (fun x -> x) rows

    method private append_task ~parent ~task =
      let row = model#append ~parent () in
      model#set ~row ~column:col_data (ETask task);
      model#set ~row ~column:col_name task.Task.et_name;
      model#set ~row ~column:col_default false;
      model#set ~row ~column:col_visible task.Task.et_visible;
      view#expand_row (model#get_path parent);
      row

    method select_default_configuration () =
      view#selection#unselect_all();
      model#foreach begin fun path row ->
        let default_conf = model#get ~row ~column:col_default in
        if default_conf then begin
          view#selection#select_path path;
          true
        end else false
      end;

    method private create_id () =
      let targets = List.filter_map (function Target x -> Some x | ETask _ -> None) (self#to_list()) in
      (List.fold_left (fun acc t -> max acc t.id) (-1) targets) + 1

    method add_target () =
      let id = self#create_id () in
      let name = sprintf "New Target %d" id in
      let rows = self#append [Target.create ~name ~id] in
      begin
        match rows with
        | [row] ->
            let path = model#get_path row in
            view#set_cursor path vc;
            add_target#call();
            path
        | _ -> assert false
      end;

    method add_external_task () =
      let rec f path = function
        | Target target ->
            let task = Task.create ~name:"External Build Task" ~env:[] ~dir:"" ~cmd:"" ~args:[] () in
            let parent = model#get_iter path in
            let row = self#append_task ~parent ~task in
            view#set_cursor (model#get_path row) vc;
            target.external_tasks <- target.external_tasks @ [task];
            add_etask#call();
        | ETask _ ->
            if GTree.Path.up path then (f path (self#get path))
      in
      self#with_current f;

    method private move dir =
      self#with_current begin fun path ->
        let iter = model#get_iter path in
        let scroll path =
          view#set_cursor path vc;
          (*let x = match dir with `UP -> 0.1 | `DOWN -> 0.9 in
            view#scroll_to_cell ~align:(x, x) path vc;*)
        in
        function
        | Target _ when dir = `UP && GTree.Path.prev path ->
            ignore (model#move_before ~iter ~pos:(model#get_iter path));
            scroll path;
        | Target _ when dir = `DOWN ->
            begin
              try
                GTree.Path.next path;
                ignore (model#move_after ~iter ~pos:(model#get_iter path));
                scroll path;
              with Failure _ -> ()
            end;
        | Target _ -> ()
        | ETask _ when dir = `UP && GTree.Path.prev path ->
            ignore (model#move_before ~iter ~pos:(model#get_iter path));
            scroll path;
        | ETask _ when dir = `DOWN ->
            begin
              try
                GTree.Path.next path;
                ignore (model#move_after ~iter ~pos:(model#get_iter path));
                scroll path;
              with Failure _ -> ()
            end;
        | ETask _ -> ()
      end

    method private run () =
      self#with_current begin fun _ -> function
        | Target target -> Task_console.exec ~editor `INSTALL_LIBRARY target
        | ETask etask -> Task_console.exec_sync ~editor [[`OTHER, etask]]
      end

    method private compile () =
      self#with_current (fun _ -> function Target target -> Task_console.exec ~editor `COMPILE target | ETask _ -> ());

    method private clean () =
      self#with_current (fun _ -> function Target target -> Task_console.exec ~editor `CLEAN target | ETask _ -> ());

    method private remove () =
      let delete () =
        try
          let path = List.hd view#selection#get_selected_rows in
          let last_path_index = (GTree.Path.get_indices path).(0) in
          let paths = view#selection#get_selected_rows in
          List.iter begin fun path ->
            match self#get path with
            | ETask task ->
                let parent = GTree.Path.copy path in
                if GTree.Path.up parent then begin
                  match self#get parent with
                  | Target bc ->
                      bc.external_tasks <- List.filter ((<>) task) bc.external_tasks;
                  | ETask _ -> assert false
                end
            | Target _ -> ()
          end paths;
          let data_removed = List.map self#get paths in
          let rows = List.map model#get_iter paths in
          List.iter (fun row -> ignore (model#remove row)) rows;
          let index = min last_path_index (self#length - 1) in
          view#selection#select_path (GTree.Path.create [index]);
          removed#call data_removed;
        with Failure _ -> ()
      in
      ignore (Dialog.confirm ~title:"Remove from targets"
                ~message:(sprintf "Are you sure you want to remove the selected items from target list?" )
                ~yes:("Remove", delete)
                ~no:("Cancel", ignore)
                ~cancel:false
                vbox)

    method private duplicate () =
      self#with_current begin fun path -> function
        | Target original ->
            begin
              let target = {original with id=self#create_id (); name=original.name^" - Copy"; external_tasks = []; default=false} in
              match self#append [target] with
              | [row] ->
                  view#set_cursor (model#get_path row) vc;
                  add_target#call()
              | _ -> assert false
            end;
        | ETask original when GTree.Path.up path ->
            begin
              match self#get path with
              | Target target_parent ->
                  let task = {original with et_name=original.et_name^" - Copy"} in
                  let row = self#append_task ~parent:(model#get_iter path) ~task in
                  view#set_cursor (model#get_path row) vc;
                  target_parent.external_tasks <- target_parent.external_tasks @ [task];
                  add_etask#call()
              | ETask _ -> ()
            end;
        | ETask _ -> ()
      end

    method private selection_changed () =
      match view#selection#get_selected_rows with
      | path :: _ ->
          selection_changed#call (Some path);
          begin
            match self#get path with
            | Target tg ->
                Gmisclib.Idle.add ~prio:300 (fun () ->
                    b_clean#misc#set_sensitive (tg.target_type <> External);
                    b_compile#misc#set_sensitive (tg.target_type <> External);
                    (*b_etask#misc#set_sensitive true;*)
                    b_run#misc#set_sensitive (tg.target_type = Library);
                    b_remove#misc#set_sensitive (not tg.readonly));
            | ETask et ->
                Gmisclib.Idle.add ~prio:300 (fun () ->
                    b_clean#misc#set_sensitive false;
                    b_compile#misc#set_sensitive false;
                    (*b_etask#misc#set_sensitive true;*)
                    b_run#misc#set_sensitive true;
                    b_remove#misc#set_sensitive (not et.Task.et_readonly));
          end;
      | [] -> selection_changed#call None

    method toggle_default row =
      let row = model#get_iter row in
      let active = model#get ~row ~column:col_default in
      if not active then begin
        model#foreach begin fun _ row ->
          (model#set ~row ~column:col_default false);
          begin
            match model#get ~row ~column:col_data with
            | Target bc -> bc.default <- false
            | ETask _ -> ()
          end;
          false
        end;
        model#set ~row ~column:col_default true;
        match model#get ~row ~column:col_data with
        | Target bc -> bc.default <- true
        | ETask _ -> ()
      end

    method private cell_data_func model row =
      try
        begin
          match model#get ~row ~column:col_data with
          | Target target ->
              renderer_pixbuf#set_properties [
                `VISIBLE (self#has_errors target);
                `PIXBUF ((GMisc.image ())#misc#render_icon ~size:`MENU `DIALOG_WARNING);
                `XALIGN 0.0];
              let name = model#get ~row ~column:col_name in
              let descr =
                if target.target_type = External then "" else
                  let descr = if target.byt then ["Bytecode"] else [] in
                  let descr = descr @ (if target.opt then ["Native-code"] else []) in
                  let descr = String.concat ", " descr in
                  sprintf "\n<span weight='light' size='smaller' style='italic'>%s &#8226; %s</span>" (string_of_target_type target.target_type) descr
              in
              renderer#set_properties [`MARKUP (if target.visible && not target.readonly then "<b>" ^ name ^ "</b>" ^ descr else "<b><i>" ^ name ^ "</i></b>" ^ descr)];
              if target.target_type = Executable then begin
                renderer_pixbuf#set_properties [`VISIBLE true; `PIXBUF (??? Icons.start_16); `XALIGN 0.0]
              end else if target.target_type = Plugin then begin
                renderer_pixbuf#set_properties [`VISIBLE true; `PIXBUF (??? Icons.plugin); `XALIGN 0.0]
              end else if target.target_type = External then begin
                renderer_pixbuf#set_properties [`VISIBLE true; `PIXBUF (??? Icons.etask_16); `XALIGN 0.0]
              end else begin
                renderer_pixbuf#set_properties [`VISIBLE true; `PIXBUF (??? ((*if target.is_fl_package then Icons.findlib else*) Icons.library)); `XALIGN 0.0]
              end;
              renderer_pixbuf2#set_properties [`VISIBLE target.is_fl_package];
          | ETask et ->
              renderer#set_properties [`MARKUP (if et.et_visible && (not et.et_readonly) then et.et_name else "<i>" ^ et.et_name ^ "</i>")];
              renderer_pixbuf#set_properties [`VISIBLE true; `PIXBUF (??? Icons.etask_16); `XALIGN 0.0];
              renderer_pixbuf2#set_properties [`VISIBLE false];
        end
      with Not_found -> ()

    method find_item_by_name name =
      let res = ref None in
      model#foreach begin fun path row ->
        if
          match model#get ~row ~column:col_data with
          | Target t -> t.name = name
          | ETask t -> t.et_name = name
        then (res := Some path; true) else false
      end;
      !res

    method reset () =
      let selected = ref None in
      self#with_current (fun _ item -> selected := Some item);
      model#clear();
      ignore (self#append project.Prj.targets);
      Gmisclib.Idle.add begin fun () ->
        try
          begin
            match !selected with
            | Some (Target tg) ->
                begin
                  match self#find_item_by_name tg.name with
                  | Some path -> view#selection#select_path path
                  | _ -> raise Exit
                end
            | Some (ETask et) ->
                begin
                  match self#find_item_by_name et.et_name with
                  | Some path -> view#selection#select_path path
                  | _ -> raise Exit
                end
            | None -> raise Exit
          end;
        with Exit -> self#select_default_configuration ()
      end

    method with_current ?(default : (unit -> unit) option) f =
      match view#selection#get_selected_rows with
      | path :: [] -> f path (self#get path)
      | _ -> Gaux.may default ~f:(fun g -> g())

    method private to_list () =
      let lst = ref [] in
      model#foreach begin fun _ row ->
        lst := (model#get ~row ~column:col_data) :: !lst;
        false
      end;
      List.rev !lst

    method connect = new signals ~selection_changed ~removed ~add_target ~add_etask
  end

and selection_changed () = object inherit [Gtk.tree_path option] signal () end
and removed () = object inherit [item list] signal () end
and add_target () = object inherit [unit] signal () end
and add_etask () = object inherit [unit] signal () end
and signals ~selection_changed ~removed ~add_target ~add_etask =
  object
    inherit ml_signals [selection_changed#disconnect; removed#disconnect; add_target#disconnect; add_etask#disconnect]
    method selection_changed = selection_changed#connect ~after
    method removed = removed#connect ~after
    method add_target = add_target#connect ~after
    method add_etask = add_etask#connect ~after
  end








