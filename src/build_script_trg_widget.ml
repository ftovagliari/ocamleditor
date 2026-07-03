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


open Prj
open Build_script
open Preferences

class widget ~project ?packing () =
  (* Model for install task *)
  let cols_itasks       = new GTree.column_list in
  let col_itask         = cols_itasks#add Gobject.Data.string in
  let model_itask       = GTree.list_store cols_itasks in
  (*  *)
  let targets           = project.Prj.targets in
  let hbox              = GPack.hbox ~spacing:5 ?packing () in
  let sw                = GBin.scrolled_window ~shadow_type:`IN ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:hbox#add () in
  let cols              = new GTree.column_list in
  let col_target        = cols#add Gobject.Data.caml in
  let col_show          = cols#add Gobject.Data.boolean in
  (*let col_install_task  = cols#add Gobject.Data.string in*)
  let rend_target       = GTree.cell_renderer_text [] in
  let rend_pixbuf       = GTree.cell_renderer_pixbuf [] in
  let rend_show         = GTree.cell_renderer_toggle [`XALIGN 0.0] in
  let rend_install_task = GTree.cell_renderer_combo [`EDITABLE true; `HAS_ENTRY false; `TEXT_COLUMN col_itask; `MODEL (Some model_itask#coerce)] in
  let model             = GTree.list_store cols in
  let vc_target         = GTree.view_column ~title:"Target" () in
  let _                 = vc_target#pack ~expand:false rend_pixbuf in
  let _                 = vc_target#pack ~expand:true rend_target in
  let vc_show           = GTree.view_column ~title:"Show" ~renderer:(rend_show, ["active", col_show]) () in
  (*let vc_install_task   = GTree.view_column ~title:"Installer Task" ~renderer:(rend_install_task, ["text", col_install_task]) () in*)
  let view              = GTree.view ~model ~headers_visible:true ~reorderable:true ~enable_search:false ~packing:sw#add () in
  let _                 = view#selection#set_mode `MULTIPLE in
  let _                 = view#append_column vc_target in
  let _                 = view#append_column vc_show in
  (*let _                 = view#append_column vc_install_task in*)
  object (self)
    inherit GObj.widget hbox#as_widget
    val mutable table = []

    initializer
      vc_target#set_cell_data_func rend_target begin fun model row ->
        let target = model#get ~row ~column:col_target in
        rend_target#set_properties [`TEXT target.Target.name];
        rend_show#set_properties [`ACTIVATABLE target.Target.visible];
        match target.Target.target_type with
        | Target.Executable -> rend_pixbuf#set_properties [`VISIBLE true; `PIXBUF (??? Icons.start_16); `XALIGN 0.0]
        | Target.Library -> rend_pixbuf#set_properties [`VISIBLE true; `PIXBUF (??? Icons.library); `XALIGN 0.0]
        | Target.Plugin -> rend_pixbuf#set_properties [`VISIBLE true; `PIXBUF (??? Icons.plugin); `XALIGN 0.0]
        | Target.Pack -> rend_pixbuf#set_properties [`VISIBLE true; `PIXBUF (??? Icons.library); `XALIGN 0.0]
        | Target.External -> rend_pixbuf#set_properties [`VISIBLE true; `PIXBUF (??? Icons.etask_16); `XALIGN 0.0]
      end;
      (*vc_install_task#set_cell_data_func rend_install_task begin fun model row ->
        let target = model#get ~row ~column:col_target in
        match target.Target.target_type with
          | Target.Executable ->
            let show = true || model#get ~row ~column:col_show in
            rend_install_task#set_properties [`EDITABLE show; `STYLE `NORMAL; `FOREGROUND_GDK (view#misc#style#fg `NORMAL);]
          | Target.Library
          | Target.Plugin
          | Target.Pack -> rend_install_task#set_properties [`FOREGROUND_GDK (view#misc#style#fg `INSENSITIVE); `EDITABLE false; `TEXT "Not Applicable"; `STYLE `ITALIC]
        end;*)
      ignore (rend_show#connect#toggled ~callback:begin fun path ->
          let row = model#get_iter path in
          model#set ~row ~column:col_show (not (model#get ~row ~column:col_show));
        end);
      (*(* Fill model_itask *)
        ignore (rend_install_task#connect#edited ~callback:begin fun path text ->
        let row = model#get_iter path in
        let task_name =
          match text with
            | "<None>" ->
        in
        model#set ~row ~column:col_install_task text;
        end);*)
      (*ignore (view#selection#connect#after#changed ~callback:self#fill_model_itask);
        ignore (view#connect#cursor_changed ~callback:self#fill_model_itask);*)
      (* Fill model *)
      List.iter begin fun tg ->
        if tg.Target.visible then begin
          let row = model#append () in
          model#set ~row ~column:col_target tg;
          model#set ~row ~column:col_show (tg.Target.visible && not tg.Target.readonly);
          table <- (tg.Target.id, model#get_path row) :: table;
        end
      end targets;
      (*  *)
      self#set project.build_script.bs_targets;

    method set build_script_targets =
      List.iter begin fun bst ->
        match List.assoc_opt bst.bst_target.Target.id table with
        | Some path ->
            let row = model#get_iter path in
            model#set ~row ~column:col_show bst.bst_show;
        | _ -> ()
      end build_script_targets;

    method get () =
      let targets = ref [] in
      model#foreach begin fun _ row ->
        targets := {
          bst_target         = model#get ~row ~column:col_target;
          bst_show           = model#get ~row ~column:col_show;
        } :: !targets;
        false
      end;
      List.rev !targets
  end




