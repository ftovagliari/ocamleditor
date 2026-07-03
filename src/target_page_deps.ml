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

open GUtil
open Preferences

let cols     = new GTree.column_list
let col_bc   = cols#add Gobject.Data.caml
let col_name = cols#add Gobject.Data.string

(** target_list *)
class target_list ?packing () =
  let sw              = GBin.scrolled_window ~shadow_type:`IN ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ?packing () in
  let model           = GTree.list_store cols in
  let renderer        = GTree.cell_renderer_text [] in
  let renderer_pixbuf = GTree.cell_renderer_pixbuf [] in
  let vc_name         = GTree.view_column ~title:"Direct Dependencies" () in
  let _               = vc_name#pack ~expand:false renderer_pixbuf in
  let _               = vc_name#pack ~expand:true renderer in
  let _               = vc_name#add_attribute renderer "text" col_name in
  let view            = GTree.view ~model ~headers_visible:false ~reorderable:true ~enable_search:false ~packing:sw#add () in
  let _               = view#selection#set_mode `MULTIPLE in
  let _               = view#append_column vc_name in
  object
    inherit GObj.widget sw#as_widget
    initializer
      vc_name#set_cell_data_func renderer begin fun model row ->
        match (model#get ~row ~column:col_bc).Target.target_type with
        | Target.Executable -> renderer_pixbuf#set_properties [`VISIBLE true; `PIXBUF (??? Icons.start_16); `XALIGN 0.0]
        | Target.Library -> renderer_pixbuf#set_properties [`VISIBLE true; `PIXBUF (??? Icons.library); `XALIGN 0.0]
        | Target.Plugin -> renderer_pixbuf#set_properties [`VISIBLE true; `PIXBUF (??? Icons.plugin); `XALIGN 0.0]
        | Target.Pack -> renderer_pixbuf#set_properties [`VISIBLE true; `PIXBUF (??? Icons.library); `XALIGN 0.0]
        | Target.External -> renderer_pixbuf#set_properties [`VISIBLE true; `PIXBUF (??? Icons.etask_16); `XALIGN 0.0]
      end

    method model = model
    method view = view
  end

(** widget *)
class widget ~target_list ?packing () =
  let hbox          = GPack.hbox ~spacing:8 ?packing () in
  let target_widget = new target_list ~packing:hbox#add () in
  let bbox          = GPack.button_box `VERTICAL ~layout:`START ~spacing:8 ~packing:hbox#pack () in
  let button_add    = GButton.button ~stock:`ADD ~packing:bbox#pack () in
  let button_remove = GButton.button ~stock:`REMOVE ~packing:bbox#pack () in
  let model         = target_widget#model in
  let view          = target_widget#view in
  object (self)
    inherit GObj.widget hbox#as_widget
    val mutable targets = target_list#get_targets()
    val mutable changed = new changed()
    val mutable current_bc = None
    val mutable current_ids = []
    val mutable signal_ids = []

    initializer
      ignore (button_add#connect#clicked ~callback:self#add);
      ignore (button_remove#connect#clicked ~callback:self#remove);
      ignore (view#selection#connect#changed ~callback:self#update_button_state);
      self#update_button_state();
      signal_ids <- [
        model#connect#after#row_deleted ~callback:(fun _ -> changed#call());
        model#connect#after#row_inserted ~callback:(fun _ _ -> changed#call())
      ]

    method get () =
      let result = ref [] in
      model#foreach begin fun path _ ->
        try
          let row = model#get_iter path in
          let id = (model#get ~row ~column:col_bc).Target.id in
          result := id :: !result;
          false
        with Failure _ -> false;
      end;
      List.rev !result;

    method set (bc : Target.t) =
      self#update_targets ();
      List.iter model#misc#handler_block signal_ids;
      model#clear();
      current_ids <- [];
      List.iter begin fun dep_id ->
        match List.find_opt (fun x -> x.Target.id = dep_id) targets with
        | Some dep ->
            let row = model#append () in
            model#set ~row ~column:col_bc dep;
            model#set ~row ~column:col_name dep.Target.name;
            current_ids <- dep.Target.id :: current_ids;
        | _ -> ()
      end bc.Target.dependencies;
      List.iter model#misc#handler_unblock signal_ids;
      current_bc <- Some bc;

    method add () =
      match current_bc with
      | Some current ->
          self#update_targets ();
          let window = GWindow.window ~modal:true ~type_hint:`DIALOG
              ~title:"Select one or more targets"
              ~icon:(??? Icons.oe) ~height:300 ~width:300 ~position:`CENTER
              ~allow_shrink:true ~allow_grow:true ~show:false ()
          in
          Gmisclib.Window.GeometryMemo.add ~key:"dialog-target-deps" ~window Preferences.geometry_memo;
          Gaux.may (GWindow.toplevel hbox) ~f:(fun w -> window#set_transient_for w#as_window);
          Gmisclib.Util.esc_destroy_window window;
          let vbox = GPack.vbox ~spacing:8 ~border_width:5 ~packing:window#add () in
          let bclist = new target_list ~packing:vbox#add () in
          let bbox = GPack.button_box `HORIZONTAL ~layout:`END ~spacing:8 ~packing:vbox#pack () in
          let button_ok = GButton.button ~stock:`OK ~packing:bbox#pack () in
          let button_cancel = GButton.button ~stock:`CANCEL ~packing:bbox#pack () in
          ignore (button_cancel#connect#clicked ~callback:window#destroy);
          ignore (button_ok#connect#clicked ~callback:begin fun () ->
              let paths = bclist#view#selection#get_selected_rows in
              List.iter begin fun path ->
                let row = bclist#model#get_iter path in
                let target = bclist#model#get ~row ~column:col_bc in
                let name = bclist#model#get ~row ~column:col_name in
                let row = model#append () in
                let id = target.Target.id in
                model#set ~row ~column:col_bc target;
                model#set ~row ~column:col_name name;
                current_ids <- id :: current_ids;
              end paths;
              changed#call();
              self#update_button_state();
              window#destroy()
            end);
          ignore (bclist#view#connect#row_activated ~callback:(fun _ _ -> button_ok#clicked()));
          let model = bclist#model in
          List.iter begin fun bc ->
            if not (List.mem bc.Target.id (current.Target.id :: current_ids)) then begin
              let row = model#append () in
              model#set ~row ~column:col_bc bc;
              model#set ~row ~column:col_name bc.Target.name
            end
          end targets;
          window#show()
      | _ -> ()

    method remove () =
      let paths = view#selection#get_selected_rows in
      let rr = List.map model#get_row_reference paths in
      List.iter begin fun reference ->
        let row = reference#iter in
        let target = model#get ~row ~column:col_bc in
        ignore (model#remove row);
        let id = target.Target.id in
        current_ids <- List.filter ((<>) id) current_ids;
      end rr;
      changed#call();
      self#update_button_state();

    method private update_targets () = targets <- target_list#get_targets()

    method private update_button_state () =
      self#update_targets ();
      button_add#misc#set_sensitive (List.length current_ids < List.length targets - 1);
      button_remove#misc#set_sensitive (view#selection#get_selected_rows <> [] && (List.length current_ids > 0));

    method connect = new signals ~changed

  end

(** Signals *)
and changed () = object inherit [unit] signal () end

and signals ~changed =
  object
    inherit ml_signals [changed#disconnect]
    method changed = changed#connect ~after
  end

(** create *)
let create = new widget



