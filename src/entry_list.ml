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


open GdkKeysyms

class widget ?height ?packing () =
  let changed          = new changed () in
  let sw               = GBin.scrolled_window ~shadow_type:`IN ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ?packing () in
  let cols             = new GTree.column_list in
  let col_checked      = cols#add Gobject.Data.boolean in
  let col_value        = cols#add Gobject.Data.string in
  let model            = GTree.list_store cols in
  let renderer         = GTree.cell_renderer_text [`EDITABLE true] in
  let renderer_checked = GTree.cell_renderer_toggle [`ACTIVATABLE true] in
  let vc_value         = GTree.view_column ~renderer:(renderer, ["text", col_value]) ~title:"" () in
  let vc_checked       = GTree.view_column ~renderer:(renderer_checked, ["active", col_checked]) ~title:"" () in
  let view             = GTree.view ~model ~headers_visible:false ~reorderable:true ~enable_search:false ?height ~packing:sw#add () in
  let _                = view#selection#set_mode `MULTIPLE in
  let _                = view#append_column vc_checked in
  let _                = view#append_column vc_value in
  object (self)
    inherit GObj.widget sw#as_widget
    val mutable signal_edited = None;
    val mutable signal_row_inserted = None

    method model = model
    method view = view
    method view_column = vc_value

    method entries =
      let res = ref [] in
      model#foreach begin fun _ row ->
        let checked = model#get ~row ~column:col_checked in
        let value = model#get ~row ~column:col_value in
        if value <> "" then (res := (checked, value) :: !res);
        false
      end;
      List.rev !res

    method set_entries entries =
      Gaux.may ~f:model#misc#handler_block signal_row_inserted;
      Gaux.may ~f:(GtkSignal.handler_block renderer#as_renderer) signal_edited;
      model#clear();
      List.iter begin fun (enabled, value) ->
        let row = model#append () in
        model#set ~row ~column:col_checked enabled;
        model#set ~row ~column:col_value value
      end entries;
      ignore (self#append_empty());
      changed#call();
      Gaux.may ~f:(GtkSignal.handler_unblock renderer#as_renderer) signal_edited;
      Gaux.may ~f:model#misc#handler_unblock signal_row_inserted;

    method private remove_empty_lines () =
      let rows = ref [] in
      model#foreach begin fun _ row ->
        let text = model#get ~row ~column:col_value in
        if text = "" then (rows := row :: !rows);
        false
      end;
      List.iter (fun row -> ignore (model#remove row)) !rows;

    method count () =
      let count = ref 0 in
      model#foreach (fun _ _ -> incr count; false);
      !count

    method append_empty () =
      self#remove_empty_lines();
      let last = self#count () - 1 in
      if last < 0 || (model#get ~row:(model#get_iter (GTree.Path.create [last])) ~column:col_value) <> "" then begin
        let row = model#append () in
        model#set ~row ~column:col_checked false;
        model#set ~row ~column:col_value "";
        row
      end else (model#get_iter (GTree.Path.create [last]))

    method remove_selected () =
      try
        let removing = ref [] in
        let selection = view#selection#get_selected_rows in
        removing := List.map model#get_row_reference selection;
        let next = List.fold_left begin fun next reference ->
            let path = reference#path  in
            ignore (model#remove reference#iter);
            path
          end (GTree.Path.create [0]) !removing in
        changed#call();
        ignore(self#append_empty());
        view#set_cursor next vc_value;
      with Failure _ -> ()

    initializer
      let is_last path =
        let last = self#count () - 1 in
        let last = GTree.Path.create [last] in
        GTree.Path.to_string last = GTree.Path.to_string path
      in
      let block_edit_if_last = ref false in
      let edit_if_last () =
        if !block_edit_if_last then begin
          block_edit_if_last := false;
          false
        end else
          match view#selection#get_selected_rows with
          | path :: _ ->
              let path_is_last = is_last path in
              if path_is_last then (view#set_cursor ~edit:true path vc_value);
              true
          | _ -> false
      in
      ignore (view#event#connect#key_release ~callback:begin fun ev ->
          let key = GdkEvent.Key.keyval ev in
          if key = _Down then (edit_if_last ())
          else if key = _Up then (edit_if_last ())
          else false
        end);
      ignore (view#event#connect#key_press ~callback:begin fun ev ->
          let key = GdkEvent.Key.keyval ev in
          if key = _Delete then begin
            self#remove_selected();
            true
          end else false
        end);
      signal_edited <- Some (renderer#connect#edited ~callback:begin fun path txt ->
          if is_last path then (block_edit_if_last := true);
          let row = model#get_iter path in
          model#set ~row ~column:col_value txt;
          model#set ~row ~column:col_checked true;
          ignore (self#append_empty());
          let row = model#get_iter path in
          view#set_cursor (model#get_path row) vc_value;
          changed#call();
        end);
      ignore (renderer_checked#connect#toggled ~callback:begin fun path ->
          let row = model#get_iter path in
          let checked = model#get ~row ~column:col_checked in
          let value = model#get ~row ~column:col_value in
          model#set ~row ~column:col_checked
            (if String.trim value = "" then false else not checked);
          changed#call();
        end);
      signal_row_inserted <-
        Some (model#connect#after#row_inserted ~callback:begin fun path row ->
            Gmisclib.Idle.add changed#call
          end);

    method connect = new signals ~changed
  end

and changed () = object (self) inherit [unit] GUtil.signal () as super end

and signals ~changed =
  object (self)
    inherit GUtil.ml_signals [changed#disconnect]
    method changed = changed#connect ~after
  end

let create = new widget
