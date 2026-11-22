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


class widget ?packing () =
  let changed = new changed () in
  let replace_changed = new replace_changed () in
  let vbox = GPack.vbox ~spacing:3 ?packing () in
  let hbox = GPack.hbox ~border_width:8 ~spacing:3 ~packing:vbox#pack () in
  let _ = GMisc.separator `VERTICAL ~packing:vbox#pack () in
  let entry_list = Entry_list.create (*~height:100*) ~packing:vbox#add () in
  let radio_prepend = GButton.radio_button ~label:"Append/override to system environment" ~packing:hbox#pack ~active:true () in
  let radio_replace = GButton.radio_button ~label:"Replace system environment" ~packing:hbox#add ~group:radio_prepend#group () in
  let button_select = GButton.button ~label:"Select..." ~packing:hbox#pack () in
  let button_add = GButton.button (*~label:"Add"*) ~packing:hbox#pack () in
  let button_remove = GButton.button (*~label:"Remove"*) ~packing:hbox#pack () in
  let _ =
    button_add#set_image (GMisc.image ~stock:`ADD ~icon_size:`MENU ())#coerce;
    button_remove#set_image (GMisc.image ~stock:`REMOVE ~icon_size:`MENU ())#coerce;
  in
  object (self)
    inherit GObj.widget vbox#as_widget
    method entries = entry_list#entries
    method entries_activatable = entry_list#entries_activatable
    method set_entries = entry_list#set_entries
    method is_replace = radio_replace#active
    method set_replace = function true -> radio_replace#set_active true | false -> radio_prepend#set_active true
    initializer
      ignore (entry_list#connect#changed ~callback:changed#call);
      ignore (radio_prepend#connect#pressed ~callback:(fun () -> replace_changed#call false));
      ignore (radio_replace#connect#pressed ~callback:(fun () -> replace_changed#call true));
      ignore (button_add#connect#clicked ~callback:begin fun () ->
          let last = entry_list#model#get_path (entry_list#append_empty()) in
          entry_list#view#set_cursor ~edit:true last entry_list#view_column;
        end);
      ignore (button_remove#connect#clicked ~callback:entry_list#remove_selected);
      ignore (button_select#connect#clicked ~callback:begin fun () ->
          let env = List.sort Stdlib.compare (Array.to_list (Unix.environment ())) in
          let env = List.map (fun x -> false, x) env in
          let window = GWindow.window ~modal:true ~title:"Select Environment Variables" ~width:640 ~height:400 ~position:`CENTER () in
          Gmisclib.Window.GeometryMemo.add ~key:"dialog-select-environment-variables" ~window Preferences.geometry_memo;
          let vbox = GPack.vbox ~spacing:8 ~border_width:5 ~packing:window#add () in
          let checklist = new Checklist.checklist ~packing:vbox#add env in
          let bbox = GPack.button_box `HORIZONTAL ~layout:`END ~spacing:8 ~border_width:5 ~packing:vbox#pack () in
          let button_ok = GButton.button ~stock:`OK ~packing:bbox#pack () in
          let button_cancel = GButton.button ~stock:`CANCEL ~packing:bbox#pack () in
          let button_all = GButton.button ~label:"Select All" ~packing:bbox#pack () in
          let button_none = GButton.button ~label:"Unselect All" ~packing:bbox#pack () in
          bbox#set_child_secondary button_all#coerce true;
          bbox#set_child_secondary button_none#coerce true;
          ignore (button_cancel#connect#clicked ~callback:window#destroy);
          ignore (button_all#connect#clicked ~callback:(fun () -> checklist#iter_set (fun c n -> (true, n))));
          ignore (button_none#connect#clicked ~callback:(fun () -> checklist#iter_set (fun c n -> (false, n))));
          ignore (button_ok#connect#clicked ~callback:begin fun () ->
              let entries = ref [] in
              checklist#iter begin fun checked entry ->
                if checked then (entries := (true, true, entry) :: !entries)
              end;
              self#set_entries (self#entries @ (List.rev !entries));
              window#destroy()
            end);
          window#show()
        end);
    method connect = new signals ~changed ~replace_changed
  end

and changed () = object (self) inherit [unit] GUtil.signal () as super end
and replace_changed () = object (self) inherit [bool] GUtil.signal () as super end

and signals ~changed ~replace_changed =
  object (self)
    inherit GUtil.ml_signals [changed#disconnect; replace_changed#disconnect]
    method changed = changed#connect ~after
    method replace_changed = replace_changed#connect ~after
  end

let create = new widget
