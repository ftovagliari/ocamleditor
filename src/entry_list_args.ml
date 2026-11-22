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
  let changed    = new changed () in
  let vbox       = GPack.vbox ~spacing:3 ?packing () in
  let entry_list = Entry_list.create (*~height:100*) ~packing:vbox#add () in
  object (self)
    inherit GObj.widget vbox#as_widget
    method entries = entry_list#entries
    method entries_activatable = entry_list#entries_activatable
    method set_entries = entry_list#set_entries
    initializer
      ignore (entry_list#append_empty());
      ignore (entry_list#connect#changed ~callback:changed#call);
    method connect = new signals ~changed
  end

and changed () = object (self) inherit [unit] GUtil.signal () as super end
and replace_changed () = object (self) inherit [bool] GUtil.signal () as super end

and signals ~changed =
  object (self)
    inherit GUtil.ml_signals [changed#disconnect]
    method changed = changed#connect ~after
  end

let create = new widget
