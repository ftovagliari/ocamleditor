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

type t = {
  mutable menus                         : (GMenu.menu_item * GMenu.menu) array;
  mutable menu_items                    : GMenu.menu_item list;
  file_rename                           : GMenu.menu_item;
  file_recent_select                    : GMenu.menu_item;
  file_recent_clear                     : GMenu.menu_item;
  file_recent_sep                       : GMenu.menu_item;
  file_switch                           : GMenu.menu_item;
  file_close                            : GMenu.menu_item;
  file_close_all                        : GMenu.menu_item;
  file_revert                           : GMenu.menu_item;
  file_delete                           : GMenu.menu_item;
  window                                : GMenu.menu;
  mutable window_radio_group            : Gtk.radio_menu_item Gtk.group option;
  mutable window_pages                  : (int (* page oid *) * GMenu.radio_menu_item) list;
  mutable window_signal_locked          : bool;
  mutable window_n_childs               : int;
  project                               : GMenu.menu;
  mutable project_history               : (string * GMenu.check_menu_item) list;
  mutable project_history_signal_locked : bool;
}

