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
open GdkKeysyms
open Utils
open Menu_types
open Preferences
open Option_syntax

(** set_label *)
let set_label item text = item#misc#set_property "label" (`STRING (Some text));;

(** create *)
let create ~browser ~group () =
  let flags = [`VISIBLE; `LOCKED] in
  let items = {
    menus                         = [||];
    menu_items                    = [];
    window                        = GMenu.menu ();
    window_radio_group            = None;
    window_pages                  = [];
    window_signal_locked          = false;
    window_n_childs               = 1;
    project                       = GMenu.menu ();
    project_history               = [];
    project_history_signal_locked = false;
  } in
  items.menus <- [|
    Menu_file.create ~browser ~group;
    Menu_edit.create ~browser ~group;
    Menu_search.create ~browser ~group;
    Menu_view_menu.create ~browser ~group;
    Menu_project.create ~browser ~group ~items;
    Menu_tools.create ~browser ~group;
    Menu_window.create ~browser ~group ~items;
    Menu_help.create ~browser ~group;
  |];
  items.menu_items <- Array.to_list (Array.map (fun (x, _) -> x) items.menus);
  items
