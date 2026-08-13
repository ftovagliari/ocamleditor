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

(* Deprecated *)

type column_type = BOOL | STRING

type value = Bool of bool | String of string

type t_column =
  | C_bool of bool GTree.column * GTree.cell_renderer_toggle
  | C_string of string GTree.column * GTree.cell_renderer_text

type column_definition = string * column_type

class virtual model () =
  object (self)
    method virtual coerce : GTree.model
    method virtual add : ?parent:Gtk.tree_iter -> unit -> Gtk.tree_iter
    method virtual remove : Gtk.tree_iter -> bool
    method virtual get : 'a. row:Gtk.tree_iter -> column:'a GTree.column -> 'a
    method virtual set : 'a. row:Gtk.tree_iter -> column:'a GTree.column -> 'a -> unit
    method virtual values : 'a. column:'a GTree.column -> 'a list
    method virtual get_iter : Gtk.tree_path -> Gtk.tree_iter
    method virtual get_path : Gtk.tree_iter -> Gtk.tree_path
    method virtual iter_is_valid : Gtk.tree_iter -> bool
  end

type t = {
  model   : model;
  columns : (t_column * GTree.view_column) list;
  view    : GTree.view;
}

(** Stores *)

module Store =
struct
  type t = GTree.column_list -> model

  class tree cols =
    (* Righe copiate da GTree *)
    let _ = cols#lock () in
    let types = (*List.map Gobject.Type.of_fundamental*) cols#types in
    let store = GtkTree.TreeStore.create (Array.of_list types) in
    let _ = Hashtbl.add GTree.model_ids(Gobject.get_oid store) cols#id in
    object (self)
      inherit GTree.tree_store store as super_tree_store
      inherit model ()
      method add = super_tree_store#append
      method coerce = super_tree_store#coerce
      method values : 'a. column:'a GTree.column -> 'a list = fun ~column ->
        failwith "Method Tree.Store.tree#values not implemented."
    end

  class list' cols =
    (* Righe copiate da GTree *)
    let _ = cols#lock () in
    let types = cols#types (*List.map Gobject.Type.of_fundamental (cols#kinds :> Gobject.fundamental_type list)*) in
    let store = GtkTree.ListStore.create (Array.of_list types) in
    let _ = Hashtbl.add GTree.model_ids (Gobject.get_oid store) cols#id in
    object (self)
      inherit GTree.list_store store as super_list_store
      inherit model () as super_model
      method add ?parent = super_list_store#append
      method coerce = super_list_store#coerce
      method values : 'a. column:'a GTree.column -> 'a list = fun ~column ->
        try
          let row = ref (super_list_store#get_iter (GTree.Path.create [0])) in
          let data = ref [] in
          while
            data := (super_list_store#get ~row:!row ~column) :: !data;
            super_list_store#iter_next !row
          do () done;
          List.rev !data
        with _ -> []
    end

  let tree = fun cols -> ((new tree cols) :> model)

  let list = fun cols -> ((new list' cols) :> model)
end


(** Simple tree *)

(*let expander_open = (GMisc.image ~file:"tree_minus.xpm" ())#pixbuf
  let expander_closed = (GMisc.image ~file:"tree_plus.xpm" ())#pixbuf
  let leaf = (GMisc.image ~file:"tree_leaf.xpm" ())#pixbuf*)

let create ~cols ?packing ?(store=Store.tree) () =
  let column_list = new GTree.column_list in
  let columns = List.map begin function (title, datatype) ->
    match datatype with
    | BOOL ->
        let col = column_list#add Gobject.Data.boolean in
        let renderer = GTree.cell_renderer_toggle [] in
        let attrs = ["active", col] in
        C_bool (col, renderer), GTree.view_column ~title ~renderer:(renderer, attrs) ()
    | STRING ->
        let col = column_list#add Gobject.Data.string in
        let text_renderer = GTree.cell_renderer_text [(*`EDITABLE true*)] in
        let vc = GTree.view_column ~title () in
        vc#pack text_renderer;
        vc#add_attribute text_renderer "text" col;
        C_string (col, text_renderer), vc
    end cols in
  let store = store column_list in
  let sw = GBin.scrolled_window ~vpolicy:`AUTOMATIC ~hpolicy:`AUTOMATIC ?packing () in
  let view = GTree.view ~model:(store#coerce) ~packing:sw#add () in
  List.map (fun (_, vc) -> view#append_column vc) columns |> ignore;
  view#set_headers_visible true;
  { model = store;
    columns = columns;
    view = view }

let model tree = tree.model

let columns tree = tree.columns

let view tree = tree.view

let bool_of = function Bool x -> x | _ -> failwith "bool_of"

let string_of = function String x -> x | _ -> failwith "string_of"

let append ?parent ~(data : value list) tree =
  let row = tree.model#add ?parent () in
  List.iter2 begin fun (column, _) d ->
    match column with
    | C_bool (column, _) -> tree.model#set (bool_of d) ~row ~column;
    | C_string (column, _) -> tree.model#set (string_of d) ~row ~column;
  end tree.columns data;
  row

let remove ~paths tree =
  let rows = Array.of_list paths in
  let rows = Array.mapi begin fun i r ->
      (* Path meno la sua posiz. per permettere la rimozione con Array.iter *)
      GTree.Path.create [(GTree.Path.get_indices r).(0) - i]
    end rows in
  Array.iter (fun r -> ignore(tree.model#remove (tree.model#coerce#get_iter r))) rows;
  if tree.view#selection#count_selected_rows > 0 then
    tree.view#selection#select_path (rows.(0))

let values ~column tree = tree.model#values ~column






