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

type 'a item = < misc : < hide : unit -> unit; .. >; .. > as 'a

type 'a pool = {
  mutable locked : (int * 'a item) list;
  mutable free : 'a list;
  mutable max_width : int;
}

(** create *)
let create () = {
  free = [];
  locked = [];
  max_width = 0;
}

(** Iterates a function over all elements of the pool, whether they are available or not. *)
let iter f lnl =
  List.iter f lnl.free;
  List.iter (*f*) (fun (_, labl) -> f labl) lnl.locked

(** Unlocks all locked items and makes them available. *)
let reset (lnl : 'a pool) =
  let lck = snd (List.split lnl.locked) in
  lnl.free <- List.rev_append lck lnl.free;
  List.iter (fun l -> l#misc#hide()) lnl.free;
  lnl.locked <- []

(** [hide key pool] calls the [hide] method on the locked element of the pool
    identified by [key]. It does nothing if the element identified by [key] is
    not among the locked elements. *)
let hide key lnl =
  match List.assoc_opt key lnl.locked with Some lnl -> lnl#misc#hide() | _ -> ()

let find lnl x = List.assoc_opt x lnl.locked

(** Marks an item as in-use so that it cannot be obtained from the pool. *)
let lock lnl x =
  lnl.locked <- x :: lnl.locked

(** Gets a free item from the pool. *)
let get lnl =
  match lnl.free with
  | label :: tl ->
      lnl.free <- tl;
      Some label
  | [] -> None






