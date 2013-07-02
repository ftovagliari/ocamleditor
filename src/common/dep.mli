(*

  OCamlEditor
  Copyright (C) 2010-2012 Francesco Tovagliari

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


type dag = (string, string list) Hashtbl.t
exception Loop_found of string
val ocamldep : ?pp:string ->
  ?with_errors:bool ->
  ?verbose:bool  ->
  ?slash:bool -> ?search_path:string -> string -> dag
val find : ?pp:string -> ?with_errors:bool -> ?echo:bool -> string list -> string list
val find_dependants : path:string list -> modname:string -> string list
