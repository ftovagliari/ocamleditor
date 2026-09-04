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


type marker_kind = [`None | `Bookmark of int | `Error of string | `Warning of string] [@@deriving show]

type marker = {
  kind                    : marker_kind;
  mark                    : Gtk.text_mark [@opaque];
  icon                    : (string * string) option;
  mutable icon_obj        : GObj.widget option [@printer fun fmt v ->
      Format.fprintf fmt "%s" (if Option.is_some v then "Some" else "None")];
} [@@deriving show]

let destroy_markers markers =
  List.iter begin fun marker ->
    Gaux.may marker.icon_obj ~f:(fun i -> i#destroy());
    match GtkText.Mark.get_buffer marker.mark with
    | None -> ()
    | Some buffer ->
        GtkText.Buffer.delete_mark buffer marker.mark;
  end markers

