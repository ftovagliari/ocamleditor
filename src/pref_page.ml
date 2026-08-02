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

let ocaml_preview =
  "(* Syntax Coloring Preview *)
open Printf

(** ocamldoc comment block.
  * @version 1.0
  *)
class sub_logic () =
  object (self)
    inherit core_logic
    (** ocamldoc comment block. *)
    method solve ~required ?optional () =
      let (/$/) = List.nth in
      let number = 1234 in
      printf \"The number is: %d\" number;
      if required /$/ number > 0 then begin
        match optional with
          | Some a ->
            ignore (Array.fold_left
              (fun acc x -> acc && x) a [| true; false |])
          | _ -> raise Not_found
      end else ()
  end
"

let (//) = Filename.concat

(** create_align *)
let create_align ?title ?(indent=13) ~(vbox : GPack.box) ?show () =
  let box = GPack.vbox ~spacing:8 ~packing:vbox#pack () in
  let indent = match title with
    | None -> 0
    | Some title ->
        let _ = GMisc.label ~markup:(sprintf "<b>%s</b>" title) ~xalign:0.0 ~packing:box#add () in
        indent
  in
  let align = GBin.alignment ~padding:(0, 0, indent, 0) ~packing:box#add ?show () in
  align

let spacing = 13
let xalign = 0.0
let col_spacings = 21
let row_spacings = 5
let indent = 25

(** page *)
class virtual page title (box : GPack.box) =
  let tbox = GPack.vbox ~spacing:3 ~packing:box#pack () in
  let _    = GMisc.label ~markup:(sprintf "<big><b>%s</b></big>" title) ~xalign:0.0 ~packing:tbox#pack () in
  let _    = GMisc.separator `HORIZONTAL ~packing:tbox#pack () in
  let _    = box#reorder_child tbox#coerce ~pos:0 in
  object
    inherit GObj.widget box#as_widget
    method virtual write : Settings_t.settings -> unit
    method virtual read : Settings_t.settings -> unit
    method title = title
  end
