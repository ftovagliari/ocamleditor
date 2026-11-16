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

class widget ~project ?(spacing=3) ?border_width ~label_width ?packing ()=
  let vbox = GPack.vbox ~spacing ?border_width ?packing () in
  let top = ref 0 in
  let table = GPack.table ~row_spacings:spacing ~col_spacings:spacing ~packing:vbox#add () in

  let _ = incr top in
  let _ = GMisc.label ~xalign:0.0 ~width:label_width ~text:"Native Compilation:" ~packing:(table#attach ~top:!top ~left:0) () in
  let box = GPack.hbox ~spacing ~packing:(table#attach ~top:!top ~left:1 ~expand:`X) () in
  let entry_native = GEdit.entry ~editable:false ~packing:box#add () in
  let button_native = GButton.button ~label:"Test" ~packing:box#pack () in

  object (self)
    inherit GObj.widget vbox#as_widget

    method private with_ocamllib : 'a.(unit -> 'a) -> 'a = fun f ->
      Ocaml_config.unset_ocamllib ();
      let finally () = Ocaml_config.unset_ocamllib () in
      (try let result = f() in finally(); result with ex -> (finally(); raise ex))

    method get_ocaml_version () =
      try
        self#with_ocamllib begin fun () ->
          let compiler = match Ocaml_config.find_tool `OCAMLC "" with Some x -> x | _ -> assert false in
          let output = String.trim (Ocaml_config.ocaml_version ~compiler ()) in
          let lines = Utils.split "\n" output in
          let version, stdlib = match lines with [a; b] -> a, b | _ -> assert false in
          version, stdlib
        end
      with _ -> ("<Not found>", "")

    initializer
      ignore (button_native#connect#clicked ~callback:begin fun () ->
          entry_native#set_text "Please wait...";
          Gmisclib.Idle.add begin fun () ->
            let can_compile_native = self#with_ocamllib (Ocaml_config.can_compile_native) in
            entry_native#set_text (match can_compile_native with Some ccomp_type -> "Yes (" ^ ccomp_type ^ ")" | _ -> "No")
          end
        end);
  end






