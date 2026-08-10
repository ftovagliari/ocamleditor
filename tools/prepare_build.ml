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


#cd "src"
    #use "../tools/scripting.ml"

open Printf

let required_ocaml_version = "5.3.0"

let exe = if is_win32 then ".exe" else ""

let generate_oebuild_script () =
  run "ocaml -I common -I +unix -I +str str.cma unix.cma utils.cmo file_util.cmo generate_oebuild_script.ml";;

let prepare_build () =
  if Sys.ocaml_version < required_ocaml_version then begin
    eprintf "You are using OCaml-%s but version %s is required." Sys.ocaml_version required_ocaml_version;
  end else begin
    run "ocamllex err_lexer.mll";
    run "ocamlyacc err_parser.mly";
    run "atdgen -t settings.atd";
    run "atdgen -j settings.atd";
    run (rm ^ " merlin_t.* merlin_j.*");
    run "atdgen -t merlin.atd";
    run "atdgen -j merlin.atd";
    run (rm ^ " find_text_t.* find_text_j.*");
    run "atdgen -t find_text.atd";
    run "atdgen -j find_text.atd";
    (* Clean project generated files (ATD outputs) before regenerating them during build. *)
    run (rm ^ " project_t.* project_j.*");
    run "atdgen -t project.atd";
    run "atdgen -j project.atd";

    (try generate_oebuild_script() with Failure msg -> raise (Script_error ("generate_oebuild_script()", 2)));
    (*  *)
    (* let chan = open_out_bin "../src/build_id.ml" in
       kprintf (output_string chan) "let timestamp = \"%f\"\n" (Unix.gettimeofday ());
       kprintf (output_string chan) "let git_hash = \"\"\n";
       close_out_noerr chan; *)
    (*  *)
    print_newline()
  end;;

let _ = main ~default_target:prepare_build ~targets:[
    "-generate-oebuild-script", generate_oebuild_script, " (undocumented)";
  ]~options:[] ()
