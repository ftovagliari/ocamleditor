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


#cd "src";;
#use "../tools/scripting.ml";;

open Printf

let distclean () =
  remove_file "annot_lexer.ml";
  remove_file "annot_parser.ml";
  remove_file "annot_parser.mli";
  run (rm ^ " settings_t.* settings_j.*");
  run (rm ^ " merlin_t.* merlin_j.*");
  run (rm ^ " icons"//"icons.*");
  remove_file "err_lexer.ml";
  remove_file "err_parser.ml";
  remove_file "err_parser.mli";
  remove_file "oebuild_script.ml";
  remove_file "ocamleditor.opt.exe.manifest";
  remove_file "resource.res";
  remove_file "build_id.ml";
  let run_no_errors cmd = try sys_command [cmd] with Script_error _ -> () in
  ksprintf run_no_errors "%s *.exe *.bak *.annot *~" rm;
  let rmdir dir = if Sys.file_exists dir then (ksprintf run_no_errors "%s %s" rmr dir) in
  rmdir (Filename.parent_dir_name // "plugins");
  rmdir (Filename.parent_dir_name // "bak");
  rmdir (Filename.parent_dir_name // "_build");
  rmdir (Filename.parent_dir_name // ".tmp");
  rmdir (Filename.parent_dir_name // ".cache");
;;

let _ = main ~default_target:distclean ~options:[] ()
