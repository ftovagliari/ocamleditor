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
  (* ROOT directory of the project (non-persistent) *)
  mutable root               : string;
  mutable ocaml_home         : string; (* Unused *)
  (* Full path of the std. lib., either from the OCAMLLIB e. v. or from 'ocamlc -where';
     when written to the XML project file, if equals to 'ocamlc -where', it is translated in "". *)
  mutable ocamllib           : string;
  mutable ocamllib_from_env  : bool;
  mutable encoding           : string option;
  mutable name               : string;
  mutable modified           : bool;
  mutable author             : string;
  mutable description        : string;
  mutable version            : string;
  mutable files              : (Editor_file.file * (int * int)) list; (* Currently open files in the editor (non-persistent) (filename, scrollTopOffset, cursorOffset) *)
  mutable open_files         : (string * int * int * bool) list; (* filename, scroll top offset, current offset, active *)
  mutable targets            : Target.t list;
  mutable executables        : Rconf.t list;
  mutable autocomp_enabled   : bool;
  mutable autocomp_delay     : float;
  mutable autocomp_cflags    : string;
  mutable autocomp_compiler  : string;
  mutable autocomp_dflags    : string array;
  mutable search_path        : string list;
  mutable in_source_path     : string -> string option;
  mutable source_paths       : string list;
  mutable can_compile_native : bool;
  mutable symbols            : Oe.symbol_cache;
  mutable build_script       : Build_script.t;
  mutable bookmarks          : Oe.bookmark list;
  mutable file_watcher       : Inotify.watch option;
}

let default_extension = ".project"
let default_dir_src = "src"
let default_dir_bak = "bak"
let default_dir_tmp = ".tmp"
let default_dir_tools = "tools"
let old_extension = ".xml"

let find_target proj id =
  List.find_opt (fun bc -> bc.Target.id = id) proj.targets;;

let find_target_string proj id = find_target proj (int_of_string id)

let find_task proj name =
  List.find_opt (fun et -> et.Task.et_name = name)
    (List.flatten (List.map (fun bc -> bc.Target.external_tasks) proj.targets));;

