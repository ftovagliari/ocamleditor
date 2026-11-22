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


type local = {
  mutable root               : string; (** ROOT directory of the project (non-persistent) *)
  mutable ocamllib           : string; (** Full path of the std. lib. from 'ocamlc -where';
                                           when written to the project file it is translated in "". *)
  mutable editor_view_state  : (string * int * int * bool) list; (** filename, scroll top offset, current offset, active *)
  mutable bookmarks          : Oe.bookmark list;
  mutable opam_switch        : string;
}

type t = {
  local : local;
  mutable encoding           : string option;
  mutable name               : string;
  mutable modified           : bool;
  mutable author             : string;
  mutable description        : string;
  mutable version            : string;
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
  mutable build_script       : Build_script.t;
  mutable file_watcher       : Inotify.watch option;
}

let default_extension = ".project.json"
let default_local_extension = ".local.json"
let old_extension = ".project"
let old_local_extension = ".project.local"
let default_dir_src = "src"
let default_dir_bak = "bak"
let default_dir_tmp = ".tmp"
let default_dir_tools = "tools"

let find_target proj id =
  List.find_opt (fun bc -> bc.Target.id = id) proj.targets;;

let find_target_string proj id = find_target proj (int_of_string id)

let find_task proj name =
  List.find_opt (fun et -> et.Task.et_name = name)
    (List.flatten (List.map (fun bc -> bc.Target.external_tasks) proj.targets));;

