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

type t = {
  mutable id               : int;
  mutable name             : string;
  mutable descr            : string;
  mutable default          : bool;
  mutable byt              : bool;
  mutable opt              : bool;
  mutable libs             : string;
  mutable other_objects    : string;
  mutable files            : string;
  mutable package          : string;
  mutable includes         : string;
  mutable thread           : bool;
  mutable vmthread         : bool;
  mutable pp               : string;
  mutable inline           : int option;
  mutable nodep            : bool;
  mutable dontlinkdep      : bool;
  mutable cflags           : string;
  mutable lflags           : string;
  mutable target_type      : target_type;
  mutable outname          : string;
  mutable lib_install_path : string;
  mutable external_tasks   : Task.t list;
  mutable restrictions     : string list;
  mutable dependencies     : int list; (* Build dependencies, id list *)
  mutable sub_targets      : t list; (* For use with Findlib sub-packages *)
  mutable is_fl_package    : bool;
  mutable readonly         : bool;
  mutable visible          : bool;
  mutable subsystem        : subsystem option;
  mutable dontaddopt       : bool;
  mutable node_collapsed   : bool;
}
and task = [ `NONE | `CLEAN | `COMPILE | `REBUILD | `ETASK of Task.t ]
and target_type = Executable | Library | Plugin | Pack | External
and subsystem = Console | Windows

let default_runtime_build_task = `COMPILE

let string_of_task = function
  | `NONE -> "<NONE>"
  | `CLEAN -> "<CLEAN>"
  | `COMPILE -> "<COMPILE>"
  | `REBUILD -> "<REBUILD>"
  | `ETASK task -> task.Task.et_name

let markup_of_task = function
  | `NONE -> "None"
  | `CLEAN -> "Clean"
  | `COMPILE -> "Build"
  | `REBUILD -> "Rebuild <small><i>(Clean and Build)</i></small>"
  | `ETASK task -> Glib.Markup.escape_text task.Task.et_name

let task_of_string target = function
  | "<NONE>" -> `NONE
  | "<CLEAN>" -> `CLEAN
  | "<COMPILE>" -> `COMPILE
  | "<REBUILD>" -> `REBUILD
  | task_name -> begin
      try
        `ETASK (List.find (fun x -> x.Task.et_name = task_name) target.external_tasks)
      with Not_found -> default_runtime_build_task
    end

let string_of_target_type = function
  | Executable -> "Executable"
  | Library -> "Library"
  | Plugin -> "Plugin"
  | Pack -> "Pack"
  | External -> "External"

let target_type_of_string = function
  | "Executable" -> Executable
  | "Library" -> Library
  | "Plugin" -> Plugin
  | "Pack" -> Pack
  | "External" -> External
  | _ -> assert false

let subsystem_of_string = function
  | "Console" -> Console
  | "Windows" -> Windows
  | x -> ksprintf failwith "subsystem_of_string: %S" x

let string_of_subsystem = function
  | Console -> "Console"
  | Windows -> "Windows"

(** create *)
let create ~id ~name = {
  id                 = id;
  name               = name;
  descr              = "";
  default            = (id = 0);
  byt                = true;
  opt                = false;
  libs               = "";
  other_objects      = "";
  files              = "";
  package            = "";
  includes           = "";
  thread             = false;
  vmthread           = false;
  pp                 = "";
  inline             = None;
  nodep              = false;
  dontlinkdep        = false;
  cflags             = "";
  lflags             = "";
  target_type        = Executable;
  outname            = "";
  lib_install_path   = "";
  external_tasks     = [];
  restrictions       = [];
  dependencies       = [];
  sub_targets        = [];
  is_fl_package      = false;
  readonly           = false;
  visible            = true;
  subsystem          = None;
  dontaddopt         = false;
  node_collapsed     = false;
}


(** find_top_dependencies *)
let find_top_dependencies target =
  Oebuild_dep.ocamldep_toplevels (Utils.split " +" target.files) |> Oebuild_dep.sort_dependencies

(** find_dependencies *)
let find_dependencies target =
  Oebuild_dep.ocamldep_recursive (Utils.split " +" target.files) |> Oebuild_dep.sort_dependencies

(** find_target_dependencies *)
let rec find_target_dependencies targets trg =
  Utils.ListExt.remove_dupl (List.flatten (List.map begin fun id ->
      match List.find_opt (fun tg -> tg.id = id) targets with
      | Some target -> (find_target_dependencies targets target) @ [target]
      | _ -> []
    end trg.dependencies));;

(** filter_external_tasks *)
let filter_external_tasks target phase =
  List.filter_map begin fun task ->
    match task.Task.et_phase with
    | Some ph ->
        if task.Task.et_always_run_in_project && phase = ph then Some (`OTHER, task) else None
    | _ -> None
  end target.external_tasks

(** get_full_libs *)
let get_full_libs target = target.libs

(** create_cmd_line *)
let create_cmd_line ?(flags=[]) ?(can_compile_native=true) target =
  let pref = Preferences.preferences#get in
  let quote = Filename.quote in
  let files = Shell.parse_args target.files in
  let serial_jobs =
    match pref.build_parallel with
    | None -> ["-serial"]
    | Some n -> ["-jobs"; string_of_int n]
  in
  let args =
    files
    @ serial_jobs
    @ ["-bin-annot"]
    @ ["-verbose"; (string_of_int pref.build_verbosity)]
    @ (if target.pp <> "" then ["-pp"; quote target.pp] else [])
    @ (match target.inline with Some n -> ["-inline"; string_of_int n] | _ -> [])
    @ (if target.cflags <> "" then ["-cflags"; (quote target.cflags)] else [])
    @ (if target.lflags <> "" then ["-lflags"; (quote (target.lflags))] else [])
    @ (if target.package <> "" then ["-package"; (quote (target.package))] else [])
    @ (if target.includes <> "" then ["-I"; (quote (target.includes))] else [])
    @ (if target.libs <> "" then ["-l"; (quote (get_full_libs target))] else [])
    @ (if target.other_objects <> "" then ["-m"; quote (target.other_objects)] else [])
    @ begin
      match target.target_type with
      | Executable -> []
      | Library -> ["-a"]
      | Plugin -> ["-shared"]
      | Pack -> ["-pack"]
      | External -> []
    end
    @ (if target.byt then ["-byt"] else [])
    @ (if target.opt && can_compile_native then ["-opt"] else [])
    @ (if target.thread then ["-thread"] else [])
    @ (if target.vmthread then ["-vmthread"] else [])
    @ (if target.nodep then ["-no-dep"] else [])
    @ (if target.dontlinkdep then ["-dont-link-dep"] else [])
    @ (if target.dontaddopt then ["-dont-add-opt"] else [])
    @ (if target.outname <> "" then ["-o"; quote (target.outname)] else [])
    @ flags
  in
  let args = List.map (fun a -> true, a) args in
  Oe_config.oebuild_command, args

(** tasks_compile *)
let rec tasks_compile ?(name="tasks_compile") ?(flags=[]) ?(build_deps=[]) ?can_compile_native target =
  if Oebuild.check_restrictions target.restrictions then begin
    let build_deps = List.map (fun bc -> List.flatten (tasks_compile ~name:(sprintf "Build \xC2\xAB%s\xC2\xBB" bc.name) ~flags ?can_compile_native bc)) build_deps in
    let build_deps = List.flatten build_deps in
    let filter_tasks = filter_external_tasks target in
    let et_before_compile = filter_tasks Task.Before_compile in
    let et_compile = filter_tasks Task.Compile in
    let et_compile = if et_compile = [] then [`COMPILE, begin
        let cmd, args = create_cmd_line ~flags ?can_compile_native target in
        Task.create ~name ~env:[] ~dir:"" ~cmd ~args ()
      end] else et_compile in
    let et_after_compile = filter_tasks Task.After_compile in
    (* Execute sequence *)
    [build_deps @ et_before_compile @ et_compile @ et_after_compile]
  end else []

(** Convert from old file version *)
let convert_from_1 old_filename =
  let targets = if Sys.file_exists old_filename then begin
      let ichan = open_in_bin old_filename in
      let (targets : Bconf_old_1.t list) = input_value ichan in
      close_in ichan;
      List.rev targets
    end else [] in
  (* write new file version *)
  let i = ref (-1) in
  let targets = List.map begin fun t ->
      incr i;
      let target = create ~id:!i ~name:(string_of_int !i) in
      target.default <- (!i = 0);
      target.opt <- t.Bconf_old_1.opt;
      target.libs <- t.Bconf_old_1.libs;
      target.other_objects <- t.Bconf_old_1.mods;
      target.includes <- t.Bconf_old_1.includes;
      target.thread <- t.Bconf_old_1.thread;
      target.vmthread <- t.Bconf_old_1.vmthread;
      target.cflags <- t.Bconf_old_1.cflags;
      target.lflags <- t.Bconf_old_1.lflags;
      target.target_type <- (if t.Bconf_old_1.libname <> None then Library else Executable);
      target.outname <- "";
      target.lib_install_path <- "";
      target
    end targets in
  (*  if Sys.file_exists old_filename then (Sys.remove old_filename);*)
  targets






























