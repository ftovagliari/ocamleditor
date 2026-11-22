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


type kind = [ `CLEAN | `CLEANALL | `ANNOT | `COMPILE | `RUN | `OTHER]
type phase =
    Before_clean | Clean | After_clean | Before_compile | Compile | After_compile

type t = {
  mutable et_name                  : string;
  mutable et_env                   : (bool * string) list;
  mutable et_env_replace           : bool; (* After system environment *)
  mutable et_dir                   : string; (* Working directory: relative to the project source directory (actually: Sys.getcwd()) *)
  mutable et_cmd                   : string;
  mutable et_args                  : (bool * string) list;
  (* Files produced by this task, relative to the project source directory.
     A task that declares outputs can be expressed as a build rule; one that
     does not can only be run on demand. *)
  mutable et_outputs               : (bool * string) list;
  (* Files this task reads, relative to the project source directory. *)
  mutable et_deps                  : (bool * string) list;
  mutable et_phase                 : phase option;
  mutable et_always_run_in_project : bool;
  mutable et_always_run_in_script  : bool;
  mutable et_readonly              : bool;
  mutable et_visible               : bool;
}

let string_of_phase = function
  | Before_clean -> "Before_clean"
  | Clean -> "Clean"
  | After_clean -> "After_clean"
  | Before_compile -> "Before_compile"
  | Compile -> "Compile"
  | After_compile -> "After_compile"

let descr_of_phase = function
  | Before_clean -> "Pre-clean"
  | Clean -> "Clean"
  | After_clean -> "Post-clean"
  | Before_compile -> "Pre-build"
  | Compile -> "Build"
  | After_compile -> "Post-build"

let phase_of_string = function
  | "Before_clean" -> Before_clean
  | "Clean" -> Clean
  | "After_clean" -> After_clean
  | "Before_compile" -> Before_compile
  | "Compile" -> Compile
  | "After_compile" -> After_compile
  | _ -> failwith "phase_of_string"

let create ~name ~env ?(env_replace=false) ~dir ~cmd ~args ?(outputs=[]) ?(deps=[])
    ?phase ?(run_in_project=false) ?(run_in_script=true)
    ?(readonly=false) ?(visible=true) () = {
  et_name                  = name;
  et_env                   = env;
  et_env_replace           = env_replace;
  et_dir                   = dir;
  et_cmd                   = cmd;
  et_args                  = args;
  et_outputs               = outputs;
  et_deps                  = deps;
  et_phase                 = phase;
  et_always_run_in_project = run_in_project;
  et_always_run_in_script  = run_in_script;
  et_readonly              = readonly;
  et_visible               = visible;
}

(** The values of an (enabled, value) list whose flag is set. *)
let enabled entries = List.filter_map (fun (e, v) -> if e then Some v else None) entries

module LogBuilder = Log.Make(struct
    let channel = stderr
    let verbosity = `ERROR
    let print_timestamp = false
  end)

module Log = LogBuilder.Make(struct
    let prefix = "Task"
  end)

module StringSet = Set.Make (String)

let name_of_entry entry =
  match String.split_first ~sep:"=" entry with
  | Some (name, _value) -> name
  | None -> entry

let keep_first_by_name entries =
  let step (seen, kept) entry =
    let name = name_of_entry entry in
    if StringSet.mem name seen then (seen, kept)
    else (StringSet.add name seen, entry :: kept)
  in
  List.fold_left step (StringSet.empty, []) entries |> snd |> List.rev

let merge_env overrides =
  let overrides = Array.to_list overrides in
  let env = Array.to_list (Unix.environment ()) in
  let latest_first = List.rev overrides in
  let apply_override entry =
    let name = name_of_entry entry in
    latest_first
    |> List.find_opt (fun o -> name_of_entry o = name)
    |> Option.value ~default:entry
  in
  env @ overrides
  |> List.map apply_override
  |> keep_first_by_name
  |> Array.of_list

let handle f task =
  let tenv = task.et_env |> List.filter_map (fun (e, v) -> if e then Some v else None) |> Array.of_list  in
  let env =
    if task.et_env_replace then Array.concat [tenv]
    else merge_env tenv
  in
  let prog = task.et_cmd in
  let dir = if task.et_dir <> "" then task.et_dir else (Sys.getcwd ()) in
  let args =
    task.et_args
    |> List.filter (fun (e, _) -> e)
    |> List.map (fun (_, x) -> x)
  in
  begin
    match !Log.verbosity with
    | `DEBUG ->
        args |> String.concat ";" |> Log.println !Log.verbosity "%s %s\n%!" prog;
        env
        |> Array.to_list
        |> List.filter (fun e -> name_of_entry e = "PATH")
        |> List.iter (fun e -> Log.println !Log.verbosity "%s\n%!" e);
    | _ -> ()
  end;
  f ~env ~dir ~prog ~args;;
