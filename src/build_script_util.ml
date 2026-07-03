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


open Arg
open Printf
open Oebuild
open Oebuild_util

type target = {
  descr : string;
  num : int;
  id : int;
  output_name : string;
  target_type : Oebuild.output_type;
  compilation_bytecode : bool;
  compilation_native : bool;
  toplevel_modules : string;
  mutable package : string;
  search_path : string;
  required_libraries : string;
  compiler_flags : string;
  linker_flags : string;
  thread : bool;
  vmthread : bool;
  pp : string;
  inline : int option;
  nodep : bool;
  dontlinkdep : bool;
  dontaddopt : bool;
  library_install_dir : string;
  other_objects : string;
  external_tasks : int list;
  restrictions : string list;
  dependencies : int list;
  show : bool;
}

type target_map_entry = int * (string * target)

exception Error

let pushd, popd =
  let stack = Stack.create () in
  begin fun dir ->
    let cwd = Sys.getcwd () in
    Stack.push cwd stack;
    Sys.chdir dir
  end, (fun () -> Sys.chdir (Stack.pop stack));;

let rpad txt c width =
  let result = txt ^ (String.make width c) in
  String.sub result 0 width;;

let get_compilation_types native t =
  (if t.compilation_bytecode then [Bytecode] else []) @ (if t.compilation_native && native then [Native] else [])

let string_of_compilation_type native t =
  let compilation = get_compilation_types native t in
  String.concat "/" (List.map string_of_compilation_type compilation)

let create_target ?dir f x =
  (match dir with Some dir -> pushd dir | _ -> ());
  f x;
  (match dir with Some _ -> popd() | _ -> ());;

let create_target_func ?tg targets =
  match targets with
  | default_target :: _ ->
      (match tg with Some f -> create_target f | _ -> create_target default_target)
  | [] -> fun _ -> ();;

(** system_config *)
let ccomp_type = Ocaml_config.can_compile_native ()

let system_config () =
  let ocaml_version = match Shell.get_command_output "ocamlc -v" with x :: _ -> x | _ -> "" in
  let std_lib = match Shell.get_command_output "ocamlc -where" with x :: _ -> x | _ -> "" in
  let properties = [
    "OCaml", ocaml_version;
    "Standard library directory", std_lib;
    "OCAMLLIB", (try Sys.getenv "OCAMLLIB" with Not_found -> "<Not_found>");
    "Native compilation supported", (match ccomp_type with Some ccomp_type -> ccomp_type | _ -> "No");
  ] in
  let buf = Buffer.create 100 in
  Buffer.add_string buf "\nSystem configuration\n";
  let maxlength = List.fold_left (fun cand (x, _) -> let len = String.length x in max cand len) 0 properties in
  List.iter (fun (n, v) -> bprintf buf "  %s : %s\n" (rpad (n ^ " ") '.' maxlength) v) properties;
  Buffer.contents buf;;

(** Options *)
module Option = struct
  let prefix = ref ""
  let change_dir = ref "src"
  let verbosity = ref 2
end

(** ETask *)
module ETask = struct

  let filter tasks phase =
    List.filter begin fun task ->
      if task.Task.et_always_run_in_script then
        match task.Task.et_phase with
        | Some ph -> ph = phase
        | _ -> false
      else false
    end tasks;;

  let execute = Task.handle begin fun ~env ~dir ~prog ~args ->
      let exit_code = Spawn.sync
          ~process_in:Spawn.redirect_to_stdout
          ~process_err:Spawn.redirect_to_stderr
          ~working_directory:dir ~env prog (Array.of_list args)
      in
      match exit_code with
      | `ERROR _ -> raise Error
      | `SUCCESS (Unix.WEXITED code)
      | `SUCCESS (Unix.WSIGNALED code)
      | `SUCCESS (Unix.WSTOPPED code) when code <> 0 -> raise Error
      | _ -> ()
    end
end

(** add_target *)
let targets_selected : target_map_entry list ref = ref []

let add_target targets name =
  try
    begin
      try
        let num = int_of_string name in
        if num <= 0 then (raise Exit);
        let name_tg = try List.find (fun (_, tg) -> tg.num = num) targets with Not_found -> raise Exit in
        targets_selected := (num, name_tg) :: !targets_selected;
      with _ ->
        let tg = List.assoc name targets in
        if tg.num <= 0 then (raise Exit);
        targets_selected := (tg.num, (name, tg)) :: !targets_selected;
    end
  with Exit | Not_found -> (raise (Arg.Bad (sprintf "Invalid target `%s'" name)));;

(** find_target_dependencies *)
let rec find_target_dependencies targets trg =
  remove_dupl (List.flatten (List.map begin fun id ->
      try
        let _, target = List.find (fun (_, tg) -> tg.id = id) targets in
        (find_target_dependencies targets target) @ [target]
      with Not_found -> []
    end trg.dependencies));;

(** Show *)
let show = fun targets -> function num, (name, t) ->
  (*let files = Str.split (Str.regexp " +") t.toplevel_modules in*)
  (*let deps = Dep.find ~pp:t.pp ~ignore_stderr:false ~echo:false files in*)
  let b_deps = find_target_dependencies targets t in
  let b_deps = List.map begin fun tg ->
      let name, _ = List.find (fun (_, t) -> t.id = tg.id) targets in
      name
    end b_deps in
  let compilation =
    (if t.compilation_bytecode then [Bytecode] else []) @
    (if t.compilation_native && ccomp_type <> None then [Native] else [])
  in
  let outname = List.map begin fun compilation ->
      get_output_name ~compilation ~outkind:t.target_type ~outname:t.output_name ~dontaddopt:t.dontaddopt ()
    end compilation
  in
  let outkind = string_of_output_type t.target_type in
  let compilation = string_of_compilation_type (ccomp_type <> None) t in
  let prop_1 = [
    "Restrictions", (String.concat " " t.restrictions);
    "Output name", (String.concat ", " outname);
  ] in
  let prop_2 = [
    "Findlib packages", t.package;
    "Search path", t.search_path;
    "Required libraries", t.required_libraries;
    "Compiler flags", t.compiler_flags;
    "Linker flags", t.linker_flags;
    "Toplevel modules", t.toplevel_modules;
    "Target dependencies", (String.concat ", " b_deps);
  ] in
  let properties = if t.target_type = Library then prop_1 @ [
      "Install directory", (Oebuild.ocamllib // t.library_install_dir)
    ] @ prop_2 else prop_1 @ prop_2 in
  printf "%2d) %s (%s, %s)%s\n\n%!" num name outkind compilation
    (if t.descr <> "" then "\n    " ^ t.descr else "");
  let maxlength = List.fold_left (fun cand (x, _) -> let len = String.length x in max cand len) 0 properties in
  List.iter (fun (n, v) -> printf "    %s : %s\n" (rpad (n ^ " ") '.' maxlength) v) properties;;

(** install_lib *)
let install_lib ~compilation ~outname ~external_tasks ~deps target =
  match target.target_type with
  | Library ->
      let deps = deps() in
      Oebuild.install ~compilation ~outkind:target.target_type ~outname ~deps ~path:target.library_install_dir ~ccomp_type
  | Executable | Plugin | Pack | External ->
      eprintf "\"install\" not implemented for Executable, Plugin, Pack or External.";
      raise Exit;;

(** execute_target *)
let rec execute_target ~external_tasks ~targets:avail_targets ~command ?target_deps target =
  if Oebuild.check_restrictions target.restrictions then begin
    let compilation = (if target.compilation_bytecode then [Bytecode] else [])
                      @ (if target.compilation_native && (ccomp_type <> None) then [Native] else []) in
    let files = Str.split (Str.regexp " +") target.toplevel_modules in
    let deps () =
      let verbose = !Option.verbosity >= 4 in
      Oebuild_dep.ocamldep_toplevels ~verbose ~pp:target.pp ~ignore_stderr:false files |> Oebuild_dep.sort_dependencies
    in
    let etasks = List.map begin fun index ->
        let mktask = try List.assoc index external_tasks with Not_found -> assert false in
        mktask command
      end target.external_tasks in
    try
      match target.target_type with
      | External when command = `Build ->
          build ~targets:avail_targets ~external_tasks ~etasks ~deps ~compilation:Unspecified
            ~outname:target.output_name ~files ?target_deps ~verbose:!Option.verbosity target
      | External -> ()
      | Executable | Library | Pack | Plugin ->
          List.iter begin fun compilation ->
            let outname = get_output_name ~compilation ~outkind:target.target_type ~outname:target.output_name ~dontaddopt:target.dontaddopt () in
            match command with
            | `Build ->
                build ~targets:avail_targets ~external_tasks ~etasks ~deps ~compilation
                  ~outname ~files ?target_deps ~verbose:!Option.verbosity target
            | `Install_lib -> install_lib ~compilation ~outname ~external_tasks ~deps target
            | `Clean ->
                List.iter ETask.execute (ETask.filter etasks Before_clean);
                let deps = deps() in
                Oebuild.clean ~deps ();
                List.iter ETask.execute (ETask.filter etasks After_clean);
            | `Distclean ->
                if files <> [] then (Oebuild_util.remove_file ~verbose:false outname);
            | `Show | `Install | `Uninstall -> assert false
          end compilation
    with Exit -> ()
  end else begin
    let target_name, _ =
      try List.find (fun (_, t) -> t.id = target.id) avail_targets
      with Not_found -> ksprintf failwith "Target not found (id=%d)" target.id
    in
    if !Option.verbosity >= 1 then begin
      Printf.printf "=== %s ===\n%!" target_name;
      Printf.printf "Skipped: %s failed\n\n%!" (String.concat " AND " target.restrictions);
    end
  end

(** build *)
and build ~targets:avail_targets ~external_tasks ~etasks ~deps ~compilation ~outname ~files ?target_deps ~verbose target =
  let target_deps =
    match target_deps with
    | None -> [] (*find_target_dependencies avail_targets target*)
    | Some x -> x
  in
  List.iter (execute_target ~external_tasks ~targets:avail_targets ~command:`Build) target_deps;
  let target_name, _ =
    try List.find (fun (_, t) -> t.id = target.id) avail_targets
    with Not_found -> ksprintf failwith "Target not found (id=%d)" target.id
  in
  if !Option.verbosity >= 1 then Printf.printf "=== %s ===\n%!" target_name;
  List.iter ETask.execute (ETask.filter etasks Before_compile);
  let deps = if target.nodep then files else deps() in
  let tasks_compile = ETask.filter etasks Compile in
  if tasks_compile <> [] then List.iter ETask.execute (tasks_compile)
  else
    let crono = if !Option.verbosity >= 3 then Oebuild_util.crono else fun ?label f x -> f x in
    let libs = target.required_libraries in
    match crono ~label:"Build time"
            (Oebuild.build
               ~compilation
               ~package:target.package
               ~includes:target.search_path
               ~libs
               ~other_mods:target.other_objects
               ~outkind:target.target_type
               ~compile_only:false
               ~thread:target.thread
               ~vmthread:target.vmthread
               ~annot:false
               ~bin_annot:false
               ~pp:target.pp
               ?inline:target.inline
               ~cflags:target.compiler_flags
               ~lflags:target.linker_flags
               ~outname
               ~deps
               ~dontlinkdep:target.dontlinkdep
               ~dontaddopt:target.dontaddopt
               ~verbose
               ~toplevel_modules:files) ()
    with
    | Built_successfully ->
        List.iter ETask.execute (ETask.filter etasks After_compile);
    | Build_failed n -> popd(); exit n
;;

(** main *)
let main ~cmd_line_args ~external_tasks ~general_commands ~targets:avail_targets =
  let module Command = struct
    type t = Build_script_command.t

    let find_args tag = try List.assoc tag cmd_line_args with Not_found -> []

    let command tag =
      try
        let descr = snd (List.assoc tag general_commands) in
        [tag, (find_args tag), descr, ""]
      with Not_found -> [];;

    let command_install = command `Install
    let command_uninstall = command `Uninstall

    let string_of_command = function
      | `Install as c when command_install <> [] -> Build_script_command.string_of_command c
      | `Uninstall as c when command_uninstall <> [] -> Build_script_command.string_of_command c
      | `Install | `Uninstall -> assert false
      | x -> Build_script_command.string_of_command x;;

    let command_of_string = function
      | "install" when command_install <> [] -> `Install
      | "uninstall" when command_uninstall <> [] -> `Uninstall
      | ("install" | "uninstall") as c -> raise (Build_script_command.Unrecognized_command c)
      | x -> Build_script_command.command_of_string x;;

    let options =
      List.map (fun (a, b, c, d) -> a, Arg.align b, c, d) ([
          `Build,       (find_args `Build),
          "Build libraries and executables (default command)", "";
        ] @
          command_install @
          command_uninstall @ [
            `Clean,       (find_args `Clean),
            "Remove output files for the selected target",       "";
            `Distclean,   (find_args `Distclean),
            "Remove all build output",                           "";
            `Install_lib, (find_args `Install_lib),
            "Install libraries as subdirectories relative\n               to the standard library directory", "";
            `Show,        (find_args `Show),
            "Show the build options of a target",                "";
          ]);;

    let anon_fun = function
      | `Show -> add_target avail_targets
      | `Build -> add_target avail_targets
      | `Install_lib -> add_target avail_targets
      | `Clean -> add_target avail_targets
      | (`Install | `Uninstall | `Distclean) as x ->
          fun arg -> ksprintf failwith "Invalid anonymous argument `%s' for command `%s'" arg (string_of_command x);;

    (** execute *)
    let execute command =
      (if !Option.verbosity >= 5 then `DEBUG else `WARN) |>Task.Log.set_verbosity;
      pushd !Option.change_dir;
      let targets = List.rev !targets_selected in
      try
        begin
          let execute_general_command command =
            try
              let index, descr = List.assoc command general_commands in
              let task = List.assoc index external_tasks in
              ETask.execute (task command)
            with Not_found -> ()
          in
          match command with
          | `Distclean ->
              List.iter (fun (_, t) -> execute_target ~external_tasks ~targets:avail_targets ~command t) avail_targets;
              Oebuild.distclean();
              execute_general_command `Distclean;
          | (`Install | `Uninstall) as command ->
              execute_general_command command;
          | `Show ->
              printf "%s\n%!" (system_config ());
              Printf.printf "\n%!" ;
              if targets = [] then (raise (Arg.Bad "show: no target specified"));
              List.iter begin fun t ->
                show avail_targets t;
                print_newline();
                print_newline();
              end targets;
          | _ ->
              if targets = [] then (raise (Arg.Bad (sprintf "%s: no target specified" (string_of_command command))));
              List.iter begin fun (_, (name, tg)) ->
                let target_deps = remove_dupl (find_target_dependencies avail_targets tg) in
                (*Printf.printf "%s: %s\n%!" name (String.concat ", " (List.map (fun t -> string_of_int t.id) target_deps));*)
                execute_target ~external_tasks ~targets:avail_targets ~command ~target_deps tg
              end targets
        end;
        popd();
      with Arg.Bad _ as ex ->
        popd();
        raise ex
         | ex ->
             popd();
             Printf.eprintf "File \"build_script_util.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());;
  end in

  let module Argc = Argc.Make (Command) in

  let global_options = [
    ("-C",       Set_string Option.change_dir, "<dir> Change directory before running [default: src]");
    ("-verbose", Set_int Option.verbosity, " Verbosity level (0..5)");
  ] in
  let global_options = Arg.align global_options in
  let command_name = Filename.basename Sys.argv.(0) in
  (* Print targets *)
  let maxlength = List.fold_left (fun cand (x, _) -> let len = String.length x in max cand len) 0 avail_targets in
  let targets_shown = List.filter (fun (_, tg) -> tg.show) avail_targets in
  let help_of_targets = String.concat "\n" (List.map begin fun (name, tg) ->
      let name = rpad name ' ' maxlength in
      sprintf "  %2d) %s (%s, %s)%s" tg.num name
        (string_of_output_type tg.target_type) (string_of_compilation_type (ccomp_type <> None) tg)
        (if tg.descr <> "" then "\n      " ^ tg.descr else "")
    end targets_shown) in
  let usage_msg = sprintf
      "\nUSAGE\n  ocaml %s [global-options*] <command> [command-options*] [targets*]\n  ocaml %s <command> -help"
      command_name command_name
  in
  let help_string () =
    sprintf "%s\n\nGLOBAL OPTIONS%s\nCOMMANDS%s\n\nTARGETS\n%s"
      usage_msg (Arg.usage_string global_options "") Argc.help_of_commands help_of_targets
  in
  try Argc.parse ~usage_msg ~global_options ~default_command:`Build Command.execute
  with
  | Arg.Help _ -> print_endline (help_string ())
  | Arg.Bad msg -> prerr_endline msg
  | Argc.Help_Command (cmd, (specs, descr, usage), msg) ->
      let name = Command.string_of_command cmd in
      begin
        match cmd with
        | `Distclean ->
            printf "%s %s - %s\n\nUSAGE\n  ocaml %s [global_options*] %s\n\nOPTIONS%s"
              command_name name descr command_name name (Arg.usage_string specs "")
        | _ ->
            printf "%s %s - %s\n\nUSAGE\n  ocaml %s [global_options*] %s [options*] [targets*]\n\nOPTIONS%s"
              command_name name descr command_name name (Arg.usage_string specs "")
      end;
  | Build_script_command.Unrecognized_command msg -> prerr_endline msg
  | Error -> exit 2
  | ex -> prerr_endline (Printexc.to_string ex)
;;

