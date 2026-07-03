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


open Utils
open Printf
open Target
open Task
open Build_script
open Build_script_args

(** print_targets *)
let print_targets ochan targets external_tasks =
  let i = ref 0 in
  let print = fprintf ochan "  %s\n" in
  output_string ochan "let targets = [\n";
  List.iter begin fun {bst_target=tg; bst_show(*; _*)} ->
    if bst_show then (incr i);
    let num = if bst_show then !i else 0 in
    ksprintf print "\n  (\x2A %d \x2A)" !i;
    ksprintf print "%S, {" tg.name;
    ksprintf print "  descr                = %S;" tg.descr;
    ksprintf print "  num                  = %d;" num;
    ksprintf print "  id                   = %d;" tg.id;
    ksprintf print "  output_name          = %S;" tg.outname;
    ksprintf print "  target_type          = %s;" (string_of_target_type tg.target_type);
    ksprintf print "  compilation_bytecode = %b;" tg.byt;
    ksprintf print "  compilation_native   = %b;" tg.opt;
    ksprintf print "  toplevel_modules     = %S;" tg.files;
    ksprintf print "  package              = %S;" tg.package;
    ksprintf print "  search_path          = %S; (\x2A -I \x2A)" tg.includes;
    ksprintf print "  required_libraries   = %S;" tg.libs;
    ksprintf print "  compiler_flags       = %S;" tg.cflags;
    ksprintf print "  linker_flags         = %S;" tg.lflags;
    ksprintf print "  thread               = %b;" tg.thread;
    ksprintf print "  vmthread             = %b;" tg.vmthread;
    ksprintf print "  pp                   = %S;" tg.pp;
    ksprintf print "  inline               = %s;" (match tg.inline with Some x -> sprintf "Some %d" x | _ -> "None");
    ksprintf print "  nodep                = %b;" tg.nodep;
    ksprintf print "  dontlinkdep          = %b;" tg.dontlinkdep;
    ksprintf print "  dontaddopt           = %b;" tg.dontaddopt;
    ksprintf print "  library_install_dir  = %S; (\x2A Relative to the Standard Library Directory \x2A)" tg.lib_install_path;
    ksprintf print "  other_objects        = %S;" tg.other_objects;
    ksprintf print "  external_tasks       = [%s];" (String.concat "; " (List.map (fun (n(*, _*)) -> string_of_int n) (List.assoc tg external_tasks)));
    ksprintf print "  restrictions         = [%s];" (String.concat "; " (List.map (sprintf "%S") tg.restrictions));
    ksprintf print "  dependencies         = [%s];" (String.concat "; " (List.map (sprintf "%d") tg.dependencies));
    ksprintf print "  show                 = %b;" bst_show;
    ksprintf print "};";
  end targets;
  output_string ochan "];;\n";;

(** ident_of_arg *)
let ident_of_arg =
  let re = Str.regexp "[-.]" in
  let normalize_ident = Str.global_replace re "_" in
  fun arg -> sprintf "arg_%d_%s" arg.bsa_id (normalize_ident (Str.string_after arg.bsa_key 1));;

(** print_add_args *)
let print_add_args bc et args =
  List.fold_left begin fun acc arg ->
    match arg.bsa_task with
    | Some (abc, aet) ->
        if bc.Target.id = abc.Target.id && et.Task.et_name = aet.Task.et_name then
          match arg.bsa_mode with
          | `add ->
              let condition =
                sprintf "command = %s" (Build_script_command.code_of_command arg.bsa_cmd)
              in
              let arg =
                match arg.bsa_pass with
                | `key ->
                    [ sprintf "\n                                %s && (!%s = Some true), \"%s\"" condition (ident_of_arg arg) arg.bsa_key ]
                | `value when arg.bsa_type = Bool ->
                    [ sprintf "\n                                %s, (match !%s with Some x -> string_of_bool x | _ -> \"\")" condition (ident_of_arg arg) ]
                | `value ->
                    [ sprintf "\n                                %s, (match !%s with Some x -> x | _ -> \"\")" condition (ident_of_arg arg) ]
                | `key_value when arg.bsa_type = Bool ->
                    [ sprintf "\n                                %s, (match !%s with Some _ -> \"%s\" | _ -> \"\")"
                        condition (ident_of_arg arg) arg.bsa_key;
                      sprintf "\n                                %s, (match !%s with Some x -> sprintf \"%%b\" x | _ -> \"\")"
                        condition (ident_of_arg arg)
                    ]
                | `key_value ->
                    [
                      sprintf "\n                                %s, (match !%s with Some _ -> \"%s\" | _ -> \"\")"
                        condition (ident_of_arg arg) arg.bsa_key;
                      sprintf "\n                                %s, (match !%s with Some x -> sprintf \"%%s\" x | _ -> \"\")"
                        condition (ident_of_arg arg)
                    ]
              in
              arg :: acc
          | `replace _ -> acc (* TODO:  *)
        else acc
    | _ -> acc
  end [] args |> List.flatten;;

(** print_external_tasks *)
let print_external_tasks ochan project =
  let targets = project.Prj.targets in
  let args = project.Prj.build_script.bs_args in
  let i = ref 0 in
  let print = fprintf ochan "  %s\n" in
  output_string ochan "let external_tasks = [\n";
  let targets = project.Prj.build_script.bs_targets in
  let ets = List.map begin fun {bst_target=tg; _} ->
      let ets =
        List.map begin fun et ->
          let base_args = List.filter_map (fun (x, y) -> if x then Some y else None) et.et_args in
          let base_args = List.map (sprintf "true,%S") base_args in
          let custom_args = print_add_args tg et args in
          let args = base_args @ custom_args in
          let index = !i in
          ksprintf print "\n  %d, (fun command -> {" index;
          ksprintf print "  et_name                  = %S;" et.et_name;
          ksprintf print "  et_env                   = [%s];"
            (String.concat ";" (List.map (sprintf "%S")
                                  (List.filter_map (fun (x, y) -> if x then Some y else None) et.et_env)));
          ksprintf print "  et_env_replace           = %b;" et.et_env_replace;
          ksprintf print "  et_dir                   = %S;" et.et_dir;
          ksprintf print "  et_cmd                   = %S;" et.et_cmd;
          ksprintf print "  et_args                  = [%s];" (String.concat "; " args);
          ksprintf print "  et_phase                 = %s;" (match et.et_phase with Some p -> "Some " ^ (Task.string_of_phase p) | _ -> "None");
          ksprintf print "  et_always_run_in_project = %b;" et.et_always_run_in_project;
          ksprintf print "  et_always_run_in_script  = %b;" et.et_always_run_in_script;
          ksprintf print "  et_readonly              = %b;" et.et_readonly;
          ksprintf print "  et_visible               = %b;" et.et_visible;
          ksprintf print "});";
          incr i;
          index
        end tg.external_tasks
      in tg, ets
    end targets in
  output_string ochan "];;\n";
  ets;;

(** print_general_commands *)
let print_general_commands ochan external_tasks project =
  let commands = project.Prj.build_script.bs_commands in
  fprintf ochan "let general_commands = [\n";
  List.iter begin fun command ->
    let target = command.bsc_target in
    let task = command.bsc_task in
    external_tasks
    |> List.find_map begin fun (tg, et_indexes) ->
      if tg.id = target.id && task.et_name = task.et_name then
        Some begin
          let et = List.combine tg.external_tasks et_indexes in
          match List.find_opt (fun (et, _) -> et.et_name = task.et_name) et with
          | Some (_, index(*, _)*)) ->
              fprintf ochan "  `%s, (%d, %S);\n"
                (Build_script.string_of_command command.bsc_name) index command.bsc_descr;
          | _ -> ()
        end else None
    end |> ignore;


  end commands;
  fprintf ochan "]\n";;

(** print_cmd_line_args *)
let print_cmd_line_args ochan project =
  let args = project.Prj.build_script.bs_args in
  List.iter begin fun arg ->
    let default =
      match arg.bsa_default with
      | `flag x -> string_of_bool x
      | `bool x -> string_of_bool x
      | `string x -> sprintf "%S" x
    in
    fprintf ochan "let %s = ref %s\n" (ident_of_arg arg) (if arg.bsa_default_override then "(Some " ^ default ^ ")" else "None");
  end args;
  (*  *)
  let cargs = List.map (fun a -> a.bsa_cmd, a) args in
  let groups = ListExt.group_assoc cargs in
  fprintf ochan "\nlet cmd_line_args = [\n";
  List.iter begin fun (cmd, args) ->
    fprintf ochan "  %s, [\n" (Build_script_command.code_of_command cmd);
    List.iter begin fun arg ->
      let typ =
        match arg.bsa_type with
        | Flag -> sprintf "Bool (fun x -> %s := Some x)" (ident_of_arg arg)
        | Bool -> sprintf "Bool (fun x -> %s := Some x)" (ident_of_arg arg)
        | String -> sprintf "String (fun x -> %s := Some x)" (ident_of_arg arg)
      in
      let default_value =
        if arg.bsa_default_override then
          match arg.bsa_default with
          | `flag _ -> sprintf "\" ^ (match !%s with Some x -> sprintf \"%%s\" (if x then \"Set\" else \"Not Set\") | _ -> failwith \"build_script_printer (flag)\") ^ \"" (ident_of_arg arg)
          | `bool _ -> sprintf "\" ^ (match !%s with Some x -> string_of_bool x | _ ->  failwith \"build_script_printer (bool)\") ^ \"" (ident_of_arg arg)
          | `string _ -> sprintf "\" ^ (match !%s with Some x -> x | _ -> failwith \"build_script_printer (string)\") ^ \"" (ident_of_arg arg)
        else begin
          match arg.bsa_task with
          | Some (_, task) -> sprintf "see \\\"%s %s -help\\\"" task.Task.et_cmd (String.concat " " (List.map (fun (c, x) -> if c then x else "") task.Task.et_args))
          | _ -> "<unknown>"
        end
      in
      fprintf ochan "    %S, %s,\n      (\" %s [default: %s]\");\n" arg.bsa_key typ (String.escaped arg.bsa_doc) default_value;
    end args;
    fprintf ochan "  ];\n";
  end groups;
  fprintf ochan "]\n";;

module Minification = struct
  module Comments = struct
    let pat = regexp "\\((\\*\\)\\|\\(\\*)\\)"
    let lineend = regexp "$"
    let search_f_pat = Str.search_forward pat
    let scan_locale txt =
      (* Non vengono considerate le stringhe *)
      let rec f acc pos start =
        begin
          try
            let p = search_f_pat txt pos in
            begin
              try
                ignore (Str.matched_group 1 txt);
                f acc (p + 2) (p :: start);
              with Not_found -> begin
                  ignore (Str.matched_group 2 txt);
                  (match start with
                   | [] -> f acc (p + 2) []
                   | [start] -> f ((start, (p + 2), (txt.[start + 2] = '*')) :: acc) (p + 2) []
                   | _::prev -> f acc (p + 2) prev)
                end
            end
          with Not_found -> acc
        end
      in
      List.rev (f [] 0 [])
    let scan txt = scan_locale txt
  end

  let minify =
    let re = Str.regexp "[\r\n]+ *" in
    fun buf ->
      (* TODO: find something less ugly than this use of Bytes.(..) *)
      let b_buf = Bytes.of_string buf in
      buf
      |> Comments.scan
      |> List.iter begin fun (b, e, _) ->
        let len = e - b in
        String.blit (String.make len ' ') 0 b_buf b len
      end;
      Str.global_replace re " " Bytes.(to_string b_buf);;

end

(** print *)
let print ~project ~filename ~minify () =
  let ochan = open_out_bin filename in
  let finally () = close_out_noerr ochan in
  try
    Oebuild_script.code
    |> (if minify then Minification.minify else Fun.id)
    |> output_string ochan;
    output_string ochan "open Arg\n";
    output_string ochan "open Task\n";
    output_string ochan "open Printf\n";
    output_string ochan "\n";
    print_cmd_line_args ochan project;
    output_string ochan "\n";
    let ets = print_external_tasks ochan project in
    output_string ochan "\n\n";
    print_general_commands ochan ets project;
    output_string ochan "\n\n";
    output_string ochan "(\x2A Targets ==================================================== \x2A)\n\n";
    (*  *)
    print_targets ochan project.Prj.build_script.bs_targets ets;
    output_string ochan "\n";
    output_string ochan "(\x2A End of Targets ============================================= \x2A)\n\n";
    (*  *)
    output_string ochan "let _ = main ~cmd_line_args ~external_tasks ~general_commands ~targets\n";
    (*  *)
    finally();
  with ex -> begin
      finally();
      raise ex
    end;;













