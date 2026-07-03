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

let redirect_stderr = " 2>/dev/null"


let read_ocaml_config () =
  let conf = Printf.ksprintf Shell.get_command_output "ocamlc -config" in
  let re = Str.regexp ": " in
  List.map (fun l -> match Str.split re l with [n;v] -> n, v | [n] -> n, "" | _ -> assert false) conf

let cache = read_ocaml_config()

let get name = List.assoc name cache

let is_mingw = try get "system" = "mingw" with Not_found -> false

let putenv_ocamllib () = Sys.command "unset OCAMLLIB" |> ignore

let find_best_compiler compilers =
  try
    Some (List.find begin fun comp ->
        try
          let output = ksprintf Shell.get_command_output "%s -version%s" comp redirect_stderr in
          output <> []
        with _ -> false
      end compilers)
  with Not_found -> None;;

let find_tool which path =
  let commands =
    match which with
    | `BEST_OCAMLC -> ["ocamlc.opt"; "ocamlc"]
    | `BEST_OCAMLOPT -> ["ocamlopt.opt"; "ocamlopt"]
    | `BEST_OCAMLDEP -> ["ocamldep.opt"; "ocamldep"]
    | `BEST_OCAMLDOC -> ["ocamldoc.opt"; "ocamldoc"]
    | `OCAMLC -> ["ocamlc"]
    | `OCAML -> ["ocaml"]
  in
  let quote    = Fun.id in
  let path     = if path <> "" then Filename.concat path "bin" else "" in
  find_best_compiler (List.map quote (List.map (Filename.concat path) commands))

let get_home () = try Sys.getenv "OCAML_HOME" with Not_found -> ""

let expand_includes =
  let split = Str.split (Str.regexp " +") in
  fun compact ->
    if String.length compact > 0 then
      ("-I " ^ (String.concat " -I " (split compact))) else ""

(** OCaml Tools *)

let ocamlc ()   = match find_tool `BEST_OCAMLC (get_home ()) with Some x -> x | _ -> failwith "Cannot find 'ocamlc'"
let ocamlopt () = find_tool `BEST_OCAMLOPT (get_home ())
let ocamldep () = match find_tool `BEST_OCAMLDEP (get_home ()) with Some x -> x | _ -> failwith "Cannot find 'ocamldep'"
let ocamldoc () = match find_tool `BEST_OCAMLDOC (get_home ()) with Some x -> x | _ -> failwith "Cannot find 'ocamldoc'"
let ocaml ()    = match find_tool `OCAML (get_home ()) with Some x -> x | _ -> failwith "Cannot find 'ocaml'"
let ocamllib () = match Shell.get_command_output ((ocamlc()) ^ " -where") with x :: _ -> x | _ -> ""

(** OCaml Version *)

let ocaml_version ?(compiler=ocamlc()) () =
  String.concat "\n" (Shell.get_command_output (compiler ^ " -v " ^ redirect_stderr))

(** can_compile_native *)
let can_compile_native ?ocaml_home () =
  let result = ref false in
  let filename = Filename.temp_file "test_native" ".ml" in
  let ochan = open_out filename in
  begin
    try
      output_string ochan ("0");
      close_out ochan
    with _ -> (close_out ochan)
  end;
  let outname = Filename.chop_extension filename in
  let exename = outname in
  let compiler = match ocaml_home with
    | Some home -> find_tool `BEST_OCAMLOPT home
    | _ -> Some "ocamlopt"
  in
  match compiler with
  | Some compiler ->
      let cmd = sprintf "%s -o %s %s%s" compiler exename filename
          ((*if App_config.application_debug then redirect_stderr else*) "")
      in
      result := (Sys.command cmd) = 0;
      if Sys.file_exists filename then (Sys.remove filename);
      if Sys.file_exists exename then (Sys.remove exename);
      let cmi = outname ^ ".cmi" in
      if Sys.file_exists cmi then (Sys.remove cmi);
      let cmx = outname ^ ".cmx" in
      if Sys.file_exists cmx then (Sys.remove cmx);
      let obj = outname ^ ".o" in
      if Sys.file_exists obj then (Sys.remove obj);
      let obj = outname ^ ".obj" in
      if Sys.file_exists obj then (Sys.remove obj);
      if !result then begin
        let conf = String.concat "\n" (ksprintf Shell.get_command_output "%s -config" compiler) in
        let re = Str.regexp "ccomp_type: \\(.*\\)\n" in
        if Str.search_forward re conf 0 >= 0 then begin
          Some (Str.matched_group 1 conf)
        end else Some "<unknown ccomp_type>"
      end else None;
  | _ -> None
;;
