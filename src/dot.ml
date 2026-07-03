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
open Utils

let round f =
  int_of_float (if f < 0. then f -. 0.5 else f +. 0.5)

let ocamldoc = Ocaml_config.ocamldoc ()

(** mk_ocamldoc_cmd *)
let mk_ocamldoc_cmd ~project ?(dot_include_all=false) ?(dot_reduce=true) ?(dot_types=false) ~outfile sourcefiles =
  let search_path = Project.get_search_path_i_format project in
  ocamldoc, Array.concat [
    [| "-dot";
       "-I";
       "+threads";
    |];
    (Array.of_list (Utils.split " +" search_path));
    [|
      (if dot_include_all then "-dot-include-all" else "");
      (if dot_reduce then "-dot-reduce" else "");
      (if dot_types then "-dot-types" else "");
      "-o";
      outfile
    |];
    (Array.of_list sourcefiles);
  ];;

(** mk_dot_cmd *)
let mk_dot_cmd ~outlang ~outfile ?(label="") ?(rotate=0.) filename =
  "dot", [|
    ("-T" ^ outlang);
    "-o";
    outfile;
    "-Glabel=\"" ^ label ^ "\"";
    (sprintf "-Grotate=%.2f" rotate);
    Oe_config.dot_attributes;
    filename
  |]
;;

(** draw *)
let draw ~project ~filename ?dot_include_all ?dot_types () =
  let module Device = (val (module Dot_viewer_pdf.PDF) : Dot_viewer_types.DEVICE) in
  let outlang       = Device.lang in
  let basename      = Filename.basename filename in
  let label         = sprintf "Dependency graph for \xC2\xAB%s\xC2\xBB" basename in
  let prefix = Filename.chop_extension basename in
  let search_path = "-I " ^ String.concat " -I " (Project.get_search_path_local project) in
  let dependencies =
    Oebuild_dep.ocamldep_recursive ~search_path [filename] |> Oebuild_dep.sort_dependencies |> List.map Oebuild_util.replace_extension_to_ml
  in
  let dependants =
    let path = [Filename.dirname filename] in
    let modname = Utils.modname_of_path filename in
    Oebuild_dep.find_dependants ~path ~modname
  in
  let sourcefiles   = dependencies @ dependants in
  let dotfile       = Filename.temp_file prefix ".dot" in
  let outfile       = dotfile ^ "." ^ outlang in
  (*  *)
  let ocamldoc_cmd, oargs  = mk_ocamldoc_cmd ?dot_include_all ?dot_types ~project ~outfile:dotfile sourcefiles in
  let dot_cmd, dargs       = mk_dot_cmd ~outlang ~outfile ~label dotfile in
  let activity_name = "Generating module dependency graph, please wait..." in
  Activity.add Activity.Other activity_name;
  Spawn.async ~continue_with:begin fun _ ->
    let modname = Utils.modname_of_path filename in
    let re = ksprintf Str.regexp "\"%s\" \\[.*color=\\(.+\\).*\\]" modname in
    (*let re1 = Str.regexp "\\(\".*\"\\) \\[style=filled, color=darkturquoise\\];$" in*)
    map_file_lines dotfile begin fun ~lnum ~line ->
      (*if Str.string_match re line 0 then (sprintf "\"%s\" [style=filled, color=black, fontcolor=white];\n" modname)*)
      if Str.string_match re line 0 then (sprintf "\"%s\" [style=filled, color=black, fillcolor=%s, fontsize=28, shape=box];\n" modname (Str.matched_group 1 line))
      (*else if Str.string_match re1 line 0 then (sprintf "%s;\n" (Str.matched_group 1 line))*)
      else line
    end;
    Spawn.async ~continue_with:begin fun _ ->
      if Sys.file_exists dotfile then (Sys.remove dotfile);
      Device.draw ~filename:outfile;
      Activity.remove activity_name;
    end dot_cmd dargs |> ignore;
  end ocamldoc_cmd oargs |> ignore
