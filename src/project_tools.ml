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
open Prj
open Printf
open Target

type meta = {
  meta_target                 : Target.t;
  meta_name                   : string;
  meta_version                : string;
  meta_description            : string;
  meta_requires               : string;
  meta_archive_byte           : string list;
  meta_archive_native         : string list;
  meta_cmis                   : string list;
  meta_mlis                   : string list;
  mutable meta_subpackages    : meta list;
}

type files = {
  archives : string list;
  cmis     : string list;
  mlis     : string list;
}

let default_fl_installer_basename = "findlib.ml"
let re_quote = Utils.regexp "\""

let convert_target_type =
  function
  | Target.Executable -> Oebuild.Executable
  | Target.Library -> Oebuild.Library
  | Target.Plugin -> Oebuild.Plugin
  | Target.Pack -> Oebuild.Pack
  | Target.External -> Oebuild.External

let preamble = "\
#directory \"+unix\"
#directory \"+str\"
#load \"unix.cma\"
#load \"str.cma\"
open Printf

let unquote =
  let re = Str.regexp \"^\\\"\\\\(.*\\\\)\\\"$\" in
  fun x -> if Str.string_match re x 0 then Str.matched_group 1 x else x

let get_command_output command =
  let ch = Unix.open_process_in command in
  set_binary_mode_in ch false;
  let output = ref [] in
  try
    while true do output := (input_line ch) :: !output done;
    assert false
  with End_of_file -> begin
    ignore (Unix.close_process_in ch);
    List.rev !output
  end | e -> begin
    ignore (Unix.close_process_in ch);
    raise e
  end"

let create_meta proj ~parent tg =
  let output_name compilation tg =
    Oebuild.get_output_name
      ~compilation
      ~outkind:(convert_target_type tg.target_type)
      ~outname:(tg.outname)
      ~dontaddopt:tg.dontaddopt ()
  in
  let all_target_deps = Target.find_target_dependencies proj.targets tg in
  let findlib_target_deps =
    List.filter (fun t -> t.is_fl_package) all_target_deps
  in
  let target_deps =
    match parent with
    | Some parent -> List.filter (fun x ->
        x.id <> parent.id && not (List.mem x.id (List.map (fun y -> y.id) (Target.find_target_dependencies proj.targets parent)))) all_target_deps
    | None -> all_target_deps
  in
  let arb =
    if tg.byt
    then List.rev_append (List.map (fun d -> output_name Oebuild.Bytecode d) target_deps) [output_name Oebuild.Bytecode tg]
    else []
  in
  let arn =
    if tg.opt
    then List.rev_append (List.map (fun d -> output_name Oebuild.Native d) target_deps) [output_name Oebuild.Native tg]
    else []
  in
  let cmis =
    List.fold_left begin fun acc tg ->
      let cmoxs = (*List.filter (fun x -> not (Filename.check_suffix x ".cmi"))*) (Target.find_dependencies tg) in
      let cmis = List.map (fun x -> (Filename.chop_extension x) ^ ".cmi") cmoxs in
      cmis @ acc
    end [] (target_deps @ [tg]);
  in
  let cmis = Utils.ListExt.remove_dupl cmis in
  let mlis = List.filter (fun x -> Sys.file_exists ((Filename.chop_extension x) ^ ".mli")) cmis in
  let mlis = List.map (fun x -> (Filename.chop_extension x) ^ ".mli") mlis in
  {
    meta_target         = tg;
    meta_name           = tg.Target.name;
    meta_version        = proj.Prj.version;
    meta_description    = tg.descr;
    meta_requires       = (String.concat "," (tg.package :: (List.map (fun t -> t.Target.name) findlib_target_deps)));
    meta_archive_byte   = arb;
    meta_archive_native = arn;
    meta_cmis           = cmis;
    meta_mlis           = mlis;
    meta_subpackages    = [];
  }

let rec generate_def buf level meta =
  let indent = String.make (level * 2) ' ' in
  if level > 0 then bprintf buf "\n\n%spackage \"%s\" (\n%!" (String.make ((level - 1) * 2) ' ') meta.meta_name;
  bprintf buf "%sversion         = \"%s\"\n%!" indent meta.meta_version;
  bprintf buf "%sdescription     = \"%s\"\n%!" indent meta.meta_description;
  bprintf buf "%srequires        = \"%s\"\n%!" indent meta.meta_requires;
  bprintf buf "%sarchive(byte)   = \"%s\"\n%!" indent (String.concat "," (List.map Filename.basename meta.meta_archive_byte));
  bprintf buf "%sarchive(native) = \"%s\"%!" indent (String.concat "," (List.map Filename.basename meta.meta_archive_native));
  let (sub_archives, sub_cmis, sub_mlis) =
    List.fold_left begin fun (aa, ac, am) sp ->
      let sub = generate_def buf (level + 1) sp in
      sub.archives :: aa, sub.cmis :: ac, sub.mlis :: am
    end ([], [], []) meta.meta_subpackages
  in
  if level > 0 then bprintf buf "\n%s)%!" (String.make ((level - 1) * 2) ' ');
  {
    archives = meta.meta_archive_byte @ meta.meta_archive_native @ (List.flatten sub_archives);
    cmis     = meta.meta_cmis @ (List.flatten sub_cmis);
    mlis     = meta.meta_mlis @ (List.flatten sub_mlis);
  }

let generate_meta outchan meta =
  let buf = Buffer.create 100 in
  let files = generate_def buf 0 meta in
  fprintf outchan "  (\x2A\x2A %s \x2A)\n  %S,\"\\\n%s\",\n  [\n    %s\n  ], [\n    %s\n  ], [\n    %s\n  ];\n%!"
    meta.meta_name meta.meta_name
    (Str.global_replace re_quote "\\\"" (Buffer.contents buf))
    (String.concat ";\n    " (List.map (sprintf "%S") (List.sort compare (Utils.ListExt.remove_dupl files.archives))))
    (String.concat ";\n    " (List.map (sprintf "%S") (List.sort compare (Utils.ListExt.remove_dupl files.cmis))))
    (String.concat ";\n    " (List.map (sprintf "%S") (List.sort compare (Utils.ListExt.remove_dupl files.mlis))))

let generate_fl_installer proj outchan =
  let processed = ref [] in
  let targets = ref proj.targets in
  let rec generate ~parent acc = function
    | tg :: tl ->
        let metas =
          if tg.is_fl_package && not (List.mem tg.id !processed) then begin
            let meta = create_meta proj ~parent tg in
            meta.meta_subpackages <- generate ~parent:(Some tg) [] tg.sub_targets;
            processed := tg.id :: !processed;
            meta :: acc
          end else acc
        in
        generate ~parent:None metas tl
    | [] -> acc
  in
  let metas = List.rev (generate ~parent:None [] !targets) in
  fprintf outchan "(\x2A\n\n  This file is automatically generated by %s %s, do not edit.\n\n\x2A)\n\n" About.program_name About.version;
  fprintf outchan "%s\n\n" preamble;
  fprintf outchan "open Printf\n\n";
  fprintf outchan "let packages = [\n";
  List.iter (generate_meta outchan) metas;
  fprintf outchan "]\n\n";
  fprintf outchan "let is_mingw = List.exists ((=) \"system: mingw\") (get_command_output \"ocamlc -config\")\n\n";
  fprintf outchan "let _ = \n";
  fprintf outchan "  if Array.length Sys.argv < 2 then failwith \"Invalid parameters\";\n";
  fprintf outchan "  let cwd = Sys.getcwd() in\n";
  fprintf outchan "  Sys.chdir \"%s\";\n%!" Prj.default_dir_src;
  fprintf outchan "  let lib_ext = \".a\" in\n";
  (*  fprintf outchan "  printf \"Current working directory: %%s\\n%%!\" (Sys.getcwd());\n";*)
  fprintf outchan "  List.iter begin fun (name, defs, archives, (cmis : string list), (mlis : string list)) ->\n";
  fprintf outchan "    let cmxas = List.filter (fun x -> Filename.check_suffix x \".cmxa\") archives in\n";
  fprintf outchan "    let libs = String.concat \" \" (List.map (fun x -> (Filename.chop_extension x) ^ lib_ext) cmxas) in\n";
  fprintf outchan "    let arcs = String.concat \" \" archives in\n";
  fprintf outchan "    let cmis = String.concat \" \" cmis in\n";
  fprintf outchan "    let mlis = String.concat \" \" mlis in\n";
  fprintf outchan "    let _META = \"META\" in\n";
  fprintf outchan "    if Sys.file_exists _META then failwith \"Cannot write META file: file exists\";\n";
  fprintf outchan "    let chan = open_out_bin _META in\n";
  fprintf outchan "    try\n";
  fprintf outchan "      let sudo = if false || List.mem Sys.argv.(1) [\"print\"] then \"\" else \"sudo -E \" in\n";
  fprintf outchan "      let cmd = ref [] in\n";
  fprintf outchan "      if not (List.mem Sys.argv.(1) [\"install\"; \"uninstall\"; \"reinstall\"; \"print\"]) then failwith \"Invalid parameters\";\n";

  fprintf outchan "      output_string chan defs;\n";
  fprintf outchan "      flush chan;\n";
  fprintf outchan "      if List.mem Sys.argv.(1) [\"uninstall\"; \"reinstall\"] then cmd := (sprintf \"%%socamlfind remove %%s\" sudo name) :: !cmd;\n";

  fprintf outchan "      if List.mem Sys.argv.(1) [\"install\"; \"reinstall\"; \"print\"] then \n";
  fprintf outchan "        cmd := (sprintf \"%%socamlfind install %%s %%s %%s %%s %%s %%s\" sudo name _META arcs libs cmis mlis) :: !cmd;\n";

  fprintf outchan "      assert (!cmd <> []);\n";
  fprintf outchan "      let cmd = String.concat \" && \" (List.rev !cmd) in\n";

  fprintf outchan "      if List.mem Sys.argv.(1) [\"print\"] then printf \"\\n##### %%s.%%s #####\\n\\n%%s\\n\\n# %%s\\n%%!\" _META name defs cmd;\n";

  fprintf outchan "      let exit_code = if Sys.argv.(1) = \"print\" then 0 else Sys.command cmd in\n";
  fprintf outchan "      if exit_code <> 0 then eprintf \"Error: command %%s exited with code %%d\\n%%!\" cmd exit_code;\n";
  fprintf outchan "      if Sys.file_exists _META then (close_out_noerr chan; Sys.remove _META)\n";
  fprintf outchan "    with ex -> begin\n";
  fprintf outchan "      if Sys.file_exists _META then (close_out_noerr chan; Sys.remove _META)\n";
  fprintf outchan "    end\n";
  fprintf outchan "  end packages;\n";
  fprintf outchan "  Sys.chdir cwd;\n";
  ()

let findlib_target_name = "FINDLIB-TOOLS"

let rm_tools tg tools =
  tg.external_tasks <- List.filter (fun et -> not (List.exists (fun ft -> ft.Task.et_name = et.Task.et_name) tools)) tg.external_tasks

(** write_findlib_tools *)
let write_findlib_tools proj =
  let has_fl_packages = List.exists (fun tg -> tg.Target.is_fl_package) proj.targets in
  let dirname = proj.root // Prj.default_dir_tools in
  let filename = dirname // default_fl_installer_basename in
  let tools = List.find_opt (fun tg -> tg.Target.name = findlib_target_name && tg.target_type = Target.External) proj.targets in
  let scriptname = Prj.default_dir_tools ^ "/" ^ default_fl_installer_basename in
  let findlib_tools = [
    Task.create ~name:"install" ~env:[] ~dir:".." ~cmd:"ocaml" ~args:[true, scriptname; true, "install"] ~readonly:true ~run_in_script:false ();
    Task.create ~name:"uninstall" ~env:[] ~dir:".." ~cmd:"ocaml" ~args:[true, scriptname; true, "uninstall"] ~readonly:true ~run_in_script:false ();
    Task.create ~name:"reinstall" ~env:[] ~dir:".." ~cmd:"ocaml" ~args:[true, scriptname; true, "reinstall"] ~readonly:true ~run_in_script:false ();
    Task.create ~name:"print" ~env:[] ~dir:".." ~cmd:"ocaml" ~args:[true, scriptname; true, "print"] ~readonly:true ~run_in_script:false ();
  ] in
  if has_fl_packages then begin
    if not (Sys.file_exists dirname) then (Unix.mkdir dirname 0o777);
    let outchan = open_out_bin filename in
    try
      generate_fl_installer proj outchan;
      let tg =
        match tools with
        | None ->
            let id = (List.fold_left (fun cand t -> max t.id cand) 0 proj.targets) + 1 in
            let tg = Target.create ~id ~name:findlib_target_name in
            tg.target_type <- External;
            tg.readonly <- true;
            proj.targets <- proj.targets @ [tg];
            tg
        | Some tg -> tg
      in
      rm_tools tg findlib_tools;
      tg.external_tasks <- tg.external_tasks @ findlib_tools;
      close_out_noerr outchan;
    with ex ->
      close_out_noerr outchan;
      raise ex
  end else begin
    if Sys.file_exists filename then Sys.remove filename;
    match tools with
    | Some tg ->
        rm_tools tg findlib_tools;
        if tg.external_tasks = [] then proj.targets <- List.filter (fun t -> t.id <> tg.id) proj.targets
    | _ -> ()
  end

(** write *)
let write proj =
  write_findlib_tools proj
