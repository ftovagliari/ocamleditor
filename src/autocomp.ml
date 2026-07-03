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

let re_tmp = Utils.regexp (Printf.sprintf "File \"..[\\/]%s[\\/]" Prj.default_dir_tmp)
let (!!) = Filename.quote
let (//) = Filename.concat

let cached_ppx_flags = Hashtbl.create 21

(** Returns the ppx flags for a package. The results are cached to save some
    ocamfind invocations.
*)
let ppx_flags_package pkg =
  match Hashtbl.find cached_ppx_flags pkg with
  | flags -> flags
  | exception Not_found ->
      let flags = Shell.get_command_output (Printf.sprintf "ocamlfind printppx %s" pkg) in
      let ()    = Hashtbl.replace cached_ppx_flags pkg flags in
      flags

(** Gets the ppx flags for the packages used in current project to be used for
    auto-compilation.

    The order of ppx invokations is not specified, and may vary from run to run.
    Also if target A uses a ppx rewriter but targets B and C do not, the ppx
    rewriter will be used even for files that do not need it.

    It can get more tricky when different targets use different rewriters,
    since all will be used. This can present a problem, especially, if the ppx
    syntaxes overlap.

    Such use case is not currently supported.
*)
let ppx_flags project =
  let packages = List.concat_map (fun t -> String.split_on_char ',' t.Target.package) project.Prj.targets in
  let packages = List.fold_left (fun acc p -> Hashtbl.replace acc p true; acc) (Hashtbl.create 12) packages in
  let packages = Hashtbl.fold (fun p _ acc -> p :: acc) packages [] in
  let outputs  = List.concat_map ppx_flags_package packages in
  let flags    = List.concat_map Oebuild_util.split_args outputs in
  Array.of_list flags

(** Get a list of include flage for the case when the compilation directory is
    the $project_dir/.tmp, instdead of the default one $project_dir/src.

    for local libs we prefix them "../src" and we also add the "../src" to
    search path because it is the default one, but is not visible from ./tmp

    The global libs are left unchanged
*)
let include_flags project =
  let prefix_local filename =
    if Filename.is_relative filename && filename.[0] <> '+' then
      ".." // Prj.default_dir_src // filename
    else
      filename
  in
  let search_path = List.map prefix_local project.Prj.search_path in
  let include_flags = List.fold_left (fun acc path -> "-I" :: path :: acc)
      ["-I"; ".." // Prj.default_dir_src]
      search_path
  in
  Array.of_list include_flags

(** Replaces build artifacts in the project source directory with auto-compilation generated
    artifacts from the temporary directory.

    TODO: The function should be enhanced to only replace artifacts when the interface
    has actually changed.
*)
let replace_compiler_artifact ~project tmp_dir relpath ext =
  let file_ext = relpath ^ ext in
  let tmp_ext = tmp_dir // file_ext in
  if Sys.file_exists tmp_ext then begin
    let src_ext = Project.path_src project // file_ext in
    try
      if Sys.file_exists src_ext then Sys.remove src_ext;
      Sys.rename tmp_ext src_ext
    with Sys_error _ as ex ->
      Printf.eprintf "File \"autocomp.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());
  end

(** Compile the current buffer using the flags from all project targets.

    More specifically, `include` and `ppx` flags from all targets are included,
    as well extra debug flags.
*)
let compile_buffer ~project ~editor ~page ?(join=false) () =
  let working_directory = ".." // Prj.default_dir_tmp in
  let activity_name = "Compiling " ^ page#get_filename ^ "..." in
  Activity.add Activity.Compile_buffer activity_name;
  try
    match page#buffer#save_buffer ?filename:None () with
    | _, None -> ()
    | _, Some (_, relpath) ->
        (* Compile *)
        let args =
          Array.concat [
            project.Prj.autocomp_dflags;
            (Array.of_list (Utils.split " +" project.Prj.autocomp_cflags));
            [| "-error-style"; "short" |];
            (include_flags project);
            (ppx_flags project);
            [|relpath|];
          ]
        in
        let compiler_output = Buffer.create 101 in
        let process_err stderr =
          let line = input_line stderr in
          let line = Str.replace_first re_tmp "File \"" line in
          Buffer.add_string compiler_output (Utils.rtrim line);
          Buffer.add_char compiler_output '\n';
        in
        let at_exit _ =
          let modname = Filename.chop_extension relpath in
          replace_compiler_artifact ~project working_directory modname ".cmi";
          replace_compiler_artifact ~project working_directory modname ".cmt";

          let errors = Error.parse_string (Buffer.contents compiler_output) in
          GtkThread2.async page#error_indication#apply_tag errors;
          (* Outline *)
          let no_errors = errors.Oe.er_errors = [] in
          if editor#show_outline then begin end;
          Activity.remove activity_name;
        in
        let process_err = Spawn.loop process_err in
        if join then
          let result = Spawn.sync ~working_directory ~process_err project.Prj.autocomp_compiler args in
          begin
            match result with
            | `SUCCESS (Unix.WEXITED code)
            | `SUCCESS (Unix.WSIGNALED code)
            | `SUCCESS (Unix.WSTOPPED code) ->
                if code <> 0 then
                  Printf.eprintf "File \"autocomp.ml\": %s %s -> exited with code %d\n%!"
                    project.Prj.autocomp_compiler (args |> Array.to_list |> String.concat " ") code
            | `ERROR ex ->
                Printf.eprintf "File \"autocomp.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());
          end;
          at_exit ()
        else
          Spawn.async ~working_directory ~at_exit ~process_err project.Prj.autocomp_compiler args |> ignore
  with ex ->
    Printf.eprintf "%s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace ())
