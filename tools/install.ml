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


#cd "tools"
    #directory "."
               #use "scripting.ml"

open Printf

let ocaml_config = get_command_output "ocamlc -config"
let ext      = if is_win32 then ".exe" else ""
let gmisclib = ref false
let nsis     = ref false
let is_mingw = List.exists ((=) "system: mingw") ocaml_config
let prefix   = ref (
    if is_mingw then begin
      let re = Str.regexp ": " in
      let conf = List.map (fun l -> match Str.split re l with [n;v] -> n, v | [n] -> n, "" | _ -> assert false) ocaml_config in
      let lib = List.assoc "standard_library" conf in
      Filename.dirname lib
    end else "/usr/local")

let install () =
  if !gmisclib then begin
    let libname = "gmisclib" in
    let cmas = ["../" ^ libname] in
    let ar = String.concat " " (List.map (fun x -> x ^ (if Sys.os_type = "Win32" && not is_mingw then ".lib" else ".a")) cmas) in
    let find ext =
      let files = Array.to_list (Sys.readdir ".") in
      let files = List.filter (fun x -> Filename.check_suffix x ext) files in
      String.concat " " files
    in
    let cmas = List.map (fun x -> [x ^ ".cma"; x ^ ".cmxa"]) cmas in
    let cmas = String.concat " " (List.flatten cmas) in
    (*  *)
    pushd libname;
    ignore (ksprintf Sys.command "ocamlfind remove %s" libname);
    ignore (ksprintf Sys.command "ocamlfind install %s META %s %s %s %s" libname cmas ar (find ".cmi") (find ".mli"));
    popd();
  end else if !nsis then begin
    let exit_code = ksprintf Sys.command "\"%s\" ..\\ocamleditor.nsi" (Filename.quote "%ProgramFiles(x86)%\\NSIS\\makensis") in
    match exit_code with
    | 0 ->
        let version = match get_lines_from_file ~filename:"../VERSION" [1] with (_, x) :: [] -> x | _ -> assert false in
        let cmd = sprintf "..\\ocamleditor-%s" version in
        ignore (Sys.command cmd)
    | _ -> prerr_endline "This script is not available under Windows.
To install OCamlEditor, please use the included ocamleditor.nsi script.
You will need the free NSIS install system (http://nsis.sourceforge.net).";
  end else begin
    if not (Sys.file_exists !prefix) then failwith ("Path " ^ !prefix ^ " doesn't exist");
    let exe, cpr, cp = if Sys.win32 then ".exe", "XCOPY /S/Y", "XCOPY /Y" else "", "cp -vr", "cp -v" in
    let icons_light = sprintf "%s/share/ocamleditor/icons/light" !prefix in
    let icons_dark = sprintf "%s/share/ocamleditor/icons/dark" !prefix in
    let icons_svg = sprintf "%s/share/ocamleditor/icons/svg" !prefix in
    let fonts = sprintf "%s/share/ocamleditor/fonts" !prefix in
    mkdir_p icons_light;
    mkdir_p icons_dark;
    mkdir_p icons_svg;
    mkdir_p fonts;
    sys_command [cpr; !!"icons/light/*"; !!icons_light];
    sys_command [cpr; !!"icons/dark/*"; !!icons_dark];
    sys_command [cpr; !!"icons/svg/*"; !!icons_svg];
    sys_command [cpr; !!"fonts/*"; !!fonts];
    let bin = sprintf "%s/bin" !prefix in
    mkdir_p bin;
    let filename = if Sys.file_exists ("ocamleditor.opt" ^ exe) then ("ocamleditor.opt" ^ exe) else ("ocamleditor" ^ exe) in
    if Sys.win32 then begin
      let fn = !!(bin^"/ocamleditor"^exe) in
      if Sys.file_exists fn then Sys.remove fn;
      sys_command [cp; filename; !!bin];
      Sys.rename (!!(bin^"/"^filename)) (!!(bin^"/ocamleditor"^exe))
    end else (sys_command [cp; filename; !!(bin^"/ocamleditor"^exe)]);
    let filename = if Sys.file_exists ("oebuild/oebuild.opt" ^ exe) then ("oebuild/oebuild.opt" ^ exe) else ("oebuild/oebuild" ^ exe) in
    sys_command [cp; !!filename; !!bin];
    if Sys.win32 && is_mingw then begin
      let basename = "ocamleditor-mingw.bat" in
      sys_command [cp; basename; !!bin];
      Printf.eprintf "\n\n  Please edit\n\n      %s\\%s\n\n  to match your system configuration.\n\n%!" (!!bin) basename;
    end;
    let filename = "ocamleditor_launch" ^ exe in
    if Sys.file_exists filename then sys_command [cp; filename; !!bin];
  end;;

let _ = main ~dir:"../src" ~default_target:install ~options:[
    "-prefix",   Set_string prefix,   (sprintf " Installation prefix (default is %s)" !prefix);
    "-gmisclib", Set gmisclib,        (sprintf " Install gmisclib");
    "-nsis",     Set nsis,            (sprintf " Create a Win32 installer with NSIS");
  ] ()
