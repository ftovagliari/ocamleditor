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
#use "scripting.ml"

open Printf

let mkrelease () =
  let redirect_stdout = if Sys.win32 then " 1>NUL" else " 1>/dev/null" in
  (*let redirect_stderr = if Sys.win32 then " 2>NUL" else " 2>/dev/null" in*)
  if is_win32 then begin
    pushd "..";
    kprintf run "ocaml build.ml -verbose 0 build oebuild oeproc";
    pushd "src";
    kprintf run "ocaml -I +unix -I +str str.cma unix.cma generate_oebuild_script.ml %s" redirect_stdout;
    popd();
    kprintf run "ocaml build.ml distclean %s" redirect_stdout;
    let name = Filename.basename (Sys.getcwd ()) in
    let version = match get_lines_from_file ~filename:"VERSION" [1] with (_, x) :: [] -> x | _ -> assert false in
    let package = if Str.last_chars name (String.length version) = version then name else sprintf "%s-%s" name version in
    let path = Filename.temp_dir_name // package in
    pushd "..";
    let suffix = ".tar.gz" in
    kprintf remove_file "%s%s" package suffix;
    kprintf run "mkdir %s" path;
    kprintf run "cp -r %s/src %s/icons %s/tools %s" name name name path;
    kprintf run "cp %s/README %s/NEWS %s/COPYING %s/%s.project %s/ocamleditor.nsi %s/build.ml %s/header %s/VERSION %s"
      name name name name name name name name name path;
    let cwd = Sys.getcwd() in
    pushd (Filename.dirname path);
    kprintf run "tar -cf %s.tar %s" (cwd // package) package;
    popd();
    kprintf run "gzip -c %s.tar > %s%s" package package suffix;
    kprintf Sys.remove "%s.tar" package;
    kprintf run "%s %s" rmr path;
    let outname = sprintf "%s%s\n%!" ((Sys.getcwd()) // package) suffix in
    Printf.printf "\n%s\n%!" outname;
    popd();
    kprintf run "ocaml build.ml -verbose 0 build ocamleditor %s" redirect_stdout;
  end;;

let _ = main ~default_target:mkrelease ~options:[] ()
