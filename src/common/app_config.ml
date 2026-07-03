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

(** Alias for [Filename.concat] *)
let (//) = Filename.concat

(** Alias for [Filename.dirname] *)
let (!!) = Filename.dirname

let split sep str =
  let str = if str.[0] = sep then String.sub str 1 (String.length str - 1) else str in
  let str = if str.[(String.length str - 1)] = sep then String.sub str 0 (String.length str - 1) else str in
  let parts = ref [] in
  let i = ref (String.length str) in
  let j = ref !i in
  let str = ref str in
  while !i > 0 do
    decr i;
    if !str.[!i] = sep then begin
      parts := (String.sub !str (!i + 1) (!j - !i - 1)) :: !parts;
      str := String.sub !str 0 !i;
      j := !i;
    end else if !i = 0 then begin
      parts := (String.sub !str 0 !j) :: !parts;
    end
  done;
  !parts;;
(*
split ',' "OCAMLEDITORPARAM=debug=2,record_backtrace=1";;
split ',' ",OCAMLEDITORPARAM=debug=2,record_backtrace=1";;
split ',' ",OCAMLEDITORPARAM=debug=2,record_backtrace=1,";;
split ',' ",OCAMLEDITORPARAM=debug=2,,record_backtrace=1,,";;
#load "str.cma";;
Str.split (Str.regexp ",") "OCAMLEDITORPARAM=debug=2,record_backtrace=1";;
Str.split (Str.regexp ",") ",OCAMLEDITORPARAM=debug=2,record_backtrace=1";;
Str.split (Str.regexp ",") ",OCAMLEDITORPARAM=debug=2,record_backtrace=1,";;
Str.split (Str.regexp ",") ",OCAMLEDITORPARAM=debug=2,,record_backtrace=1,,";;
*)

let application_param =
  try
    List.fold_left begin fun acc x ->
      match split '=' x with
      | n :: v :: [] -> (n, v) :: acc
      | n :: [] -> (n, "") :: acc
      | _ -> acc
    end [] (split ',' (Sys.getenv "OCAMLEDITORPARAM"))
  with Not_found -> [];;

let application_debug = try (List.assoc "debug" application_param) = "2" with Not_found -> false;;

(** Directories *)
let user_home =
  try Sys.getenv "HOME" with Not_found ->
    (try (Sys.getenv "HOMEDRIVE") // (Sys.getenv "HOMEPATH")
     with Not_found -> failwith "Please set your HOME environment variable.")

let ocamleditor_user_home =
  let dirname =
    match Ocaml_config.is_mingw with
    | true when application_debug -> ".ocamleditor.mingw"
    | true -> ".ocamleditor.test.mingw"
    | false when application_debug -> ".ocamleditor.test"
    | false -> ".ocamleditor"
  in
  user_home // dirname

let ensure_ocamleditor_user_home () =
  if not (Sys.file_exists ocamleditor_user_home) then (Unix.mkdir ocamleditor_user_home 509)

let launcher_filename = ocamleditor_user_home // "launcher.list"

let get_application_dir name =
  let exe_dir = !! Sys.executable_name in
  let prefix = if exe_dir = Sys.getcwd() then exe_dir else !! exe_dir in
  let path = prefix // name in
  if Sys.file_exists path && Sys.is_directory path then path
  else prefix // "share" // "ocamleditor" // name

let application_icons = get_application_dir "icons"

let application_fonts = get_application_dir "fonts"

let application_plugins = get_application_dir "plugins"

let find_best ?(param="--help") prog =
  let redirect_stderr = " 2>/dev/null" in
  try
    List.find begin fun comp ->
      let ok =
        try
          let cmd = sprintf "%s %s%s" (Filename.quote comp) param redirect_stderr in
          if application_debug then (printf "Checking for %s... %!" cmd);
          Shell.get_command_output cmd |> ignore;
          true
        with _ -> false
      in
      if application_debug then (printf "%b\n%!" ok);
      ok
    end prog
  with Not_found ->
    ksprintf failwith "Cannot find: %s" (String.concat ", " prog)

let find_command name =
  let basename = name in
  let path = (!! Sys.executable_name) // basename in
  if Sys.file_exists path && not (Sys.is_directory path) then path
  else
    let path = (!! Sys.executable_name) // (if Filename.check_suffix name ".opt" then Filename.chop_extension name else name) // basename in
    if Sys.file_exists path then path
    else basename

let get_oebuild_command () =
  find_best [
    find_command "oebuild.opt";
    find_command "oebuild";
  ]
