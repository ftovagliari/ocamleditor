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


#directory "+unix";;
#directory "+str";;
#load "unix.cma";;
#load "str.cma";;
#use "../src/common/shell.ml";;
#use "../src/common/utils.ml";;

exception Script_error of string * int

let (//) = Filename.concat
let is_win32 = Sys.os_type = "Win32" (* deprecated, use Sys.win32 *)
let path_concat str =
  if Sys.win32 then List.fold_left Filename.concat "" (Str.split (Str.regexp "/") str)
  else str;;
let (!!) = path_concat

(** run *)
let run cmd =
  printf "%s\n%!" cmd;
  let exit_code = Sys.command cmd in
  if exit_code <> 0 then (raise (Script_error (cmd, exit_code)))

(* sys_command *)
let sys_command cmd =
  let cmd = String.concat " " cmd in
  printf "%s\n%!" cmd;
  let exit_code = Sys.command cmd in
  if exit_code <> 0 then (raise (Script_error (cmd, exit_code)))

(** mkdir *)
let mkdir d = printf "mkdir %s\n%!" d; Unix.mkdir d 0o755
let rec mkdir_p d =
  if not (Sys.file_exists d) then begin
    mkdir_p (Filename.dirname d);
    mkdir d
  end;;

(** Copy file *)
let copy_file ic oc =
  let buff = Bytes.create 0x1000 in
  let rec copy () =
    let n = input ic buff 0 0x1000 in
    if n = 0 then () else (output oc buff 0 n; copy())
  in copy()

let cp ?(echo=false) src dst =
  if echo then (printf "%s -> %s\n%!" src dst);
  let ic = open_in_bin src in
  let oc = open_out_bin dst in
  let finally () = close_out oc; close_in ic in
  try copy_file ic oc; finally() with ex -> (finally(); raise ex)

(** Remove file *)
let remove_file ?(verbose=false) filename =
  if Sys.file_exists filename && not (Sys.is_directory filename)
  then (if verbose then (print_endline filename); Sys.remove filename)

let rm = if is_win32 then "DEL /F /Q" else "rm -f"
let rmr = if is_win32 then "RMDIR /Q /S" else "rm -fr"

(** substitute *)
let substitute ~filename ?(regexp=false) repl =
  let perm = (Unix.stat filename).Unix.st_perm in
  let ichan = open_in_bin filename in
  let tmp, ochan = Filename.open_temp_file (Filename.basename filename) "" in
  Stdlib.set_binary_mode_out ochan true;
  let finally () =
    close_in_noerr ichan;
    close_out_noerr ochan;
    let new_filename = filename in
    remove_file new_filename;
    cp tmp new_filename;
    remove_file tmp;
    Unix.chmod new_filename perm;
  in
  begin
    try
      while true do
        let line = input_line ichan in
        let line = replace_all ~regexp repl line in
        fprintf ochan "%s\n" line;
      done;
      assert false;
    with
    | End_of_file -> finally()
    | ex -> finally(); raise ex
  end;;

(** get_command_output *)
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
    end

(** Main *)
open Arg

let main ?dir ?(targets=[]) ?default_target ~options () =
  try
    let mktarget f x =
      fun () ->
      (match dir with Some dir -> pushd dir | _ -> ());
      let finally () = match dir with Some _ -> popd() | _ -> () in
      (try f x with ex -> (finally(); raise ex));
      finally();
    in
    let target_func = ref None in
    let target f x = target_func := Some (mktarget f x) in
    let speclist =
      List.map begin fun (option, func, descr) ->
        option, Unit (target func), descr
      end targets
    in
    let speclist = options @ speclist in
    let speclist = Arg.align speclist in
    let command_name = Filename.basename Sys.argv.(0) in
    let help_message = sprintf "\nUsage:\n  ocaml %s [options]\n\nOptions:" command_name in
    Arg.parse speclist (fun x -> raise (Bad x)) help_message;
    begin
      match !target_func with
      | Some f -> f ();
      | None when default_target <> None -> (match default_target with Some f -> (mktarget f ()) () | _ -> assert false)
      | _ -> Arg.usage speclist help_message
    end;
    let code = 0 in
    Printf.printf "Exit code %d\n%!" code;
    exit code;
  with Script_error _ ->
    let code = 0 in
    Printf.printf "Exit code %d\n%!" code;
    exit code;


