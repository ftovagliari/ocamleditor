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
open Unix
open Str
open Printf

(*#load "unix.cma"
  #load "str.cma"
  #directory "C:\\ocaml\\devel\\ocamleditor\\src\\common"
  #load "utils.cmo"

  exact_match ~pat:(unix_regexp "*.{ml,mli,mll,mly,bat,cmd,txt,css,js}") "a.mli";;*)


module Regexp =
struct

  (**** Memoized rexgexp *)

  let (~!) = Utils.Memo.create Str.regexp;;

  (* Convert Windows-style directory separator '\' to caml-style '/' *)
  let caml_dir path = path

  let rec fixpoint ~f v =
    let v' = f v in
    if v = v' then v else fixpoint ~f v';;

  let unix_regexp s =
    let re = ~! "\\*\\.{\\(.*\\)}" in
    let s =
      if Str.string_match re s 0 then begin
        let elements = Str.split (!~ " *, *") (Str.matched_group 1 s) in
        let elements = List.sort (fun x y -> compare y x) elements in
        "*.{" ^ String.concat "," elements ^ "}"
      end else s
    in
    let s = Str.global_replace ~!"\\." "[.]" s in
    let s = Str.global_replace ~!"\\*" ".*" s in
    let s = Str.global_replace ~!"\\?" ".?" s in
    let s =
      fixpoint s
        ~f:(Str.replace_first ~!"\\({.*\\),\\(.*}\\)" "\\1\\|\\2") in
    let s =
      Str.global_replace ~!"{\\(.*\\)}" "\\(\\1\\)" s in
    Str.regexp s;;

  let exact_match ~pat s =
    Str.string_match pat s 0 && Str.match_end () = String.length s;;

  let mtime filename = (Unix.stat filename).st_mtime

  let format_time time =
    let t = Unix.localtime time in
    Printf.sprintf "%i/%i/%i %02i:%02i" t.tm_mday (t.tm_mon + 1) (t.tm_year + 1900)
      t.tm_hour t.tm_min

end;;

let is_directory name =
  try (Unix.stat name).Unix.st_kind = Unix.S_DIR with _ -> false;;

let get_directories_in_files path =
  (*  let path = if is_directory path then path else Filename.dirname path in*)
  let entries = Array.to_list (Sys.readdir path) in
  List.filter (fun e -> is_directory (Filename.concat path e)) entries;;

let get_files_in_directory path =
  (*  let path = if is_directory path then path else Filename.dirname path in*)
  let entries = Array.to_list (Sys.readdir path) in
  List.filter (fun e -> not (is_directory (Filename.concat path e))) entries

let get_directories_and_files ?(directories_first=false) path =
  let entries = Array.to_list (Sys.readdir path) in
  if directories_first then
    List.sort (fun e1 _ -> if is_directory (Filename.concat path e1) then -1 else 0) entries
  else entries;;

let ls ~dir ~pattern =
  let files = get_directories_and_files dir in
  let regexp = Regexp.unix_regexp pattern in
  List.filter (Regexp.exact_match ~pat:regexp) files;;

let lsd ~dir ~pattern =
  let directories = get_directories_in_files dir in
  let regexp = Regexp.unix_regexp pattern in
  List.filter (Regexp.exact_match ~pat:regexp) directories

(** read *)
let read filename =
  let ch = open_in_bin filename in
  lazy begin
    let len = in_channel_length ch in
    let buf = Buffer.create len in
    Buffer.add_channel buf ch len;
    buf
  end /*finally*/ lazy (close_in ch)

(** write *)
let write filename text =
  let ch = open_out_bin filename in
  lazy begin
    output_string ch text;
  end /*finally*/ lazy (close_out ch)

(** Copy file *)
let copy_file ic oc =
  let buff = Bytes.create 0x1000 in
  let rec copy () =
    let n = input ic buff 0 0x1000 in
    if n = 0 then () else (output oc buff 0 n; copy())
  in copy()

let cp ?(echo=false) src dst =
  let ic = open_in_bin src in
  let oc = open_out_bin dst in
  if echo then (printf "%s -> %s\n%!" src dst);
  let finally () = close_out oc; close_in ic in
  try copy_file ic oc; finally() with ex -> (finally(); raise ex)

(** readdirs *)
let rec readdirs ?(links=false) ?(recursive=true) filter path =
  if not links && (Unix.lstat path).Unix.st_kind = Unix.S_LNK then []
  else if Sys.is_directory path then begin
    try
      let filenames = Array.to_list (Sys.readdir path) in
      let filenames = List.map (Filename.concat path) filenames in
      let dirnames, filenames = List.partition Sys.is_directory filenames in
      let filenames = match filter with
        | Some f -> List.filter f filenames
        | _ -> filenames
      in
      if recursive then filenames @ List.flatten (List.map (readdirs filter) dirnames)
      else filenames
    with Sys_error _ -> []
  end else [path];;

(** readtree *)
let rec readtree path =
  try
    if Sys.is_directory path && (Unix.lstat path).Unix.st_kind <> Unix.S_LNK then begin
      let basenames = Sys.readdir path in
      path :: (Array.fold_left begin fun acc x ->
          if String.unsafe_get x 0 <> '.' then begin
            let filename = Filename.concat path x in
            List.rev_append (readtree filename) acc
          end else acc
        end [] basenames)
    end else []
  with Sys_error _ -> []
;;

(** is_writeable *)
let is_writeable filename = try access filename [W_OK]; true with Unix_error _ -> false

