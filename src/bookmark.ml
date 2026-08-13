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

open Oe
open Preferences

let bookmarks_filename = Filename.concat App_config.ocamleditor_user_home "bookmarks-1.5.0"

let icon = function
  | 0 -> Some (??? Icons.b0)
  | 1 -> Some (??? Icons.b1)
  | 2 -> Some (??? Icons.b2)
  | 3 -> Some (??? Icons.b3)
  | 4 -> Some (??? Icons.b4)
  | 5 -> Some (??? Icons.b5)
  | 6 -> Some (??? Icons.b6)
  | 7 -> Some (??? Icons.b7)
  | 8 -> Some (??? Icons.b8)
  | 9 -> Some (??? Icons.b9)
  | _ -> None;;

(** remove *)
let rec remove bm =
  try
    begin
      match bm.bm_loc with
      | Mark mark ->
          begin
            match GtkText.Mark.get_buffer mark with
            | None -> ()
            | Some buffer ->
                GtkText.Buffer.delete_mark buffer mark;
          end
      | Offset _ -> ()
    end;
    bm.bm_loc <- (Offset 0);
  with Not_found -> ()

(** apply *)
and apply bm f =
  match bm.bm_loc with
  | Mark mark ->
      begin
        match GtkText.Mark.get_buffer mark with
        | None ->
            remove bm;
            invalid_arg "bookmark"
        | Some buffer ->
            let iter = Gmisclib.Util.get_iter_at_mark_safe buffer mark in
            f (`ITER iter)
      end
  | Offset offset -> f (`OFFSET offset)

(** mark_to_offset *)
let mark_to_offset bm =
  ignore (apply bm begin function
    | `ITER iter ->
        let loc = Offset (GtkText.Iter.get_offset iter) in
        begin
          match bm.bm_loc with
          | Mark m ->
              begin
                match GtkText.Mark.get_buffer m with
                | None -> assert false
                | Some buffer -> GtkText.Buffer.delete_mark buffer m;
              end
          | Offset _ -> assert false
        end;
        bm.bm_loc <- loc;
        -1
    | `OFFSET _ -> -1
    end)

(** offset_to_mark *)
let offset_to_mark (buffer : GText.buffer) bm =
  match bm.bm_loc with
  | Mark mark -> mark
  | Offset offset ->
      let offset = min offset buffer#char_count in
      let mark = buffer#create_mark(* ~name:(Gtk_util.create_mark_name "Bookmark.offset_to_mark")*) (buffer#get_iter (`OFFSET offset)) in
      bm.bm_loc <- (Mark mark);
      mark

(** create *)
let create ~num ~filename ~mark ~marker () =
  let bm = {
    bm_filename = filename;
    bm_loc = Mark mark;
    bm_num = num;
    bm_marker = Some marker;
  } in
  bm;;
