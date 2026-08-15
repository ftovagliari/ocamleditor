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

(** interval (milliseconds) *)
let interval = Oe_config.autosave_interval

let keep_backup = Oe_config.autosave_keep_backup

(** path *)
let path =
  let path = App_config.ocamleditor_user_home // "autosave" in
  if not (Sys.file_exists path) then (Unix.mkdir path 509);
  let path_bak = App_config.ocamleditor_user_home // "autosave" // "bak" in
  if not (Sys.file_exists path_bak) then (Unix.mkdir path_bak 509);
  path

(** table_filename *)
let table_filename = path // ".autosave"

(** table *)
let table : (string, string) Hashtbl.t =
  if not (Sys.file_exists table_filename) then begin
    let chan = open_out_bin table_filename in
    lazy (output_value chan (Hashtbl.create 17)) /*finally*/ lazy (close_out chan);
  end;
  let chan = open_in_bin table_filename in
  lazy (input_value chan) /*finally*/ lazy (close_in chan)

(** backup *)
let backup ~filename ~text =
  let id =
    try Hashtbl.find table filename
    with Not_found ->
      let id = (sprintf "%s.%f.autosave" (Filename.basename filename) (Unix.gettimeofday())) in
      Hashtbl.add table filename id;
      id
  in
  (* Save recovery copy *)
  let chan = open_out_bin (path // id) in
  lazy (output_string chan text) /*finally*/ lazy (close_out chan);
  (* Update table *)
  let chan = open_out_bin table_filename in
  lazy (output_value chan table) /*finally*/ lazy (close_out chan)

(** delete *)
let delete ?(bak=false) ~filename () =
  try
    let id = Hashtbl.find table filename in
    Hashtbl.remove table filename;
    let chan = open_out_bin table_filename in
    lazy (output_value chan table) /*finally*/ lazy (close_out_noerr chan);
    let pathid = path // id in
    if Sys.file_exists pathid then begin
      if bak then begin
        let path_bak = (path // "bak") in
        Utils.mkdir_p path_bak;
        Sys.rename pathid (path_bak // (Filename.basename pathid));
      end else (Sys.remove pathid)
    end
  with Not_found -> ()

(** clean_backup *)
let clean_backup () =
  (* Clean old backup copies *)
  let bak = path // "bak" in
  let backups = Sys.readdir bak in
  let limit = Unix.time() -. keep_backup in
  Array.iter begin fun basename ->
    let filename = bak // basename in
    let stat = Unix.stat filename in
    let tm = stat.Unix.st_mtime in
    if (Sys.file_exists filename) && tm < limit then (Sys.remove filename)
  end backups;;

open Preferences
(** recover *)
let recover () =
  let files = ref [] in
  Hashtbl.iter begin fun filename _ ->
    files := filename :: !files;
  end table;
  let files = List.map (fun x -> true, x) (List.rev !files) in
  if files <> [] then begin
    let dialog = GWindow.dialog ~position:`CENTER ~border_width:5
        ~icon:(??? Icons.oe) ~modal:true ~title:"Auto Recovery" () in
    let checklist = new Checklist.checklist ~packing:dialog#vbox#add files in
    dialog#vbox#set_spacing 5;
    dialog#add_button_stock `OK `OK;
    dialog#add_button_stock `CANCEL `CANCEL;
    dialog#action_area#add checklist#button_all#coerce;
    dialog#action_area#add checklist#button_none#coerce;
    dialog#action_area#set_child_secondary checklist#button_all#coerce true;
    dialog#action_area#set_child_secondary checklist#button_none#coerce true;
    let clean_up _ =
      Hashtbl.iter (fun filename _ -> delete ~bak:true ~filename ()) table;
    in
    match dialog#run () with
    | `OK ->
        checklist#iter begin fun active filename ->
          if active then begin
            let id = try Hashtbl.find table filename with Not_found -> assert false in
            let ichan = open_in_bin (path // id) in
            let buf = Buffer.create 100 in
            lazy (Buffer.add_channel buf ichan (in_channel_length ichan))
            /*finally*/ lazy (close_in ichan);
            let ochan = open_out_bin filename in
            lazy (output_string ochan (Buffer.contents buf)) /*finally*/ lazy (close_out ochan);
          end
        end;
        clean_up();
        dialog#destroy()
    | _ -> clean_up(); dialog#destroy()
  end;
  clean_backup();;


















