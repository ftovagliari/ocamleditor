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
open Printf
open Editor_file_type

class file filename =
  object (self)

    val mutable ts = (stat filename).st_mtime
    val mutable filename = filename

    method filename = filename
    method dirname = Filename.dirname filename
    method basename = Filename.basename filename

    method last_modified () = (stat filename).st_mtime

    method is_readonly =
      let exists, read, write, execute = self#perm in
      exists && read && not write

    method is_writeable = File_util.is_writeable filename

    (** Se il file è cambiato sul disco dall'ultima operazione di
        creazione, lettura o scrittuta *)
    method changed = ts < self#last_modified()

    method read =
      self#update_ts ();
      Buffer.contents (File_util.read filename)

    method write text =
      File_util.write filename text;
      self#update_ts ();

    method private perm =
      let exists = try access filename [F_OK]; true with Unix_error _ -> false in
      let read = try access filename [R_OK]; true with Unix_error _ -> false in
      let write = try access filename [W_OK]; true with Unix_error _ -> false in
      let execute = (*try access filename [X_OK]; true with Unix_error _ -> false*) read in
      exists, read, write, execute

    method private update_ts () = ts <- self#last_modified()

    (** Crea una copia di backup del file aggiungendo al nome il suffisso [.~n~], dove l'indice [n] è il
        massimo più uno tra gli indici delle altre copie di backup del file che già
        esistono. Se non esiste nessuna copia di backup precedente, il primo indice è 1.
        Restituisce il nome del file creato. Se il file non esiste non viene fatta nessuna copia e
        restituisce [""].
        @param move_to Se specificato crea il backup in questa directory e se non esiste la crea. *)
    method backup ?move_to () =
      if Sys.file_exists filename then begin
        let dir = match move_to with
          | None -> filename
          | Some path ->
              if not (Sys.file_exists path) then begin
                let dirs = filename_split path in
                ignore (List.fold_left begin fun acc dir ->
                    let dir = Filename.concat acc dir in
                    if not (Sys.file_exists dir) then (printf "%s, %s\n%!" path dir; Unix.mkdir dir 0o777);
                    dir
                  end "" dirs);
              end;
              path
        in
        let name_match = Str.string_match (Utils.regexp ((Filename.chop_extension self#basename) ^ "\\.~\\([0-9]+\\)~\\..+$")) in
        let backups = List.filter_map begin fun n ->
            if name_match n 0 then
              Some (int_of_string (Str.matched_group 1 n))
            else None
          end (Array.to_list (Sys.readdir dir)) in
        let n = try ListExt.max backups + 1 with Invalid_argument _ -> 1 in
        let backup_name =
          let pos = String.rindex filename '.' in
          let ext = String.sub filename pos (String.length filename - pos) in
          (Filename.chop_extension filename) ^ ".~" ^ (string_of_int n) ^ "~" ^ ext
        in
        let backup_name = match move_to with
          | None -> backup_name
          | Some path -> Filename.concat path (Filename.basename backup_name)
        in
        File_util.cp filename backup_name;
        backup_name
      end else ""

    method rename newname =
      Sys.rename filename newname;
      filename <- newname

    method remove = if self#exists then Sys.remove filename

    method exists = Sys.file_exists filename

    method remote : remote_login option = None

    method list : unit -> string list = fun () -> []

    method set_filename fn = filename <- fn

    method cleanup () = ()

    method stat () =
      try
        let ustats = Unix.stat filename in
        Some {
          Editor_file_type.perm = ""; (* TODO *)
          size = ustats.Unix.st_size;
          mtime = ustats.Unix.st_mtime;
        }
      with Unix.Unix_error _ -> None (* TODO *)
  end

let create_remote = ref
    (fun ~host ~user ~pwd ~sslkey ~sshpublickeyfile ~sslkeypasswd ~filename -> failwith "create_remote")

(** create *)
let create ?remote filename =
  match remote with
  | None -> (new file filename :> abstract_file)
  | Some {Editor_file_type.host; user; pwd; sslkey; sshpublickeyfile; sslkeypasswd} ->
      !create_remote ~host ~user ~pwd ~sslkey ~sshpublickeyfile ~sslkeypasswd ~filename;
