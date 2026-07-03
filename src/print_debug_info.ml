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

let count_marks (page : Editor_page.page) =
  let buffer = page#buffer#as_gtext_buffer in
  let iter = ref (buffer#get_iter `START) in
  let stop = buffer#get_iter `END in
  let count_deleted = ref 0 in
  let names = ref [] in
  while not (!iter#equal stop) do
    let marks = !iter#marks in
    let deleted = List.filter (fun m -> GtkText.Mark.get_deleted m) marks in
    count_deleted := !count_deleted + List.length deleted;
    names := !names @ (List.map (fun m -> GtkText.Mark.get_name m, GtkText.Mark.get_deleted m) marks);
    iter := !iter#forward_char
  done;
  !names, !count_deleted
;;

let print ~editor () =
  printf "%-80s  %4s/%4s %10s\n-----------------------\n%!" "Filename" "undo" "redo" "marks";
  let current_page_marks = ref None in
  let current_page = editor#get_page `ACTIVE in
  List.iter begin fun p ->
    let l1, l2 = (p#buffer :> Text.buffer)#undo#length in
    let (mnames, mdel) as marks = count_marks p in
    Gaux.may current_page ~f:(fun page ->
        if page#get_oid = p#get_oid then current_page_marks := Some (page, marks));
    printf "%-80s: %4d/%4d %4d(%d)\n%!" p#get_filename l1 l2 (List.length mnames) mdel;
  end editor#pages;
  (*  *)
  begin
    match !current_page_marks with
    | Some (current_page, (mnames, mdel)) ->
        printf "-----------------------\n%!" ;
        printf "%s, %d marks (%d deleted)\n%!" current_page#get_filename (List.length mnames) mdel;
        let mnames = List.map (function (Some m, d) ->
            m ^ (if d then "(del)" else "") | _ -> "") mnames in
        printf "%s\n%!" (String.concat "; " mnames);
        let open Location in
        Lex.comments ((current_page#buffer :> Text.buffer)#as_gtext_buffer#get_text())
        |> List.iter begin fun (c, loc) ->
          let t1, start_ln, start_cn = Location.get_pos_info loc.loc_start in
          let t2, stop_ln, stop_cn = Location.get_pos_info loc.loc_end in
          Printf.printf "comment: %d,%d - %d,%d %S\n%!" start_ln start_cn stop_ln stop_cn c;
        end;
    | _ -> ()
  end;
  (*  *)
  Names.Cache.dump editor#project;
  (*  *)
  Prf.print()
;;
