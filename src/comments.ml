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

open Str

let pat = regexp "\\((\\*\\)\\|\\(\\*)\\)"
let lineend = regexp "$"
let search_f_pat = search_forward pat

type t =
    Utf8 of (int * int * int * bool) list (* begin * end * extra bytes * ocamldoc *)
  | Locale of (int * int * bool) list  (*  begin * end * ocamldoc *)

let scan_locale txt =
  (* Non vengono considerate le stringhe *)
  let rec f acc pos start =
    begin
      try
        let p = search_f_pat txt pos in
        begin
          try
            ignore (matched_group 1 txt);
            f acc (p + 2) (p :: start);
          with Not_found -> begin
              ignore (matched_group 2 txt);
              (match start with
               | [] -> f acc (p + 2) []
               | [start] -> f ((start, (p + 2), (txt.[start + 2] = '*')) :: acc) (p + 2) []
               | _::prev -> f acc (p + 2) prev)
            end
        end
      with Not_found -> acc
    end
  in
  List.rev (f [] 0 [])

let scan txt = Locale (scan_locale txt)

let scan_utf8 txt =
  match scan txt with
  | Locale comments -> Utf8 (
      List.map
        begin fun (x, y, ocamldoc) ->
          let c = String.sub txt x (y - x) in
          x, y, String.length c - String.length c, ocamldoc
        end comments)
  | (Utf8 _) as res -> res

let enclosing comments pos =
  let rec f = function
    | [] -> None
    | (start, stop, _) :: rest ->
        if start < pos && pos < stop then Some (start, stop)
        else f rest
  in
  f (match comments with Locale x -> x | Utf8 x -> List.map (fun (x, y, _, od) -> x, y, od) x)

let nearest comments pos =
  let rec f = function
    | [] -> None
    | (b, e, _) :: [] -> Some (b, e)
    | (b1, e1, _) :: (((b2, e2, _) :: _) as next) ->
        if (pos <= b1) || (b1 <= pos && pos < e1) then Some (b1, e1)
        else if e1 <= pos && pos < b2 then
          if pos - e1 < b2 - pos then Some (b1, e1)
          else if pos - e1 >= b2 - pos then Some (b2, e2)
          else None
        else f next
  in
  f (match comments with Locale x -> x | Utf8 x -> List.map (fun (x, y, _, od) -> x, y, od) x)

let partition comments pos =
  let pos = ref pos in
  match comments with
  | Utf8 comments ->
      let a, b = List.partition
          begin
            fun (_, y, e, _) -> if y <= !pos + e then (pos := !pos + e; true) else false
          end comments in
      Utf8 a, Utf8 b, !pos
  | Locale [] -> (Utf8 []), (Utf8 []), !pos
  | Locale comments ->
      let a, b = List.partition
          begin
            fun (_, y, _) -> if y <= !pos then true else false
          end comments in
      Locale a, Locale b, !pos


let enclosing_comment2 text pos =
  begin
    try
      if String.length text < 2 then raise Not_found;
      let start = search_backward pat text (if pos = 0 then 0 else pos - 1) in
      begin
        try
          ignore (matched_group 1 text);
          begin
            try
              let stop = search_forward pat text pos in
              begin
                try
                  ignore (matched_group 2 text);
                  Some (start, stop + 2) (* start - stop *)
                with Not_found -> Some (start,
                                        (try search_forward lineend text pos with Not_found -> String.length text)
                                       )  (* start - start *)
              end;
            with Not_found ->
              Some (start,
                    (try search_forward lineend text pos with Not_found -> String.length text)
                   ) (* start - lineend | textend *)
          end
        with Not_found -> None (* stop - ? *)
      end;
    with Not_found -> None (* no_start, no_stop - ? *)
  end
