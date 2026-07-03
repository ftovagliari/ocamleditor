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

open Lexing
open Parser

type result = {
  lexeme : string;
  start: int;
  length: int
}

let comments text =
  Lexer.init();
  let lexbuf = Lexing.from_string text in
  begin
    try
      while true do
        if Lexer.token lexbuf = EOF then raise End_of_file
      done
    with
    | End_of_file -> ()
    | Lexer.Error (err, loc) -> ()
  end;
  Lexer.comments ()

(** Funzione generale di analisi lessicale.*)
let analyse ?pend ?(error=ignore) text f =
  let pend = ref pend in
  let lexbuf = Lexing.from_string text in
  try
    while true do
      let token = match !pend with None -> Lexer.token lexbuf | Some t -> t in
      if token = EOF then raise End_of_file
      else
        let start = Lexing.lexeme_start lexbuf in
        let stop = Lexing.lexeme_end lexbuf in
        let length = stop - start in
        let lexeme = Bytes.sub_string lexbuf.lex_buffer start length in
        pend := f ~token ~lexeme ~start ~stop ~lexbuf;
    done
  with
  | End_of_file -> ()
  | Lexer.Error (err, loc) -> begin
      error (err, loc)
    end

(** scan *)
let scan ?(utf8=true) ?(ignore_lexer_error=true) text f =
  let lexbuf = Lexing.from_string text in
  try
    while true do
      let token = Lexer.token lexbuf in
      if token = EOF then raise End_of_file
      else
        let start = Lexing.lexeme_start lexbuf in
        let stop = Lexing.lexeme_end lexbuf in
        f ~token ~start ~stop;
    done
  with End_of_file -> () | Lexer.Error _ when ignore_lexer_error -> ()

(** Moduli aperti con "open" nel testo. *)
let paths_opened text =
  let paths = ref [] in
  analyse text begin fun ~token ~lexeme:_ ~start:_ ~stop:_ ~lexbuf ->
    match [@warning "-4"] token with
    | OPEN ->
        let path = ref "" in
        let next_token = ref token in
        while
          next_token := Lexer.token lexbuf;
          match !next_token with
          | UIDENT _ | DOT ->
              path := (!path)^(Lexing.lexeme lexbuf);
              true
          | _ -> false
        do () done;
        paths := !path :: !paths;
        Some !next_token
    | _ -> None
  end;
  (List.rev !paths)

(** Tutte le stringhe. *)
let strings text =
  let strings = ref [] in
  analyse text begin fun ~token ~lexeme:_ ~start ~stop ~lexbuf:_ ->
    match [@warning "-4"] token with
    | STRING (s, _, _) ->
        strings := {lexeme = s; start = start; length = stop - start} :: !strings;
        None
    | _ -> None
  end;
  (*List.rev*) !strings

(** in_string *)
let in_string text =
  let strings = ref [] in
  analyse text begin fun ~token ~lexeme:_ ~start ~stop ~lexbuf:_ ->
    match [@warning "-4"] token with
    | STRING _ ->
        strings := (start, stop) :: !strings;
        None
    | _ -> None
  end;
  let rec find strings pos =
    match strings with
    | (start, stop) :: rest ->
        if start < pos && pos < stop then true else (find rest pos)
    | [] -> false
  in find !strings

let for_renaming text pos =
  let exception Found of
      [ `None | `Label | `Record_label_semi | `Record_label_equal | `Dot_lident
      | `Lident | `Uident ]
  in
  let tilde = ref false in
  let lbrace = ref 0 in
  let check_record_label lexbuf =
    match [@warning "-4"] Lexer.token lexbuf with
    | LIDENT _ ->
        let start = Lexing.lexeme_start lexbuf in
        let stop = Lexing.lexeme_end lexbuf in
        if start <= pos && pos <= stop then begin
          match Lexer.token lexbuf with
          | SEMI -> raise (Found `Record_label_semi)
          | EQUAL -> raise (Found `Record_label_equal)
          | _ -> ()
        end
    | _ -> ()
  in
  try
    analyse text begin fun ~token ~lexeme ~start ~stop ~lexbuf ->
      if !tilde && start <= pos && pos <= stop then begin
        match [@warning "-4"] token with
        | LIDENT _ -> raise (Found `Label)
        | _ -> None
      end else begin
        tilde := false;
        match [@warning "-4"] token with
        | TILDE | QUESTION ->
            tilde := true;
            None
        | LBRACE ->
            incr lbrace;
            check_record_label lexbuf;
            None
        | RBRACE ->
            decr lbrace;
            None
        | SEMI when !lbrace > 0 ->
            check_record_label lexbuf;
            None
        | DOT ->
            begin
              match [@warning "-4"] Lexer.token lexbuf with
              | LIDENT _ ->
                  let start = Lexing.lexeme_start lexbuf in
                  let stop = Lexing.lexeme_end lexbuf in
                  if start <= pos && pos <= stop then begin
                    raise (Found `Dot_lident)
                  end
              | _ -> ()
            end;
            None
        | LIDENT _ when !lbrace = 0 && start <= pos && pos <= stop ->
            raise (Found `Lident);
        | UIDENT _ when start <= pos && pos <= stop ->
            raise (Found `Uident);
        | _ -> None
      end
    end;
    `None
  with
  | Found result -> result
  | _ -> `None




