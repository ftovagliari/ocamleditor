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

type let_info = {
  name: string;
  ident_start: int;
  ident_stop: int;
  mutable body_end: int option;
}

type inherit_info = {
  inherit_start: int;
  mutable inherit_end: int;
  mutable class_name: string option;
}

let analyze_class text =
  let lets = ref [] in
  let current_let_group = ref [] in
  let let_stack = ref [] in
  let expecting_ident = ref false in
  let in_object = ref false in

  let found_init = ref false in
  let init_start_val = ref 0 in
  let init_end_val = ref 0 in
  let in_initializer = ref false in
  let init_depth = ref 0 in

  let inherits = ref [] in
  let current_inherit = ref None in
  let in_inherit = ref false in
  let inherit_depth = ref 0 in
  let expecting_class_name = ref false in
  let skip_brackets = ref false in
  let inherit_bracket_depth = ref 0 in

  scan text (fun ~token ~start ~stop ->
      (* 1. Track initializer body and trim trailing whitespace *)
      if !in_initializer then (
        match token with
        | Parser.LPAREN | Parser.LBRACE | Parser.LBRACKET | Parser.BEGIN ->
            incr init_depth;
            init_end_val := stop
        | Parser.RPAREN | Parser.RBRACE | Parser.RBRACKET | Parser.END ->
            if !init_depth > 0 then (
              decr init_depth;
              init_end_val := stop
            )
            else if token = Parser.END then
              in_initializer := false
        | Parser.METHOD | Parser.VAL | Parser.INHERIT | Parser.INITIALIZER | Parser.CONSTRAINT
          when !init_depth = 0 ->
            in_initializer := false
        | _ ->
            init_end_val := stop
      );

      (* 2. Track inherit body and extract class name (skipping type parameters like [...]) *)
      if !in_inherit then (
        match token with
        | Parser.LPAREN | Parser.LBRACE | Parser.LBRACKET | Parser.BEGIN ->
            incr inherit_depth;
            if !skip_brackets then
              incr inherit_bracket_depth
            else if !expecting_class_name && token = Parser.LBRACKET then (
              skip_brackets := true;
              inherit_bracket_depth := 1;
              expecting_class_name := false
            );
            (match !current_inherit with Some ih -> ih.inherit_end <- stop | None -> ())
        | Parser.RPAREN | Parser.RBRACE | Parser.RBRACKET | Parser.END ->
            if !skip_brackets && token = Parser.RBRACKET then (
              decr inherit_bracket_depth;
              if !inherit_bracket_depth = 0 then (
                skip_brackets := false;
                expecting_class_name := true
              )
            );
            if !inherit_depth > 0 then (
              decr inherit_depth;
              (match !current_inherit with Some ih -> ih.inherit_end <- stop | None -> ())
            )
            else if token = Parser.END then (
              in_inherit := false;
              current_inherit := None;
              expecting_class_name := false;
              skip_brackets := false
            )
        | Parser.METHOD | Parser.VAL | Parser.INHERIT | Parser.INITIALIZER | Parser.CONSTRAINT
          when !init_depth = 0 ->
            in_inherit := false;
            current_inherit := None;
            expecting_class_name := false;
            skip_brackets := false
        | _ ->
            if !expecting_class_name && not !skip_brackets then (
              match token with
              | Parser.UIDENT name | Parser.LIDENT name ->
                  (match !current_inherit with Some ih -> ih.class_name <- Some name | None -> ());
                  expecting_class_name := false
              | _ -> ()
            );
            (match !current_inherit with Some ih -> ih.inherit_end <- stop | None -> ())
      );

      (* 3. Analyze let-bindings before OBJECT *)
      if not !in_object then (
        match token with
        | Parser.OBJECT ->
            in_object := true;
            expecting_ident := false
        | Parser.LET ->
            if !current_let_group <> [] then (
              let_stack := !current_let_group :: !let_stack;
              current_let_group := []
            );
            expecting_ident := true
        | Parser.AND ->
            expecting_ident := true
        | Parser.IN ->
            let group =
              if !current_let_group <> [] then !current_let_group
              else match !let_stack with
                | h :: t -> let_stack := t; h
                | [] -> []
            in
            List.iter (fun b -> b.body_end <- Some stop) group;
            lets := !lets @ group;
            current_let_group := []
        | _ when !expecting_ident ->
            (match token with
             | Parser.LIDENT name | Parser.UIDENT name ->
                 let b = { name; ident_start = start; ident_stop = stop; body_end = None } in
                 current_let_group := !current_let_group @ [b];
                 expecting_ident := false
             | Parser.REC -> ()
             | _ -> expecting_ident := false)
        | _ -> ()
      );

      (* 4. Detect start of initializer or inherit *)
      match token with
      | Parser.INITIALIZER when !in_object && not !found_init ->
          found_init := true;
          init_start_val := start;
          init_end_val := stop;
          in_initializer := true;
          init_depth := 0
      | Parser.INHERIT when !in_object ->
          let ih = { inherit_start = start; inherit_end = stop; class_name = None } in
          inherits := !inherits @ [ih];
          current_inherit := Some ih;
          in_inherit := true;
          inherit_depth := 0;
          expecting_class_name := true;
          skip_brackets := false;
          inherit_bracket_depth := 0
      | _ -> ()
    );

  let initializer_range =
    if !found_init then Some (!init_start_val, !init_end_val) else None
  in
  (!lets, initializer_range, !inherits)
