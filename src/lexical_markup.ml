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


open Parser
open Printf
open Location
open Lexing
open Preferences

exception Continue

module Range = struct
  let range_of_loc {loc_start; loc_end; _} =
    loc_start.pos_cnum - loc_start.pos_bol,
    loc_end.pos_cnum - loc_end.pos_bol

  let (!!) = range_of_loc

  let rec range_intersection ((a1, b1) as r1) ((a2, b2) as r2) =
    (* a1 <= b1 && a2 <= b2 *)
    if a1 <= a2 then
      if b2 <= b1 then Some r2
      else if a2 <= b1 then Some (a2, b1)
      else None
    else range_intersection r2 r1;;

  let (^^^) = range_intersection
end

open Range

let [@inline] append buf ?(debug="") s =
  Buffer.add_string buf (debug ^ s ^ debug)

type type_label_state = Possible_label of string | Lident of string | Label of string | Off

type state = {
  mutable pos : int;
  mutable tag : string;
  mutable lexeme : string;
  mutable prev_token : Parser.token;
  mutable in_record : bool;
  mutable in_record_label : bool;
  mutable in_annotation : bool;
  mutable in_type_var : string option;
  mutable in_type_label : type_label_state;
  mutable in_highlight : bool;
  mutable is_span_pending : bool;
  mutable text : string;
  mutable output : Buffer.t;
  mutable span_highlight : string -> string;
  mutable span : bool * string -> string;
}

let create_state () = {
  pos = 0;
  tag = "";
  lexeme = "";
  prev_token = EOF;
  in_record = false;
  in_record_label = false;
  in_annotation = false;
  in_type_var = None;
  in_type_label = Off;
  in_highlight = false;
  is_span_pending = false;
  text = "";
  output = Buffer.create 200;
  span_highlight = (fun _ -> "");
  span = (fun _ -> "");
}

let partial_reset_state (s : state) =
  s.tag <- "";
  s.prev_token <- EOF;
  s.in_record <- false;
  s.in_record_label <- false;
  s.in_annotation <- false;
  s.in_type_var <- None;
  s.in_type_label <- Off;
  s.in_highlight <- false;
  s.is_span_pending <- false

let reset_state (s : state) text =
  partial_reset_state s;
  s.lexeme <- "";
  s.text <- text;
  s.output <- Buffer.create (String.length text);
  s.pos <- 0

let close_pending state =
  if state.is_span_pending then begin
    append state.output "</span>";
    state.is_span_pending <- false
  end

let get_tag token state =
  match token with
  | AMPERAMPER
  | AMPERSAND
  | OR
  | BARBAR
  | DO | DONE
  | DOWNTO
  | ELSE
  | FOR
  | IF
  | LAZY
  | MATCH
  | THEN
  | TO
  | TRY
  | WHEN
  | WHILE
  | WITH
    -> "control"
  | AND
  | ANDOP _
  | AS
  | BAR
  | CLASS
  | CONSTRAINT
  | EXCEPTION
  | EXTERNAL
  | FUN
  | FUNCTION
  | FUNCTOR
  | INHERIT
  | LET
  | LETOP _
  | METHOD
  | MODULE
  | MUTABLE
  | NONREC
  | PRIVATE
  | REC
  | TYPE
  | VAL
  | VIRTUAL
    -> "define"
  | IN | INITIALIZER | NEW | OF -> "define"
  | BEGIN
  | END
  | INCLUDE
  | OBJECT
  | OPEN
  | SIG
  | STRUCT
    -> "structure"
  | CHAR _
  | STRING _
  | QUOTED_STRING_EXPR _ | QUOTED_STRING_ITEM _
    -> "char"
  (*| BACKQUOTE*)
  | INFIXOP0 _
  | INFIXOP1 _
  | INFIXOP2 _
  | INFIXOP3 _
  | INFIXOP4 _
  | PREFIXOP _
  | HASH | HASHOP _
  | BANG | MINUSGREATER
    -> "infix"
  | LABEL _
  | OPTLABEL _
  | QUESTION
  | TILDE
    -> "label"
  | UIDENT _ | BACKQUOTE -> "uident"
  | LIDENT _ when state.in_annotation -> "annotation"
  | LIDENT lident when state.in_type_var <> None ->
      state.in_type_var <- Some lident;
      "annotation"
  | LIDENT _ ->
      begin match [@warning "-4"] state.prev_token with
      | QUESTION | TILDE -> "label"
      | BACKQUOTE -> "number"
      | HASH | HASHOP _ -> "lident"
      | _ ->
          (match state.in_type_label with
           | Possible_label x -> state.in_type_label <- Lident x;
           | Lident _ | Label _ | Off ->
               state.in_type_label <- Possible_label state.lexeme);
          "lident"
      end
  | COLON ->
      (match [@warning "-4"] state.prev_token with LIDENT lident -> state.in_type_label <- Label (lident ^ state.lexeme); "label" | _ -> "")
  | INT _ | FLOAT _  | TRUE | FALSE -> "number"
  | LBRACE -> state.in_record <- true; state.in_record_label <- true; "symbol"
  | RBRACE -> state.in_record <- false; state.in_record_label <- false; "symbol"
  | EQUAL when state.in_record -> state.in_record_label <- false; "symbol"
  | LPAREN | RPAREN | LBRACKET | BARRBRACKET | LBRACKETLESS | LBRACKETGREATER | GREATERRBRACKET
  | LBRACELESS | GREATERRBRACE | LBRACKETBAR | LESSMINUS
  | EQUAL | PLUS | MINUS | STAR | SEMI | SEMISEMI
  | COMMA | DOT | DOTDOT | COLONCOLON | COLONEQUAL | UNDERSCORE
  | PLUSDOT | MINUSDOT | LESS | GREATER
  | PLUSEQ | PERCENT
  | COLONGREATER
  | DOTOP _
    -> "symbol"
  | QUOTE ->
      state.in_type_var <- Some "";
      ""
  | LBRACKETAT | LBRACKETPERCENT | LBRACKETPERCENTPERCENT | LBRACKETATAT | LBRACKETATATAT
    -> state.in_annotation <- true; "annotation"
  | RBRACKET -> if state.in_annotation then (state.in_annotation <- false; "annotation") else "symbol"
  | ASSERT -> "custom"
  | DOCSTRING _ | COMMENT _ -> "comment" (* Lexer ignores comments *)
  | METAOCAML_ESCAPE | METAOCAML_BRACKET_OPEN | METAOCAML_BRACKET_CLOSE | EFFECT -> ""
  | EOL -> ""
  | EOF -> raise End_of_file;;

let finalize_lident state =
  match [@warning "-4"] state.in_type_label with
  | Possible_label lident ->
      (*Printf.printf "  finalize_lident Possible_label %s\n%!" lident;*)
      state.in_type_label <- Off;
      append state.output (Glib.Markup.escape_text lident);
  | _ ->
      (*Printf.printf "  finalize_lident %S\n%!" state.lexeme;*)
      ()

let add highlights state ((lstart, lstop) as range) with_span =
  match highlights |> List.fold_left (fun acc h -> match h ^^^ range with None -> acc | x -> x) None with
  | Some (a, b) when a < b ->
      (*Printf.printf "  ADD lexeme %S, with_span %b, tag %s %d,%d\n%!" state.lexeme with_span state.tag a b;*)
      if with_span then begin
        if a = lstart && b = lstop then begin
          state.in_highlight <- true;
          append state.output (state.span (true, state.tag));
          append state.output (Glib.Markup.escape_text state.lexeme);
          append state.output "</span>";
          raise Continue
        end else begin (* TODO: Other cases non implemented *)
          append state.output (state.span (false, state.tag));
          append state.output (Glib.Markup.escape_text state.lexeme);
        end
      end else begin
        if a = lstart && b = lstop then begin
          let before = String.sub state.text lstart (b - lstart) in
          let after = String.sub state.text b (lstop - b) in
          if before <> "" then (append state.output (state.span_highlight before));
          if after <> "" then (append state.output (Glib.Markup.escape_text after));
        end else begin
          let before = String.sub state.text lstart (a - lstart) in
          let middle = String.sub state.text a (b - a) in
          let after = String.sub state.text b (lstop - b) in
          if before <> "" then (append state.output (Glib.Markup.escape_text before););
          if middle <> "" then (append state.output (state.span_highlight middle));
          if after <> "" then (append state.output (Glib.Markup.escape_text after););
        end
      end
  | _ ->
      (*Printf.printf "  ADD lexeme %S, with_span %b, tag %s\n%!" state.lexeme with_span state.tag;*)
      if with_span then (append state.output (state.span (false, state.tag)));
      begin
        match state.in_type_var with
        | Some "" -> ()
        | Some lident ->
            state.in_type_var <- None;
            append state.output (Glib.Markup.escape_text ("'" ^ lident));
        | None ->
            begin
              match state.in_type_label with
              | Label type_label ->
                  (*Printf.printf "    Label %S\n%!" type_label;*)
                  state.in_type_label <- Off;
                  append state.output (Glib.Markup.escape_text type_label);
              | Lident lident ->
                  (*Printf.printf "    Lident %S\n%!" lident;*)
                  state.in_type_label <- Off;
                  append state.output (state.span (false, "lident"));
                  append state.output (Glib.Markup.escape_text lident);
                  append state.output "</span>";
                  append state.output (Glib.Markup.escape_text state.lexeme);
              | Off ->
                  (*Printf.printf "    Off\n%!";*)
                  append state.output (Glib.Markup.escape_text state.lexeme);
              | Possible_label x ->
                  (*Printf.printf "    Possible_label %S\n%!" x;*)
                  ()
            end;
      end

let parse ?(use_bold=true) pref =
  let tags = pref.Settings_t.editor_tags in
  let bgcolor_highlight = preferences#get.editor_mark_occurrences_bg_color in
  let open_span_tag (highlight, tagname) =
    match List.find_opt (fun t -> t.Settings_t.name = tagname) tags with
    | Some t ->
        let weight    = if use_bold && t.weight > 0 then sprintf " font_weight='%d'" t.weight else "" in
        let style     = match t.style with `ITALIC -> " font_style='italic'" | _ -> "" in
        let underline = match t.underline with `NONE -> "" | _ -> " underline='low'" in
        let bgcolor   = if highlight then " bgcolor='" ^ (?? bgcolor_highlight) ^"'" else "" in
        String.concat "" ["<span color='"; ??(t.color); "'"; weight; style; underline; bgcolor; ">"]
    | _ -> assert false
  in
  let state = create_state () in
  state.span_highlight <- (fun text ->
      (* For tokens without a tag? *)
      String.concat "" ["<span bgcolor='"; ?? bgcolor_highlight; "'>"; (Glib.Markup.escape_text text); "</span>"]);
  state.span <- (Utils.Memo.create open_span_tag);
  fun ?(highlights=[]) text ->
    reset_state state text;
    let buffer = Lexing.from_string text in
    let add = add highlights state in
    try
      while true do
        try
          let token = Lexer.token buffer in
          let (lstart, lstop) as range = Lexing.lexeme_start buffer, Lexing.lexeme_end buffer in
          state.lexeme <- Lexing.lexeme buffer;
          state.tag <- get_tag token state;
          (*Printf.printf "TOKEN lexeme %S, tag %s %d,%d\n%!" state.lexeme state.tag lstart lstop;*)
          let add_with_span = add range in
          begin
            match [@warning "-4"] token with
            | COLON | LIDENT _ -> ()
            | _ ->
                begin
                  match state.in_type_label with
                  | Possible_label lident -> state.in_type_label <- Lident lident
                  | _ -> ()
                end
          end;
          let blanks = String.sub text state.pos (lstart - state.pos) in
          if blanks <> "" then begin
            match [@warning "-4"] state.in_type_label with
            | Lident _ when state.in_highlight ->
                state.in_type_label <- Lident blanks;
                state.in_highlight <- false;
            | Lident lident -> state.in_type_label <- Lident (lident ^ blanks)
            | _ -> append state.output blanks;
          end;
          close_pending state;
          begin
            try
              if state.tag <> "" then begin
                add_with_span true;
                if state.tag <> "char" then (append state.output "</span>") else (state.is_span_pending <- true);
              end else
                add_with_span false
            with Continue -> partial_reset_state state
          end;
          state.pos <- lstart + (String.length state.lexeme);
          state.prev_token <- token;
        with Lexer.Error (err, _) ->
          begin
            let open Lexer in
            match err with
            | Illegal_character c -> printf "Illegal_character %C\n%!" c
            | Illegal_escape (s, sopt) -> printf "Illegal_escape \n%!"
            | Reserved_sequence (s, sopt) -> printf "Reserved_sequence \n%!"
            | Unterminated_comment loc -> printf "Unterminated_comment \n%!"
            | Unterminated_string -> printf "Unterminated_string \n%!"
            | Unterminated_string_in_comment (l1, l2) -> printf "Unterminated_string_in_comment \n%!"
            | Empty_character_literal -> printf "Empty_character_literal \n%!"
            | Keyword_as_label s -> printf "Keyword_as_label \n%!"
            | Invalid_literal s -> printf "Invalid_literal \n%!"
            | Invalid_directive (s, sopt) -> printf "Invalid_directive \n%!"
            | Capitalized_label _ -> printf "Capitalized_label \n%!"
            | Invalid_encoding _ -> printf "Invalid_encoding \n%!"
            | Invalid_char_in_ident _ -> printf "Invalid_char_in_ident \n%!"
            | Non_lowercase_delimiter _ -> printf "Non_lowercase_delimiter \n%!"
            | Capitalized_raw_identifier _ -> printf "Capitalized_raw_identifier \n%!"
            | Unknown_keyword _ -> printf "Unknown_keyword \n%!"
          end
      done;
      close_pending state;
      Buffer.contents state.output
    with
    | End_of_file ->
        (* TODO: consider highlights *)
        finalize_lident state;
        append state.output (Glib.Markup.escape_text state.lexeme);
        close_pending state;
        (*Printf.printf "===> %S\n%!" (Buffer.contents state.output);*)
        Buffer.contents state.output
    | Lexer.Error _ ->
        finalize_lident state;
        close_pending state;
        Buffer.contents state.output
    | ex ->
        finalize_lident state;
        close_pending state;
        (printf "Lexical_markup: %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace ()));
        ""
