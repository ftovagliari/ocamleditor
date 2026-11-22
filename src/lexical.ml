(* $Id: lexical.ml,v 1.14.2.1 2003/03/13 11:47:20 garrigue Exp $ *)

open Parser
open Str
open Printf
open Preferences

let multi_space = regexp "\\( \\( +\\)\\)\\|(\\*\\*\\(\\*+\\)\\|(\\*\\* \\|(\\* \\|(\\*\\|\\*)"

let _ = Lexer.init()

(* Line_offset *)
let line_starts s =
  let len = String.length s in
  let rec next_line ~accu pos =
    if pos >= len then accu else
      let res = try 1 + String.index_from s pos '\n' with Not_found -> 0 in
      if res = 0 then accu else
        next_line ~accu:(res :: accu) res
  in
  next_line ~accu:[0] 0

(** [pos_to_line_col ~lines pos] computes the tuple [(line_index, column_offset)] for the
    given character position [pos], based on the list of line-start character
    positions [lines], sorted in descending order.
    Raises [Invalid_argument] if [lines] is empty. *)
let rec pos_to_line_col ~lines pos =
  match lines with
  | [] -> invalid_arg __FUNCTION__
  | last :: prev ->
      if pos >= last then (List.length prev, pos - last)
      else pos_to_line_col ~lines:prev pos

(** [move_iter_to_pos ~start ~lines pos] moves the text iterator [start] to the target character
    position [pos] based on the line offsets [lines], which is a list of line-start
    character positions, sorted in descending order. *)
let move_iter_to_pos ~(start : GText.iter) ~lines pos =
  let l, c = pos_to_line_col ~lines pos in
  if l = 0 then begin
    let li = start#line_index in
    start#set_line_index (c + li)
  end else
    (start#forward_lines l)#set_line_index c

let tag_lident = function
  | _, METHOD, _, _ | _, PRIVATE, _, _ -> "method_name_def"
  | _, IN, _, _ | _, INITIALIZER, _, _ | _, NEW, _, _ | _, OF, _, _ -> "lident"
  | "define", _, _, _  -> "name_def"
  | _ -> "lident"


let [@inline] use_custom lexeme = lexeme = "failwith" || lexeme = "raise" || lexeme = "invalid_arg"

let tag ?start ?stop ?(tags=Text_tag.main) (buffer : GText.buffer) =
  let start = Gaux.default buffer#start_iter ~opt:start
  and stop = Gaux.default buffer#end_iter ~opt:stop in
  (* Se start e stop sono all'interno di commenti allora prendo come start e stop
     l'inizio del primo commento e la fine del secondo. *)
  let global_comments = Comment.scan_positions (buffer#get_text ()) in
  let start =
    let ln = start#line + 1 in
    let cn = start#line_index in
    match Comment.find_at global_comments ln cn with
    | Some c -> buffer#get_iter (`LINECHAR (c.start_line - 1, c.start_col))
    | _ -> start
  in
  let stop =
    let ln = stop#line + 1 in
    let cn = stop#line_index in
    match Comment.find_at global_comments ln cn with
    | Some c -> buffer#get_iter (`LINECHAR (c.stop_line - 1, c.stop_col))
    | _ -> stop
  in
  (*  *)
  let text = buffer#get_text ~start ~stop () in
  let lines = line_starts text in
  let move_start_to_pos = move_iter_to_pos ~start ~lines in
  let lexbuf = Lexing.from_string text in
  let last = ref ("", EOF, 0, 0) in
  let last_but_one = ref ("", EOF, 0, 0) in
  let in_record = ref false in
  let in_record_label = ref false in
  let in_annotation = ref false in
  List.iter (fun (n, t) -> buffer#remove_tag_by_name n ~start ~stop) tags.Text_tag.lexical;
  try
    while true do
      try
        let token = Lexer.token lexbuf in
        let lstart = Lexing.lexeme_start lexbuf in
        let lstop = Lexing.lexeme_end lexbuf in
        let start = lstart in
        let tag =
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
          | BANG | LESS | GREATER | COLONCOLON | LESSMINUS | COLONEQUAL
            -> "infix"
          | LABEL _
          | OPTLABEL _
          | QUESTION
          | TILDE
            -> "label"
          | UIDENT _ | BACKQUOTE -> "uident"
          | LIDENT _ when !in_annotation -> "annotation"
          | LIDENT _ ->
              let lexeme = Lexing.lexeme lexbuf in
              begin match  [@warning "-4"] !last with
              | _, (QUESTION | TILDE), _, _ -> "label"
              | _, BACKQUOTE, _, _ -> "number"
              (* TODO:  *)
              | _, LBRACE, _, _ when !in_record -> "record_label"
              | _, MUTABLE, _, _ when !in_record -> "record_label"
              | _, WITH, _, _ when !in_record -> "record_label"
              | _, SEMI, _, _ when !in_record -> "record_label"
              | _, DOT, _, _ when !in_record && !in_record_label -> "record_label"
              | _, LPAREN, _, _ ->
                  (match [@warning "-4"] !last_but_one with
                   | _, (QUESTION | TILDE), _, _ -> "label"
                   | _ ->
                       (if use_custom lexeme
                        then "custom" else (*if lexeme.[0] = '_' then "annotation" else*) "lident"))
              | (_, DOT, _, _) as last ->
                  (if use_custom lexeme then "custom" else tag_lident last)
              | last ->
                  (if use_custom lexeme then "custom"
                   else (*if lexeme.[0] = '_' then "annotation" else*) tag_lident last)
              end
          | COLON ->
              begin match [@warning "-4"] !last with
                _, LIDENT _, lstart, lstop ->
                  if lstop = start then
                    buffer#apply_tag_by_name "label" ~start:(move_start_to_pos lstart) ~stop:(move_start_to_pos lstop);
                  ""
              | _ -> ""
              end
          | INT _ | FLOAT _  | TRUE | FALSE -> "number"
          | LBRACE -> in_record := true; in_record_label := true; "symbol"
          | RBRACE -> in_record := false; in_record_label := false; "symbol"
          | EQUAL when !in_record -> in_record_label := false; "symbol"
          | LPAREN | RPAREN | LBRACKET | BARRBRACKET | LBRACKETLESS | LBRACKETGREATER | GREATERRBRACKET
          | LBRACELESS | GREATERRBRACE | LBRACKETBAR
          | EQUAL | PLUS | MINUS | STAR | QUOTE | SEMI | SEMISEMI | MINUSGREATER
          | COMMA | DOT | DOTDOT | UNDERSCORE
          | PLUSDOT | MINUSDOT
          | PLUSEQ | PERCENT
          | COLONGREATER
          | DOTOP _
          | BAR
            -> "symbol"
          | LBRACKETAT | LBRACKETPERCENT | LBRACKETPERCENTPERCENT | LBRACKETATAT | LBRACKETATATAT
            -> in_annotation:= true; "annotation"
          | RBRACKET -> if !in_annotation then (in_annotation := false; "annotation") else "symbol"
          | ASSERT -> "custom"
          | DOCSTRING _ | COMMENT _ -> "comment"
          | METAOCAML_ESCAPE | METAOCAML_BRACKET_OPEN | METAOCAML_BRACKET_CLOSE | EFFECT -> ""
          | EOL -> ""
          | EOF -> raise End_of_file
        in
        if tag <> "" then
          buffer#apply_tag_by_name tag ~start:(move_start_to_pos start) ~stop:(move_start_to_pos lstop);
        last_but_one := !last;
        last := (tag, token, start, lstop)
      with Lexer.Error _ -> ()
    done;
  with
  | End_of_file ->
      (* comments *)
      let open Comment in
      global_comments |> List.iter begin fun c ->
        let start = buffer#get_iter (`LINECHAR (c.start_line - 1, c.start_col)) in
        let stop = buffer#get_iter (`LINECHAR (c.stop_line - 1, c.stop_col)) in
        if c.is_odoc then begin
          let tag, start =
            let iter = start#backward_line in
            if iter#ends_line
            then (tags.Text_tag.ocamldoc_paragraph, start#set_line_index 0)
            else (tags.Text_tag.ocamldoc, start#set_line_index 0) (* start *)
          in
          buffer#apply_tag tag ~start ~stop;
        end else
          buffer#apply_tag_by_name Text_tag.Name.comment ~start ~stop;
      end
  | (Lexer.Error (error, _)) as ex -> ()
  | ex -> (printf "Lexical: %s\n%!" (Printexc.to_string ex))



(*let tag ?(start : GText.iter option) ?(stop : GText.iter option) ?(tags : Text_tag.group option) (tb : GText.buffer) = ()*)








