(* $Id: lexical.ml,v 1.14.2.1 2003/03/13 11:47:20 garrigue Exp $ *)

open Parser
open Str
open Printf
open Preferences

let multi_space = regexp "\\( \\( +\\)\\)\\|(\\*\\*\\(\\*+\\)\\|(\\*\\* \\|(\\* \\|(\\*\\|\\*)"

let _ = Lexer.init()

let tags, colors =
  let tags, colors =
    Preferences.preferences#get.editor_tags
    |> List.map (fun (t : Settings_t.editor_tag) -> t.name, t)
    |> List.split
  in ref tags, ref colors

(* Initialization *)
let init_tags ?(tags=(!tags)) ?(colors=(!colors))
    ?(ocamldoc_paragraph_enabled=Oe_config.ocamldoc_paragraph_bgcolor_enabled)
    ?(ocamldoc_paragraph_bgcolor_1=(Preferences.preferences#get.editor_ocamldoc_paragraph_bgcolor_1))
    ?(ocamldoc_paragraph_bgcolor_2=(Preferences.preferences#get.editor_ocamldoc_paragraph_bgcolor_2))
    (tb : #GText.buffer) =
  let table = new GText.tag_table tb#tag_table in
  let to_gdk_color x = GDraw.color (`NAME ?? x) in
  List.iter2
    begin fun tagname (tagprop : Settings_t.editor_tag) ->
      if tagname <> "highlight_current_line" then begin
        begin
          match table#lookup tagname with
          | None -> ()
          | Some t -> table#remove t
        end;
        let fg_color = `FOREGROUND_GDK (to_gdk_color tagprop.color) in
        let properties = [
          Some fg_color;
          (match tagprop.scale with 1.0 -> None | _ -> Some (`SCALE (`CUSTOM tagprop.scale)));
          (match tagprop.style with `NORMAL -> None | _ -> Some (`STYLE `ITALIC));
          (match tagprop.underline with `NONE -> None | _ -> Some (`UNDERLINE `SINGLE));
          (if tagprop.weight > 0 then Some (`WEIGHT (`CUSTOM tagprop.weight)) else None)
        ] |> List.filter_map Fun.id in
        let properties = if tagprop.bg_default then properties else (`BACKGROUND_GDK (to_gdk_color tagprop.bg_color)) :: properties in
        let tag = tb#create_tag ~name:tagname properties in
        if tagname = "ocamldoc" then begin
          if ocamldoc_paragraph_enabled then begin

            ?? ocamldoc_paragraph_bgcolor_2
            |> Gaux.may ~f:(fun c -> Gmisclib.Util.set_tag_paragraph_background tag c);

            (*            Gaux.may ocamldoc_paragraph_bgcolor_2 ~f:begin fun bg2 ->
                          Gmisclib.Util.set_tag_paragraph_background tag (?? bg2);
                          end;*)
          end;
          Gaux.may (table#lookup "ocamldoc-paragraph") ~f:table#remove;
          let properties = [
            Some fg_color;
            (if tagprop.weight > 0 then Some (`WEIGHT (`CUSTOM tagprop.weight)) else None);
            (match tagprop.scale with 1.0 -> None | _ -> Some (`SCALE (`CUSTOM tagprop.scale)));
            (match tagprop.style with `NORMAL -> None | _ -> Some (`STYLE `ITALIC));
            (match tagprop.underline with `NONE -> None | _ -> Some (`UNDERLINE `SINGLE));
            Some (`PIXELS_BELOW_LINES 1);
            Some (`PIXELS_ABOVE_LINES 1)
          ] |> List.filter_map (fun x -> x) in
          let tag = tb#create_tag ~name:"ocamldoc-paragraph" properties in
          if ocamldoc_paragraph_enabled then begin
            ?? ocamldoc_paragraph_bgcolor_1
            |> Gaux.may ~f:(fun c -> Gmisclib.Util.set_tag_paragraph_background tag c);

            (*            Gaux.may ocamldoc_paragraph_bgcolor_1 ~f:begin fun bg1 ->
                          Gmisclib.Util.set_tag_paragraph_background tag (?? bg1);
                          end*)
          end
        end
      end
    end tags colors;
  (* Give the unused warning tag the highest priority *)
  table#lookup Oe_config.warning_unused_tag_name
  |> Option.iter (fun t -> GtkText.Tag.set_priority t (table#size - 1))

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

let rec line_offset ~lines pos =
  match lines with [] -> invalid_arg "Lexical.line_offset"
                 | last :: prev ->
                     if pos >= last then (List.length prev, pos - last)
                     else line_offset ~lines:prev pos

let tpos ~(start : GText.iter) ~lines pos =
  let l, c = line_offset ~lines pos in
  let result =
    if l = 0 then begin
      let li = start#line_index in
      let result = start#set_line_index (c + li) in
      result
    end else
      (start#forward_lines l)#set_line_index c
  in
  result

let tag_lident = function
  | _, METHOD, _, _ | _, PRIVATE, _, _ -> "method_name_def"
  | _, IN, _, _ | _, INITIALIZER, _, _ | _, NEW, _, _ | _, OF, _, _ -> "lident"
  | "define", _, _, _  -> "name_def"
  | _ -> "lident"

(* Tagging *)

let tag ?start ?stop (tb : GText.buffer) =
  let start = Gaux.default tb#start_iter ~opt:start
  and stop = Gaux.default tb#end_iter ~opt:stop in
  (* Se start e stop sono all'interno di commenti allora prendo come start e stop
     l'inizio del primo commento e la fine del secondo. *)
  let global_comments = Comments.scan (tb#get_text ()) in
  let start = match Comments.enclosing global_comments start#offset with
    | None -> start
    | Some (x, _) -> tb#get_iter_at_char x in
  let stop = match Comments.enclosing global_comments stop#offset with
    | None -> stop
    | Some (_, y) -> tb#get_iter_at_char y in
  (*  *)
  let text = tb#get_text ~start ~stop () in
  let lines = line_starts text in
  let tpos = tpos ~start ~lines in
  let buffer = Lexing.from_string text in
  let comments = Comments.scan_utf8 text in
  let succ_comments = ref comments in
  let last = ref ("", EOF, 0, 0) in
  let last_but_one = ref ("", EOF, 0, 0) in
  let in_record = ref false in
  let in_record_label = ref false in
  let in_annotation = ref false in
  List.iter begin function
  | tagname when tagname <> "highlight_current_line" -> tb#remove_tag_by_name tagname ~start ~stop
  | _ -> ()
  end !tags;
  try
    while true do
      try
        let token = Lexer.token buffer in
        let lstart = Lexing.lexeme_start buffer in
        let lstop = Lexing.lexeme_end buffer in
        let _, succ, start = Comments.partition !succ_comments lstart  in
        succ_comments := succ;
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
          | BANG | LESS | GREATER
            -> "infix"
          | LABEL _
          | OPTLABEL _
          | QUESTION
          | TILDE
            -> "label"
          | UIDENT _ | BACKQUOTE -> "uident"
          | LIDENT _ when !in_annotation -> "annotation"
          | LIDENT _ ->
              let lexeme = Lexing.lexeme buffer in
              begin match !last with
              | _, (QUESTION | TILDE), _, _ -> "label"
              | _, BACKQUOTE, _, _ -> "number"
              (* TODO:  *)
              | _, LBRACE, _, _ when !in_record -> "record_label"
              | _, MUTABLE, _, _ when !in_record -> "record_label"
              | _, WITH, _, _ when !in_record -> "record_label"
              | _, SEMI, _, _ when !in_record -> "record_label"
              | _, DOT, _, _ when !in_record && !in_record_label -> "record_label"
              | _, LPAREN, _, _ ->
                  (match !last_but_one with
                   | _, (QUESTION | TILDE), _, _ -> "label"
                   | _ -> (if lexeme = "failwith" || lexeme = "raise" || lexeme = "invalid_arg" then "custom" else "lident"))
              | last ->
                  (if lexeme = "failwith" || lexeme = "raise" || lexeme = "invalid_arg" then "custom" else tag_lident last)
              end
          | COLON ->
              begin match !last with
                _, LIDENT _, lstart, lstop ->
                  if lstop = start then
                    tb#apply_tag_by_name "label" ~start:(tpos lstart) ~stop:(tpos lstop);
                  ""
              | _ -> ""
              end
          | INT _ | FLOAT _  | TRUE | FALSE -> "number"
          | LBRACE -> in_record := true; in_record_label := true; "symbol"
          | RBRACE -> in_record := false; in_record_label := false; "symbol"
          | EQUAL when !in_record -> in_record_label := false; "symbol"
          | LPAREN | RPAREN | LBRACKET | BARRBRACKET | LBRACKETLESS | LBRACKETGREATER | GREATERRBRACKET
          | LBRACELESS | GREATERRBRACE | LBRACKETBAR | LESSMINUS
          | EQUAL | PLUS | MINUS | STAR | QUOTE | SEMI | SEMISEMI | MINUSGREATER
          | COMMA | DOT | DOTDOT | COLONCOLON | COLONEQUAL | UNDERSCORE
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
          tb#apply_tag_by_name tag ~start:(tpos start) ~stop:(tpos lstop);
        last_but_one := !last;
        last := (tag, token, start, lstop)
      with Lexer.Error _ -> ()
    done;
  with
  | End_of_file ->
      (* comments *)
      List.iter begin fun (b, e, _, ocamldoc) ->
        if not ocamldoc then begin
          let ms = Utils.Search.all multi_space begin fun ~pos ~matched_string:mat ->
              Utils.Search.Append (pos, pos + String.length mat, mat)
            end (String.sub text b (e - b)) in
          let (*b = b and*) e = e - 2 in
          let tag = "comment" in
          tb#apply_tag_by_name tag ~start:(tpos b) ~stop:(tpos e);
          List.iter begin fun (b1, e1, _) ->
            tb#apply_tag_by_name tag ~start:(tpos (b + b1)) ~stop:(tpos (b + e1))
          end ms
        end
      end (match comments with Comments.Utf8 x -> x | _ -> failwith "Lexical: Comments.Locale");
      (* ocamldoc and ocamldoc-paragraph *)
      List.iter begin fun (b, e, ocamldoc) ->
        if ocamldoc then begin
          let start = tb#get_iter (`OFFSET b) in
          let stop = tb#get_iter (`OFFSET e) in
          let tag, start =
            let iter = start#backward_line in
            if iter#ends_line
            then ("ocamldoc-paragraph", start#set_line_index 0)
            else ("ocamldoc", start#set_line_index 0) (* start *)
          in
          tb#apply_tag_by_name tag ~start ~stop;
        end
      end (match global_comments with Comments.Locale x -> x | _ -> failwith "Lexical: Comments.Utf8")
  | (Lexer.Error (error, _)) as ex -> ()
  | ex -> (printf "Lexical: %s\n%!" (Printexc.to_string ex))















