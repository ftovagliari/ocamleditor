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

type t =
  | Templ of templ
  | Action of (Ocaml_text.view -> unit)

and scope = Project | User | Application

and templ = t_elem list

and t_elem =
  | SELECTION        (* Inserts the currently-selected text *)
  | SELECTION_TRIM   (* Inserts the currently-selected text without trailing and leading whitespace *)
  | SELECTION_OPT of string (* Inserts the currently-selected text; if none inserts the text specified *)
  | IN               (* Increases the current indentation level *)
  | OUT              (* Decreases the current indentation level *)
  | T0 of string     (* Inserts a string with the same indentation as the line preceding the template *)
  | TI of string     (* Inserts a string with the current indentation level *)
  | T of string      (* Inserts a string without indenting *)
  | NL               (* Inserts a newline *)
  | CURRENT_LINE
  | CURRENT_FILENAME (* Inserts the name of the currently open file *)
  | DESCRIPTION      (* Project description *)
  | I                (* Represents the insertion point *)
  | S                (* Represents the selection bound *)

and spec = scope * string * string * t

let sort = List.sort (fun (_, x1, _, _) (_, x2, _, _) -> Stdlib.compare x1 x2);;

let blanks = [13;10;32;9]
let is_blank c = List.mem c blanks
let is_not_blank c = not (List.mem c blanks)

module Action = struct
  let get_indent ~(buffer : GText.buffer) ~(iter : GText.iter) =
    let bol = iter#set_line_index 0 in
    let bot = bol#forward_find_char is_not_blank in
    bot#line_index;;

  let get_text_within_delimiters view =
    let view = (view :> Text.view) in
    let where = view#buffer#get_iter `INSERT in
    view#matching_delim_goto ~select:true ();
    let start, stop = view#buffer#selection_bounds in
    let text_within_delimiters = view#buffer#get_text ~start ~stop () in
    view#buffer#place_cursor ~where;
    view#matching_delim_goto ~select:true ~strict:false ();
    let start, stop = view#buffer#selection_bounds in
    view#matching_delim_remove_tag ();
    view#buffer#place_cursor ~where;
    start, stop, text_within_delimiters;;

  let remove_delimiters view =
    let start, stop, text_within_delimiters = get_text_within_delimiters view in
    view#buffer#delete ~start ~stop;
    let text_within_delimiters = String.trim text_within_delimiters in
    view#buffer#insert text_within_delimiters;
    view#matching_delim_remove_tag ();
    let iter = view#buffer#get_iter `INSERT in
    view#buffer#select_range iter (iter#backward_chars (Glib.Utf8.length text_within_delimiters));;

  let be_parent view =
    let start, stop, text_within_delimiters = get_text_within_delimiters view in
    view#buffer#delete ~start ~stop;
    let text = String.trim text_within_delimiters in
    let text = if text.[String.length text - 1] = ';' then String.sub text 0 (String.length text - 1) else text in
    let new_text = "(" ^ text ^ ")" in
    view#buffer#insert new_text;
    let iter = view#buffer#get_iter `INSERT in
    view#buffer#select_range iter (iter#backward_chars (Glib.Utf8.length new_text));;

  let be_unparent view =
    let start, stop, text_within_delimiters = get_text_within_delimiters view in
    let indent = get_indent ~buffer:view#buffer ~iter:start in
    let d1 = start#get_text ~stop:start#forward_char in
    let d2 = stop#get_text ~stop:stop#backward_char in
    if d1 = "(" && d2 = ")" then begin
      let break_after_arrow =
        let start = start#forward_char in
        let stop = stop#backward_char in
        let eol = start#forward_to_line_end in
        match start#forward_search ~limit:eol "->" with
        | Some (_, arrow) ->
            let break = arrow#forward_find_char ~limit:eol is_not_blank in
            let has_break = break#equal eol in
            let break = if has_break then break#forward_char else break in
            let t1 = start#get_text ~stop:break in
            let t2 = break#get_text ~stop in
            Some (Utils.rtrim t1, t2, has_break)
        | _ -> None
      in
      view#buffer#delete ~start ~stop;
      let indent_step = Alignment.mk_spaces view#tbuffer#tab_width in
      view#buffer#delete ~start ~stop;
      let spaces = Alignment.mk_spaces indent in
      let new_text =
        match break_after_arrow with
        | None ->
            "begin\n" ^ spaces ^ indent_step ^
            (Str.global_replace (!~~ "\n") ("\n" ^ indent_step) text_within_delimiters) ^ "\n" ^
            spaces ^ "end"
        | Some (t1, t2, has_break) ->
            "begin " ^  t1 ^ "\n" ^
            (if has_break then t2 else
               spaces ^ indent_step ^ (Str.global_replace (!~~ "\n") ("\n" ^ indent_step) t2)) ^
            "\n" ^ spaces ^ "end"
      in
      view#buffer#insert new_text;
      let iter = view#buffer#get_iter `INSERT in
      view#buffer#select_range iter (iter#backward_chars (Glib.Utf8.length new_text));
    end;;

  let align view =
    if view#buffer#has_selection then begin
      let start, stop = view#buffer#selection_bounds in
      Alignment.align ~buffer:view#buffer ~start ~stop;
    end;;

  let case_analysis (view : Ocaml_text.view) =
    let start, stop = view#buffer#selection_bounds in
    let filename = match (view#obuffer#as_text_buffer :> Text.buffer)#file with Some file -> file#filename | _ -> "" in
    let open Merlin_j in
    Merlin.case_analysis ~start ~stop
      ~filename
      ~buffer:(view#buffer#get_text ())
    |> Async.start_with_continuation begin function
    | Merlin.Ok (range, text) ->
        GtkThread.async begin fun () ->
          let start = view#buffer#get_iter (`LINECHAR (range.start.line - 1, range.start.col)) in
          let stop = view#buffer#get_iter (`LINECHAR (range.stop.line - 1, range.stop.col)) in
          view#buffer#delete ~start ~stop |> ignore;
          view#buffer#insert text |> ignore;
        end ()
    | Merlin.Failure _ | Merlin.Error _ -> ()
    end

end

(** spec
    scope * template_name * description * code
*)
let spec : spec list ref =
  (** Actions *)
  let actions : ((string * string * (Ocaml_text.view -> unit)) list) = [
    "() remove", "Remove delimiters", Action.remove_delimiters;
    "be()", "Replace currently highlighted delimiters with parentheses", Action.be_parent;
    "be)(", "Replace currently highlighted parentheses with \"begin ... end\"", Action.be_unparent;
    "align", "Align/collapse definitions", Action.align;
    (*"meroutline", "Merlin outline", Merlin.outline;*)
    "matcase", "(match <selection> with ...case-analysis of the selected expression)", Action.case_analysis;
  ] in
  (** Templates *)
  let templates = [
    "()", "(<selection>)", [T "("; SELECTION_TRIM ; T ")"; I];

    "descr", "Insert project description", [DESCRIPTION];
    "curf",  "Insert current filename", [CURRENT_FILENAME];

    "be", "begin <selection> end",
    [T0 "begin"; NL; IN; SELECTION; OUT; T0 "end;"; I; NL];

    "ifthen", "if ... then begin <selection> end else begin ... end;", [
      T0 "if "; I; T " then begin"; NL;
      IN; SELECTION; OUT;
      T0 "end else begin"; NL;
      TI "end;"; NL;
    ];

    "ifte", "if <selection> then begin ... end else begin ... end", [
      T "if "; I; SELECTION; S; T " then begin"; NL;
      IN; TI ""; NL; OUT;
      TI "end else begin"; NL;
      IN; TI ""; NL; OUT;
      TI "end;";
    ];

    "ign@@", "ignore @@ <selection>", [T "ignore @@ "; SELECTION; T ";"; I];
    "ign |>", "<selection> |> ignore;", [SELECTION; T " |> ignore;"; I];

    "ignbe", "ignore begin ... end",
    [T0 "ignore begin"; NL; IN; SELECTION; OUT; T0 "end;"; I; NL];

    "letfun", "let <selection> = function ...", [
      T "let "; SELECTION_TRIM; T " = function "; NL;
      IN; TI "| "; I; NL;
      TI "| _ -> "
    ];

    "letfunin", "let <selection> = function ... in", [
      T "let "; SELECTION_TRIM; T " = function "; NL;
      IN; TI "| "; I; T " -> "; NL;
      TI "| _ -> "; NL;
      OUT; TI "in";
    ];

    "errback", "Print backtrace on standard error", [
      T "Printf.eprintf \"File \\\""; CURRENT_FILENAME; T"\\\": %s\\n%s\\n%!\" (Printexc.to_string "; I;
      SELECTION_OPT "ex"; S; T ") (Printexc.get_backtrace());"
    ];

    "errassert", "if not (<selection>) then begin eprintf ...; assert false end;", [
      T "if not ("; SELECTION_TRIM; T ") then begin"; NL;
      IN; TI "Printf.eprintf \""; I; T"Assertion failed: "; SELECTION_TRIM; T""; S; T"\\n%!\";"; NL;
      TI "assert false;"; OUT; NL;
      TI "end;";
    ];

    "print", "Printf.printf \"<selection> = %s\\n%!\" <selection>;",
    [T "Printf.printf \""; SELECTION_TRIM; T " = %"; I; T "s"; S; T "\\n%!\" "; SELECTION_TRIM; T ";"];

    "matbe", "begin match ... with ... -> <selection> | _ -> ... end;", [
      T0 "begin"; NL;
      IN; TI "match "; I; T" with"; NL;
      IN; TI "| "; T" ->"; NL;
      IN; SELECTION;
      OUT; TI "| _ -> "; NL;
      OUT; OUT; T0 "end;"; NL
    ];

    "matsel", "match <selection> with ...", [
      T "match "; SELECTION_TRIM; T " with "; NL;
      IN; TI "| "; I; T" -> "; NL;
      TI "| _ -> "; NL
    ];

    "matsome", "match <selection> with | Some x -> ... | _ -> ...",
    [T "match "; SELECTION_TRIM; T " with "; NL; IN; TI "| Some x -> "; NL; TI "| _ -> "; I; NL];

    "matsomeinline", "match <selection> with Some x | _ -> ...",
    [T "match "; SELECTION_TRIM; T " with Some x -> "; I; T " | _ -> "];

    "try", "try <selection> with ...",
    [T0 "try"; NL; IN; SELECTION; OUT; T0 "with "; I; T " -> "; NL];

    "tryf", "let finally = ... in try <slection>; finally() with ex -> (finally())", [
      T0 "let finally () = "; I; T " in"; NL;
      T0 "try"; NL;
      IN; SELECTION; TI "finally();"; NL;
      OUT; T0 "with ex -> (finally())"; NL
    ];

    "trybe", "begin try <selection> with _ -> ... end", [
      T0 "begin"; NL;
      IN; TI "try"; NL;
      IN; SELECTION;
      OUT; TI "with "; I; T " -> "; NL;
      OUT; T0 "end;"; NL
    ];

    "callback", "~callback:begin fun () -> ... end |> ignore",
    [T " ~callback:begin fun () ->"; IN; NL; TI ""; I; NL; OUT; TI "end |> ignore;"];

    "for", "for ... do ... done", [
      TI "for i = 0 to "; I; T " do"; NL;
      IN; SELECTION; OUT;
      T0 "done;"; NL;
    ];

    "while", "while ... do ... done", [
      TI "while "; I; T " do"; NL;
      IN; SELECTION; OUT;
      T0 "done;"; NL;
    ];

    "TODO", "(* TODO: <selection> *)", [ T"(* TODO: "; I; SELECTION; S; T" *)" ];

    "q", "(* <selection> *)", [T "(*"; SELECTION; T "*)"];
    "qb", "(* <block> *)", [T0 "(*"; NL; SELECTION; T0 "*)"; NL];
    "qbi", "(* <block> *)", [T0 "(*"; NL; IN; SELECTION; OUT; T0 "*)"; NL];

    "qq", "(** <selection> *)", [T "(**"; SELECTION; T "*)"];
    "qqb", "(** <block> *)", [T0 "(**"; NL; SELECTION; T0 "*)"; NL];
    "qqbi", "(** <block> *)", [T0 "(**"; NL; IN; SELECTION; OUT; T0 "*)"; NL];
  ] in
  let actions = List.map (fun (a, b, c) -> Application, a, b, Action c) actions in
  let templates = List.map (fun (a, b, c) -> Application, a, b, Templ c) templates in
  ref (sort (actions @ templates))






