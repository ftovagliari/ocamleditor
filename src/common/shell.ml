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


let redirect_stderr = " 2>/dev/null"

(** get_command_output *)
let get_command_output command =
  let ch = Unix.open_process_in command in
  set_binary_mode_in ch false;
  let output = ref [] in
  try
    while true do output := (input_line ch) :: !output done;
    assert false
  with End_of_file -> begin
      ignore (Unix.close_process_in ch);
      List.rev !output
    end | e -> begin
      ignore (Unix.close_process_in ch);
      raise e
    end

(** quote_path *)
let quote_path = Fun.id

(** quote_arg *)
let quote_arg = Fun.id

type state = StartArg | InUnquotedArg | InQuotedArg | InQuotedArgAfterQuote;;

let format_args = String.concat " ";;

let parse_args line =
  let args = ref [] in
  let buf = Buffer.create 10 in
  let state = ref StartArg in
  let start_arg () =
    state := StartArg;
    args := (Buffer.contents buf) :: !args;
    Buffer.clear buf;
  in
  String.iter begin function
  | (' ' as ch) when !state = InQuotedArg -> Buffer.add_char buf ch
  | ' ' when !state = StartArg -> ()
  | ' ' when !state = InUnquotedArg -> start_arg ();
  | ' ' -> start_arg ()
  | ('"' as ch) when !state = StartArg ->
      state := InQuotedArg;
      Buffer.add_char buf ch
  | ('"' as ch) when !state = InQuotedArg ->
      Buffer.add_char buf ch;
      start_arg ();
  | ('"' as ch) when !state = InQuotedArgAfterQuote ->
      Buffer.add_char buf ch;
      state := InQuotedArg;
  | ('"' as ch) when !state = InUnquotedArg ->
      start_arg ();
      Buffer.add_char buf ch;
      state := InQuotedArg;
  | ('\\' as ch) when !state = InQuotedArg ->
      state := InQuotedArgAfterQuote;
      Buffer.add_char buf ch
  | ch when !state = InQuotedArgAfterQuote ->
      state := InQuotedArg;
      Buffer.add_char buf ch;
  | ch when !state = StartArg ->
      state := InUnquotedArg;
      Buffer.add_char buf ch;
  | ch -> Buffer.add_char buf ch;
  end line;
  if Buffer.length buf > 0 then (start_arg ());
  List.rev !args;;

(*

parse "";;
parse "a b c";;
parse " a b c ";;
parse "a \"b\" c ";;
parse "a \"b b\" c";;
parse "a \"b\"\" x\" c \"\"\"\" ";;
parse "1 \"2 \\\" 2\"";;

*)
