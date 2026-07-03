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

let empty = {Oe.er_warnings = []; er_errors = []}

let create messages =
  let errors, warnings = List.partition (fun x -> x.Oe.er_level = Oe.Error) messages in
  let errors = List.rev errors in
  let warnings = List.rev warnings in
  {Oe.er_warnings = warnings; er_errors = errors}

let parse_string buf =
  let lexbuf = Lexing.from_string (String.trim buf) in
  try
    let messages = Err_parser.compiler_output Err_lexer.token lexbuf in
    create messages
  with Parsing.Parse_error ->
    eprintf "Parsing.Parse_error: <<<\n%s\n>>>\n%!" buf;
    empty

let parse chan =
  let lexbuf = Lexing.from_channel chan in
  let messages = Err_parser.compiler_output Err_lexer.token lexbuf in
  create messages





