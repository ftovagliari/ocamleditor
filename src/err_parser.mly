/*

  OCamlEditor
  Copyright (C) 2010 Francesco Tovagliari

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

*/

%{
open Printf
open Oe
%}

%token <string * string * int * int * int *int> LOCATION
%token <string> LINE_OF_MESSAGE
%token <string> ALERT
%token EOF ERROR
%token <int * string option> WARNING

%start compiler_output
%type <Oe.error_message list> compiler_output
%%
compiler_output:
  | /* empty */                           { [] }
  | message compiler_output               { $1 :: $2 }
;
message:
  | LOCATION level line_of_message {
    let message_text = String.concat "\n" $3 in
    let loc, filename, line0, line1, c1, c2 = $1 in {
      er_filename = filename;
      er_lines = (line0, line1);
      er_characters = (c1, c2);
      er_level = $2;
      er_location = loc;
      er_message = (
      (
        match $2 with
        | Error -> "Error: "
        | Warning (num, None) -> sprintf "Warning %d: " num (* before 4.12 *)
        | Warning (num, Some name) -> sprintf "Warning %d [%s]: " num name
        | Alert s -> "Alert " ^ s ^ ": "
      ) ^ message_text);
    }
  }
  | LOCATION line_of_message {
    let message_text = String.concat "\n" $2 in
    let loc, filename, line0, line1, c1, c2 = $1 in {
      er_filename = filename;
      er_lines = (line0, line1);
      er_characters = (c1, c2);
      er_level = Error;
      er_location = loc;
      er_message = message_text;
     }
  }
;
line_of_message:
  | /* empty */                     { [] }
  | LINE_OF_MESSAGE line_of_message { $1 :: $2}
;
level:
  | WARNING { let n, so = $1 in Warning (n, so) }
  | ALERT { Alert $1 }
  | ERROR   { Error }
;





