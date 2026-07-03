(*

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

*)

{
open Err_parser
exception Eof

let warning_num_of_letter = function
  | 'C' -> 1 (*, 2 *)
  | 'D' -> 3
  | 'E' -> 4
  | 'F' -> 5
  | 'L' -> 6
  | 'M' -> 7
  | 'P' -> 8
  | 'R' -> 9
  | 'S' -> 10
  | 'U' -> 11 (*, 12 *)
  | 'V' -> 13
  | 'X' -> 14 (*, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25 *)
  | 'Y' -> 26
  | 'Z' -> 27
  | _ -> 0
}

let lf = '\x0A'
let sp = '\x20'

rule token = parse
  | ("File" sp '"' ([^'"']+ as filename) "\", line " (['0'-'9']+ as line) ", characters " (['0'-'9']+ as a) '-' (['0'-'9']+ as b) ':' lf) as loc
    {LOCATION (loc, filename, (int_of_string line), (int_of_string line), (int_of_string a), (int_of_string b))}

  | ("File" sp '"' ([^'"']+ as filename) "\", lines " (['0'-'9']+ as line0) '-' (['0'-'9']+ as line1) ", characters " (['0'-'9']+ as a) '-' (['0'-'9']+ as b) ':' lf) as loc
    {LOCATION (loc, filename, (int_of_string line0), (int_of_string line1), (int_of_string a), (int_of_string b))}

  | ("File" sp '"' ([^'"']+ as filename) "\", line " (['0'-'9']+ as line) ':' lf) as loc
    {LOCATION (loc, filename, (int_of_string line), (int_of_string line), 0, 0)}

  | "Warning" (sp ((['0'-'9']+) as wtype))? sp '[' ((['a'-'z']|'-')+ as name) ']' ':' sp
      { WARNING ((match wtype with None -> 0 | Some x -> int_of_string x), Some name)}

  | "Warning" (sp ((['0'-'9']+) as wtype))? ':' sp
      { WARNING ((match wtype with None -> 0 | Some x -> int_of_string x), None)}

  | "Warning" (sp ((['A'-'Z']) as wtype))? ':' sp
      { WARNING ((match wtype with None -> 0 | Some x -> warning_num_of_letter x), None)}

  | "Alert" sp (([^':']+) as msg) ':' sp { ALERT msg }

  | "Error:" sp { ERROR }

  | eof { EOF }
  | _ as x {
      let buffer = Buffer.create 100 in
      Buffer.add_char buffer x;
      line_of_message buffer lexbuf
    }

and line_of_message buffer = parse
  | eof                           { LINE_OF_MESSAGE (Buffer.contents buffer) }
  | lf +                          { LINE_OF_MESSAGE (Buffer.contents buffer) }
  | _                             {
      Buffer.add_string buffer (Lexing.lexeme lexbuf);
      line_of_message buffer lexbuf
    }

