(*

  OCamlEditor
  Copyright (C) 2010-2012 Francesco Tovagliari

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


type t = {
  bsa_type    : bsa_type;
  bsa_key     : string;
  bsa_doc     : string;
  bsa_default : [ `flag of bool | `bool of bool | `string of string ];
  bsa_task    : Bconf.t * Task.t;
  bsa_mode    : [`add | `replace of string];
  bsa_pass    : [ `key | `value | `key_value ];
}
and bsa_type = Flag_Set | Flag_Clear | Bool | String

let string_of_add = "<ADD>"

let string_of_type = function
  | Flag_Set -> "Flag_Set"
  | Flag_Clear -> "Flag_Clear"
  | Bool -> "Bool"
  | String -> "String"

let type_of_string = function
   | "Flag_Set" -> Flag_Set
   | "Flag_Clear" -> Flag_Clear
   | "Bool" -> Bool
   | "String" -> String
   | _ -> invalid_arg "type_of_string"

let string_of_pass = function
   | `key -> "-key"
   | `value -> "value"
   | `key_value -> "-key value"

let pass_of_string = function
   | "-key" -> `key
   | "value" -> `value
   | "-key value" -> `key_value
   | _ -> invalid_arg "pass_of_string"

