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

let glib_is_utf8, glib_charset = Glib.Convert.get_charset()

let locale_is_utf8, locale_charset =
  if true then glib_is_utf8, glib_charset else begin
    let charset =
      match Shell.get_command_output "locale -c LC_CTYPE | head -6 | tail -1" with
      | line :: _ -> line
      | _ -> ""
    in
    (charset = "UTF-8"), charset
  end

let is_utf8, default_charset = locale_is_utf8, locale_charset

let to_utf8 str =
  if Glib.Utf8.validate str then str else
    try
      Glib.Convert.convert ~from_codeset:default_charset ~to_codeset:"UTF-8" str
    with Glib.Convert.Error _ ->
      (* Fallback: replace bytes outside ASCII with '?' to avoid exceptions when
         conversion fails due to invalid byte sequences. This preserves display
         albeit with lossy replacement. *)
      String.init (String.length str) (fun i -> let c = str.[i] in if Char.code c <= 127 then c else '?')

let from_utf8 = Glib.Convert.convert ~from_codeset:"UTF-8" ~to_codeset:default_charset

let offset_from_pos text ~pos =
  if Glib.Utf8.validate text then (Glib.Utf8.length (String.sub text 0 pos)) else pos

let offset_to_pos = Glib.Utf8.offset_to_pos









