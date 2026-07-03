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


#use "scripting.ml"
#directory "+unix"
#load "unix.cma"
#directory "../src/common"
#use "utils.ml"

open Printf

let _ =
  let version = Sys.argv.(1) in
  substitute ~filename:"../src/about.ml" ~regexp:true [
    "let version = \"[0-9]+[.][0-9]+[.][0-9]+\"",
      (sprintf "let version = \"%s\"" version);
  ];

  substitute ~filename:"../ocamleditor.project" ~regexp:true [
    "  <version>[0-9]+[.][0-9]+[.][0-9]+</version>",
      (sprintf "  <version>%s</version>" version);
  ];

  substitute ~filename:"../src/gmisclib/META" ~regexp:true [
    "version=\"[0-9]+[.][0-9]+[.][0-9]+\"",
      (sprintf "version=\"%s\"" version);
  ];

  substitute ~filename:"../ocamleditor.nsi" ~regexp:true [
    "OutFile \"ocamleditor-[0-9]+[.][0-9]+[.][0-9]+.exe\"",
      (sprintf "OutFile \"ocamleditor-%s.exe\"" version);
    "VIProductVersion \"[0-9]+[.][0-9]+[.][0-9]+.0\"",
      (sprintf "VIProductVersion \"%s.0\"" version);
    "VIAddVersionKey \"FileVersion\" \"[0-9]+[.][0-9]+[.][0-9]+\"",
      (sprintf "VIAddVersionKey \"FileVersion\" \"%s\"" version);
    "  WriteRegStr HKLM \"Software[\\]Microsoft[\\]Windows[\\]CurrentVersion[\\]Uninstall[\\]OCamlEditor\" \"DisplayVersion\" \"[0-9]+[.][0-9]+[.][0-9]+\"",
      (sprintf "  WriteRegStr HKLM \"Software\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\OCamlEditor\" \"DisplayVersion\" \"%s\"" version);
  ];

  substitute ~filename:"../ocamleditor+gtk.nsi" ~regexp:true [
    "OutFile \"OCamlEditor-[0-9]+[.][0-9]+[.][0-9]+-Windows.exe\"",
      (sprintf "OutFile \"OCamlEditor-%s-Windows.exe\"" version);
    "VIProductVersion \"[0-9]+[.][0-9]+[.][0-9]+.0\"",
      (sprintf "VIProductVersion \"%s.0\"" version);
    "VIAddVersionKey \"FileVersion\" \"[0-9]+[.][0-9]+[.][0-9]+\"",
      (sprintf "VIAddVersionKey \"FileVersion\" \"%s\"" version);
    "  WriteRegStr HKLM \"Software[\\]Microsoft[\\]Windows[\\]CurrentVersion[\\]Uninstall[\\]OCamlEditor\" \"DisplayVersion\" \"[0-9]+[.][0-9]+[.][0-9]+\"",
      (sprintf "  WriteRegStr HKLM \"Software\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\OCamlEditor\" \"DisplayVersion\" \"%s\"" version);
  ];

  let chan = open_out_bin "../VERSION" in
  output_string chan version;
  let commit = get_command_output "git log --no-color | head -1" in
  output_string chan "\n";
  output_string chan (String.trim (String.concat "" commit));
  close_out_noerr chan
;;
