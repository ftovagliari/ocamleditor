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

let first_line cmd =
  let tmp = Filename.temp_file "ocamleditor" "" in
  let cmd = cmd ^ " > " ^ tmp in
  Sys.command cmd |> ignore;
  let chan = open_in_bin tmp in
  let line = try String.trim (input_line chan) with ex -> (close_in_noerr chan; raise ex) in
  close_in_noerr chan;
  if Sys.file_exists tmp then Sys.remove tmp;
  line

(*let shortname path =
  first_line (Printf.sprintf "cmd /c for %%A in (\"%s\") do @echo %%~sA" path)*)

let launch () =
  let dir = Filename.dirname Sys.executable_name in
  (*let cmd = "START /D" ^ dir ^ " /B ocamleditor.bat" in (* Rimane *)
    let cmd = "START /D" ^ dir ^ " ocamleditor.bat" in (* rimane e lampeggia *)
    let cmd = "CD " ^  dir ^ " && CMD /C ocamleditor.bat" in (* rimane *)
    let cmd = "CD " ^  dir ^ " && CMD /K ocamleditor.bat" in (* rimane *)
    let cmd = "CD " ^  dir ^ " && ocamleditor.bat" in (* rimane *)*)
  let cmd = Filename.concat dir "ocamleditor" in
  Sys.command cmd |> ignore
;;

let _ =
  let dest = App_config.launcher_filename in
  if Array.length Sys.argv > 1 then begin
    let param = Sys.argv.(1) in
    let executable = "ocamleditor" in
    let chan = open_out_gen [Open_append; Open_binary; Open_creat] 0o644 dest in
    output_string chan param;
    output_char chan '\n';
    close_out_noerr chan;
    let cmd = "ps -e | grep -w -c " ^ executable in
    let line = first_line cmd in
    let num = int_of_string (String.make 1 line.[0]) in
    if num = 0 then launch();
  end else launch();
  exit 0
;;
