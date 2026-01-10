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


#cd "../../src";;
#use "../../../tools/scripting.ml";;

open Printf

let generate ochan path =
  let icons = ".." // ".." // ".." // "icons" // path in
  let files =
    icons
    |> Sys.readdir
    |> Array.to_list
    |> List.sort Stdlib.compare
    |> List.filter (fun x -> Filename.check_suffix x ".png")
  in
  try
    fprintf ochan "module %s = struct\n" (String.capitalize_ascii path);
    fprintf ochan "  let path = App_config.application_icons // \"%s\"\n" path;
    let icon_names =
      files |>
      List.map begin fun file ->
        let new_name = Str.global_replace (Str.regexp "-") "_" file in
        Sys.rename (icons // file) (icons // new_name);
        let icon_name = Filename.basename (Filename.chop_extension new_name) in
        fprintf ochan "  let %s = GdkPixbuf.from_file (path // \"%s\")\n"
          icon_name new_name;
        icon_name
      end;
    in
    fprintf ochan "end\n\n";
    icon_names
  with _ ->
    [];;

let mkicons () =
  if not (Sys.file_exists "icons") then (mkdir "icons");
  let filename = "icons/icons.ml" in
  let ochan = open_out_gen [Open_creat; Open_trunc; Open_append; Open_binary] 0o644 filename in
  try
    fprintf ochan "type themed_icon = { light : GdkPixbuf.pixbuf; dark : GdkPixbuf.pixbuf }\n\n";
    fprintf ochan "let (//) = Filename.concat\n\n";
    fprintf ochan "let create pixbuf = GMisc.image ~pixbuf ()\n\n";
    let light_names = generate ochan "light" in
    let dark_names = generate ochan "dark" in
    assert (List.length light_names = List.length dark_names);
    light_names
    |> List.iter begin fun name ->
      fprintf ochan "let %s = Light.%s, Dark.%s\n" name name name;
    end;
    close_out_noerr ochan;
  with ex ->
    begin
      Printf.eprintf "File \"mkicons.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());
      close_out_noerr ochan
    end

let _ = main ~default_target:mkicons ~options:[] ()

