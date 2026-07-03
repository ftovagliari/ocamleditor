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

(** get_locale *)
let get_locale () =
  try
    let lines = Shell.get_command_output "locale" in
    let locale =
      List.find (fun l -> Str.string_match (Str.regexp ".*=.+") l 0) lines
    in
    Str.string_match (Str.regexp ".*=\\(.*\\)") locale 0 |> ignore;
    Some (Str.matched_group 1 locale)
  with ex ->
    Printf.eprintf "File \"utils.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());
    None

(** findlib_package_exists *)
let findlib_package_exists packagename =
  let cmd = sprintf "ocamlfind query -format %%v %s %s" packagename Shell.redirect_stderr in
  match Shell.get_command_output cmd with
  | line :: _ when line = "" -> Some "OK"
  | line :: _ -> Some line
  | _ -> None

(** print *)
let to_string () =
  let buf = Buffer.create 1000 in
  let copyright = Str.replace_first (Str.regexp_string "©") "(c)" About.copyright in
  Printf.bprintf buf "\n%s %s\n\n%!" About.program_name About.version (*copyright*);
  let a, b, c = GMain.Main.version in
  Printf.bprintf buf "-------------------------------------------------------------------------------\n%!" ;
  let groups = [
    10, "ocaml";
    20, "ocamleditor";
    30, "msvc";
    40, "gtk";
    50, "package";
    60, "system";
    70, "command";
  ] in
  let properties = [
    10, "version", Ocaml_config.get "version";
    10, "system", Ocaml_config.get "system";
    10, "standard_library", Ocaml_config.get "standard_library";
    10, "ccomp_type", Ocaml_config.get "ccomp_type";
    20, "user_home", App_config.ocamleditor_user_home;
    20, "executable", Sys.executable_name;
    20, "icons", App_config.application_icons;
    20, "plugins", App_config.application_plugins;
    20, "oebuild", Oe_config.oebuild_command;
    20, "native_compilation", (match Ocaml_config.can_compile_native () with Some x -> "Yes (" ^ x ^ ")" | _ -> "No");
    70, "xdg-open", (Option.value Oe_config.xdg_open_version ~default:"<Not Found>");
    70, "dot", (Option.value Oe_config.dot_version ~default:"<Not Found>");
    50, "findlib", (Option.value (findlib_package_exists "findlib") ~default:"<Not Found>");
    50, "lablgtk2", (Option.value (findlib_package_exists "lablgtk2") ~default:"<Not Found>");
    50, "ocamldiff", (Option.value (findlib_package_exists "ocamldiff") ~default:"<Not Found>");
    50, "lablgtk2.rsvg", (Option.value (findlib_package_exists "lablgtk2.rsvg") ~default:"<Not Found>");
    50, "curl", (Option.value (findlib_package_exists "curl") ~default:"<Not Found>");
    70, "git", (Option.value Oe_config.git_version ~default:"<Not Found>");
    40, "version", (sprintf "%d.%d.%d" a b c);
    60, "locale", (Option.value (get_locale ()) ~default:"<Not Found>");
    60, "charset", (let x, charset = Glib.Convert.get_charset () in sprintf "%b, %s" x charset);
    20, "backtrace_status", (sprintf "%b" (Printexc.backtrace_status ()));
  ] in
  let properties = List.sort compare properties in
  let properties = List.map (fun (g, n, v) -> (List.assoc g groups) ^ "." ^ n, v) properties in
  List.iter (bprintf buf "%s\n") (Text_util.dot_leaders properties);
  Printf.bprintf buf "-------------------------------------------------------------------------------\n%!" ;
  Buffer.contents buf
