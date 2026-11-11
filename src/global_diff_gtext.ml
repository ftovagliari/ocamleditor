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
open Preferences

(** insert *)
let insert (buffer : GText.buffer) ignore_whitespace filename1 filename2 =
  let diffs = ref [] in
  let process_in ic =
    try diffs := Diff_parse.from_channel ic
    with ex -> (Printf.eprintf "File \"plugin_diff.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace()); diffs := [] ) ;
  in
  let diff = Preferences.preferences#get.program_diff in
  let args =
    [ if ignore_whitespace then "--ignore-all-space" else ""; filename1; filename2 ]
    |> List.filter (fun x -> x <> "") |> Array.of_list
  in
  Spawn.async diff args
    ~process_in
    ~at_exit:begin fun _ ->
      let tag_del = Text_tag.main.Text_tag.diff_delete in
      let tag_add = Text_tag.main.Text_tag.diff_add in
      let open Diff_parse in
      let insert_lines prefix l1 text =
        let lines = Str.split (Str.regexp "\n") text in
        let i = ref l1 in
        let tags = if prefix = "-" then [tag_del] else [tag_add] in
        List.iter begin fun l ->
          buffer#insert ~tags (sprintf "#%-5d %s\n" !i l);
          incr i;
        end lines
      in
      Gmisclib.Idle.add ~prio:100 buffer#begin_user_action;
      List.iteri begin fun i diff ->
        Gmisclib.Idle.add ~prio:200 begin fun () ->
          begin
            match diff with
            | Add (_, One ln, line) ->
                buffer#insert ~tags:[tag_add] (sprintf "#%-5d %s\n" ln line);
            | Add (_, Many (l1, _), text) ->
                insert_lines "+" l1 text
            | Delete (_, One ln, text) ->
                buffer#insert ~tags:[tag_del] (sprintf "#%-5d\n%s\n" ln text);
            | Delete (_, Many (l1, _), text) ->
                insert_lines "-" l1 text
            | Change (_, a, One ln, b) ->
                buffer#insert (sprintf "#%-5d\n" ln);
                buffer#insert ~tags:[tag_del] (sprintf "%s\n" a);
                buffer#insert ~tags:[tag_add] (sprintf "%s\n" b);
            | Change (_, a, Many (l1, _), b) ->
                insert_lines "-" l1 a;
                insert_lines "+" l1 b;
          end;
          buffer#insert "(...)\n";
          if i = (List.length !diffs) - 1 then
            buffer#end_user_action ();
        end;
      end !diffs;
    end |> ignore
