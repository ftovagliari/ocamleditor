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
[@@@warning "-48"]

(* Vertical space is scarce, so at most one expander is open at a time and the
   open one gets the extra space. Each item pairs an expander with a callback
   that syncs its decorations to its expanded state. *)
let link_expanders (vbox : GPack.box) items =
  let items = Array.of_list items in
  let sync () =
    Array.iter begin fun (expander, on_sync) ->
      let expanded = expander#expanded in
      vbox#set_child_packing ~expand:expanded ~fill:expanded expander#coerce;
      on_sync expanded
    end items
  in
  Array.iteri begin fun i (expander, _) ->
    ignore (expander#connect#after#activate ~callback:begin fun () ->
        if expander#expanded then
          Array.iteri (fun j (other, _) -> if j <> i then other#set_expanded false) items;
        sync ()
      end)
  end items;
  sync ()

(* An expander holding a plain (enabled, value) list, with an optional hint
   shown only while it is open. *)
let add_list_expander (vbox : GPack.box) ?help ?(expanded=false) ~title () =
  let expander = GBin.expander ~expanded ~packing:vbox#add ~show:true () in
  let lbox     = GPack.hbox ~spacing:8 () in
  let _        = GMisc.label ~markup:title ~xalign:0.0 ~packing:lbox#add () in
  let hint     = Option.map (fun markup -> GMisc.label ~markup ~xalign:1.0 ~packing:lbox#pack ()) help in
  let entry    = Entry_list_args.create ~packing:expander#add () in
  expander#set_label_widget lbox#coerce;
  let on_sync expanded =
    match hint with
    | Some label -> if expanded then label#misc#show () else label#misc#hide ()
    | None -> ()
  in
  expander, entry, on_sync

let add_env_expander (vbox : GPack.box) =
  let expander = GBin.expander ~packing:vbox#add () in
  let lbox     = GPack.hbox ~spacing:8 () in
  let _        = GMisc.label ~markup:"Environment Variables" ~xalign:0.0 ~packing:lbox#add () in
  let hint     = GMisc.label ~markup:"(<small><tt>NAME=VALUE</tt></small>)" ~xalign:1.0 ~packing:lbox#pack () in
  let entry    = Entry_list_env.create ~packing:expander#add () in
  expander#set_label_widget lbox#coerce;
  let on_sync expanded = if expanded then hint#misc#show () else hint#misc#hide () in
  expander, entry, on_sync

let args_title = "Command Line Arguments"
let path_hint  = "(<small>relative to the project source path</small>)"

let create (vbox : GPack.box) =
  let expander_args, entry_args, sync_args = add_list_expander vbox ~expanded:true ~title:args_title () in
  let expander_env, entry_env, sync_env = add_env_expander vbox in
  link_expanders vbox [expander_args, sync_args; expander_env, sync_env];
  entry_args, entry_env

(* As [create], plus the two lists that let a task declare what it produces and
   reads, which is what makes it expressible as a build rule. *)
let create_task (vbox : GPack.box) =
  let expander_args, entry_args, sync_args = add_list_expander vbox ~expanded:true ~title:args_title () in
  let expander_out, entry_out, sync_out =
    add_list_expander vbox ~help:path_hint ~title:"Rule Outputs" () in
  let expander_deps, entry_deps, sync_deps =
    add_list_expander vbox ~help:path_hint ~title:"Rule Dependencies" () in
  let expander_env, entry_env, sync_env = add_env_expander vbox in
  link_expanders vbox [
    expander_args, sync_args;
    expander_out,  sync_out;
    expander_deps, sync_deps;
    expander_env,  sync_env;
  ];
  entry_args, entry_env, entry_out, entry_deps

