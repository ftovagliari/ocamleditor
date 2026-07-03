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

open Build_script
open Task
open Target
open Printf
open Option_syntax

class widget kind ~label ~project ?packing () =
  let vbox        = GPack.vbox ~spacing:13 ?packing () in
  let box         = GPack.vbox ~spacing:3 ~packing:vbox#pack () in
  let _           = GMisc.label ~markup:label ~xalign:0.0 ~packing:box#pack () in
  let cols        = new GTree.column_list in
  let col_pixbuf  = cols#add (Gobject.Data.gobject_by_name "GdkPixbuf") in
  let col_name    = cols#add Gobject.Data.string in
  let col_descr   = cols#add Gobject.Data.string in
  let col_task    = cols#add Gobject.Data.caml in
  let model       = GTree.list_store cols in
  let combo       = GEdit.combo_box ~model ~packing:box#pack () in
  let rend_pixbuf = GTree.cell_renderer_pixbuf [] in
  let rend        = GTree.cell_renderer_text [`XPAD 5; `WIDTH 250] in
  let rend_descr  = GTree.cell_renderer_text [] in
  let _           = combo#pack rend_pixbuf in
  let _           = combo#pack rend in
  let _           = combo#pack rend_descr in
  let _           = combo#add_attribute rend_pixbuf "pixbuf" col_pixbuf in
  let _           = combo#add_attribute rend "markup" col_name in
  let _           = combo#add_attribute rend_descr "markup" col_descr in
  let targets     = project.Prj.targets in
  object (self)
    inherit GObj.widget vbox#as_widget

    initializer
      let open Preferences in
      let row = model#append () in
      model#set ~row ~column:col_name "<span>&lt;None&gt;</span>";
      model#set ~row ~column:col_descr "";
      model#set ~row ~column:col_task None;
      List.iter begin fun tg ->
        List.iter begin fun task ->
          let row = model#append () in
          model#set ~row ~column:col_name (sprintf
                                             "<span size='small' color='%s'>%s</span>\n<span>%s</span>"
                                             ?? Oe_config.module_browser_secondary_title_color
                                             (Glib.Markup.escape_text tg.name)
                                             (Glib.Markup.escape_text task.et_name));
          model#set ~row ~column:col_descr (sprintf "<tt>%s</tt>" (Glib.Markup.escape_text (self#string_of_task task)));
          model#set ~row ~column:col_task (Some (tg.id, task.et_name));
        end tg.external_tasks
      end targets;
      self#set project

    method private string_of_task task =
      sprintf "%s %s%s"
        task.et_cmd
        (if task.et_dir <> "" then task.et_dir ^ "/" else "")
        (String.concat " " (List.map (fun (active, arg) -> if active then arg else "") task.et_args))

    method set project =
      project.Prj.build_script.bs_commands
      |> List.find_map begin fun command ->
        if command.bsc_name = kind then
          Some begin
            model#foreach begin fun _ row ->
              match model#get ~row ~column:col_task with
              | Some (target_id, task_name) ->
                  if target_id = command.bsc_target.id && task_name = command.bsc_task.et_name then begin
                    combo#set_active_iter (Some row);
                    true
                  end else false
              | _ -> false
            end
          end
        else None
      end |> ignore;
      match combo#active_iter with None -> combo#set_active 0 | _ -> ()

    method get : unit -> Build_script.command option = fun () ->
      let+ row = combo#active_iter in
      let+ (target_id, task_name) = model#get  ~row ~column:col_task in
      let+ bsc_target = Prj.find_target project target_id in
      let+ bsc_task = Prj.find_task project task_name in
      Some { bsc_name = kind
           ; bsc_descr = task_name
           ; bsc_target
           ; bsc_task
           }
  end




