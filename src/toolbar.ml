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
open GUtil
open Utils
open Preferences

class ['a] toolbar ~(messages : Messages.messages) ~(hmessages : Messages.messages) ~(editor : Editor.editor) () =
  let tool_messages_clicked    = new tool_messages_clicked () in
  let tool_hmessages_clicked   = new tool_hmessages_clicked () in
  let toolbar                  = GButton.toolbar ~style:`ICONS () in
  let _                        = toolbar#set_icon_size `MENU in
  let tool_new_file            = GButton.tool_button (*~stock:`NEW*) ~label:"New" () in
  let _                        = tool_new_file#set_icon_widget (GMisc.image ~pixbuf:(??? Icons.new_file) ())#coerce in
  let tool_open_file           = GButton.tool_button (*~stock:`OPEN*) ~label:"Open" () in
  let _                        = tool_open_file#set_icon_widget (GMisc.image ~pixbuf:(??? Icons.open_file) ())#coerce in
  let tool_save                = GButton.tool_button ~stock:`SAVE ~homogeneous:false ~label:"Save" () in
  let tool_save_all            = Gmisclib.Toolbar.menu_tool_button ~homogeneous:false ~toolbar ~label:"Save All" () in
  let tool_close_file          = GButton.tool_button ~label:"Close" () in
  let _                        = tool_close_file#set_icon_widget (Icons.create (??? Icons.close_16))#coerce in
  let tool_undo                = GButton.tool_button ~label:"Undo" () in
  let _                        = tool_undo#set_icon_widget (Icons.create (??? Icons.undo_16))#coerce in
  let tool_redo                = GButton.tool_button ~label:"Redo" () in
  let _                        = tool_redo#set_icon_widget (Icons.create (??? Icons.redo_16))#coerce in
  let tool_find_repl           = GButton.tool_button (*~stock:`FIND_AND_REPLACE*) ~label:"Find and Replace" () in
  let _                        = tool_find_repl#set_icon_widget (GMisc.image  ~pixbuf:(??? Icons.find_replace) ())#coerce in
  let tool_item_find_entry     = GButton.tool_item ~homogeneous:false () in
  let tool_find                = GButton.tool_button ~label:"Find" () in
  let tool_messages            = GButton.toggle_tool_button ~active:messages#visible ~label:"Messages" (*~homogeneous:false*) () in
  let _                        = tool_messages#set_icon_widget (GMisc.image ~pixbuf:(??? Icons.paned_bottom_large) ())#coerce in
  let _                        = tool_messages#set_homogeneous false in
  let tool_hmessages           = GButton.toggle_tool_button ~active:messages#visible ~label:"Messages" (*~homogeneous:false*) () in
  let _                        = tool_hmessages#set_icon_widget (GMisc.image ~pixbuf:(??? Icons.paned_right) ())#coerce in
  let tool_messages_sign       = tool_messages#connect#clicked ~callback:(fun () ->
      tool_messages_clicked#call (not messages#visible)) in
  let tool_hmessages_sign      = tool_hmessages#connect#clicked ~callback:(fun () ->
      tool_hmessages_clicked#call (not hmessages#visible)) in
  let tool_eval                = GButton.tool_button ~label:"Toplevel" () in
  let _                        = tool_eval#set_icon_widget (GMisc.image ~pixbuf:(??? Icons.toplevel) ())#coerce in
  let tool_clean               = Gmisclib.Toolbar.menu_tool_button ~toolbar ~homogeneous:false () in
  let tool_targets             = GButton.tool_button ~label:"Targets" () in
  let tool_compile_file        = GButton.tool_button ~label:"Compile File" () in
  let tool_compile             = Gmisclib.Toolbar.menu_tool_button ~toolbar ~homogeneous:false () in
  let tool_build               = Gmisclib.Toolbar.menu_tool_button ~toolbar ~homogeneous:false () in
  let tool_run                 = Gmisclib.Toolbar.menu_tool_button ~toolbar ~homogeneous:false () in
  let tool_back                = Gmisclib.Toolbar.menu_tool_button ~toolbar ~label:"Previous Location" ~homogeneous:false () in
  let _                        = tool_back#set_image (GMisc.image ~pixbuf:(??? Icons.go_back) ())#coerce in
  let tool_forward             = Gmisclib.Toolbar.menu_tool_button ~toolbar ~label:"Next Location" ~homogeneous:false () in
  let _                        = tool_forward#set_image (GMisc.image ~pixbuf:(??? Icons.go_forward) ())#coerce in
  let tool_last_edit_loc       = GButton.tool_button ~label:"Last Edit Location" () in
  let _                        = tool_last_edit_loc#set_icon_widget (GMisc.image ~pixbuf:(??? Icons.goto_last) ())#coerce in
  let tool_entry_find          = GEdit.combo_box_entry
      ~wrap_width:3
      ~focus_on_click:false
      ~model:Find_text.status.Find_text.h_find.Find_text.model
      ~text_column:Find_text.status.Find_text.h_find.Find_text.column
      ~packing:tool_item_find_entry#add ()
  in
  let _ = tool_entry_find#entry#set_text begin
      match Find_text.status.Find_text.h_find.Find_text.model#get_iter_first with
      | None -> ""
      | Some row -> Find_text.status.Find_text.h_find.Find_text.model#get ~row
                      ~column:Find_text.status.Find_text.h_find.Find_text.column
    end in
  let _ = Find_text.status.Find_text.text_find#connect#changed ~callback:tool_entry_find#entry#set_text in
  object (self)
    inherit GObj.widget toolbar#as_widget
    method children = toolbar#children
    method tool_new_file = tool_new_file
    method tool_open_file = tool_open_file
    method tool_save = tool_save
    method tool_save_all = tool_save_all
    method tool_close_file = tool_close_file
    method tool_undo = tool_undo
    method tool_redo = tool_redo
    method tool_find_repl = tool_find_repl
    method tool_entry_find = tool_entry_find
    method tool_find = tool_find
    method tool_eval = tool_eval
    method tool_compile_file = tool_compile_file
    method tool_targets = tool_targets
    method tool_back = tool_back
    method tool_forward = tool_forward
    method tool_last_edit_loc = tool_last_edit_loc
    method tool_messages_handler_block () = tool_messages#misc#handler_block tool_messages_sign
    method tool_messages_handler_unblock () = tool_messages#misc#handler_unblock tool_messages_sign
    method tool_messages_set_active = tool_messages#set_active
    method tool_hmessages_handler_block () = tool_hmessages#misc#handler_block tool_hmessages_sign
    method tool_hmessages_handler_unblock () = tool_hmessages#misc#handler_unblock tool_hmessages_sign
    method tool_hmessages_set_active = tool_hmessages#set_active


    method clean_current browser =
      browser#with_current_project (fun project ->
          browser#with_default_target begin fun target ->
            ignore (Task_console.exec ~editor `CLEAN target)
          end)

    method clean_menu browser (label, menu) =
      browser#with_current_project (fun project ->
          browser#with_default_target begin fun target ->
            label := Some (sprintf "Clean \xC2\xAB%s\xC2\xBB" target.Target.name);
            let targets = project.Prj.targets in
            List.iter begin fun tg ->
              let item = GMenu.menu_item ~label:tg.Target.name ~packing:menu#add () in
              ignore (item#connect#activate ~callback:begin fun () ->
                  ignore (Task_console.exec ~editor `CLEAN tg)
                end);
            end targets;
            let _ = GMenu.separator_item ~packing:menu#add () in
            let item = GMenu.menu_item ~label:"Clean Project" ~packing:menu#add () in
            item#connect#activate ~callback:browser#project_clean |> ignore;
          end)

    method compile_current browser =
      browser#with_current_project (fun project ->
          browser#with_default_target begin fun target ->
            ignore (Task_console.exec ~editor `COMPILE_ONLY target)
          end)

    method compile_menu browser (label, menu) =
      browser#with_current_project (fun project ->
          browser#with_default_target begin fun target ->
            label := Some (sprintf "Compile \xC2\xAB%s\xC2\xBB" target.Target.name);
            let targets = project.Prj.targets in
            List.iter begin fun tg ->
              let item = GMenu.menu_item ~label:tg.Target.name ~packing:menu#add () in
              ignore (item#connect#activate ~callback:begin fun () ->
                  ignore (Task_console.exec ~editor `COMPILE_ONLY tg)
                end);
            end targets;
          end)

    method build_current browser =
      browser#with_current_project (fun project ->
          browser#with_default_target begin fun target ->
            ignore (Task_console.exec ~editor `COMPILE target)
          end)

    method build_menu browser (label, menu) =
      browser#with_current_project (fun project ->
          browser#with_default_target begin fun target ->
            label := Some (sprintf "Build \xC2\xAB%s\xC2\xBB" target.Target.name);
            let targets = project.Prj.targets in
            List.iter begin fun tg ->
              let item = GMenu.menu_item ~label:tg.Target.name ~packing:menu#add () in
              ignore (item#connect#activate ~callback:begin fun () ->
                  ignore (Task_console.exec ~editor `COMPILE tg)
                end); ()
            end targets;
          end)

    method run_current browser =
      browser#with_current_project (fun project ->
          browser#with_default_runtime_config ~open_dialog:true (fun rc ->
              let bc = List.find (fun b -> b.Target.id = rc.Rconf.target_id) project.Prj.targets in
              ignore (Task_console.exec ~editor (`RCONF rc) bc)))

    method run_menu browser (label, menu) =
      browser#with_current_project (fun project ->
          browser#with_default_runtime_config ~open_dialog:true (fun default_rc ->
              label := Some (sprintf "Run \xC2\xAB%s\xC2\xBB" default_rc.Rconf.name);
              let targets = project.Prj.targets in
              List.iter begin fun rc ->
                let item = GMenu.menu_item ~label:rc.Rconf.name ~packing:menu#add () in
                ignore (item#connect#activate ~callback:begin fun () ->
                    try
                      let bc = List.find (fun b -> b.Target.id = rc.Rconf.target_id) targets in
                      ignore (Task_console.exec ~editor (`RCONF rc) bc)
                    with Not_found -> ()
                  end);
              end project.Prj.executables))

    initializer
      (** File *)
      toolbar#insert tool_new_file;
      toolbar#insert tool_open_file;
      toolbar#insert tool_save;
      tool_save#set_icon_widget (GMisc.image ~pixbuf:(??? Icons.save_16) ())#coerce;
      toolbar#insert tool_save_all#as_tool_item;
      tool_save_all#set_image (GMisc.image ~pixbuf:(??? Icons.save_all_16) ())#coerce;
      (*tool_save_all#connect#popup ~callback:save_all_popup#call;*)
      (** Undo/Redo *)
      let _ = GButton.separator_tool_item ~packing:toolbar#insert () in
      toolbar#insert tool_undo;
      toolbar#insert tool_redo;
      (** Find/replace *)
      let _ = GButton.separator_tool_item ~packing:toolbar#insert () in
      toolbar#insert tool_find_repl;
      toolbar#insert tool_item_find_entry;
      toolbar#insert tool_find;
      tool_find#set_icon_widget (GMisc.image ~pixbuf:(??? Icons.search_again_16) ())#coerce;
      (** Messages, eval, compile file *)
      let _ = GButton.separator_tool_item ~packing:toolbar#insert () in
      toolbar#insert tool_messages;
      toolbar#insert tool_hmessages;
      let _ = GButton.separator_tool_item ~packing:toolbar#insert () in
      toolbar#insert tool_eval;
      tool_eval#misc#set_tooltip_text "Eval in Toplevel";
      toolbar#insert tool_compile_file;
      tool_compile_file#set_icon_widget (Icons.create (??? Icons.compile_file_16))#coerce;
      let _ = GButton.separator_tool_item ~packing:toolbar#insert () in
      toolbar#insert tool_targets;
      tool_targets#set_icon_widget (Icons.create (??? Icons.target_16))#coerce;
      (** Clean, Compile, Build, Run *)
      let _ = GButton.separator_tool_item ~packing:toolbar#insert () in
      toolbar#insert tool_clean#as_tool_item;
      tool_clean#set_image (GMisc.image ~pixbuf:(??? Icons.clear_build_16) ())#coerce;
      toolbar#insert tool_compile#as_tool_item;
      tool_compile#set_image (GMisc.image ~pixbuf:(??? Icons.compile_all_16) ())#coerce;
      tool_compile#misc#set_tooltip_text "Compile only";
      toolbar#insert tool_build#as_tool_item;
      tool_build#set_image (GMisc.image ~pixbuf:(??? Icons.build_16) ())#coerce;
      tool_build#misc#set_tooltip_text "Build";
      toolbar#insert tool_run#as_tool_item;
      tool_run#set_image (GMisc.image ~icon_size:`MENU ~pixbuf:(??? Icons.start_16) ())#coerce;
      (** Location History *)
      let _ = GButton.separator_tool_item ~packing:toolbar#insert () in
      toolbar#insert tool_back#as_tool_item;
      toolbar#insert tool_forward#as_tool_item;
      toolbar#insert tool_last_edit_loc;
      let _ = GButton.separator_tool_item ~draw:false ~expand:true ~packing:toolbar#insert () in
      toolbar#insert tool_close_file;
      tool_close_file#set_icon_widget (GMisc.image ~pixbuf:(??? Icons.close_16) ())#coerce;
      ()

    method bind_signals : 'a -> unit = fun browser ->
      (** file *)
      ignore (tool_new_file#connect#clicked ~callback:browser#dialog_file_new);
      ignore (tool_open_file#connect#clicked ~callback:editor#dialog_file_open);
      ignore (tool_save#connect#clicked ~callback:begin fun () ->
          Gaux.may ~f:editor#save (editor#get_page `ACTIVE)
        end);
      ignore (tool_save_all#connect#clicked ~callback:browser#save_all);
      ignore (tool_save_all#connect#show_menu ~callback:begin fun (label, menu) ->
          List.iter begin fun p ->
            if p#view#buffer#modified then begin
              let item = GMenu.menu_item ~label:(Filename.basename p#get_filename) ~packing:menu#append () in
              ignore (item#connect#activate ~callback:p#save);
            end
          end editor#pages;
        end);
      ignore (tool_close_file#connect#clicked ~callback:(fun () ->
          editor#with_current_page (fun page -> ignore (editor#dialog_confirm_close page))));
      (** undo/redo *)
      ignore (tool_undo#connect#clicked ~callback:(fun () -> editor#with_current_page (fun p -> p#undo())));
      ignore (tool_redo#connect#clicked ~callback:(fun () -> editor#with_current_page (fun p -> p#redo())));
      (** find/replace *)
      let search_again () =
        browser#with_current_project begin fun project ->
          Find_text.update_status
            ~project
            ~text_find:tool_entry_find#entry#text ();
          Menu_search.search_again editor;
        end;
      in
      ignore (tool_find_repl#connect#clicked ~callback:(fun () ->
          Menu_search.find_replace ?find_all:None ?search_word_at_cursor:None editor));
      ignore (tool_entry_find#entry#event#connect#key_press ~callback:begin fun ev ->
          if GdkEvent.Key.keyval ev = GdkKeysyms._Return then begin
            search_again ();
            true
          end else false
        end);
      ignore (tool_find#connect#clicked ~callback:search_again);
      (** Messages, eval, compile file *)
      ignore (self#connect#tool_messages_clicked ~callback:(fun _-> ((Messages.vmessages#set_visible  (not messages#visible)) : unit)));
      ignore (self#connect#tool_hmessages_clicked ~callback:(fun _-> ((Messages.hmessages#set_visible (not hmessages#visible)) : unit)));
      ignore (tool_eval#connect#clicked ~callback:begin fun () ->
          Gaux.may ~f:(fun p -> p#ocaml_view#obuffer#send_to_shell ()) (editor#get_page `ACTIVE)
        end);
      ignore (tool_compile_file#connect#clicked ~callback:begin fun () ->
          editor#with_current_page begin fun p ->
            if Preferences.preferences#get.editor_save_all_bef_comp then (editor#save_all());
            p#compile_buffer ?join:None ()
          end
        end);
      (** Targets *)
      ignore (tool_targets#connect#clicked ~callback:(fun () -> browser#dialog_project_properties ?page_num:(Some 1) ?show:(Some true) ()));
      (* clean *)
      ignore (tool_clean#connect#clicked ~callback:(fun () -> self#clean_current browser));
      ignore (tool_clean#connect#show_menu ~callback:(self#clean_menu browser));
      (* compile *)
      ignore (tool_compile#connect#clicked ~callback:(fun () -> self#compile_current browser));
      ignore (tool_compile#connect#show_menu ~callback:(self#compile_menu browser));
      (* build *)
      ignore (tool_build#connect#clicked ~callback:(fun () -> self#build_current browser));
      ignore (tool_build#connect#show_menu ~callback:(self#build_menu browser));
      (* run *)
      ignore (tool_run#connect#clicked ~callback:(fun () -> self#run_current browser));
      ignore (tool_run#connect#show_menu ~callback:(self#run_menu browser));
      (** Location History *)
      tool_back#connect#clicked ~callback:(fun () -> browser#goto_location `PREV);
      tool_forward#connect#clicked ~callback:(fun () -> browser#goto_location `NEXT);
      tool_last_edit_loc#connect#clicked ~callback:(fun () -> browser#goto_location `LAST);
      tool_back#connect#show_menu ~callback:(fun (dir, menu) -> browser#create_menu_history `BACK ~menu);
      tool_forward#connect#show_menu ~callback:(fun (dir, menu) -> browser#create_menu_history `FORWARD ~menu);
      ()

    method update current_project =
      let dbf = match current_project with None -> None | Some x -> Project.default_target x in
      let editor_empty = List.length editor#pages = 0 in
      let current_modified = match editor#get_page `ACTIVE
        with Some p when p#buffer#modified -> true | _ -> false in
      tool_messages#misc#set_sensitive true;
      tool_hmessages#misc#set_sensitive true;
      tool_new_file#misc#set_sensitive (current_project <> None);
      tool_open_file#misc#set_sensitive (current_project <> None);
      tool_save#misc#set_sensitive (current_project <> None && current_modified);
      tool_close_file#misc#set_sensitive (not editor_empty);
      (*  *)
      tool_find_repl#misc#set_sensitive (current_project <> None && not editor_empty);
      tool_find#misc#set_sensitive (current_project <> None && not editor_empty);
      tool_item_find_entry#misc#set_sensitive (current_project <> None && not editor_empty);
      (*  *)
      tool_targets#misc#set_sensitive (current_project <> None);
      tool_clean#misc#set_sensitive (current_project <> None && dbf <> None);
      tool_compile#misc#set_sensitive (current_project <> None && dbf <> None);
      tool_build#misc#set_sensitive (current_project <> None && dbf <> None);
      tool_run#misc#set_sensitive (current_project <> None && dbf <> None);
      editor#with_current_page begin fun p ->
        let name = Filename.basename p#get_filename in
        if name ^^^ ".ml" || name ^^^ ".mli" then begin
          ksprintf tool_compile_file#misc#set_tooltip_text "Compile \xC2\xAB%s\xC2\xBB" name;
          tool_compile_file#misc#set_sensitive true
        end else begin
          tool_compile_file#misc#set_tooltip_text "";
          tool_compile_file#misc#set_sensitive false
        end
      end;
      begin
        let back, forward, last = editor#location_history_is_empty () in
        try
          tool_back#misc#set_sensitive (not back);
          tool_forward#misc#set_sensitive (not forward);
          tool_last_edit_loc#misc#set_sensitive (not last);
        with _ -> ()
      end;

    method connect = new signals ~tool_messages_clicked ~tool_hmessages_clicked
  end

(** Signals *)
and tool_messages_clicked () = object inherit [bool] signal () as super end
and tool_hmessages_clicked () = object inherit [bool] signal () as super end

and signals ~tool_messages_clicked ~tool_hmessages_clicked =
  object
    inherit ml_signals [tool_messages_clicked#disconnect; tool_hmessages_clicked#disconnect]
    method tool_messages_clicked = tool_messages_clicked#connect ~after
    method tool_hmessages_clicked = tool_hmessages_clicked#connect ~after
  end
