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


open Utils
open Printf
open Build_script
open Preferences

let enable_widget_args = true

class widget ~project ?packing () =
  let build_script    = project.Prj.build_script in
  let spacing         = 8 in
  let border_width    = 5 in
  let vbox            = GPack.vbox ~spacing:13 ?packing () in
  let text_filename   = "Specify the file to be created by the generation process" in
  let fbox            = GPack.vbox ~spacing ~packing:vbox#pack () in
  let _               = GMisc.label ~text:text_filename ~xalign:0.0 ~packing:fbox#pack () in
  let box             = GPack.hbox ~spacing:5 ~packing:fbox#pack () in
  let entry_filename  = GEdit.entry
      ~text:(project.Prj.root // build_script.bs_filename)
      ~packing:box#add () in
  let button_filename = GButton.button ~label:"  ...  " ~packing:box#pack () in
  let notebook        = GPack.notebook ~packing:vbox#add () in
  (* Targets *)
  let abox            = GPack.vbox ~border_width ~spacing () in
  let tab_label       = (GMisc.label ~text:"Targets" ())#coerce in
  let _               = notebook#append_page ~tab_label abox#coerce in
  let _               = GMisc.label ~text:"" ~xalign:0.0 ~packing:abox#pack ~show:true () in
  let widget_trg      = new Build_script_trg_widget.widget ~project ~packing:abox#add () in
  (* Command Line Arguments *)
  let abox            = GPack.vbox ~border_width ~spacing () in
  let tab_label       = (GMisc.label ~text:"Command Line Arguments" ())#coerce in
  let _               = notebook#append_page ~tab_label abox#coerce in
  let _               = GMisc.label ~text:"Define the command line arguments for the build script commands" ~xalign:0.0 ~packing:abox#pack ~show:enable_widget_args () in
  let widget_args     = new Build_script_args_widget.widget ~project ~packing:abox#add () in
  (* General Commands *)
  let abox            = GPack.vbox ~border_width ~spacing () in
  let tab_label       = (GMisc.label ~text:"General Commands" ())#coerce in
  let _               = notebook#append_page ~tab_label abox#coerce in
  let _               = GMisc.label ~text:"" ~xalign:0.0 ~packing:abox#pack () in
  let label           = sprintf "Select an external task to be executed after the <b>%s</b> command: " (Build_script.string_of_command `Distclean) in
  let cmd_distclean   = new Build_script_cmds_widget.widget `Distclean ~label ~project ~packing:abox#pack () in
  let label           = sprintf "Select an external task to be executed as <b>%s</b> command: " (Build_script.string_of_command `Install) in
  let cmd_install     = new Build_script_cmds_widget.widget `Install ~label ~project ~packing:abox#pack () in
  let label           = sprintf "Select an external task to be executed as <b>%s</b> command: " (Build_script.string_of_command `Uninstall) in
  let cmd_uninstall   = new Build_script_cmds_widget.widget `Uninstall ~label ~project ~packing:abox#pack () in
  (* Preview Help *)
  let helpbox         = GPack.vbox ~border_width ~spacing () in
  let tab_label       = (GMisc.label ~text:"Preview Help" ())#coerce in
  let _               = notebook#append_page ~tab_label helpbox#coerce in
  let cbox            = GPack.hbox ~border_width ~spacing ~packing:helpbox#pack () in
  let sw              = GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~shadow_type:`IN ~packing:helpbox#add () in
  let help            = GText.view ~packing:sw#add () in
  let help_buttons    =
    let pref = Preferences.preferences#get in
    help#misc#modify_font_by_name pref.editor_base_font;
    let b0 = GButton.radio_button ~relief:`NONE ~draw_indicator:false ~active:false ~packing:cbox#pack () in
    let _ = GMisc.label ~markup:"<span face='monospace' size='smaller'>General</span>" ~packing:b0#add () in
    let _ = GMisc.separator `VERTICAL ~packing:cbox#pack () in
    let bb =
      List.map begin fun c ->
        let name = Build_script_command.string_of_command c in
        let b = GButton.radio_button ~relief:`NONE ~group:b0#group ~draw_indicator:false ~active:false ~packing:cbox#pack () in
        let _ = GMisc.label ~markup:(sprintf "<span face='monospace' size='smaller'>%s</span>" name) ~packing:b#add () in
        b, name
      end Build_script_command.commands
    in
    (b0, "") :: bb
  in
  let _ = GMisc.label ~packing:cbox#add () in
  let spinner = GMisc.image ~file:(Icon.get_themed_filename "spinner_16.gif") ~show:false ~packing:cbox#pack () in
  (*  *)
  object (self)
    inherit GObj.widget vbox#as_widget
    val mutable is_valid = new GUtil.variable true

    initializer
      if not enable_widget_args then (widget_args#misc#hide());
      ignore (button_filename#connect#clicked ~callback:self#choose_file);
      ignore (entry_filename#connect#changed
                ~callback:(fun () -> is_valid#set (Filename.check_suffix entry_filename#text ".ml")));
      Gmisclib.Idle.add entry_filename#misc#grab_focus;
      match help_buttons with
      | (b0, _) :: bb ->
          let filename = Filename.temp_file "build" ".tmp" in
          let mkcallback name butt () =
            if butt#active then begin
              spinner#misc#show();
              self#save ~tmp:filename ~filename:entry_filename#text ();
              let text = ref "" in
              let process_in = Spawn.loop (fun ic -> text := !text ^ (input_line ic) ^ "\n") in
              Spawn.async ~process_in ~at_exit:begin function
              | Some ex -> Printf.eprintf "File \"build_script_ui.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace())
              | _ ->
                  if !text <> help#buffer#get_text () then GtkThread.sync help#buffer#set_text !text;
                  Gmisclib.Idle.add ~prio:300 spinner#misc#hide;
              end "ocaml" [| filename; name; "-help" |] |> ignore;
            end
          in
          b0#connect#clicked ~callback:(mkcallback "" b0) |> ignore;
          List.iter (fun (b, name) -> b#connect#clicked ~callback:(mkcallback name b) |> ignore) bb;
          self#misc#connect#destroy ~callback:(fun () -> if Sys.file_exists filename then Sys.remove filename) |> ignore;
          notebook#connect#switch_page ~callback:begin fun page ->
            if page = 3 then begin
              try
                let b, _ = List.find (fun (b, _) -> b#active) ((b0, "") :: bb) in
                b#clicked();
                Gmisclib.Idle.add b#misc#grab_focus
              with Not_found -> ()
            end
          end |> ignore;
          b0#clicked()
      | _ -> ()

    method is_valid = is_valid

    method private save ?tmp ~filename () =
      project.Prj.build_script <- {
        bs_filename = Filename.basename filename;
        bs_targets  = widget_trg#get();
        bs_args     = widget_args#get();
        bs_commands = List.filter_map Fun.id [cmd_distclean#get(); cmd_install#get(); cmd_uninstall#get()];
      };
      Build_script_printer.print ~minify:false ~project ~filename:(match tmp with Some x -> x | _ -> filename) ();
      Project.save project (* NB Save project file without passing the editor *)

    method apply () =
      let filename = entry_filename#text in
      if Sys.file_exists filename then begin
        let response = Dialog.confirm ~message:(sprintf
                                                  "Are you sure you want to overwrite file \xC2\xAB%s\xC2\xBB?" (Filename.basename filename))
            ~yes:("Overwrite", (fun () -> self#save ~filename ()))
            ~no:("Do Not Overwrite", ignore)
            ~title:"Overwrite File" self
        in response <> `CANCEL
      end else (self#save ~filename (); true);

    method private choose_file () =
      let window = GWindow.file_chooser_dialog
          ~action:`SAVE ~icon:(??? Icons.oe)
          ~title:text_filename
          ~position:`CENTER ~modal:true ~show:false ()
      in
      window#set_select_multiple false;
      window#add_select_button_stock `OK `OK;
      window#add_button_stock `CANCEL `CANCEL;
      window#set_default_response `OK;
      let filter = GFile.filter ~name:"OCaml Source File" ~patterns:["*.ml"] () in
      window#set_filter filter;
      ignore (window#set_filename entry_filename#text);
      match window#run () with
      | `OK ->
          Gaux.may window#filename ~f:entry_filename#set_text;
          window#destroy()
      | _ -> window#destroy()
  end

let window ~project () =
  let window = GWindow.window ~title:"Generate Build Script"
      ~modal:true ~border_width:8 ~position:`CENTER ~icon:(??? Icons.oe) ~show:false () in
  Gmisclib.Window.GeometryMemo.add ~key:"dialog-build-script" ~window Preferences.geometry_memo;
  let vbox = GPack.vbox ~spacing:8 ~packing:window#add () in
  let widget = new widget ~project ~packing:vbox#add () in
  let bbox = GPack.button_box `HORIZONTAL ~layout:`END ~spacing:8 ~packing:vbox#pack () in
  let button_ok = GButton.button ~label:"OK" ~packing:bbox#pack () in
  let button_cancel = GButton.button ~label:"Cancel" ~packing:bbox#pack () in
  ignore (button_ok#connect#clicked ~callback:(fun () -> if widget#apply() then window#destroy()));
  ignore (button_cancel#connect#clicked ~callback:window#destroy);
  ignore (widget#is_valid#connect#changed ~callback:button_ok#misc#set_sensitive);
  (*ignore (window#event#connect#key_press ~callback:begin fun ev ->
    let key = GdkEvent.Key.keyval ev in
    if key = _Return then (button_ok#clicked(); true)
    else if key = _Escape then (window#destroy(); true)
    else false
    end);*)
  Gmisclib.Util.esc_destroy_window window;
  window#show();
  window;;





