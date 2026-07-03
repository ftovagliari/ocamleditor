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

open Project
open Prj
open Utils
open Printf
open Preferences


class widget ~editor ?(callback=ignore) ~project ?page_num ?packing ?show () =
  let width = 165 in
  let box = GPack.vbox ?packing ~spacing:5 () in
  let notebook = GPack.notebook ~packing:(box#pack ~fill:true ~expand:true) () in
  (* General tab *)
  let gbox = GPack.vbox ~border_width:8 ~spacing:8 () in
  let _ = notebook#append_page ~tab_label:(GMisc.label ~text:"General" ())#coerce gbox#coerce in
  let entry_box = GPack.vbox ~border_width:0 ~spacing:3 ~packing:(gbox#pack ~expand:false ~fill:false) () in
  let mk_entry (ebox : GPack.box) label entry =
    let box = GPack.hbox ~spacing:3 ~packing:(ebox#pack ~expand:false) () in
    let _ = GMisc.label ~text:label ~width ~xalign:0.0 ~packing:(box#pack ~expand:false) () in
    box#pack ~expand:true entry#coerce;
  in
  let encodings = ["UTF-8"; "CP1252"; "Default"] in
  let entry_encoding, (_, _) = GEdit.combo_box_entry_text ~strings:encodings () in
  let name_entry = GEdit.entry () in
  let desc_entry = GEdit.entry () in
  let author_entry = GEdit.entry () in
  let version_entry = GEdit.entry () in
  let _ = mk_entry entry_box "Encoding:" entry_encoding in
  let _ = mk_entry entry_box "Name:" name_entry in
  let _ = mk_entry entry_box "Description:" desc_entry in
  let _ = mk_entry entry_box "Author:" author_entry in
  let _ = mk_entry entry_box "Version:" version_entry in
  (** Paths *)
  let frame = GBin.frame ~label:" Paths " ~border_width:0 ~packing:(gbox#pack ~expand:false) () in
  let entry_box = GPack.vbox ~border_width:5 ~spacing:3 ~packing:frame#add () in
  (* Project Home *)
  let home_box = GPack.hbox ~spacing:3 () in
  let home_entry = GEdit.entry ~width:300 ~editable:false ~packing:home_box#add () in
  let home_choose = GButton.button ~label:"  ...  " ~packing:home_box#pack () in
  let chooser = GWindow.file_chooser_dialog ~action:`SELECT_FOLDER () in
  let _ =
    chooser#add_button_stock `OK `OK;
    chooser#add_button_stock `CANCEL `CANCEL;
    let choose () =
      Gaux.may chooser#filename ~f:begin fun dir ->
        home_entry#set_text (Filename.concat dir name_entry#text);
      end
    in
    home_choose#connect#clicked ~callback:begin fun () ->
      ignore (chooser#connect#current_folder_changed ~callback:choose);
      match chooser#run() with
      | _ -> chooser#misc#hide()
    end;
  in
  let src_entry = GEdit.entry ~editable:false () in
  let bak_entry = GEdit.entry ~editable:false () in
  let doc_entry = GEdit.entry ~editable:false () in
  let _ = List.iter (fun x -> x#misc#set_sensitive false) [src_entry; bak_entry; doc_entry] in
  let _ = mk_entry entry_box "Project directory:" home_box in
  let _ = mk_entry entry_box "Project source path:" src_entry in
  let _ = mk_entry entry_box "Backup path:" bak_entry in
  (** OCaml Home *)
  let frame = GBin.frame ~label:" OCaml " ~border_width:0 ~packing:(gbox#pack ~expand:false) () in
  let ocaml_home = new Ocaml_home.widget ~project ~border_width:5 ~label_width:width ~packing:frame#add () in
  (** Autocomp settings *)
  let frame = GBin.frame ~label:" Automatic compilation " ~border_width:0 ~packing:gbox#pack () in
  let acbox = GPack.vbox ~spacing:3 ~border_width:5 ~packing:frame#add () in
  let check_autocomp_enabled = GButton.check_button ~label:"Enable automatic compilation" () in
  let _ = frame#set_label_widget (Some check_autocomp_enabled#coerce) in
  let table = GPack.table ~row_spacings:3 ~col_spacings:3 ~border_width:5 ~packing:acbox#add () in
  let _ = GMisc.label ~width ~xalign:0.0 ~text:"Delay: " ~packing:(table#attach ~top:0 ~left:0) () in
  let range_box = GPack.hbox  ~packing:(table#attach ~top:0 ~left:1 ~expand:`X) () in
  let adjustment = GData.adjustment ~lower:500. ~upper:5010. ~value:1000. ~step_incr:50. (*~page_incr:50.*) (*~page_size:50.*) () in
  let range_autocomp_delay = GRange.scale `HORIZONTAL ~adjustment ~digits:0 ~value_pos:`RIGHT ~packing:range_box#add () in
  let _ = GMisc.label ~text:" ms" ~packing:range_box#pack () in
  let _ = GMisc.label ~width ~xalign:0.0 ~text:"Compiler flags: " ~packing:(table#attach ~top:1 ~left:0) () in
  let entry_autocomp_cflags = GEdit.entry ~packing:(table#attach ~top:1 ~left:1 ~expand:`X) () in
  let _ =
    let enable value =
      range_box#misc#set_sensitive (check_autocomp_enabled#active);
      entry_autocomp_cflags#misc#set_sensitive (check_autocomp_enabled#active);
    in
    ignore (check_autocomp_enabled#connect#after#toggled ~callback:begin fun () ->
        enable check_autocomp_enabled#active;
      end);
  in
  (** Targets Tab *)
  let target_box = GPack.vbox ~spacing:8 ~border_width:8 () in
  let _ = notebook#append_page
      ~tab_label:(GMisc.label ~text:"Targets" ())#coerce target_box#coerce in
  let hbox = GPack.hbox ~spacing:8 ~packing:target_box#add () in
  let target_list = new Target_list.view ~editor ~project ~packing:hbox#pack () in
  let _ =
    if List.length project.Prj.targets = 0 then (ignore (target_list#add_target()));
  in
  let vbox = GPack.vbox ~spacing:8 ~packing:hbox#add () in
  let label_title = GMisc.label ~markup:"" ~xalign:0.0 ~packing:vbox#pack () in
  let target_page = new Target_page.view ~target_list ~project ~packing:vbox#add () in
  let etask_page = new Etask_page.view ~packing:vbox#add () in
  let set_title x = ksprintf label_title#set_label "<b><big>%s</big></b>" x in
  let hide_all () =
    target_page#misc#hide ();
    etask_page#misc#hide ();
    target_page#misc#set_sensitive true;
    etask_page#misc#set_sensitive true;
  in
  let _ = hide_all() in
  let _ = target_list#connect#selection_changed ~callback:begin function
    | None ->
        target_page#misc#set_sensitive false;
        etask_page#misc#set_sensitive false;
    | Some path ->
        begin
          match target_list#get path with
          | Target_list.Target tg ->
              set_title "Target";
              Gmisclib.Idle.add (fun () -> target_page#set_target tg);
              Gmisclib.Idle.add begin fun () ->
                if not (target_page#misc#get_flag `SENSITIVE) then (target_page#misc#set_sensitive true);
                if not (target_page#misc#get_flag `VISIBLE) then begin
                  etask_page#misc#hide ();
                  target_page#misc#show ();
                end
              end
          | Target_list.ETask et ->
              set_title "External Build Task";
              Gmisclib.Idle.add (fun () -> etask_page#set_task et);
              Gmisclib.Idle.add begin fun () ->
                (*                if not (etask_page#misc#get_flag `SENSITIVE) then (etask_page#misc#set_sensitive true);*)
                if not (etask_page#misc#get_flag `VISIBLE) then begin
                  target_page#misc#hide ();
                  etask_page#misc#show ();
                end;
              end
        end
    end in
  let _ = target_list#connect#add_target ~callback:(fun () -> Gmisclib.Idle.add target_page#entry_name#misc#grab_focus) in
  let _ = target_list#connect#add_etask ~callback:(fun () -> Gmisclib.Idle.add etask_page#entry_name#misc#grab_focus) in
  let _ = target_page#entry_name#connect#changed ~callback:begin fun () ->
      target_list#with_current begin fun path _ ->
        let row = target_list#model#get_iter path in
        let column = target_list#column_name in
        target_list#model#set ~row ~column target_page#entry_name#text
      end;
    end in
  let _ = etask_page#entry_name#connect#changed ~callback:begin fun () ->
      target_list#with_current begin fun path _ ->
        let row = target_list#model#get_iter path in
        let column = target_list#column_name in
        target_list#model#set ~row ~column etask_page#entry_name#text
      end
    end in
  let _ = target_box#pack target_page#entry_cmd_line#coerce in
  let _ = target_list#select_default_configuration () in
  (** Runtime Configurations Tab *)
  let runtime_box = GPack.vbox ~spacing:8 ~border_width:8 () in
  let runtime_tab_label = GMisc.label ~text:"Executables" () in
  let _ = notebook#append_page
      ~tab_label:runtime_tab_label#coerce runtime_box#coerce in
  let hbox = GPack.hbox ~spacing:8 ~packing:runtime_box#add () in
  let rconf_page = new Rconf_page.view ~target_list ~packing:(hbox#pack ~from:`END ~expand:true ~fill:true) () in
  let rconf_list = new Rconf_list.view ~target_list ~editor ~project ~page:rconf_page ~packing:hbox#pack () in
  (** Buttons *)
  let bb = GPack.button_box `HORIZONTAL ~layout:`END ~spacing:8 ~border_width:8
      ~packing:(box#pack ~expand:false) () in
  let button_ok = GButton.button ~label:"OK" ~packing:bb#add () in
  let button_apply = GButton.button ~label:"Apply" ~packing:bb#add () in
  let button_close = GButton.button ~label:"Close" ~use_mnemonic:false ~packing:bb#add () in
  let button_help = GButton.button ~label:"Help" ~use_mnemonic:false ~packing:bb#add () in
  let _ = bb#set_child_secondary button_help#coerce true in
  let _ = button_help#misc#set_sensitive false in
  let _ = target_list#misc#connect#map ~callback:(fun () -> button_help#misc#set_sensitive true) in
  let _ = target_list#misc#connect#unmap ~callback:(fun () -> button_help#misc#set_sensitive false) in
  let _ = button_help#connect#clicked ~callback:begin fun () ->
      let cmd = sprintf "\"%s\" --help" Oe_config.oebuild_command in
      let text = String.concat "\n" (Shell.get_command_output cmd) in
      let window = GWindow.dialog ~title:cmd ~position:`CENTER () in
      window#add_button_stock `OK `OK;
      window#set_border_width 5;
      let label = GMisc.label ~text ~packing:window#vbox#add () in
      let fd = Gtk_util.increase_font_size ~increment:(-2) label#coerce in
      Pango.Font.set_family fd "monospace";
      label#misc#modify_font fd;
      match window#run () with _ -> window#destroy()
    end in
  object (self)
    inherit GObj.widget box#as_widget

    val project_changed = new project_changed ()
    val project_name_changed = new project_name_changed ()
    val show = new show ()

    method button_close = button_close
    method target_list = target_list

    (*method private targets_ok =
      target_list#length > 0 && project.Prj.build <> [] && begin
        List.for_all (fun bc -> bc.Target.files <> "") project.Prj.build
      end && (not target_page#changed)*)

    method reset () =
      entry_encoding#set_active (match project.encoding with None -> (List.length encodings - 1)
                                                           | Some x -> (try Utils.ListExt.pos x encodings with Not_found -> 0));
      name_entry#set_text project.name;
      desc_entry#set_text project.description;
      author_entry#set_text project.author;
      version_entry#set_text project.version;
      home_entry#set_text project.root;
      ignore (chooser#set_current_folder (Filename.dirname project.root));
      src_entry#set_text (project.root // Prj.default_dir_src);
      bak_entry#set_text (project.root // Prj.default_dir_bak);
      doc_entry#set_text "";
      GtkThread2.async ocaml_home#reset ();
      check_autocomp_enabled#set_active project.autocomp_enabled;
      range_box#misc#set_sensitive (check_autocomp_enabled#active);
      entry_autocomp_cflags#misc#set_sensitive (check_autocomp_enabled#active);
      range_autocomp_delay#adjustment#set_value (project.autocomp_delay *. 1000.);
      entry_autocomp_cflags#set_text project.autocomp_cflags;
      GtkThread2.async target_list#reset ();
      GtkThread2.async rconf_list#reset ();

    method save () =
      try
        Project.set_ocaml_home ~ocamllib:ocaml_home#ocamllib project;
        project.encoding         <- (match entry_encoding#entry#text with "Default" -> None | enc -> Some enc);
        project.name             <- name_entry#text;
        project.description      <- desc_entry#text;
        project.author           <- author_entry#text;
        project.version          <- version_entry#text;
        project.root             <- home_entry#text;
        project.autocomp_enabled <- check_autocomp_enabled#active;
        project.autocomp_delay   <- range_autocomp_delay#adjustment#value /. 1000.;
        project.autocomp_cflags  <- entry_autocomp_cflags#text;
        project.targets          <- (target_list#get_targets ());
        project.executables      <- List.filter begin fun rtc ->
            List.exists (fun bc -> bc.Target.id = rtc.Rconf.target_id) project.targets
          end (rconf_list#get_rconfigs());
        project.search_path    <- get_search_path project;
        callback project;
        GtkThread.sync (Project.save ~editor) project;
        GtkThread.async Project.refresh project;
        GtkThread.sync project_changed#call project;
        (*  *)
        GtkThread.sync target_page#set_changed false;
        Gmisclib.Idle.add target_list#reset;
        (*  *)
        if project.autocomp_enabled then begin
          editor#with_current_page (fun p -> p#compile_buffer ?join:None ());
        end else begin
          List.iter begin fun page ->
            page#compile_buffer ?join:None ();
            page#error_indication#remove_tag();
            page#global_gutter#misc#draw (Some (Gdk.Rectangle.create
                                                  ~x:page#global_gutter#misc#allocation.Gtk.x
                                                  ~y:page#global_gutter#misc#allocation.Gtk.y
                                                  ~width:page#global_gutter#misc#allocation.Gtk.width
                                                  ~height:page#global_gutter#misc#allocation.Gtk.height
                                               )) end editor#pages;
        end
      with
      | Project.Project_already_exists path ->
          Dialog.info ~message:("Directory \""^path^
                                "\" already exists.\nPlease choose another name for your project.") self
      | ex -> Dialog.display_exn ~parent:self ex

    method goto_page = notebook#goto_page

    method connect = new signals ~project_changed ~project_name_changed ~show

    initializer
      self#reset();
      if Sys.file_exists project.root then begin
        name_entry#set_editable false;
        home_choose#misc#set_sensitive false;
      end else begin
        notebook#remove target_box#coerce;
        notebook#remove runtime_box#coerce;
      end;
      (* Entries *)
      let signal_name_entry_changed = ref None in
      signal_name_entry_changed := Some (name_entry#connect#changed ~callback:begin fun () ->
          Gaux.may !signal_name_entry_changed ~f:name_entry#misc#handler_block;
          if name_entry#text <> "" then begin
            home_entry#set_text (Filename.concat (Filename.dirname home_entry#text) name_entry#text);
            project_name_changed#call name_entry#text;
          end;
          Gaux.may !signal_name_entry_changed ~f:name_entry#misc#handler_unblock;
        end);
      let set_paths () =
        src_entry#set_text (Filename.concat home_entry#text "src");
        bak_entry#set_text (Filename.concat home_entry#text "bak");
        doc_entry#set_text (Filename.concat home_entry#text "doc");
      in
      ignore (home_entry#connect#changed ~callback:set_paths);
      set_paths();
      name_entry#misc#grab_focus();
      (* Buttons *)
      ignore (button_apply#connect#clicked ~callback:(fun () -> Gmisclib.Idle.add self#save));
      ignore (button_ok#connect#clicked ~callback:(fun () -> button_close#clicked(); self#save()));
      (* *)
      notebook#goto_page 0;
      (*    notebook#connect#switch_page ~callback:begin fun num ->
            if num = 2 && target_page#changed then (GtkSignal.stop_emit(); notebook#goto_page 1)
            end;*)
      ignore (target_page#connect#changed ~callback:begin fun () ->
          Gmisclib.Idle.add (fun () -> GtkBase.Widget.queue_draw target_list#view#as_widget);
        end);
      Gaux.may page_num ~f:notebook#goto_page;

  end

and project_changed () = object inherit [Prj.t] GUtil.signal () end
and project_name_changed () = object inherit [string] GUtil.signal () end
and show () = object inherit [unit] GUtil.signal () end

and signals ~project_changed ~project_name_changed ~show =
  object
    inherit GUtil.ml_signals [
        project_changed#disconnect;
        project_name_changed#disconnect;
        show#disconnect]
    method project_changed = project_changed#connect ~after
    method project_name_changed = project_name_changed#connect ~after
    method show = show#connect ~after
  end

(** create *)
let create ~editor ?callback ?new_project ?page_num ?show () =
  let project = match new_project with None -> editor#project | Some p -> p in
  let window = GWindow.window ~modal:false ~title:("Project \""^project.name^"\"")
      ~icon:(??? Icons.oe) ~border_width:5 ~position:`CENTER ~show:false () in
  let widget = new widget ~editor ~packing:window#add ~project ?callback ?page_num ?show () in
  ignore (widget#button_close#connect#clicked ~callback:window#misc#hide);
  ignore (widget#connect#project_name_changed ~callback:begin fun name ->
      window#set_title (replace_all ["\".*\"", "\"" ^ name ^ "\""] window#title)
    end);
  ignore (window#event#connect#key_press ~callback:begin fun ev ->
      if GdkEvent.Key.keyval ev = GdkKeysyms._Escape
      then (widget#button_close#clicked(); true) else false
    end);
  Gmisclib.Window.GeometryMemo.add ~key:"project-properties" ~window Preferences.geometry_memo;
  if show = Some true then (window#show());
  window, widget























