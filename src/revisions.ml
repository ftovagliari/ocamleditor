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

type t = {
  rev      : string;
  date     : float;
  comment  : string;
  author   : string;
  filename : file;
}

and file = Filename of string | Command of (string * string)

let cols         = new GTree.column_list
let col_rev      = cols#add Gobject.Data.string
let col_date     = cols#add Gobject.Data.string
let col_comment  = cols#add Gobject.Data.string
let col_author   = cols#add Gobject.Data.string
let col_filename = (cols#add Gobject.Data.caml : file GTree.column)

let print_time tm =
  sprintf "%4d-%d-%d %02d:%02d:%02d" (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

let get_filename_extension ~dir_sep filename =
  let len = String.length filename in
  let pos = String.rindex filename '.' in
  let re = Str.regexp_string dir_sep in
  let pos_sep = try Str.search_backward re filename len with Not_found -> -1 in
  if pos < pos_sep then "" else String.sub filename pos (len - pos)

let is_ocaml_filename filename =
  filename ^^^ ".ml" || filename ^^^ ".mli" || filename ^^^ ".mll" || filename ^^^ ".mly"


(** ocamlview *)
class ocamlview ~colorize ?packing () =
  let sw = GBin.scrolled_window ~shadow_type:`IN ?packing () in
  let buffer = new Ocaml_text.buffer () in
  let view = new Ocaml_text.view ~buffer () in
  let _ = Preferences_apply.apply (view :> Text.view) Preferences.preferences#get in
  let _ = sw#add view#coerce in
  object (self)

    initializer
      if colorize then begin
        Gmisclib.Idle.add begin fun () ->
          self#colorize ();
          sw#vadjustment#connect#value_changed ~callback:self#colorize |> ignore;
        end;
      end

    method buffer = buffer
    method view = view

    method colorize () =
      let vrect = view#visible_rect in
      let h0 = Gdk.Rectangle.height vrect in
      let y0 = Gdk.Rectangle.y vrect in
      let start, _ = view#get_line_at_y y0 in
      let stop, _ = view#get_line_at_y (y0 + h0) in
      Lexical.tag ~start ~stop view#buffer;

  end

open Preferences

(** widget *)
class widget ~page ?packing () =
  let pref_diff_cmd  = Preferences.preferences#get.program_diff_graphical  in
  let diff_cmd       = Str.split (Utils.regexp " +") pref_diff_cmd in
  let project        = page#project in
  let mbox           = GPack.vbox ~spacing:0 ?packing () in
  let toolbar        = GButton.toolbar ~style:`ICONS ~orientation:`HORIZONTAL ~packing:(mbox#pack ~from:`END) () in
  let _              = toolbar#set_icon_size `MENU in
  let button_compare_ext = GButton.tool_button ~label:"Compare" ~packing:toolbar#insert () in
  let _              = button_compare_ext#set_icon_widget (GMisc.image ~pixbuf:(??? Icons.diff) ())#coerce in
  let _              = ksprintf button_compare_ext#misc#set_tooltip_markup "External diff with <span size='x-small' font-family='monospace'>%s</span>" pref_diff_cmd in
  let _              = GButton.separator_tool_item ~packing:toolbar#insert () in
  let button_ignore_ws = GButton.toggle_tool_button ~label:"Ignore Whitespace" ~active:true ~packing:toolbar#insert () in
  let button_ws      = GButton.toggle_tool_button ~label:"View Whitespaces" ~active:true ~packing:toolbar#insert () in
  let _              = button_ws#set_icon_widget (GMisc.image ~pixbuf:(??? Icons.whitespace_off_14) ())#coerce in
  let _              = GButton.separator_tool_item ~packing:toolbar#insert () in
  let button_refresh = GButton.tool_button ~label:"Compare" ~packing:toolbar#insert () in
  let _              = button_refresh#set_icon_widget (GMisc.image ~pixbuf:(??? Icons.refresh16) ())#coerce in
  let _              = GButton.separator_tool_item ~packing:toolbar#insert () in
  let button_detach  = GButton.tool_button ~label:"Detach" ~packing:toolbar#insert () in
  let _              = button_detach#set_icon_widget (GMisc.image ~pixbuf:(??? Icons.detach) ())#coerce in
  let item_message   = GButton.tool_item ~packing:toolbar#insert () in
  let label_message  = GMisc.label ~markup:(sprintf "<b>%s</b>" page#get_filename) ~xalign:0.0 ~yalign:0.5 ~xpad:8 ~packing:item_message#add () in
  let pane           = GPack.paned `HORIZONTAL ~packing:mbox#add () in
  (*  *)
  let rend_text      = GTree.cell_renderer_text [`SCALE `SMALL; `YPAD 0] in
  let vc_rev         = GTree.view_column ~title:"Revision" ~renderer:(rend_text, ["text", col_rev]) () in
  let vc_date        = GTree.view_column ~title:"Date" ~renderer:(rend_text, ["text", col_date]) () in
  let vc_comment     = GTree.view_column ~title:"Comment" ~renderer:(rend_text, ["text", col_comment]) () in
  let vc_author      = GTree.view_column ~title:"Author" ~renderer:(rend_text, ["text", col_author]) () in
  (*  *)
  let sw             = GBin.scrolled_window ~shadow_type:`IN ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:pane#add1 () in
  let model          = GTree.list_store cols in
  let view           = GTree.view ~model ~headers_clickable:true ~packing:sw#add () in
  let _              = Gaux.may !Gtk_theme.find_text_output_font_condensed ~f:view#misc#modify_font_by_name in
  let _              = view#selection#set_mode `MULTIPLE in
  let _              = view#append_column vc_rev in
  let _              = view#append_column vc_date in
  let _              = view#append_column vc_author in
  let _              = view#append_column vc_comment in
  let _              = vc_rev#set_sort_column_id col_rev.GTree.index in
  let _              = vc_date#set_sort_column_id col_date.GTree.index in
  let _              = vc_comment#set_sort_column_id col_comment.GTree.index in
  let _              = vc_author#set_sort_column_id col_author.GTree.index in
  let _              = vc_date#set_sort_indicator true in
  (*  *)
  let _              = view#misc#set_property "enable-grid-lines" (`INT 3) in
  let destroy_right_pane () =
    (try pane#child2#destroy() with Gpointer.Null -> pane#set_position (pane#misc#allocation.Gtk.width / 2)) in
  object (self)
    inherit GObj.widget mbox#as_widget
    inherit Messages.page ~role:"history"

    val mutable temp_files = []
    val mutable oview = None
    val mutable ignore_whitespace = false (* TODO persist *)

    initializer
      button_detach#connect#clicked ~callback:(fun () -> self#detach button_detach) |> ignore;
      button_compare_ext#connect#clicked ~callback:self#compare_ext |> ignore;
      button_ws#connect#clicked ~callback:self#set_show_withespaces |> ignore;
      button_ignore_ws#connect#clicked ~callback:self#set_ignore_whitespace |> ignore;
      button_ignore_ws#set_active ignore_whitespace;
      button_refresh#connect#clicked ~callback:begin fun () ->
        let selection =
          List.map (fun path -> (model#get ~row:(model#get_iter path) ~column:col_rev)) view#selection#get_selected_rows
        in
        self#load ~selection ()
      end |> ignore;
      view#connect#row_activated ~callback:(fun _ _ -> self#view_file ()) |> ignore;
      view#selection#connect#changed ~callback:begin fun () ->
        match view#selection#get_selected_rows with
        | _ :: b :: c :: _ ->
            begin
              match view#get_cursor () with
              | Some path, _ ->
                  let p = GTree.Path.to_string path in
                  if p <> GTree.Path.to_string b then view#selection#unselect_path b
                  else if p <> GTree.Path.to_string c then view#selection#unselect_path c
              | _ -> ()
            end;
        | _ -> ()
      end |> ignore;
      view#selection#connect#changed ~callback:begin fun () ->
        match view#selection#get_selected_rows with
        | _ :: _ :: [] ->
            button_compare_ext#misc#set_sensitive true;
            self#view_diff();
        | _ :: [] ->
            button_compare_ext#misc#set_sensitive false;
            self#view_file();
        | [] ->
            button_compare_ext#misc#set_sensitive false;
            destroy_right_pane()
        | _ ->
            button_compare_ext#misc#set_sensitive false;
            destroy_right_pane()
      end |> ignore;
      self#misc#connect#destroy ~callback:begin fun () ->
        List.iter (fun fn -> if Sys.file_exists fn then Sys.remove fn) temp_files;
        temp_files <- []
      end |> ignore;
      self#load ();

    method private set_show_withespaces () =
      match oview with
      | Some ov ->
          ov#view#options#set_show_whitespace_chars button_ws#get_active;
          GtkBase.Widget.queue_draw ov#view#as_widget;
      | _ -> ()

    method private set_ignore_whitespace () =
      ignore_whitespace <- button_ignore_ws#get_active;
      self#view_diff();

    method private reduce_font_size ocamlview =
      let fd = ocamlview#view#misc#pango_context#font_description in
      let size = Pango.Font.get_size fd in
      if size - Pango.scale >= (7 * Pango.scale) then begin
        let size = size - 2 * Pango.scale in
        Pango.Font.modify fd ~size ();
        ocamlview#view#misc#modify_font fd;
      end;

    method private view_diff () =
      match view#selection#get_selected_rows with
      | path2 :: path1 :: [] ->
          let row1 = model#get_iter path1 in
          let filename1 = self#get_filename ~row:row1 in
          let row2 = model#get_iter path2 in
          let filename2 = self#get_filename ~row:row2 in
          destroy_right_pane();
          let colorize = is_ocaml_filename filename1 && is_ocaml_filename filename2 in
          let vbox = GPack.vbox ~packing:pane#add2 () in
          let _ = self#create_rev_indicator ~label:"From: " ~packing:vbox#pack ~row:row1 () in
          let _ = self#create_rev_indicator ~label:"To: " ~packing:vbox#pack ~row:row2 () in
          let ocamlview = new ocamlview ~colorize ~packing:vbox#add () in
          ocamlview#view#set_editable false;
          self#reduce_font_size ocamlview;
          ocamlview#view#options#set_show_line_numbers false;
          ocamlview#view#options#set_show_whitespace_chars button_ws#get_active;
          ocamlview#buffer#connect#end_user_action ~callback:ocamlview#colorize |> ignore;
          ocamlview#buffer#set_text "";
          Global_diff.to_buffer ocamlview#buffer#as_gtext_buffer ignore_whitespace filename1 filename2;
          oview <- Some ocamlview;
          ocamlview#view#misc#connect#destroy ~callback:(fun () -> oview <- None) |> ignore;
      | _ -> ()

    method private create_rev_indicator ?label ~row ~packing () =
      let rbox = GPack.hbox ~spacing:0 ~packing () in
      Gaux.may label ~f:begin fun markup ->
        GMisc.label ~markup ~xalign:0.0 ~width:50 ~packing:rbox#pack ()
      end;
      let _ = GEdit.entry ~text:(model#get ~row ~column:col_rev) ~width_chars:8 ~editable:false ~packing:rbox#pack () in
      let _ = GEdit.entry ~text:(model#get ~row ~column:col_date) ~editable:false ~packing:rbox#pack () in
      let _ = GEdit.entry ~text:(model#get ~row ~column:col_author) ~editable:false ~packing:rbox#pack () in
      let entry_comment = GEdit.entry ~text:(model#get ~row ~column:col_comment) ~editable:false ~packing:rbox#add () in
      entry_comment#misc#set_tooltip_text entry_comment#text;

    method private view_file () =
      match view#selection#get_selected_rows with
      | path :: [] ->
          let row = model#get_iter path in
          let filename = self#get_filename ~row in
          destroy_right_pane();
          let vbox = GPack.vbox ~spacing:0 ~packing:pane#add2 () in
          let _ = self#create_rev_indicator ~packing:vbox#pack ~row () in
          let colorize = is_ocaml_filename filename in
          let ocamlview = new ocamlview ~colorize ~packing:vbox#add () in
          self#reduce_font_size ocamlview;
          ocamlview#view#set_editable false;
          ocamlview#view#options#set_show_whitespace_chars button_ws#get_active;
          ocamlview#buffer#set_text (Buffer.contents (File_util.read filename));
          ocamlview#colorize ();
          oview <- Some ocamlview;
          ocamlview#view#misc#connect#destroy ~callback:(fun () -> oview <- None) |> ignore;
      | _ -> ()

    method private compare_ext () =
      match view#selection#get_selected_rows with
      | path2 :: path1 :: [] ->
          let row = model#get_iter path1 in
          let filename1 = self#get_filename ~row in
          let row = model#get_iter path2 in
          let filename2 = self#get_filename ~row in
          begin
            match diff_cmd with
            | [] -> eprintf "Unable to execute graphics diff command"
            | program :: args ->
                Spawn.async program (Array.of_list (args @ [ filename1; filename2 ])) |> ignore
          end
      | _ -> ()

    method private get_filename ~row =
      match model#get ~row ~column:col_filename with
      | Filename x -> x
      | Command (cmd, ext) -> self#create_tmp_rev_file cmd ext

    method private create_tmp_rev_file cmd ext =
      let tmp = Filename.temp_file "ocamleditor" ext in
      let cmd = sprintf "%s > %s" cmd tmp in
      let exit_code = Sys.command cmd in
      temp_files <- tmp :: temp_files;
      tmp

    method private load ?selection () =
      model#clear();
      (* Buffer *)
      GtkThread.sync begin fun () ->
        page#buffer#save_buffer ?filename:None ();
        let date = print_time (Unix.localtime (Unix.stat page#buffer#tmp_filename).Unix.st_mtime) in
        let row = model#append () in
        model#set ~row ~column:col_rev "Buffer";
        model#set ~row ~column:col_date date;
        model#set ~row ~column:col_comment "";
        model#set ~row ~column:col_author (Glib.get_user_name ());
        model#set ~row ~column:col_filename (Filename page#buffer#tmp_filename);
      end ();
      (* Local Saved File *)
      GtkThread.sync begin fun () ->
        let date = print_time (Unix.localtime (Unix.stat page#get_filename).Unix.st_mtime) in
        let row = model#append () in
        model#set ~row ~column:col_rev "File";
        model#set ~row ~column:col_date date;
        model#set ~row ~column:col_comment "Local Saved File";
        model#set ~row ~column:col_author (Glib.get_user_name ());
        model#set ~row ~column:col_filename (Filename page#get_filename);
      end ();
      (*  *)
      Gmisclib.Idle.add begin fun () ->
        GtkThread.sync self#read_local_backups ();
        GtkThread.sync self#read_rev_history ();
        match selection with
        | None ->
            Gaux.may model#get_iter_first ~f:(fun row -> view#selection#select_path (model#get_path row));
        | Some rev ->
            let find_path_by_rev (model : GTree.list_store) rev =
              let found = ref None in
              model#foreach begin fun path row ->
                let r = model#get ~row ~column:col_rev in
                if r = rev then begin
                  found := Some path;
                  true
                end else false
              end;
              !found
            in
            List.iter (fun r -> Gaux.may (find_path_by_rev model r) ~f:view#selection#select_path) rev;
            List.iter (fun r -> Gaux.may (find_path_by_rev model r) ~f:(fun path -> view#scroll_to_cell path vc_rev)) (List.rev rev);
      end;

    method private read_rev_history () =
      match Oe_config.git_version with
      | None -> ()
      | _ ->
          let root = Filename.dirname (Project.path_src project) in
          match Utils.filename_relative root page#get_filename with
          | Some rel ->
              let sep = "\x1F" in
              let rel = List.fold_left (fun acc x -> acc ^ "/" ^ x) Filename.parent_dir_name (Utils.filename_split rel) in
              let args = [|
                "log";
                (sprintf "--format=%%h%s%%an%s%%ai%s%%s" sep sep sep);
                "--abbrev-commit";
                rel
              |] in
              let re = Str.regexp_string sep in
              let process_in =
                Spawn.loop begin fun ic ->
                  let line = Str.split re (input_line ic) in
                  let row = model#append () in
                  match line with
                  | [rev; author; date; comment] ->
                      model#set ~row ~column:col_rev rev;
                      model#set ~row ~column:col_author author;
                      model#set ~row ~column:col_date date;
                      model#set ~row ~column:col_comment comment;
                      model#set ~row ~column:col_filename (Command ((sprintf "git show %s:%s" rev rel), get_filename_extension ~dir_sep:"/" rel));
                  | _ -> ()
                end
              in
              Spawn.async ~process_in "git" args |> ignore;
          | _ -> ()

    method private read_local_backups () =
      let src = Project.path_src project in
      let bak = Project.path_bak project in
      match Utils.filename_relative src page#get_filename with
      | Some rel ->
          let pos = String.rindex rel '.' in
          let prefix = String.sub rel 0 pos  in
          let suffix = String.sub rel pos (String.length rel - pos) in
          let re = (Str.quote prefix) ^ "\\.\\(~[0-9]+~\\)" ^ (Str.quote suffix) in
          let re = Str.regexp re in
          let dirname = bak // Filename.dirname rel in
          let files =
            if Sys.file_exists dirname then
              Array.to_list (Sys.readdir dirname)
            else []
          in
          let basenames = List.filter_map begin fun basename ->
              if Str.string_match re basename 0 then Some (basename, (Str.matched_group 1 basename))
              else None
            end files
          in
          let filenames = List.map (fun (basename, rev) -> dirname // (Filename.basename basename), rev) basenames in
          let filenames = List.map (fun (filename, rev) -> {filename=Filename filename; rev; date=(Unix.stat filename).Unix.st_mtime; comment="Local Backup"; author=(Glib.get_user_name())}) filenames in
          let backups = List.sort (fun a b -> - compare a.date b.date) filenames in
          List.iter begin fun entry ->
            let date = print_time (Unix.localtime entry.date) in
            let row = model#append () in
            model#set ~row ~column:col_rev entry.rev;
            model#set ~row ~column:col_date date;
            model#set ~row ~column:col_comment entry.comment;
            model#set ~row ~column:col_author entry.author;
            model#set ~row ~column:col_filename entry.filename;
          end backups;
      | _ -> ()
  end

let create ~page =
  let widget = new widget ~page () in
  widget




