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
open Utils
open Preferences
open GUtil
open Settings_t
open Gtk_util

module Log = Common.Log.Make(struct let prefix = "EDITOR_PAGE" end)
let _ =
  Log.set_print_timestamp true;
  Log.set_verbosity `DEBUG

type load_event_phase = [ `Begin | `End ]

let create_view ~project ~buffer ?file ?packing () =
  let sw = GBin.scrolled_window ~width:100 ~height:100 ~shadow_type:`NONE
      ~hpolicy:`NEVER ~vpolicy:`NEVER ?packing () in
  let view = new Ocaml_text.view ~project ~buffer () in
  Preferences_apply.apply (view :> Text.view) Preferences.preferences#get;
  let tview = (view :> Text.view) in
  let _  = sw#add view#coerce in
  sw, tview, view

let shortname filename =
  let basename = Filename.basename filename in
  if Preferences.preferences#get.tab_label_type = 1 then begin
    try Filename.chop_extension basename
    with Invalid_argument _ -> basename
  end else basename

let markup_label filename =
  let shortname = shortname filename in
  if filename ^^^ ".mli" then "<i>"^shortname^"</i>" else shortname

let create_small_button ?button ?tooltip ?pixbuf ?icon ?callback ?packing ?show () =
  let button =
    match button with Some b -> b | _ -> GButton.button ~relief:`NONE ?packing ?show ()
  in
  begin
    match pixbuf with
    | Some pixbuf -> button#set_image (GMisc.image ~pixbuf ())#coerce;
    | _ ->
        icon |> Option.iter (fun icon -> (Gtk_util.label_icon ~width:22 ~height:16 ~packing:button#add icon)#coerce |> ignore)
  end;
  button#set_focus_on_click false;
  button#misc#set_name "smallbutton";
  Gaux.may tooltip ~f:button#misc#set_tooltip_text;
  Gaux.may callback ~f:(fun callback -> ignore (button#connect#clicked ~callback));
  button;;

let create_small_toggle_button ?tooltip ~icon ?callback ?packing ?show () =
  let button = GButton.toggle_button ~relief:`NONE ?packing ?show () in
  let _ = create_small_button ~button:(button :> GButton.button) ?tooltip ~icon ?callback ?packing () in
  button;;

(** Editor page *)
class page ?file ~project ~scroll_offset ~offset ~editor () =
  let file_changed             = new file_changed () in
  let scroll_changed           = new scroll_changed () in
  let load                     = new load () in
  let signals                  = new signals ~file_changed ~scroll_changed ~load in
  let buffer                   = new Ocaml_text.buffer ~project ?file () in
  let sw, text_view, ocaml_view = create_view ~project ~buffer ?file () in
  let vbox                     = GPack.vbox ~spacing:0 () in
  let paned                    = GPack.paned `HORIZONTAL ~packing:vbox#add () in
  let textbox                  = GPack.hbox ~spacing:0 ~packing:paned#add2 () in
  let _                        = textbox#add sw#coerce in (* Text box *)
  let svbox                    = GPack.vbox ~spacing:1 ~packing:textbox#pack () in (* Vertical scrollbar box *)
  let global_gutter_ebox       = GBin.event_box ~packing:textbox#pack () in (* Global gutter box *)
  let editorbar = new Statusbar.editorbar ~view:ocaml_view () in
  let hscrollbar = GRange.scrollbar `HORIZONTAL ~adjustment:sw#hadjustment ~packing:editorbar#add_scrollbar () in
  let _ =
    sw#misc#connect#size_allocate ~callback:begin fun _ ->
      if hscrollbar#adjustment#page_size = hscrollbar#adjustment#upper
      then hscrollbar#misc#hide() else hscrollbar#misc#show();
    end |> ignore;
    if not Oe_config.unify_statusbars then begin
      GMisc.separator `HORIZONTAL ~packing:(vbox#pack ~expand:false) () |> ignore;
      vbox#pack editorbar#coerce;
    end
  in
  let vscrollbar = GRange.scrollbar `VERTICAL ~adjustment:sw#vadjustment (*~update_policy:`DELAYED*) ~packing:svbox#add () in
  let _ =
    text_view#event#connect#scroll ~callback:begin fun ev ->
      let sign = match GdkEvent.Scroll.direction ev with
        | `UP when sw#vadjustment#value > sw#vadjustment#lower -> (-.1.)
        | `DOWN when sw#vadjustment#value < sw#vadjustment#upper -. sw#vadjustment#page_size -> 1.
        | _ -> 0.
      in
      if sign <> 0. then begin
        let value = sw#vadjustment#value +. (sw#vadjustment#step_increment *. sign) in
        (sw#vadjustment#set_value value);
      end;
      false
    end
  in
  (** Global gutter *)
  let global_gutter = GMisc.drawing_area ~width:Oe_config.global_gutter_size ~packing:global_gutter_ebox#add
      ~show:Preferences.preferences#get.editor_show_global_gutter () in
  let _ = global_gutter#misc#set_has_tooltip true in
  let _ = global_gutter#event#add [`BUTTON_PRESS; `BUTTON_RELEASE] in
  let _                        =
    buffer#create_tag ~name:"tag_matching_delim" [
      `BACKGROUND_GDK (Preferences.editor_tag_color "highlight");
      `BACKGROUND_FULL_HEIGHT_SET true;
      (*`UNDERLINE (Preferences.tag_underline "highlight");*)
    ]
  in
  object (self)
    inherit GObj.widget vbox#as_widget
    val mutable view = text_view
    val mutable file : Editor_file_type.abstract_file option = file
    val mutable read_only = false;
    val mutable tab_widget : (GBin.alignment * GButton.button * GMisc.label) option = None
    val mutable resized = false
    val mutable last_autosave_time = buffer#last_edit_time
    val mutable load_complete = false
    val mutable quick_info = Quick_info.create ocaml_view
    val error_indication = new Error_indication.error_indication ocaml_view vscrollbar global_gutter
    val mutable dotview = None
    val mutable word_wrap = editor#word_wrap
    val mutable show_whitespace = editor#show_whitespace_chars
    val mutable signal_button_toggle_wrap = None
    val mutable signal_button_toggle_whitespace = None
    val mutable signal_button_dotview = None
    val mutable global_gutter_tooltips : ((int * int * int * int) * (unit -> GObj.widget)) list = []
    val mutable outline : Oe.outline option = None

    method outline = outline
    method set_outline ol = outline <- Some ol

    method global_gutter_tooltips = global_gutter_tooltips
    method set_global_gutter_tooltips x = global_gutter_tooltips <- x

    method error_indication = error_indication

    method global_gutter = global_gutter
    method vscrollbar = vscrollbar

    method is_changed_after_last_autosave = last_autosave_time < buffer#last_edit_time
    method sync_autosave_time () = last_autosave_time <- Unix.gettimeofday()

    method statusbar = editorbar

    method read_only = read_only

    method set_read_only ro =
      read_only <- ro;
      text_view#set_editable (not ro)

    method set_tab_widget x = tab_widget <- Some x
    method tab_widget = match tab_widget with None -> assert false | Some x -> x
    method file = file
    method set_file file_obj =
      file <- file_obj;
      begin
        match file_obj with
        | Some file_obj ->
            let _, _, label = self#tab_widget in
            label#set_text (shortname file_obj#filename);
            self#update_statusbar();
        | None -> ()
      end;
      buffer#reset_tmp_filename();
      file_changed#call file_obj

    method get_filename = match file with None -> "untitled.ml" | Some f -> f#filename

    method get_title =
      match file with
      | Some file ->
          begin
            match file#remote with
            | None -> file#filename
            | Some rmt -> sprintf "%s@%s%s" rmt.Editor_file_type.user rmt.Editor_file_type.host file#filename
          end;
      | _ -> ""

    method view = view
    method ocaml_view = ocaml_view
    method buffer = buffer
    method project = project
    method vadjustment = sw#vadjustment
    method status_pos_sel = editorbar#pos_sel, editorbar#pos_sel_chars
    method undo () = if not (buffer#undo#undo()) then (text_view#scroll_lazy (buffer#get_iter `INSERT))
    method redo () = if not (buffer#undo#redo()) then (text_view#scroll_lazy (buffer#get_iter `INSERT))

    method initial_offset : int = offset
    method scroll_offset = scroll_offset

    method redisplay () =
      Colorize.colorize_buffer ocaml_view;
      Preferences_apply.apply view Preferences.preferences#get;
      Gaux.may (GtkText.TagTable.lookup buffer#tag_table "tag_matching_delim")
        ~f:(fun x -> GtkText.TagTable.remove buffer#tag_table x);
      ignore (buffer#create_tag ~name:"tag_matching_delim" [
          `BACKGROUND_GDK (Preferences.editor_tag_color "highlight");
          `BACKGROUND_FULL_HEIGHT_SET true;
        ]);
      self#error_indication#create_tags();
      self#error_indication#set_flag_underline Preferences.preferences#get.editor_err_underline;
      self#error_indication#set_flag_tooltip Preferences.preferences#get.editor_err_tooltip;
      self#error_indication#set_flag_gutter Preferences.preferences#get.editor_err_gutter;
      self#view#create_highlight_current_line_tag();
      error_indication#set_phase ();

    method update_statusbar () =
      match self#file with
      | None -> ()
      | Some file ->
          try
            editorbar#filename#set_label self#get_title;
            begin
              match file#stat() with
              | Some stat ->
                  let tm = Unix.localtime (stat.Editor_file_type.mtime) in
                  let last_modified = sprintf "%4d-%d-%d %02d:%02d:%02d" (tm.Unix.tm_year + 1900)
                      (tm.Unix.tm_mon + 1) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
                  in
                  ksprintf editorbar#filename#misc#set_tooltip_markup "%s%s\nLast modified: %s\n%s bytes - %s lines - %s characters"
                    (if project.Prj.in_source_path file#filename <> None then "<b>" ^ project.Prj.name ^ "</b>\n" else "")
                    self#get_title
                    last_modified
                    (Text_util.format_int stat.Editor_file_type.size)
                    (Text_util.format_int buffer#line_count)
                    (Text_util.format_int buffer#char_count);
              | _ -> ()
            end;
          with _ -> ()

    method backup () = Gaux.may file ~f:begin fun (file : Editor_file.file) ->
        Project.backup_file project file
      end

    method save () = Gaux.may file ~f:begin fun file ->
        if not file#is_readonly then begin
          if Preferences.preferences#get.editor_bak then (self#backup());
          file#write (buffer#get_text ());
          Gmisclib.Idle.add self#update_statusbar;
          Gmisclib.Idle.add (fun () -> self#compile_buffer ?join:None ());
          (* Delete existing recovery copy *)
          self#sync_autosave_time ();
          Autosave.delete ~filename:file#filename ();
          (*  *)
          buffer#set_modified false;
        end
      end

    method revert () = Gaux.may file ~f:begin fun file ->
        Project.save_bookmarks project;
        let old_markers =
          try
            let bms = List.filter (fun bm -> bm.Oe.bm_filename = file#filename) project.Prj.bookmarks in
            List.iter Bookmark.mark_to_offset bms;
            List.filter_map (fun bm -> bm.Oe.bm_marker) bms
          with Not_found -> []
        in
        Gutter.destroy_markers view#gutter old_markers;
        (*  *)
        let vv = vscrollbar#adjustment#value in
        buffer#block_signal_handlers();
        buffer#delete ~start:buffer#start_iter ~stop:buffer#end_iter;
        ignore (self#load());
        buffer#unblock_signal_handlers();
        self#set_file (Some file);
        (*  *)
        self#sync_autosave_time ();
        Autosave.delete ~filename:file#filename ();
        (*  *)
        Gmisclib.Idle.add ~prio:300 (fun () -> vscrollbar#adjustment#set_value vv);
        Gmisclib.Idle.add ~prio:400 begin fun () ->
          let rect = view#visible_rect in
          let where, _ = view#get_line_at_y (Gdk.Rectangle.y rect + Gdk.Rectangle.height rect / 2) in
          buffer#place_cursor ~where
        end
      end

    method load_complete = load_complete

    method load ?(scroll=true) () =
      match file with
      | None -> false
      | Some file ->
          begin
            try
              view#misc#hide();
              load#call `Begin;
              buffer#insert file#read;
              (* Initial cursor position and syntax highlighting *)
              Gmisclib.Idle.add begin fun () ->
                if scroll then begin
                  let where = buffer#get_iter (`OFFSET scroll_offset) in
                  self#view#scroll_to_iter ~use_align:(self#view#scroll_to_iter where) ~xalign:1.0 where |> ignore;
                  let where = buffer#get_iter (`OFFSET offset) in
                  buffer#place_cursor ~where;
                end;
                Colorize.colorize_buffer ocaml_view;
                view#misc#show();
                view#misc#grab_focus();
              end;
              buffer#set_modified false;
              if not buffer#undo#is_enabled then (buffer#undo#enable());
              load_complete <- true;
              buffer#save_buffer ~filename:buffer#orig_filename () |> ignore;
              (*buffer#set_last_edit_time (Unix.gettimeofday());*)
              last_autosave_time <- buffer#last_edit_time;
              (*  *)
              self#view#matching_delim ();
              Gmisclib.Idle.add ~prio:300 (fun () -> self#compile_buffer ?join:None ());
              (* Bookmarks: offsets to marks *)
              let redraw = ref false in
              List.iter begin fun bm ->
                if bm.Oe.bm_filename = file#filename then
                  let mark = (Bookmark.offset_to_mark (self#buffer :> GText.buffer) bm) in
                  let callback =
                    if bm.Oe.bm_num >= Bookmark.limit then Some (fun _ ->
                        editor#bookmark_remove ~num:bm.Oe.bm_num;
                        redraw := true;
                        true)
                    else None
                  in
                  let marker = Gutter.create_marker ~mark ?pixbuf:(Bookmark.icon bm.Oe.bm_num) ?callback () in
                  bm.Oe.bm_marker <- Some marker;
                  view#gutter.Gutter.markers <- marker :: view#gutter.Gutter.markers
              end project.Prj.bookmarks;
              Project.save_bookmarks project;
              load#call `End;
              if !redraw then (GtkBase.Widget.queue_draw text_view#as_widget);
              true
            with Glib.Convert.Error (_, message) -> begin
                let message = if project.Prj.encoding <> Some "UTF-8" then (ksprintf Convert.to_utf8
                                                                              "Cannot convert file\n\n%s\n\nfrom %s codeset to UTF-8.\n\n%s"
                                                                              file#filename
                                                                              (match project.Prj.encoding with None -> "Default" | Some x -> x)
                                                                              message) else message
                in
                let dialog = GWindow.message_dialog ~title:"Text file contains invalid characters."
                    ~message_type:`ERROR ~message ~buttons:GWindow.Buttons.ok () in
                (match dialog#run () with _ -> dialog#destroy())
              end;
              false;
          end

    method compile_buffer ?join () =
      let filename = self#get_filename in
      if project.Prj.autocomp_enabled
      && ((project.Prj.in_source_path filename) <> None)
      && (filename ^^^ ".ml" || filename ^^^ ".mli") then begin
        buffer#sync_autocomp_time ();
        Autocomp.compile_buffer ~project ~editor ~page:self ?join ();
      end else begin
      end

    method tooltip ((*(x, y) as*) location) =
      if Preferences.preferences#get.editor_err_tooltip
      then (error_indication#tooltip (`XY location))

    method status_modified_icon = editorbar#modified

    method create_menu () = Editor_menu.create ~editor ~page:self ()

    method set_word_wrap value =
      Gaux.may signal_button_toggle_wrap ~f:editorbar#button_toggle_wrap#misc#handler_block;
      editorbar#button_toggle_wrap#set_active value;
      Gaux.may signal_button_toggle_wrap ~f:editorbar#button_toggle_wrap#misc#handler_unblock;
      text_view#options#set_word_wrap value;
      word_wrap <- value;
      Gmisclib.Idle.add (fun () -> GtkBase.Widget.queue_draw text_view#as_widget);

    method set_show_whitespace value =
      Gaux.may signal_button_toggle_whitespace ~f:editorbar#button_toggle_whitespace#misc#handler_block;
      editorbar#button_toggle_whitespace#set_active value;
      Gaux.may signal_button_toggle_whitespace ~f:editorbar#button_toggle_whitespace#misc#handler_unblock;
      text_view#options#set_show_whitespace_chars value;
      show_whitespace <- value;
      Gmisclib.Idle.add (fun () -> GtkBase.Widget.queue_draw text_view#as_widget);

    method button_dep_graph = editorbar#button_dotview

    method show_revision_history () =
      let rev = Revisions.create ~page:self in
      let hbox = GPack.hbox ~spacing:1 () in
      let _ = GMisc.image ~pixbuf:(??? Icons.history) ~packing:hbox#pack () in
      let label = GMisc.label ~text:(sprintf "\xC2\xAB%s\xC2\xBB history" (Filename.basename self#get_filename)) ~packing:hbox#pack () in
      Messages.vmessages#append_page ~label_widget:hbox#coerce ~with_spinner:false rev;
      rev#set_title label#text;
      rev#present();
      rev#set_icon None;

    method private show_dep_graph () =
      match dotview with
      | Some widget ->
          widget#destroy();
          dotview <- None;
          textbox#misc#show();
          List.iter (fun b -> b#misc#set_sensitive true)
            [ editorbar#button_font_incr;
              editorbar#button_font_decr;
              editorbar#button_rowspacing_incr;
              editorbar#button_rowspacing_decr; (*button_h_prev; button_h_next; button_h_last*)];
          List.iter (fun b -> b#misc#set_sensitive true) [editorbar#button_toggle_wrap; editorbar#button_toggle_whitespace];
          hscrollbar#misc#show();
          editorbar#pos_box#misc#show();
      | None ->
          begin
            let reset_button () =
              Gaux.may signal_button_dotview ~f:editorbar#button_dotview#misc#handler_block;
              editorbar#button_dotview#set_active false;
              Gaux.may signal_button_dotview ~f:editorbar#button_dotview#misc#handler_unblock;
            in
            try
              begin match editor#project.Prj.in_source_path self#get_filename with
              | Some filename ->
                  let filename = String.concat "/" (Utils.filename_split filename) in
                  Dot.draw ~project:editor#project ~filename ();
                  reset_button()

              | None ->
                  let title = "Could not show dependency graph" in
                  let message = sprintf "%s for file: \n\n%s" title self#get_filename in
                  Dialog.message ~title ~message `INFO;
                  reset_button ()
              end
            with ex ->
              reset_button();
              Dialog.display_exn ~parent:self ~title:"Error creating dependency graph" ex
          end

    method cut () =
      self#copy ();
      self#delete ()

    method copy () =
      let clipboard = GData.clipboard Gdk.Atom.clipboard in
      clipboard#set_text (buffer#selection_text ());

    method delete () =
      let start, stop = buffer#selection_bounds in
      buffer#delete ~start ~stop

    method paste () =
      match (GData.clipboard (Gdk.Atom.clipboard))#text with
      | None -> ()
      | Some text ->
          if view#editable then begin
            GtkSignal.stop_emit();
            ignore (buffer#delete_interactive ~start:(buffer#get_iter `INSERT) ~stop:(buffer#get_iter `SEL_BOUND) ());
            buffer#insert text;
            let iter = buffer#get_iter `INSERT in
            if buffer#lexical_enabled then begin
              Lexical.tag view#buffer
                ~start:(iter#backward_chars (Glib.Utf8.length text))#backward_line
                ~stop:iter#forward_line;
            end;
            buffer#remove_tag view#highlight_current_line_tag
              ~start:(iter#backward_chars (Glib.Utf8.length text))#backward_line
              ~stop:iter#forward_line;
            view#draw_current_line_background ~force:true iter;
            Gmisclib.Idle.add (fun () -> ignore (view#scroll_to_iter iter));
          end

    method quick_info = quick_info

    method quick_info_at_iter iter =
      Quick_info.at_iter quick_info iter ()

    initializer
      global_gutter#misc#connect#query_tooltip ~callback:begin fun ~x ~y ~kbd tooltip ->
        try
          let _, f = List.find (fun ((x0, y0, w, h), _) -> x0 <= x && x <= x0 + w && y0 <= y && y <= y0 + h) global_gutter_tooltips in
          GtkBase.Tooltip.set_custom tooltip (f())#as_widget;
          true
        with Not_found -> false
      end |> ignore;
      ignore (self#misc#connect#destroy ~callback:begin fun () ->
          Option.iter (fun f -> f#cleanup()) file
        end);
      (**  *)
      view#hyperlink#enable();
      (** Expose: Statusbar *)
      let signal_expose = ref (self#view#event#connect#after#expose ~callback:begin fun _ ->
          let iter = self#buffer#get_iter `INSERT in
          editorbar#pos_lin#set_text (string_of_int (iter#line + 1));
          editorbar#pos_col#set_text (string_of_int (iter#line_offset + 1));
          editorbar#pos_off#set_text (string_of_int iter#offset);
          false
        end)
      in
      ignore (vscrollbar#connect#value_changed ~callback:begin fun () ->
          view#misc#handler_block !signal_expose;
          scroll_changed#call()
        end);
      ignore (vscrollbar#connect#after#value_changed ~callback:(fun () ->
          Gmisclib.Idle.add ~prio:300 (fun () -> view#misc#handler_unblock !signal_expose)));
      (* After focus_in, check if the file is changed on disk *)
      ignore (text_view#event#connect#after#focus_in ~callback:begin fun _ ->
          Gaux.may self#file ~f:begin fun f ->
            if f#exists && f#changed then
              if buffer#modified then begin
                let message = "File\n" ^ self#get_filename^"\nchanged on disk, reload?" in
                let yes = "Reload", self#revert in
                let no = "Do Not Reload", (fun () -> self#set_file (Some f)) in
                ignore (Dialog.confirm ~title:"Reload File" ~message ~yes ~no self);
              end else self#revert();
            self#set_read_only f#is_readonly;
            if read_only then begin
              editorbar#modified#set_label "\u{ea75}";
              editorbar#modified#misc#set_tooltip_text "Read-only"
            end;
          end;
          false
        end);
      (* Hyperlinks *)
      let current_hyperlink_exists = ref false in
      let remove_hover = ref None in
      self#view#hyperlink#connect#hover ~callback:begin
        let get_bounds iter =
          let start, stop = self#buffer#select_word ~iter ~select:false ~pat:Ocaml_word_bound.longid () in
          if !current_hyperlink_exists then begin
            begin
              match !remove_hover with
              | None ->
                  remove_hover := Some (self#view#hyperlink#connect#remove_hover ~callback:begin fun () ->
                      current_hyperlink_exists := false;
                      !remove_hover |> Option.iter (fun sig_id ->
                          remove_hover := None;
                          self#view#hyperlink#connect#disconnect sig_id);
                    end);
              | _ -> ()
            end;
            Some (start, stop)
          end else None
        in
        fun (bounds, iter) ->
          if iter#inside_word then
            if not !current_hyperlink_exists then begin
              match
                Definition.locate
                  ~filename:self#get_filename ~text:(buffer#get_text ()) ~iter
              with
              | Merlin.Ok (Some _) -> current_hyperlink_exists := true;
              | Merlin.Ok None | Merlin.Failure _ | Merlin.Error _ -> ()
            end;
          bounds := get_bounds iter
      end |> ignore;
      self#view#hyperlink#connect#activate ~callback:(fun iter -> editor#scroll_to_definition ~page:self ~iter) |> ignore;
      (*  *)
      ocaml_view#buffer#connect#mark_set ~callback:begin fun it mark ->
        match GtkText.Mark.get_name mark with
        | Some "insert" -> error_indication#paint_global_gutter()
        | _ -> ()
      end |> ignore;
      (** Mark occurrences *)
      ignore (view#mark_occurrences_manager#connect#mark_set ~callback:error_indication#paint_global_gutter);
      (**  *)
      let callback _ = self#set_word_wrap (not word_wrap) in
      self#set_word_wrap word_wrap;
      signal_button_toggle_wrap <- Some (editorbar#button_toggle_wrap#connect#clicked ~callback);
      let callback _ = self#set_show_whitespace (not show_whitespace) in
      self#set_show_whitespace show_whitespace;
      signal_button_toggle_whitespace <- Some (editorbar#button_toggle_whitespace#connect#clicked ~callback);
      (** Dotview *)
      if Oe_config.dot_version <> None then begin
        (signal_button_dotview <- Some (editorbar#button_dotview#connect#clicked ~callback:self#show_dep_graph));
        editorbar#button_dotview#misc#set_tooltip_text "Dependency Graph";
        editorbar#button_dotview#misc#set_sensitive (Menu_view.get_switch_view_sensitive editor#project self);
      end

    method show_outline () =
      match outline with
      | Some outline ->
          let outline_view = new Outline.view ~outline ~source_view:self#ocaml_view () in
          paned#add1 (outline_view :> GObj.widget);
          Gmisclib.Idle.add ~prio:300 outline_view#refresh
      | _ -> Log.println `ERROR "outline does not exist"

    method hide_outline () =
      try paned#child1#destroy()
      with Gpointer.Null -> ()

    method connect = signals
    method disconnect = signals#disconnect
  end

(** Signals *)
and file_changed () = object inherit [Editor_file.file option] signal () end
and scroll_changed () = object inherit [unit] signal () end
and load () = object inherit [load_event_phase] signal () end

and signals ~file_changed ~scroll_changed ~load =
  object
    inherit ml_signals [file_changed#disconnect; scroll_changed#disconnect; load#disconnect]
    method file_changed = file_changed#connect ~after
    method scroll_changed = scroll_changed#connect ~after
    method load = load#connect ~after
  end
