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

open GUtil
open Utils
open Printf
open Preferences

module Log = Common.Log.Make(struct let prefix = "EDITOR" end)
let _ =
  Log.set_print_timestamp true;
  Log.set_verbosity `ERROR

let set_menu_item_nav_history_sensitive = ref (fun () -> failwith "set_menu_item_nav_history_sensitive")

(** Editor *)
class editor () =
  let hpaned = GPack.paned `HORIZONTAL () in
  let notebook = GPack.notebook ~tab_border:0 ~show_border:true
      ~packing:(hpaned#pack2 ~resize:true ~shrink:true) ~scrollable:true () in
  let _ = hpaned#set_position Preferences.preferences#get.outline_width in
  let incremental_search = new Incremental_search.incremental () in
  let switch_page = new switch_page () in
  let remove_page = new remove_page () in
  let modified_changed = new modified_changed () in
  let file_history_changed = new file_history_changed () in
  let outline_visibility_changed = new outline_visibility_changed () in
  let file_saved = new file_saved () in
  let notification = new notification () in
  let changed = new changed () in
  let add_page = new add_page () in
  object (self)
    inherit GObj.widget hpaned#as_widget

    (*val mutable closing_all = false*)
    val mutable history_switch_page_locked = false
    val mutable pages = []
    val mutable pages_cache = []
    val mutable project = Project.create ~filename:"untitled.xyz" ()
    val tout_delim = Timeout.create ~delay:1.5 ~len:3 ()
    val tout_fast = Timeout.create ~delay:0.3 ~len:2 ()
    val location_history = Location_history.create()
    val mutable file_history =
      File_history.create
        ~filename:Oe_config.file_history_filename
        ~max_length:Oe_config.file_history_max_length
    val mutable history_switch_page = [];
    val code_folding_enabled = new GUtil.variable false
    val show_global_gutter = new GUtil.variable false
    val mutable show_whitespace_chars = Preferences.preferences#get.editor_show_whitespace_chars
    val mutable word_wrap = Preferences.preferences#get.editor_wrap
    val mutable show_outline = true

    method status_message = notification#call

    method tout_delim = tout_delim

    method paned = hpaned

    method create_file ?remote filename = Editor_file.create ?remote filename

    method clear_cache () =
      List.iter (fun (_, p) -> p#destroy()) pages_cache;
      pages_cache <- []

    method set_history_switch_page_locked x =
      history_switch_page_locked <- x;
      Gaux.may (self#get_page `ACTIVE) ~f:begin fun page ->
        if not history_switch_page_locked then (switch_page#call page)
      end

    method show_outline = show_outline

    method set_show_outline show =
      if show then
        pages |> List.iter (fun page -> page#show_outline())
      else
        pages |> List.iter (fun page -> page#hide_outline());
      show_outline <- show;

    method show_whitespace_chars = show_whitespace_chars
    method set_show_whitespace_chars x =
      show_whitespace_chars <- x;
      List.iter begin fun page ->
        page#view#options#set_show_whitespace_chars x;
        page#set_show_whitespace x;
        (*GtkBase.Widget.queue_draw page#view#as_widget*)
      end (pages @ (snd (List.split pages_cache)))

    method word_wrap = word_wrap
    method set_word_wrap x =
      word_wrap <- x;
      List.iter begin fun page ->
        page#view#options#set_word_wrap x;
        page#set_word_wrap x;
        (*GtkBase.Widget.queue_draw page#view#as_widget;*)
      end (pages @ (snd (List.split pages_cache)))

    method with_current_page (f : Editor_page.page -> unit) =
      match self#get_page `ACTIVE with Some page -> f page | _ -> ()

    method code_folding_enabled = code_folding_enabled
    method show_global_gutter = show_global_gutter

    method file_history = file_history
    method file_history_clear () =
      File_history.clear file_history;
      file_history_changed#call file_history

    method location_history = location_history

    method project = project
    method set_project proj = project <- proj

    method tab_pos = notebook#tab_pos

    method show_tabs = notebook#show_tabs
    method set_show_tabs x =
      notebook#set_show_tabs x;
      if x then (notebook#misc#grab_focus())

    method pages = List.rev pages

    method goto_view v =
      match self#get_page (`VIEW v) with
      | None -> ()
      | Some page ->
          if not page#load_complete then (self#load_page page);
          notebook#goto_page (notebook#page_num page#coerce);

    method get_page :
      [`ACTIVE | `FILENAME of string | `NUM of int | `VIEW of Text.view] -> Editor_page.page option =
      function
      | `ACTIVE ->
          List.find_opt (fun p ->
              p#get_oid = (notebook#get_nth_page notebook#current_page)#get_oid) pages
      | `FILENAME filename ->
          let uncapitalize = Fun.id in
          let filename = uncapitalize filename in
          List.find_opt (fun p ->
              match p#file with None -> false | Some f -> uncapitalize f#filename = filename) pages
      | `NUM n -> List.find_opt (fun p -> p#get_oid = (notebook#get_nth_page n)#get_oid) pages
      | `VIEW v -> List.find_opt (fun p -> p#view#get_oid = v#get_oid) pages

    method dialog_file_open () = Editor_dialog.file_open ~editor:self ()

    method switch_mli_ml (page : Editor_page.page) =
      let filename = page#get_filename in
      if filename ^^^ ".ml" then begin
        let mli = (Filename.chop_extension filename) ^ ".mli" in
        self#load_mli page#project mli
      end else if filename ^^^ ".mli" then begin
        let ml = (Filename.chop_extension filename) ^ ".ml" in
        ignore (self#open_file ~active:true ~scroll_offset:0 ~offset:0 ml)
      end

    method private load_mli proj filename =
      if Sys.file_exists filename then (ignore (self#open_file ~active:true ~scroll_offset:0 ~offset:0 filename))
      else begin
        ignore (Dialog.confirm
                  ~title:"Create File"
                  ~message:("File\n\""^filename^"\"\ndoes not exist, create?")
                  ~yes:("Create", begin fun () ->
                      let ml = (Filename.chop_extension filename) ^ ".ml" in
                      let old = Sys.getcwd () in
                      Sys.chdir (Filename.dirname filename);
                      let search_path = Project.get_search_path_i_format project in
                      let cmd = (sprintf "ocamlc -i -w a -thread %s %s > %s"
                                   search_path
                                   (Filename.basename ml) (Filename.basename filename)) in
                      ignore (Sys.command cmd);
                      Sys.chdir old;
                      self#load_mli proj filename
                    end)
                  ~no:("Do Not Create", fun () -> ()) self);
      end

    method location_history_add ~page ~iter ~kind () =
      Location_history.add location_history
        ~kind ~view:(page#view :> GText.view)
        ~filename:page#get_filename ~offset:iter#offset;
      Timeout.set tout_delim 1 !set_menu_item_nav_history_sensitive;

    method location_history_goto location =
      let filename = location.Location_history.filename in
      if Sys.file_exists filename then begin
        match self#get_page (`FILENAME filename) with
        | None ->
            ignore (self#open_file ~active:true ~scroll_offset:0 ~offset:0 filename);
            self#location_history_goto location
        | Some page ->
            let view = (page#view :> Text.view) in
            self#goto_view view;
            let where =
              match location.Location_history.mark with
              | Some mark -> page#buffer#get_iter_at_mark (`MARK mark)
              | None ->
                  let offset = location.Location_history.offset in
                  (* Now that the page is loaded, marks are used in place of offsets. *)
                  location.Location_history.mark <-
                    (Some (Location_history.create_mark ~buffer:(view :> GText.view)#buffer ~offset));
                  page#buffer#get_iter (`OFFSET offset);
            in
            view#buffer#place_cursor ~where;
            ignore (view#scroll_lazy where);
            view#misc#grab_focus();
      end

    method bookmark_remove ~num =
      self#with_current_page begin fun page ->
        let old_marker =
          try (List.find (fun bm -> bm.Oe.bm_num = num) project.Prj.bookmarks).Oe.bm_marker
          with Not_found -> None
        in
        Gaux.may old_marker ~f:(fun old -> Gutter.destroy_markers page#view#gutter [old]);
        Project.remove_bookmark num project;
        page#view#draw_gutter();
      end

    method bookmark_create ~num ?where ?(callback : (Gtk.text_mark -> bool) option) () =
      self#with_current_page begin fun page ->
        let filename = page#get_filename in
        let where = match where with Some x -> x | _ -> page#buffer#get_iter `INSERT in
        let mark = page#buffer#create_mark(* ~name:(Gtk_util.create_mark_name "Editor.bookmark_create")*) where in
        let old_marker =
          try (List.find (fun bm -> bm.Oe.bm_num = num) project.Prj.bookmarks).Oe.bm_marker
          with Not_found -> None
        in
        Gaux.may old_marker ~f:(fun old -> Gutter.destroy_markers page#view#gutter [old]);
        let marker = Gutter.create_marker ~kind:(`Bookmark num) ~mark ?pixbuf:(Bookmark.icon num) ?callback () in
        let bm = Bookmark.create ~num ~filename ~mark ~marker () in
        Project.set_bookmark bm project;
        page#view#gutter.Gutter.markers <- marker :: page#view#gutter.Gutter.markers;
        page#view#draw_gutter();
      end

    method bookmark_goto ~num =
      try
        let bm = List.find (fun bm -> bm.Oe.bm_num = num) project.Prj.bookmarks in
        match self#get_page (`FILENAME bm.Oe.bm_filename) with
        | None when Sys.file_exists bm.Oe.bm_filename ->
            let _ = self#open_file ~active:true ~scroll_offset:0 ~offset:0 bm.Oe.bm_filename in
            self#bookmark_goto ~num
        | None ->
            Dialog.info ~title:"File does not exist" ~message_type:`INFO
              ~message:(sprintf "File \xC2\xAB%s\xC2\xBB does not exist." bm.Oe.bm_filename) self;
            self#bookmark_remove ~num
        | Some page ->
            if not (page#view#misc#get_flag `REALIZED) then (self#goto_view page#view);
            Gmisclib.Idle.add ~prio:300 begin fun () ->
              ignore (Bookmark.apply bm begin function
                | `OFFSET _ ->
                    let _ = Bookmark.offset_to_mark (page#buffer :> GText.buffer) bm in
                    self#bookmark_goto ~num;
                    -1
                | `ITER it ->
                    let where = new GText.iter it in
                    page#view#scroll_lazy where;
                    page#buffer#place_cursor ~where;
                    page#view#misc#grab_focus();
                    -1
                end)
            end;
            if page#view#misc#get_flag `REALIZED then (Gmisclib.Idle.add (*~prio:300*) (fun () -> self#goto_view page#view));
            Gmisclib.Idle.add ~prio:300 (fun () -> Project.save_bookmarks project);
      with Not_found -> ()

    method scroll_to_definition ~page ~iter =
      match self#get_page `ACTIVE with
      | Some page ->
          Definition.locate
            ~filename:page#get_filename
            ~text:(page#buffer#get_text ())
            ~iter
          |> begin function
          | Merlin.Ok (Some range) ->
              GtkThread.async begin fun () ->
                let open Merlin_t in
                self#location_history_add ~page ~iter ~kind:`BROWSE ();
                let filename = match range.file with None -> "" | Some x -> x in
                self#goto_location filename range.start.line range.start.col;
              end ()
          | Merlin.Ok None -> ()
          | Merlin.Failure msg -> Printf.printf "%s\n%!" msg
          | Merlin.Error msg -> Printf.eprintf "File %s: %s\n%!" __FILE__ msg
          end
      | _ -> ()

    method goto_location filename line col =
      let target_page = if filename = "" then `ACTIVE else `FILENAME filename in
      match self#get_page target_page with
      | Some page ->
          if not page#load_complete then (self#load_page ~scroll:false page);
          let buffer = page#buffer in
          let ts = buffer#last_edit_time in
          let start = buffer#get_iter (`LINE line) in
          let old = page#view#options#mark_occurrences in
          page#view#options#set_mark_occurrences (false, false, "");
          page#ocaml_view#scroll_lazy start;
          page#buffer#place_cursor ~where:(buffer#get_iter (`LINECHAR (line, col)));
          page#view#options#set_mark_occurrences old;
          self#goto_view page#view;
          switch_page#call page;
          let iter = buffer#get_iter `INSERT in
          self#location_history_add ~page ~iter ~kind:(`BROWSE : Location_history.kind) ();
          Gmisclib.Idle.add page#view#misc#grab_focus
      | _ ->
          ignore (self#open_file ~active:false ~scroll_offset:0 ~offset:0 filename);
          self#goto_location filename line col

    method dialog_file_select () = Editor_dialog.file_select ~editor:self ()

    method set_tab_pos ?(page:Editor_page.page option) pos =
      notebook#set_tab_pos pos;
      let angle = match pos with
        | `RIGHT when preferences#get.tab_vertical_text -> 270.
        | `LEFT when preferences#get.tab_vertical_text -> 90.
        | _ -> 0.
      in
      let pgs = match page with None -> pages | Some p -> [p] in
      List.iter begin fun page ->
        let filename = page#get_filename in
        let (align : GBin.alignment), (button : GButton.button), (label : GMisc.label) = page#tab_widget in
        label#set_label (Editor_page.markup_label filename);
        label#set_angle angle;
        let tbox = match pos with
          | `RIGHT when preferences#get.tab_vertical_text ->
              let tbox = GPack.vbox () in
              if button#misc#parent <> None then (button#misc#reparent tbox#coerce) else (tbox#pack button#coerce);
              if label#misc#parent <> None then (label#misc#reparent tbox#coerce) else (tbox#pack label#coerce);
              List.iter (fun child -> child#destroy()) align#children;
              align#add tbox#coerce;
              label#set_xalign 0.5;
              label#set_yalign 0.0;
              tbox
          | `LEFT when preferences#get.tab_vertical_text ->
              let tbox = GPack.vbox () in
              if button#misc#parent <> None then (button#misc#reparent tbox#coerce) else (tbox#pack button#coerce);
              if label#misc#parent <> None then (label#misc#reparent tbox#coerce) else (tbox#pack label#coerce);
              List.iter (fun child -> child#destroy()) align#children;
              align#add tbox#coerce;
              label#set_xalign 0.5;
              label#set_yalign 1.0;
              tbox
          | _ ->
              let tbox = GPack.hbox () in
              if button#misc#parent <> None then (button#misc#reparent tbox#coerce) else (tbox#pack button#coerce);
              if label#misc#parent <> None then (label#misc#reparent tbox#coerce) else (tbox#pack label#coerce);
              List.iter (fun child -> child#destroy()) align#children;
              align#add tbox#coerce;
              label#set_xalign 0.0;
              label#set_yalign 0.5;
              tbox
        in
        tbox#set_child_packing ~expand:false ~fill:false button#coerce;
        tbox#set_child_packing ~expand:false ~fill:false label#coerce;
      end pgs;

    val tags_to_toggle = 3 (* to match: ?(label=...) 3 *)
    method private colorize_within_nearest_tag_bounds buffer iter =
      (*Prf.crono Prf.prf_colorize_within_nearest_tag_bounds begin fun () ->*)
      (*let tag_table_lexical = buffer#tag_table_lexical in*)
      let count = ref tags_to_toggle in
      let start =
        let start = ref iter in
        while !count <> 0 && not !start#is_start do
          start := !start#backward_to_tag_toggle None;
          if !start#begins_tag None then decr count;
          (*start := !start#backward_char;
            List.iter (fun tag -> if !start#begins_tag tag then decr count) tag_table_lexical;*)
        done;
        !start;
      in
      count := tags_to_toggle;
      let stop =
        let stop = ref iter in
        while !count <> 0 && not !stop#is_end do
          stop := !stop#forward_to_tag_toggle None;
          (*if !stop#ends_tag None then *)
          decr count;
          (*stop := !stop#forward_char;
            List.iter (fun tag -> if !stop#ends_tag tag then decr count) tag_table_lexical;*)
        done;
        !stop
      in
      Lexical.tag buffer ~start ~stop
    (*end ()*)

    method load_page ?(scroll=true) (page : Editor_page.page) =
      if not page#load_complete then begin
        (* Load page *)
        ignore (page#load ~scroll ());
        page#view#options#set_show_whitespace_chars show_whitespace_chars;
        page#view#options#set_word_wrap word_wrap;
        (* Insert_text and Delete range *)
        let buffer = page#buffer in
        let gtext_buffer = buffer#as_gtext_buffer in
        let view = page#view in
        let ocaml_view = page#ocaml_view in
        let cb_tout_fast () =
          if buffer#lexical_enabled then begin
            let iter = buffer#get_iter `INSERT in
            self#colorize_within_nearest_tag_bounds gtext_buffer iter;
          end;
          view#draw_gutter ();
        in
        let callback iter _ =
          Gmisclib.Idle.add ~prio:100 (fun () -> view#draw_current_line_background ~force:true (buffer#get_iter `INSERT));
          Timeout.set tout_fast 0 cb_tout_fast;
          Timeout.set tout_delim 0 (self#cb_tout_delim page);
          self#location_history_add ~page ~iter ~kind:`EDIT ();
        in
        buffer#add_signal_handler (buffer#connect#insert_text ~callback);
        buffer#add_signal_handler (buffer#connect#after#delete_range ~callback:(fun ~start ~stop -> callback start stop));
        (* Mark Set *)
        let lab_sel_lines, lab_sel_chars = page#status_pos_sel in
        buffer#add_signal_handler (buffer#connect#after#mark_set ~callback:begin fun _ mark ->
            let mark_occurrences, under_cursor, _ = view#options#mark_occurrences in
            let is_insert = match GtkText.Mark.get_name mark with Some "insert" -> true | _ -> false in
            if mark_occurrences && under_cursor then begin
              Timeout.set tout_fast 1 page#view#mark_occurrences_manager#mark_words;
              Timeout.set tout_delim 2 page#view#mark_occurrences_manager#mark_refs;
            end;
            if buffer#has_selection then begin
              let start, stop = buffer#selection_bounds in
              let nlines = stop#line - start#line in
              let nchars = stop#offset - start#offset in
              lab_sel_lines#set_text (string_of_int nlines);
              lab_sel_chars#set_text (string_of_int nchars);
              if is_insert && mark_occurrences && not under_cursor then begin
                Timeout.set tout_fast 1 page#view#mark_occurrences_manager#mark_words;
                Timeout.set tout_delim 2 page#view#mark_occurrences_manager#mark_refs;
              end
            end else begin
              if mark_occurrences && not under_cursor then
                page#view#mark_occurrences_manager#clear();
              lab_sel_lines#set_text "0";
              lab_sel_chars#set_text "0";
            end;
            if is_insert then Timeout.set tout_delim 0 (self#cb_tout_delim page)
          end);
        (* Paste clipboard *)
        ignore (page#view#connect#paste_clipboard ~callback:page#paste);
        (*  *)
        modified_changed#call();
        page#update_statusbar();
        Gmisclib.Idle.add page#create_menu;
      end

    method private create_tab_menu page ev =
      let menu = GMenu.menu () in
      let filename = page#get_filename in
      let basename = Filename.basename filename in
      let item = GMenu.image_menu_item ~label:(sprintf "Close \xC2\xAB%s\xC2\xBB" basename) ~packing:menu#add () in
      item#set_image (Icons.create (??? Icons.close_16))#coerce;
      ignore (item#connect#activate ~callback:(fun () -> ignore (self#dialog_confirm_close page)));
      let item = GMenu.image_menu_item ~label:(sprintf "Close All Except \xC2\xAB%s\xC2\xBB" basename) ~packing:menu#add () in
      ignore (item#connect#activate ~callback:(fun () -> self#close_all ~except:page ()));
      let item = GMenu.image_menu_item ~label:(sprintf "Revert \xC2\xAB%s\xC2\xBB" basename) ~packing:menu#add () in
      item#set_image (GMisc.image ~pixbuf:(??? Icons.revert_to_saved_16) (*~stock:`REVERT_TO_SAVED*) ~icon_size:`MENU ())#coerce;
      ignore (item#connect#activate ~callback:(fun () -> self#revert page));
      let _ = GMenu.separator_item ~packing:menu#add () in
      let item = GMenu.menu_item ~label:"Copy Full Path" ~packing:menu#add () in
      ignore (item#connect#activate ~callback:begin fun () ->
          let clipboard = GData.clipboard Gdk.Atom.clipboard in
          clipboard#set_text filename;
        end);
      let item = GMenu.menu_item ~label:"Open Containing Folder" ~packing:menu#add () in
      ignore (item#connect#activate ~callback:begin fun () ->
          let cmd =
            if Oe_config.xdg_open_version <> None
            then Some (sprintf "xdg-open %s" (Filename.quote (Filename.dirname filename)))
            else None
          in
          Option.iter (fun cmd -> ignore (Thread.create (fun () -> ignore (Sys.command cmd)) ())) cmd
        end);
      let _ = GMenu.separator_item ~packing:menu#add () in
      let item = GMenu.image_menu_item ~label:"Switch to Implementation/Interface" ~packing:menu#add () in
      ignore (item#connect#activate ~callback:(fun () -> self#switch_mli_ml page));
      item#misc#set_sensitive (Menu_file.get_file_switch_sensitive page);
      self#with_current_page begin fun page ->
        let switch_viewer = GMenu.menu_item ~label:"Dependency Graph" ~packing:menu#add () in
        ignore (switch_viewer#connect#activate ~callback:page#button_dep_graph#clicked);
        switch_viewer#misc#set_sensitive (Menu_view.get_switch_view_sensitive self#project page)
      end;
      let item = GMenu.image_menu_item
          ~image:(GMisc.image ~pixbuf:(??? Icons.history) ())#coerce
          ~label:"Revision History" ~packing:menu#add ()
      in
      ignore (item#connect#activate ~callback:(fun () -> self#with_current_page (fun page -> page#show_revision_history ())));
      let _ = GMenu.separator_item ~packing:menu#add () in
      let item = GMenu.image_menu_item ~label:"Save As..." ~packing:menu#add () in
      item#set_image (GMisc.image ~pixbuf:(??? Icons.save_as_16) (*~stock:`SAVE_AS*) ~icon_size:`MENU ())#coerce;
      ignore (item#connect#activate ~callback:(fun () -> self#dialog_save_as page));
      let item = GMenu.image_menu_item ~label:(sprintf "Rename \xC2\xAB%s\xC2\xBB" basename) ~packing:menu#add () in
      ignore (item#connect#activate ~callback:(fun () -> self#dialog_rename page));
      Gaux.may page#file ~f:(fun file -> item#misc#set_sensitive file#is_writeable);
      let item = GMenu.image_menu_item ~label:(sprintf "Delete \xC2\xAB%s\xC2\xBB" basename) ~packing:menu#add () in
      item#set_image (Icons.create (??? Icons.close_window))#coerce;
      ignore (item#connect#activate ~callback:self#dialog_delete_current);
      Gaux.may page#file ~f:(fun file -> item#misc#set_sensitive file#is_writeable);
      let _ = GMenu.separator_item ~packing:menu#add () in
      let item = GMenu.image_menu_item ~label:(sprintf "Compile \xC2\xAB%s\xC2\xBB" basename) ~packing:menu#add () in
      item#set_image (GMisc.image ~pixbuf:(??? Icons.compile_file_16) ())#coerce;
      ignore (item#connect#activate ~callback:(fun () -> page#compile_buffer ?join:None ()));
      item#misc#set_sensitive (Menu_file.get_file_switch_sensitive page);
      menu#popup ~time:(GdkEvent.Button.time ev) ~button:3;
      Gdk.Window.set_cursor menu#misc#window (Gdk.Cursor.create `ARROW);

    method private callback_query_tooltip (page : Editor_page.page) ~x ~y ~kbd _ =
      if x > page#view#gutter.Gutter.size && y > 10 && y < (Gdk.Rectangle.height page#view#visible_rect) - 10 then begin
        let f () =
          let location = page#view#window_to_buffer_coords ~tag:`WIDGET ~x ~y in
          page#tooltip location;
        in
        if (*true ||*) preferences#get.editor_annot_type_tooltips_delay = 1 then begin
          Timeout.set tout_delim 0 (GtkThread.async f);
        end else (f());
      end else (page#error_indication#hide_tooltip ~force:false ());
      false;

    method open_file ~active ~scroll_offset ~offset ?remote filename =
      try
        let page =
          try
            let page = List.find (fun p -> p#get_filename = filename) pages in
            notebook#goto_page (notebook#page_num page#coerce);
            page
          with Not_found -> begin
              begin
                try
                  let page = List.assoc filename pages_cache in
                  pages_cache <- List.remove_assoc filename pages_cache;
                  pages <- page :: pages;
                  page#misc#show_all();
                  if active then (notebook#goto_page (notebook#page_num page#coerce));
                  add_page#call page;
                  page
                with Not_found -> begin
                    let file = Editor_file.create ?remote filename in
                    let page = new Editor_page.page ~file ~project ~scroll_offset ~offset ~editor:self () in
                    ignore (page#connect#file_changed ~callback:(fun _ -> switch_page#call page));
                    (* Outline *)
                    page#set_outline (new Outline.model ~buffer:page#buffer () :> Oe.outline);
                    (* Tab Label with close button *)
                    let button_close = GButton.button ~relief:`NONE () in
                    let image = Icons.create (??? Icons.button_close) in
                    ignore (button_close#event#connect#enter_notify ~callback:begin fun _ ->
                        image#set_pixbuf (if page#buffer#modified then (??? Icons.button_close_hi_b) else (??? Icons.button_close_hi));
                        false
                      end);
                    ignore (button_close#event#connect#leave_notify ~callback:begin fun _ ->
                        image#set_pixbuf (if page#buffer#modified then (??? Icons.button_close_b) else (??? Icons.button_close));
                        false
                      end);
                    ignore (page#view#misc#connect#query_tooltip ~callback:(self#callback_query_tooltip page));
                    ignore (page#buffer#connect#modified_changed ~callback:begin fun () ->
                        if page#buffer#modified then begin
                          page#status_modified_icon#set_label "\u{f0c7}\u{2005}";
                          page#status_modified_icon#misc#set_tooltip_text "Modified";
                          image#set_pixbuf (??? Icons.button_close_b)
                        end else begin
                          page#status_modified_icon#set_label "    ";
                          page#status_modified_icon#misc#set_tooltip_text "";
                          image#set_pixbuf (??? Icons.button_close)
                        end;
                        modified_changed#call();
                      end);
                    (* Annot type tooltips *)
                    page#view#misc#set_has_tooltip true;
                    ignore (page#buffer#undo#connect#after#redo ~callback:(fun ~name -> changed#call()));
                    ignore (page#buffer#undo#connect#after#undo ~callback:(fun ~name -> changed#call()));
                    ignore (page#buffer#undo#connect#can_redo_changed ~callback:(fun _ -> changed#call()));
                    ignore (page#buffer#undo#connect#can_undo_changed ~callback:(fun _ -> changed#call()));
                    ignore (page#buffer#connect#after#changed ~callback:changed#call);
                    (* Tab menu *)
                    let is_in_src_path = project.Prj.in_source_path filename <> None in
                    let ebox = GBin.event_box () in
                    ebox#misc#set_property "visible-window" (`BOOL (not is_in_src_path));
                    ignore (ebox#event#connect#button_release ~callback:begin fun ev ->
                        if GdkEvent.Button.button ev = 3 then begin
                          notebook#goto_page (notebook#page_num page#coerce);
                          self#create_tab_menu page ev;
                          true
                        end else false
                      end);
                    (* Tab close button *)
                    let align = GBin.alignment ~packing:ebox#add () in
                    button_close#set_image image#coerce;
                    ignore (button_close#connect#clicked ~callback:(fun () -> ignore (self#dialog_confirm_close page)));
                    let markup = Editor_page.markup_label filename in
                    let lab = GMisc.label ~markup ~xalign:0.0 ~yalign:1.0 ~xpad:0 () in
                    if not is_in_src_path then begin
                      ebox#misc#modify_bg [`NORMAL, Oe_config.editor_tab_color_alt_normal; `ACTIVE, Oe_config.editor_tab_color_alt_active];
                      lab#misc#modify_fg [`NORMAL, `NAME "#ffffff"; `ACTIVE, `NAME "#000000"];
                    end;
                    page#set_tab_widget (align, button_close, lab);
                    (* Append tab *)
                    let _ = notebook#append_page ~tab_label:ebox#coerce page#coerce in
                    notebook#set_tab_reorderable page#coerce true;
                    self#set_tab_pos ~page Preferences.preferences#get.tab_pos;
                    if active then begin
                      self#load_page page;
                      notebook#goto_page (notebook#page_num page#coerce);
                      switch_page#call page;
                    end;
                    pages <- page :: pages;
                    add_page#call page;
                    page
                  end
              end;
            end
        in
        Some page
      with e -> begin
          Dialog.display_exn ~title:"Error while opening file" ~parent:self e;
          None
        end

    method revert (page : Editor_page.page) = Gaux.may page#file ~f:begin fun _ ->
        if page#buffer#modified then ignore (Dialog.confirm
                                               ~title:"Revert File"
                                               ~image:(GMisc.image ~pixbuf:(??? Icons.revert_to_saved_16) (*~stock:`REVERT_TO_SAVED*) ~icon_size:`DIALOG ())#coerce
                                               ~message:("File\n\""^page#get_filename^"\"\nmodified, revert?")
                                               ~yes:("Revert", fun () -> page#revert())
                                               ~no:("Do Not Revert", ignore) page)
      end

    method save_all () =
      List.iter (fun p -> if p#view#buffer#modified then self#save p) self#pages;

    method dialog_save_as page =
      match page#file with
      | Some file when file#remote <> None ->
          Remote.dialog_save_as ~editor:self ~page ()
      | _ -> Dialog_save_as.window ~editor:self ~page ()

    method dialog_rename page =
      match page#file with
      | Some file when file#remote <> None ->
          Remote.dialog_rename ~editor:self ~page ()
      | _ -> Dialog_rename.window ~editor:self ~page ()

    method save (page : Editor_page.page) =
      let filename = page#get_filename in
      let extension = Filename.extension filename in
      let pref = Preferences.preferences#get in
      if pref.editor_trim_lines
      then page#buffer#trim_lines ();
      if pref.editor_format_on_save
      then ignore @@ Ocp_indent.indent_for_extension ~project: self#project ~view:page#view ~extension `ALL;
      page#save();
      file_saved#call filename;

    method dialog_confirm_close = Editor_dialog.confirm_close ~editor:self

    method dialog_save_modified = Editor_dialog.save_modified ~editor:self

    method dialog_delete_current () =
      self#with_current_page begin fun page ->
        match page#file with
        | Some file ->
            let message = GWindow.message_dialog
                ~title:"Delete file?"
                ~message:("Delete file\n\n"^page#get_title^"?")
                ~message_type:`INFO
                ~position:`CENTER
                ~allow_grow:false
                ~destroy_with_parent:false
                ~modal:true
                ~buttons:GWindow.Buttons.yes_no () in
            Gaux.may (GWindow.toplevel self) ~f:(fun x -> message#set_transient_for x#as_window);
            (match message#run() with
             | `YES -> self#delete_page page
             | _ -> ());
            message#destroy();
        | _ -> ()
      end

    method delete_page page =
      if page#buffer#modified then (self#save page);
      Gaux.may page#file ~f:(fun file -> file#remove);
      self#close page;
      pages_cache <- List.filter (fun (_, p) -> p#misc#get_oid <> page#misc#get_oid) pages_cache;
      notebook#remove page#coerce;

    method close_all ?except () =
      let except = match except with
        | None -> fun _ -> false
        | Some page -> fun p -> p = page
      in
      let modified, close = List.partition (fun p -> p#buffer#modified && (not (except p))) pages in
      List.iter (fun p -> if not (except p) then (GtkThread2.sync self#close p)) close;
      if modified <> [] then begin
        let pages = List.rev_map (fun p -> true, p) modified in
        self#dialog_save_modified ~close:true ~callback:ignore pages
      end;

    method close page =
      Project.save_bookmarks page#project;
      if page#buffer#modified then (page#revert());
      page#buffer#set_modified false;
      remove_page#call page;
      pages <- List.filter ((<>) page) pages;
      (* Location history and autosave *)
      begin
        match page#file with
        | None -> ()
        | Some file ->
            pages_cache <- (file#filename, page) :: pages_cache;
            page#misc#hide();
            (* File history *)
            File_history.add file_history file#filename;
            file_history_changed#call file_history;
            (* Delete existing recovery copy *)
            page#sync_autosave_time ();
            page#buffer#sync_autocomp_time ();
            Autosave.delete ~filename:file#filename ();
      end;

    method redisplay_views () =
      self#with_current_page begin fun current_page ->
        Gmisclib.Idle.add ~prio:100 current_page#redisplay;
        Gmisclib.Idle.add ~prio:300 (fun () -> current_page#compile_buffer ?join:None ());
        pages
        |> List.filter ((<>) current_page)
        |> List.iter begin fun p ->
          let sig_id = ref None in
          sig_id := Some (self#connect#switch_page ~callback:begin fun cp ->
              if p = cp then begin
                Gmisclib.Idle.add ~prio:200 p#redisplay;
                (*Gmisclib.Idle.add ~prio:400 (fun () -> p#compile_buffer ());*)
                Gaux.may !sig_id ~f:self#disconnect
              end
            end)
        end
      end

    method i_search ?(full_find : (Gdk.Tags.modifier list * Gdk.keysym * (unit -> unit)) option) () =
      self#with_current_page begin fun page ->
        incremental_search#i_search ?full_find ~view:(page#view :> Text.view) ~project:self#project ()
      end

    method location_history_is_empty () =
      (List.length (Location_history.get_history_backward self#location_history) = 0),
      (List.length (Location_history.get_history_forward self#location_history) = 0),
      ((Location_history.last_edit_location self#location_history) = None)

    method private cb_tout_delim page () =
      page#view#matching_delim ();
      page#error_indication#hide_tooltip();

    val signals = new signals hpaned#as_widget ~add_page ~switch_page ~remove_page ~changed ~modified_changed
      ~file_history_changed ~outline_visibility_changed ~file_saved ~notification
    method connect = signals
    method disconnect = signals#disconnect

    method private add_timeouts () =
      (* Auto-compilation *)
      let id_timeout_autocomp = ref None in
      let create_timeout_autocomp () =
        match !id_timeout_autocomp with
        | None ->
            id_timeout_autocomp := Some (GMain.Timeout.add ~ms:500 ~callback:begin fun () ->
                if project.Prj.autocomp_enabled then begin
                  try
                    self#with_current_page begin fun page ->
                      if page#view#misc#get_flag `HAS_FOCUS && page#buffer#is_changed_after_last_autocomp then begin
                        if Unix.gettimeofday() -. page#buffer#last_edit_time > project.Prj.autocomp_delay (*/. 2.*)
                        then (page#compile_buffer ?join:None ())
                      end
                    end
                  with ex -> (eprintf "%s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace ()))
                end;
                true
              end)
        | _ -> ()
      in
      (* Autosave *)
      let id_timeout_autosave = ref None in
      let create_timeout_autosave () =
        match !id_timeout_autosave with
        | None ->
            if Oe_config.autosave_enabled then begin
              id_timeout_autosave := Some (GMain.Timeout.add ~ms:Autosave.interval ~callback:begin fun () ->
                  (*Prf.crono Prf.prf_autosave*) (List.iter begin fun page ->
                  if page#is_changed_after_last_autosave then begin
                    let filename = page#get_filename in
                    let text = (page#buffer :> GText.buffer)#get_text () in
                    Autosave.backup ~filename ~text;
                    page#sync_autosave_time ();
                  end
                end) pages;
                  true
                end)
            end
        | _ -> ()
      in
      (* highlight matching delimiters *)
      let last_cursor_offset = ref 0 in
      let id_timeout_delim = ref None in
      let create_timeout_delim () =
        match !id_timeout_delim with
        | None ->
            id_timeout_delim := Some (GMain.Timeout.add ~ms:1500 ~callback:begin fun () ->
                self#with_current_page begin fun page ->
                  if page#view#misc#get_flag `HAS_FOCUS then begin
                    let offset = (page#buffer#get_iter `INSERT)#offset in
                    if not page#buffer#has_selection && offset <> !last_cursor_offset then begin
                      page#view#matching_delim ();
                      last_cursor_offset := (page#buffer#get_iter `INSERT)#offset
                    end;
                  end;
                end;
                true
              end)
        | _ -> ()
      in
      (*  *)
      self#misc#connect#map ~callback:begin fun _ ->
        Gaux.may (GWindow.toplevel self#coerce) ~f:begin fun (w : GWindow.window) ->
          w#event#connect#focus_in ~callback:begin fun _ ->
            create_timeout_autocomp();
            create_timeout_autosave();
            create_timeout_delim();
            false
          end |> ignore;
          w#event#connect#focus_out ~callback:begin fun _ ->
            Gaux.may !id_timeout_autocomp ~f:GMain.Timeout.remove;
            Gaux.may !id_timeout_autosave ~f:GMain.Timeout.remove;
            Gaux.may !id_timeout_delim ~f:GMain.Timeout.remove;
            id_timeout_autocomp := None;
            id_timeout_autosave := None;
            id_timeout_delim := None;
            false
          end |> ignore;
        end;
      end |> ignore;

    method show_doc_at_cursor () = ()

    initializer
      File_history.read file_history;
      (*  *)
      ignore (Preferences.preferences#connect#changed ~callback:(fun _ -> self#redisplay_views()));
      (*  *)
      code_folding_enabled#set Preferences.preferences#get.editor_code_folding_enabled;
      (*  *)
      ignore (show_global_gutter#connect#changed ~callback:begin fun enabled ->
          List.iter (fun p -> if enabled then p#global_gutter#misc#show()
                      else p#global_gutter#misc#hide()) (pages @ (snd (List.split pages_cache)))
        end);
      show_global_gutter#set Preferences.preferences#get.editor_show_global_gutter;
      (*  *)
      self#add_timeouts();
      ignore (Timeout.start tout_delim);
      ignore (Timeout.start tout_fast);
      (* Switch page: update the statusbar and remove annot tag *)
      ignore (notebook#connect#after#switch_page ~callback:begin fun _ ->
          (* Current page *)
          self#with_current_page begin fun page ->
            if not page#load_complete && not history_switch_page_locked then (self#load_page page);
            page#update_statusbar();
            page#view#draw_current_line_background (page#buffer#get_iter `INSERT);
            if not history_switch_page_locked then (switch_page#call page);
          end;
        end);
      ignore (self#connect#switch_page ~callback:begin fun _ ->
          self#with_current_page begin fun page ->
            ()
          end
        end);
      (* Record last active page *)
      let rec get_history project =
        match List.assoc_opt project.Prj.name history_switch_page with
        | Some x -> x
        | _ ->
            history_switch_page <- (project.Prj.name, []) :: history_switch_page;
            get_history project
      in
      let replace_history project hist =
        history_switch_page <- List.remove_assoc project.Prj.name history_switch_page;
        history_switch_page <- (project.Prj.name, hist) :: history_switch_page;
      in
      ignore (notebook#connect#switch_page ~callback:begin fun _ ->
          if not history_switch_page_locked && notebook#current_page >= 0 then begin
            let last_page = notebook#get_nth_page notebook#current_page in
            let hist = get_history project in
            replace_history project (last_page :: hist);
          end;
        end);
      (* Remove Page: editor goes to the last active page *)
      ignore (self#connect#remove_page ~callback:begin fun removed ->
          match self#get_page `ACTIVE with
          | Some cur when not history_switch_page_locked && cur#get_oid = removed#get_oid ->
              let rec find_page () =
                let history = get_history project in
                match history with
                | last :: tl ->
                    begin
                      let finally () = replace_history project tl in
                      begin
                        match List.find_opt (fun p -> p#misc#get_oid = last#misc#get_oid) pages with
                        | None -> finally(); find_page()
                        | _ ->
                            notebook#goto_page (notebook#page_num last);
                            finally()
                      end;
                    end;
                | _ -> ()
              in
              find_page()
          | _ -> ()
        end);
      (* Replace marks with offsets in location history *)
      ignore (self#connect#remove_page ~callback:begin fun page ->
          Location_history.iter location_history ~f:begin function
          | loc when loc.Location_history.filename = page#get_filename ->
              if not page#buffer#modified (* i.e. saved *) then begin
                match loc.Location_history.mark with
                | Some mark when (not (GtkText.Mark.get_deleted mark)) ->
                    let iter = page#buffer#get_iter_at_mark (`MARK mark) in
                    loc.Location_history.offset <- iter#offset;
                    loc.Location_history.mark <- None;
                | Some _ -> loc.Location_history.mark <- None;
                | _ -> ()
              end else begin
                (* If the buffer is not saved, location is unmeaningful; it only
                   records the file name. *)
                loc.Location_history.mark <- None;
                loc.Location_history.offset <- 0;
              end
          | _ -> ()
          end
        end);
      Margin_fold.init_editor self;
      Global_diff.init_editor self
  end

(** Signals *)
and switch_page () = object inherit [Editor_page.page] signal () end
and remove_page () = object inherit [Editor_page.page] signal () end
and modified_changed () = object inherit [unit] signal () end
and changed () = object inherit [unit] signal () end
and add_page () = object inherit [Editor_page.page] signal () end
and file_history_changed () = object inherit [File_history.t] signal () end
and outline_visibility_changed () = object inherit [bool] signal () end
and file_saved () = object inherit [string] signal () end
and notification () = object inherit [string] signal () end

and signals hpaned ~add_page ~switch_page ~remove_page ~changed ~modified_changed
    ~file_history_changed ~outline_visibility_changed ~file_saved ~notification =
  object
    inherit GObj.widget_signals_impl hpaned
    inherit add_ml_signals hpaned [switch_page#disconnect;
                                   remove_page#disconnect; modified_changed#disconnect;
                                   add_page#disconnect; changed#disconnect;
                                   outline_visibility_changed#disconnect;
                                   notification#disconnect]
    method switch_page = switch_page#connect ~after
    method remove_page = remove_page#connect ~after
    method modified_changed = modified_changed#connect ~after
    method changed = changed#connect ~after
    method add_page = add_page#connect ~after
    method file_history_changed = file_history_changed#connect ~after
    method outline_visibility_changed = outline_visibility_changed#connect ~after
    method file_saved = file_saved#connect ~after
    method notification = notification#connect ~after
  end

