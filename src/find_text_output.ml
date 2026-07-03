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
open GdkKeysyms
open GUtil
open Find_text
open Utils
module ColorOps = Color
open Preferences

let strip_cr =
  let re = Str.regexp "\r$" in
  fun line -> Str.replace_first re "" line

let new_mark_name = let i = ref 0 in fun () -> incr i; sprintf "find_text_%d" !i

(** widget *)
class widget
    ~(dialog : GWindow.window)
    ~editor(* : Editor.editor)*)
    ?(buffer : GText.buffer option)
    ?packing () =
  let search_started    = new search_started () in
  let search_finished   = new search_finished () in
  let editor_buffer     = buffer in
  let vbox              = GPack.vbox ?packing () in
  let paned             = GPack.paned `HORIZONTAL ~packing:vbox#add () in
  let toolbar           = GButton.toolbar ~style:`ICONS ~orientation:`HORIZONTAL ~packing:vbox#pack () in
  let _                 = toolbar#set_icon_size `MENU in
  let button_stop       = GButton.tool_button ~packing:toolbar#insert () in
  let _                 = button_stop#set_label_widget (Gtk_util.label_icon ~color:"red" "\u{f04d}")#coerce in
  let _                 = GButton.separator_tool_item ~packing:toolbar#insert () in
  let button_prev_file  = GButton.tool_button ~packing:toolbar#insert () in
  let _                 = button_prev_file#set_label_widget (Gtk_util.label_icon "\u{ea9b}")#coerce in
  let _                 = button_prev_file#misc#set_tooltip_text "Previous file" in
  let button_prev_line  = GButton.tool_button ~packing:toolbar#insert () in
  let _                 = button_prev_line#set_label_widget (Gtk_util.label_icon "\u{eab5}")#coerce in
  let _                 = button_prev_line#misc#set_tooltip_text "Previous line" in
  let button_next_line  = GButton.tool_button ~packing:toolbar#insert () in
  let _                 = button_next_line#set_label_widget (Gtk_util.label_icon "\u{eab6}")#coerce in
  let _                 = button_next_line#misc#set_tooltip_text "Next line" in
  let button_next_file  = GButton.tool_button ~packing:toolbar#insert () in
  let _                 = button_next_file#set_label_widget (Gtk_util.label_icon "\u{ea9c}")#coerce in
  let _                 = button_next_file#misc#set_tooltip_text "Next file" in
  let _                 = GButton.separator_tool_item ~packing:toolbar#insert () in
  let button_remove     = GButton.tool_button ~packing:toolbar#insert () in
  let _                 = button_remove#set_label_widget (Gtk_util.label_icon "\u{eb3b}")#coerce in
  let _                 = button_remove#misc#set_tooltip_text "Remove entry" in
  let _                 = GButton.separator_tool_item ~packing:toolbar#insert () in
  let button_restart    = GButton.tool_button ~packing:toolbar#insert () in
  let _                 = button_restart#set_label_widget (Gtk_util.label_icon "\u{eb37}")#coerce in
  let _                 = button_restart#misc#set_tooltip_text "Repeat current search" in
  let button_new_search = GButton.tool_button ~packing:toolbar#insert () in
  let _                 = button_new_search#set_label_widget (Gtk_util.label_icon "\u{f422}")#coerce in
  let _                 = button_new_search#misc#set_tooltip_text "New search" in
  let _                 = GButton.separator_tool_item ~packing:toolbar#insert () in
  let button_detach     = GButton.tool_button ~label:"Detach" ~packing:toolbar#insert () in
  let _                 = button_detach#set_label_widget (Gtk_util.label_icon "\u{eb23}")#coerce in
  let _                 = GButton.separator_tool_item ~packing:toolbar#insert () in
  let item_message      = GButton.tool_item ~packing:toolbar#insert () in
  let label_message     = GMisc.label ~packing:item_message#add () in
  (*  *)
  let lsw               = GBin.scrolled_window ~shadow_type:`IN ~hpolicy:`NEVER ~vpolicy:`AUTOMATIC
      ~packing:(paned#pack1 ~resize:false ~shrink:true) () in
  let rsw               = GBin.scrolled_window ~shadow_type:`IN ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
      ~packing:(paned#pack2 ~resize:true ~shrink:true) () in
  let tbuf              = new Ocaml_text.buffer () in
  let preview           = new Ocaml_text.view ~buffer:tbuf () in
  let _                 = Preferences_apply.apply (preview :> Text.view) Preferences.preferences#get in
  let _                 = rsw#add preview#coerce in
  let _                 = tbuf#create_tag ~name:"find_text_result_line" [] in
  (*  *)
  let cols              = new GTree.column_list in
  let col_file          = cols#add Gobject.Data.string in
  let col_hits          = cols#add Gobject.Data.int in
  let col_path          = cols#add Gobject.Data.string in
  let model             = GTree.list_store cols in
  let view              = GTree.view ~model:model ~headers_clickable:true ~packing:lsw#add () in
  let _                 = view#misc#set_property "enable-grid-lines" (`INT 3) in
  let _                 = Gaux.may !Gtk_theme.find_text_output_font_condensed ~f:view#misc#modify_font_by_name in
  let renderer          = GTree.cell_renderer_text [(*`STRETCH `CONDENSED*) `YPAD 0] in
  let vc_file           = GTree.view_column ~title:"File" ~renderer:(renderer, ["text", col_file]) () in
  let vc_hits           = GTree.view_column ~title:"Hits" ~renderer:(renderer, ["text", col_hits]) () in
  let vc_path           = GTree.view_column ~title:"Directory" ~renderer:(renderer, ["text", col_path]) () in
  let _                 = vc_file#set_resizable true in
  let _                 = vc_hits#set_resizable true in
  let _                 = vc_path#set_resizable true in
  object (self)
    inherit GObj.widget vbox#as_widget
    inherit Messages.page ~role:"find-text-output"

    val mutable options = None
    val project = editor#project
    (*  *)
    val mutable sigids = []
    val mutable sigid_changed = None
    val mutable delete_marks = []
    val mutable selected_text_bounds = None
    val mutable canceled = false
    val mutable results : result_entry list = []
    val mutable hits = 0
    val mutable count_dirs = 0
    val mutable find_all = None
    val mutable current_line_selected = None
    val mutable n_rows = 0

    (*method parent_changed messages = ()*)

    initializer
      ignore (button_detach#connect#clicked ~callback:(fun () -> self#detach button_detach));
      ignore (self#connect_detach#detached ~callback:begin fun _ ->
          Gaux.may (preview#get_window `TEXT) ~f:(fun w -> Gdk.Window.set_cursor w (Gdk.Cursor.create `ARROW))
        end);
      ignore (view#append_column vc_file);
      ignore (view#append_column vc_hits);
      ignore (view#append_column vc_path);
      vc_file#set_sort_column_id 0;
      vc_hits#set_sort_column_id 1;
      vc_path#set_sort_column_id 2;
      preview#set_smart_click false;
      let current_line_border_color = ColorOps.name_of_gdk (Preferences.editor_tag_color "highlight_current_line") in
      let current_line_color = ColorOps.name_of_gdk (Preferences.editor_tag_bg_color "highlight_current_line") in
      preview#options#set_highlight_current_line (Some (current_line_border_color, current_line_color));
      preview#options#set_show_line_numbers false;
      preview#options#set_show_markers false;
      preview#options#set_current_line_border_color (`NAME current_line_border_color);
      preview#set_editable false;
      preview#set_cursor_visible false;
      preview#set_pixels_above_lines 3;
      preview#set_pixels_below_lines 3;
      preview#set_border_window_size ~typ:`LEFT ~size:0;
      ignore (preview#buffer#connect#after#mark_set ~callback:begin fun iter mark ->
          match GtkText.Mark.get_name mark with
          | Some "insert" ->
              if preview#buffer#has_selection then begin
                GtkSignal.stop_emit();
                preview#buffer#place_cursor ~where:(preview#buffer#get_iter `SEL_BOUND)
              end
          | _ -> ()
        end);
      let bgcolor =
        match fst Oe_config.find_text_output_highlight with
        | `DEFAULT -> [`BACKGROUND_GDK (preview#misc#style#bg `ACTIVE)]
        | `NAME name -> [`BACKGROUND name]
        | `NONE -> []
      in
      let fgcolor =
        match snd Oe_config.find_text_output_highlight with
        | `DEFAULT -> [`FOREGROUND_GDK (preview#misc#style#fg `ACTIVE)]
        | `NAME name -> [`FOREGROUND name]
        | `NONE -> []
      in
      let _ = tbuf#create_tag ~name:"find_text_result_hit" (bgcolor @ fgcolor) in
      let _ = tbuf#create_tag ~name:"find_text_result_linenum"
          [`FOREGROUND (?? Oe_config.find_text_output_linenumber_fgcolor); `WEIGHT `NORMAL;
           `BACKGROUND_FULL_HEIGHT_SET true; `SCALE `SMALL;] in
      let _ = tbuf#create_tag ~name:"find_text_insensitive"
          [`FOREGROUND "#b0b0b0"; `STYLE `ITALIC] in
      let tag_test = tbuf#create_tag ~name:"test" [`FOREGROUND "green"; `STYLE `ITALIC] in
      Gmisclib.Util.set_tag_paragraph_background tag_test "yellow";
      let _ = Gaux.may (preview#get_window `TEXT) ~f:(fun w -> Gdk.Window.set_cursor w (Gdk.Cursor.create `ARROW)) in
      (*  *)
      ignore (button_remove#connect#clicked ~callback:self#remove_entry);
      ignore (button_stop#connect#clicked ~callback:(fun () -> canceled <- true));
      ignore (button_restart#connect#clicked ~callback:self#restart);
      ignore (button_new_search#connect#clicked ~callback:self#new_search);
      ignore (button_prev_file#connect#clicked ~callback:self#prev_file);
      ignore (button_next_file#connect#clicked ~callback:self#next_file);
      ignore (button_prev_line#connect#clicked ~callback:self#prev_line);
      ignore (button_next_line#connect#clicked ~callback:self#next_line);
      ignore (vbox#connect#destroy ~callback:dialog#destroy);
      (* Tooltips *)
      view#misc#set_has_tooltip true;
      ignore (view#misc#connect#query_tooltip ~callback: begin fun ~x ~y ~kbd tooltip ->
          try
            begin
              match GtkTree.TreeView.Tooltip.get_context view#as_tree_view ~x ~y ~kbd with
              | (x, y, Some (_, _, row)) ->
                  begin
                    match view#get_path_at_pos ~x ~y with
                    | Some (tpath, _, _, _) ->
                        let filename =
                          (model#get ~row ~column:col_path) // (model#get ~row ~column:col_file)
                        in
                        GtkBase.Tooltip.set_text tooltip filename;
                        GtkTree.TreeView.Tooltip.set_row view#as_tree_view tooltip tpath;
                        true
                    | _ -> false
                  end
              | _ -> false
            end
          with Not_found | Gpointer.Null -> false
        end);

    method set_options x = options <- Some x
    method private options = match options with Some x -> x | _ -> invalid_arg "Options not specified"
    method text_to_find = self#options.text_find#get

    method set_selected_text_bounds x = selected_text_bounds <- x

    method private canceled = canceled

    method clear () =
      hits <- 0;
      count_dirs <- 0;
      n_rows <- 0;
      results <- [];
      canceled <- false;
      sigids <- [];
      List.iter (fun f -> try f () with GText.No_such_mark _ -> ()) delete_marks;
      delete_marks <- [];
      GtkThread2.sync model#clear ();
      GtkThread2.sync tbuf#set_text "";
      GtkThread2.sync label_message#set_text "";
      current_line_selected <- None;

    method new_search () = dialog#present();

    method prev_file () =
      let path = match view#selection#get_selected_rows with
        | path :: _ ->
            ignore (GTree.Path.prev path);
            path
        | _ -> GTree.Path.create [List.length results - 1]
      in
      view#selection#select_path path;
      view#scroll_to_cell ~align:(0.6, 0.0) path vc_file;

    method next_file () =
      let path = match view#selection#get_selected_rows with
        | path :: _ ->
            GTree.Path.next path;
            path
        | _ -> GTree.Path.create [0]
      in
      view#selection#select_path path;
      if (GTree.Path.get_indices path).(0) < List.length results then
        view#scroll_to_cell ~align:(0.4, 0.0) path vc_file;

    method prev_line () = self#move_line `PREV
    method next_line () = self#move_line `NEXT

    method private move_line dir =
      let iter = preview#buffer#get_iter `INSERT in
      let iter =
        if current_line_selected <> None then begin
          if dir = `NEXT then iter#forward_line else iter#backward_line
        end else iter
      in
      self#select_line iter;
      self#activate ~grab_focus:false iter;
      current_line_selected <- Some iter#line;

    method remove_entry () =
      match view#selection#get_selected_rows with
      | path :: _ ->
          let next =
            if GTree.Path.to_string path = GTree.Path.to_string (GTree.Path.create [n_rows - 1])
            then let next = GTree.Path.copy path in ignore ((GTree.Path.prev next)); next
            else path
          in
          ignore (model#remove (model#get_iter path));
          n_rows <- n_rows - 1;
          view#selection#select_path next;
          view#scroll_to_cell ~align:(0.5, 0.0) next vc_file;
      | _ -> ()

    (** restart *)
    method restart () = ignore (Thread.create (fun () -> self#find ?all:find_all ()) ())

    (** find *)
    method find ?all () =
      if String.length self#options.text_find#get > 0 then begin
        self#clear();
        find_all <- all;
        GtkThread2.sync begin fun () ->
          button_stop#misc#set_sensitive true;
          button_restart#misc#set_sensitive false;
          button_prev_file#misc#set_sensitive false;
          button_prev_line#misc#set_sensitive false;
          button_next_line#misc#set_sensitive false;
          button_next_file#misc#set_sensitive false;
          button_new_search#misc#set_sensitive false;
          button_remove#misc#set_sensitive false;
          label_message#set_text "Searching...";
        end ();
        results <- [];
        begin
          try
            if all <> (Some false) then begin
              GtkThread2.sync search_started#call ();
              begin
                match buffer with
                | None ->
                    (* Search  *)
                    self#find_in_path (*begin
                                        match *)self#options.path(* with
                                                                    | Project_source -> Project.path_src project
                                                                    | Specified path -> path
                                                                    | Only_open_files -> assert false
                                                                    end*)
                | Some buffer ->
                    (* Search  *)
                    self#find_in_buffer buffer;
              end;
              GtkThread2.sync begin fun () ->
                button_stop#misc#set_sensitive false;
                label_message#set_text "Please wait...";
              end ();
              if all <> (Some false) then (GtkThread2.sync self#display ());
              GtkThread2.sync search_finished#call ();
            end else begin
              let view = match editor#get_page `ACTIVE with Some p -> (p#view :> Text.view) | _ -> assert false in
              (* Search  *)
              GtkThread2.sync begin fun () ->
                Find_text_in_buffer.find self#options.direction ~view ~canceled:(fun () -> self#canceled)
              end ();
            end;
          with Canceled -> (GtkThread2.sync self#display ())
        end;
        GtkThread2.sync begin fun () ->
          button_stop#misc#set_sensitive false;
          button_restart#misc#set_sensitive true;
          button_prev_file#misc#set_sensitive (hits > 0 && buffer = None);
          button_prev_line#misc#set_sensitive (hits > 0);
          button_next_line#misc#set_sensitive (hits > 0);
          button_next_file#misc#set_sensitive (hits > 0 && buffer = None);
          button_new_search#misc#set_sensitive true;
          button_remove#misc#set_sensitive (hits > 0 && buffer = None);
          if hits = 0 then label_message#set_text "No matches found"
          else match buffer with
            | None ->
                let n_res = List.length results in
                ksprintf label_message#set_text "%d hit%s in %d file%s%s"
                  hits (if hits = 1 then "" else "s") n_res (if n_res = 1 then "" else "s")
                  (if n_res > 1 then sprintf " (%d %s)" count_dirs
                       (if count_dirs = 1 then "directory" else "directories") else "");
            | _ ->
                let bufname = match editor#get_page `ACTIVE with Some p -> p#get_filename | _ -> assert false in
                ksprintf label_message#set_text "%d hits in \xC2\xAB%s\xC2\xBB" hits (Filename.basename bufname)
        end ()
      end

    (** find_in_buffer *)
    method private find_in_buffer buffer =
      let start, stop =
        match selected_text_bounds with
        | None -> None, None
        | Some (start, stop) -> (Some start), (Some stop)
      in
      match Find_text_in_buffer.find_forward ?start ?stop ~all:true ~buffer
              ~regexp:(match self#options.current_regexp with Some x -> x | _ -> assert false)
              ~canceled:(fun () -> self#canceled) () with
      | None -> ()
      | Some lines_involved ->
          let filename = match editor#get_page `ACTIVE with Some p -> p#get_filename | _ -> assert false in
          results <- {filename = filename; lines = lines_involved} :: results;
          hits <- List.fold_left (fun acc res -> acc + (List.fold_left (fun acc l ->
              acc + (List.length l.offsets)) 0 res.lines)) 0 results

    (** find_in_file *)
    method private find_in_file filename =
      if Sys.file_exists filename then begin
        let ichan = open_in filename in
        let linenum = ref 0 in
        let bol = ref 0 in
        let lines_involved = ref [] in
        let finally () =
          close_in ichan;
          if List.length !lines_involved > 0 then begin
            results <- {filename = filename; lines = (List.rev !lines_involved)} :: results;
          end;
        in
        let l_regexp = match self#options.current_regexp with Some x -> x | _ -> assert false in
        begin
          try
            while true do
              let real_line = input_line ichan in
              let line = strip_cr real_line in
              incr linenum;
              let offsets = ref [] in
              let pos = ref 0 in
              begin
                try
                  while true do
                    if canceled then (raise Canceled);
                    if Str.search_forward l_regexp line !pos >= 0 then begin
                      let start, stop = Str.group_beginning 0, Str.group_end 0 in
                      let start_offset = Convert.offset_from_pos line ~pos:start in
                      let stop_offset = Convert.offset_from_pos line ~pos:stop in
                      offsets := (start_offset, stop_offset) :: !offsets;
                      pos := stop;
                      hits <- hits + 1;
                    end
                  done
                with Not_found -> ()
              end;
              if List.length !offsets > 0 then begin
                lines_involved := {line = line; linenum = !linenum; bol = !bol; offsets = (List.rev !offsets); marks = []} ::
                                  !lines_involved;
              end;
              bol := !bol + (String.length real_line + 1);
            done
          with
          | End_of_file -> ()
          | Canceled as ex -> (finally (); raise ex)
        end;
        finally()
      end

    (** find_in_path *)
    method private find_in_path = function
      | Project_source ->
          self#find_in_path'
            ~recursive:self#options.recursive
            ?pattern:(Gaux.may_map self#options.pattern ~f:File_util.Regexp.unix_regexp)
            [Project.path_src project]
      | Specified spec_path ->
          self#find_in_path'
            ~recursive:self#options.recursive
            ?pattern:(Gaux.may_map self#options.pattern ~f:File_util.Regexp.unix_regexp)
            [spec_path]
      | Only_open_files ->
          List.iter (fun page (*: Editor_page.page)*) ->
              self#find_in_file page#get_filename) editor#pages

    method private find_in_path' ?recursive ?pattern paths =
      List.iter begin fun path ->
        let entries = if (Unix.lstat path).Unix.st_kind = Unix.S_LNK then [] else Array.to_list (Sys.readdir path) in
        let entries = List.map (fun x -> path // x) entries in
        let dirs, files = List.partition (fun x ->  Sys.file_exists x && Sys.is_directory x) entries in
        let files =
          match pattern with
          | Some pat -> List.filter (File_util.Regexp.exact_match ~pat) files
          | _ -> files
        in
        let old_hits = hits in
        List.iter self#find_in_file files;
        if hits > old_hits then (count_dirs <- count_dirs + 1);
        match recursive with
        | Some true when dirs <> [] -> self#find_in_path' ?recursive ?pattern dirs;
        | _ -> ()
      end paths

    (** replace *)
    method replace () =
      let dialog_confirm_replace = GWindow.dialog ~show:false ~title:"Confirm Replace"
          ~type_hint:`DIALOG ~urgency_hint:true ~border_width:8
          ~position:`CENTER ?parent:(GWindow.toplevel self) () in
      dialog_confirm_replace#vbox#set_border_width 8;
      dialog_confirm_replace#vbox#set_spacing 8;
      let table = GPack.table ~col_spacings:8 ~row_spacings:5 ~packing:dialog_confirm_replace#vbox#pack () in
      let _ = GMisc.label ~text:"File:" ~xalign:0.0 ~packing:(table#attach ~top:0 ~left:0) () in
      let _ = GMisc.label ~text:"Text found:" ~xalign:0.0 ~packing:(table#attach ~top:1 ~left:0) () in
      let _ = GMisc.label ~text:"Replace with:" ~xalign:0.0 ~packing:(table#attach ~top:2 ~left:0) () in
      let entry_filename = GEdit.entry ~editable:false ~packing:(table#attach ~top:0 ~left:1 ~expand:`X) () in
      let entry_find = GEdit.entry ~editable:false ~packing:(table#attach ~top:1 ~left:1 ~expand:`X) () in
      let entry_repl = GEdit.combo_box_entry
          ~model:self#options.h_repl.model ~text_column:self#options.h_repl.column
          ~packing:(table#attach ~top:2 ~left:1 ~expand:`X) () in
      entry_filename#misc#set_can_focus false;
      entry_find#misc#set_can_focus false;
      entry_repl#entry#misc#grab_focus();
      dialog_confirm_replace#add_button "Skip File" `SKIP_FILE;
      dialog_confirm_replace#add_button "Skip" `SKIP;
      dialog_confirm_replace#add_button "Replace" `REPLACE;
      dialog_confirm_replace#add_button "Replace in File" `REPLACE_FILE;
      dialog_confirm_replace#add_button "Replace All" `REPLACE_ALL;
      dialog_confirm_replace#add_button "Done" `DONE;
      let replace_all = ref false in
      let replace_file = ref "" in
      view#selection#unselect_all();
      let buffers = ref [] in
      let pages = ref [] in
      try
        List.iter begin function {filename = filename; lines = lines} as res ->
          let pagefile = `FILENAME filename in
          let page = match editor#get_page pagefile with
            | None ->
                ignore (editor#open_file ~active:false ~scroll_offset:0 ~offset:0 ?remote:None filename);
                begin
                  match editor#get_page pagefile with
                  | None -> assert false
                  | Some page -> page
                end
            | Some page ->
                (*if not page#load_complete then (ignore (page#load()));*)
                if not page#load_complete then (editor#load_page ?scroll:(Some false) page);
                page
          in
          let old_error_indication_enabled = page#error_indication#enabled in
          let old_mark_occurrences = page#view#options#mark_occurrences in
          page#view#options#set_mark_occurrences (false, false, "");
          page#error_indication#set_enabled false;
          page#buffer#undo#begin_block ~name:"replace";
          buffers := page#buffer :: !buffers;
          pages := (page, old_mark_occurrences) :: !pages;
          self#place_marks res;
          let path =
            match view#selection#get_selected_rows with
            | path :: _ ->
                GTree.Path.next path;
                path
            | _ -> (GTree.Path.create [0])
          in
          if not !replace_all && !replace_file = "" then begin
            view#selection#select_path path;
            view#scroll_to_cell path vc_file;
          end;
          let i = ref 0 in
          let l_regexp = match self#options.current_regexp with Some x -> x | _ -> assert false in
          let replacement_text m1 m2 =
            let start = page#buffer#get_iter_at_mark (`NAME m1) in
            let stop = page#buffer#get_iter_at_mark (`NAME m2) in
            let text = page#buffer#get_text ?start:(Some start) ?stop:(Some stop) ?slice:None ?visible:None () in
            let templ =
              try
                if self#options.use_regexp then Str.replace_first l_regexp self#options.text_repl text else self#options.text_repl
              with Failure _ -> self#options.text_repl
            in
            start, stop, templ
          in
          let replace m1 m2 =
            let start, stop, _ = replacement_text m1 m2 in
            let templ = entry_repl#entry#text in
            page#buffer#delete ~start ~stop;
            page#buffer#insert ?iter:None ?tag_names:None ?tags:None templ;
            if page#buffer#lexical_enabled then begin
              let iter = page#buffer#get_iter `INSERT in
              Lexical.tag page#view#buffer
                ~start:((iter#backward_chars (Glib.Utf8.length templ))#set_line_index 0)
                ~stop:iter#forward_line;
            end;
            page#buffer#move_mark (`NAME m2) ~where:(page#buffer#get_iter `INSERT);
          in
          try
            List.iter begin fun {line=line; linenum=linenum; offsets=offsets; marks=marks} ->
              begin
                match marks with [] -> () | marks ->
                  List.iter begin fun (m1, m2) ->
                    editor#goto_view page#view;
                    if not !replace_all && !replace_file = "" then (self#select_line (preview#buffer#get_iter (`LINE !i)));
                    page#view#tbuffer#select_marks ~start:(`NAME m1) ~stop:(`NAME m2);
                    let text = page#view#tbuffer#selection_text () in
                    let _, _, templ = replacement_text m1 m2 in
                    entry_filename#set_text filename;
                    entry_find#set_text text;
                    entry_repl#entry#set_text templ;
                    if not !replace_all && !replace_file = "" then begin
                      page#view#scroll_lazy (page#view#buffer#get_iter_at_mark (`NAME m1));
                      match dialog_confirm_replace#run () with
                      | `SKIP -> ();
                      | `SKIP_FILE -> raise Skip_file;
                      | `REPLACE -> replace m1 m2;
                      | `REPLACE_FILE -> replace_file := filename; replace m1 m2;
                      | `REPLACE_ALL -> replace_all := true;
                      | _ -> raise Exit;
                    end;
                    if !replace_all || !replace_file = filename then (replace m1 m2);
                  end marks
              end;
              incr i;
            end res.lines;
            page#error_indication#set_enabled old_error_indication_enabled;
            replace_file := "";
          with Skip_file -> ()
        end results;
        raise Exit
      with Exit ->
        List.iter (fun b -> b#undo#end_block()) !buffers;
        List.iter (fun (p, old) -> p#view#options#set_mark_occurrences old) !pages;
        dialog_confirm_replace#destroy()


    (** place_marks *)
    method private place_marks res =
      try
        let page =
          match editor#get_page (`FILENAME res.filename) with
          | None -> raise Not_found
          | Some page ->
              (*if not page#load_complete then (ignore (page#load()));*)
              if not page#load_complete then (editor#load_page ?scroll:(Some false) page);
              page
        in
        (*let max_ln = List.fold_left (fun acc {linenum=x} -> max x acc) 0 res.lines in
          if page#buffer#line_count < max_ln then (raise (Buffer_changed (page#buffer#line_count - max_ln, "", "")));*)
        List.iter begin function
        | ({line=line; linenum=ln; offsets=offsets; marks=marks} as result_line) when marks = [] ->
            begin
              let ln = ln - 1 in
              List.iter begin fun (start, stop) ->
                let iter = page#buffer#get_iter (`LINECHAR (ln, 0)) in
                let bline = page#buffer#get_text ?start:(Some iter) ?stop:(Some iter#forward_to_line_end) ?slice:None ?visible:None () in
                if strip_cr line = strip_cr bline then begin
                  let name_start = new_mark_name() in
                  let (_ : Gtk.text_mark) = page#buffer#create_mark ?name:(Some name_start) ?left_gravity:None (iter#forward_chars start) in
                  delete_marks <- (fun () -> page#buffer#delete_mark (`NAME name_start)) :: delete_marks;
                  let name_stop = new_mark_name() in
                  let (_ : Gtk.text_mark) = page#buffer#create_mark ?name:(Some name_stop) ?left_gravity:None (iter#forward_chars stop) in
                  delete_marks <- (fun () -> page#buffer#delete_mark (`NAME name_stop)) :: delete_marks;
                  result_line.marks <- result_line.marks @ [(name_start, name_stop)];
                end else (raise (Buffer_changed ((ln + 1), bline, line)))
              end offsets;
            end
        | _ -> ()
        end res.lines
      with
      | Not_found -> begin
          let sigid = editor#connect#add_page ~callback:begin fun page ->
              if page#get_filename = res.filename then (self#place_marks res)
            end in
          sigids <- sigid :: sigids
        end
      | Buffer_changed (n, bline, line) (*as ex*) -> begin
          if selected_text_bounds = None then begin
            fprintf stderr "Find_text_output: exception Buffer_changed\nFile %s: at line %d found %S, expected %S\n%!"
              res.filename n bline line;
          end;
          self#set_insensitive()
        end

    (** select_line *)
    method private select_line iter =
      let where = iter#set_line_offset 0 in
      preview#buffer#place_cursor ~where;
      preview#buffer#remove_tag_by_name "find_text_result_line"
        ~start:preview#buffer#start_iter ~stop:preview#buffer#end_iter;
      let _ = self#get_selected_result () in
      preview#buffer#apply_tag_by_name "find_text_result_line"
        ~start:(iter#set_line_offset 0)
        ~stop:((iter#set_line_offset 0)#forward_to_line_end);
      preview#draw_current_line_background ~force:true iter;
      preview#scroll_lazy (preview#buffer#get_iter_at_mark `INSERT);

      (** get_selected_result *)
    method private get_selected_result () =
      let path =
        match view#selection#get_selected_rows with
        | x :: _ -> x
        | _ ->
            let first = GTree.Path.create [0] in
            view#selection#select_path first;
            first
      in
      let row = model#get_iter path in
      let file = model#get ~row ~column:col_file in
      let path = model#get ~row ~column:col_path in
      let filename = path // file in
      List.find (fun {filename=fn} -> fn = filename) results

    (** set_insensitive *)
    method private set_insensitive () =
      preview#buffer#remove_tag_by_name "find_text_result_line"
        ~start:preview#buffer#start_iter ~stop:preview#buffer#end_iter;
      preview#buffer#apply_tag_by_name "find_text_insensitive"
        ~start:preview#buffer#start_iter ~stop:preview#buffer#end_iter;

      (** activate *)
    method private activate ?(grab_focus=false) iter =
      let res = self#get_selected_result () in
      try
        let lines_involved = res.lines in
        let filename = res.filename in
        ignore (editor#open_file ~active:true ~scroll_offset:0 ~offset:0 ?remote:None filename);
        let page =
          match editor#get_page (`FILENAME filename)
          with None -> raise Not_found | Some page -> page
        in
        self#place_marks res;
        Gmisclib.Idle.add ~prio:300 (fun () -> begin
              match List.nth lines_involved iter#line with
              | {marks = ((mark_start, mark_stop) :: _); _} ->
                  begin
                    try
                      let where = page#buffer#get_iter_at_mark (`NAME mark_start) in
                      page#buffer#select_range where (page#buffer#get_iter_at_mark (`NAME mark_stop));
                      page#view#scroll_lazy where;
                      if grab_focus then (page#view#misc#grab_focus()) else (preview#misc#grab_focus())
                    with GText.No_such_mark _ -> begin
                        List.iter (fun x -> x.marks <- []) res.lines;
                        self#activate ~grab_focus iter;
                      end
                  end;
              | _ -> ()
            end)
      with Not_found -> (if App_config.application_debug then assert false)

    (** display *)
    method private display () =
      (* Sort *)
      List.iter begin fun result ->
        result.lines <- List.sort (fun {linenum = l1} {linenum = l2} -> compare l1 l2) result.lines
      end results;
      results <- List.sort (fun {filename=f1} {filename=f2} -> compare f1 f2) results;
      (*  *)
      Gaux.may (preview#get_window `TEXT) ~f:(fun w -> Gdk.Window.set_cursor w (Gdk.Cursor.create `ARROW));
      (*    (* Line result at iter *)
            let result_line iter =
            let res = self#get_selected_result () in
            let lines_involved = res.lines in
            List.nth lines_involved iter#line
            in*)
      List.iter begin function {filename=filename; lines=lines_involved} ->
        let row = model#append () in
        model#set ~row ~column:col_file (Filename.basename filename);
        model#set ~row ~column:col_hits (List.fold_left (fun acc {offsets=x} -> acc + (List.length x)) 0 lines_involved);
        model#set ~row ~column:col_path (Filename.dirname filename);
        n_rows <- n_rows + 1;
        (*(*GtkThread.async*) self#place_marks res;*)
      end results;
      let get_text filename = Buffer.contents (File_util.read filename) in
      let get_text = Utils.Memo.create get_text in
      let get_comments filename =
        if List.exists ((^^^) filename) [".ml"; ".mli"; ".mll"; ".mly"] then
          let text = get_text filename in
          let comments = match Comments.scan_utf8 text with Comments.Utf8 x -> x | _ -> assert false in
          let comments = List.map (fun (a, b, _, odoc) ->
              (Convert.offset_from_pos text ~pos:a, Convert.offset_from_pos text ~pos:b, odoc)) comments
          in
          let strings = Lex.strings text in
          comments, strings
        else [], []
      in
      let get_comments = Utils.Memo.create get_comments in
      Gaux.may sigid_changed ~f:(fun id -> view#selection#misc#disconnect id);
      sigid_changed <- Some (view#selection#connect#after#changed ~callback:begin fun () ->
          try
            let path = List.hd view#selection#get_selected_rows in
            let row = model#get_iter path in
            let file = model#get ~row ~column:col_file in
            let path = model#get ~row ~column:col_path in
            let filename = path // file in
            tbuf#set_lexical_enabled (filename ^^^ ".ml" || filename ^^^ ".mli" || filename ^^^ ".mll" || filename ^^^ ".mly");
            match List.find_opt (fun {filename=fn; _} -> fn = filename) results with
            | None -> () (* TODO: Under Windows sometimes I get filenames separated by "backslash" instead of "/" in results. *)
            | Some res ->
                let lines_involved = res.lines in
                tbuf#delete ~start:tbuf#start_iter ~stop:tbuf#end_iter;
                let i = ref 0 in
                let maxlinenum = List.fold_left (fun acc {linenum=x} -> max acc x) 0 lines_involved in
                let maxlinenum_length = String.length (string_of_int maxlinenum) in
                let comments, strings = get_comments filename in
                (* Display lines *)
                List.iter begin fun {line=line0; linenum=linenum; bol=bol; offsets=hits} ->
                  begin
                    try
                      let utf8_line_length = Glib.Utf8.length line0 in
                      let scarto = 0 in
                      let disaply_line = sprintf "%s: %s"
                          (Utils.lpad (string_of_int linenum) ' ' maxlinenum_length) line0 in
                      tbuf#insert (disaply_line ^ (if !i = List.length lines_involved - 1 then "" else "\n"));
                      let iter = tbuf#get_iter `INSERT in
                      if tbuf#lexical_enabled then begin
                        let eol = bol + utf8_line_length in
                        let start = iter#backward_char#backward_chars utf8_line_length in
                        let stop = iter#forward_line in
                        let is_line_not_colorized = ref true in
                        begin
                          try
                            List.iter begin fun (bc, ec, odoc) ->
                              let comment = if odoc then "ocamldoc" else "comment" in
                              if bc <= bol && eol <= ec then begin
                                tbuf#apply_tag_by_name comment ~start ~stop;
                                raise Exit
                              end else if bol <= bc && bc <= eol && eol <= ec then begin (* |----|=====| *)
                                let len = bc - bol in
                                let i = start#forward_chars len in
                                Lexical.tag (tbuf :> GText.buffer) ~start ~stop:i;
                                tbuf#apply_tag_by_name comment ~start:i ~stop:stop#forward_line;
                                raise Exit
                              end else if bc < bol && bol < ec && ec < eol then begin (* |=====|----| *)
                                let i = start#forward_chars (ec - bol) in
                                tbuf#apply_tag_by_name comment ~start ~stop:i;
                                Lexical.tag (tbuf :> GText.buffer) ~start:i ~stop:i#forward_to_line_end;
                                raise Exit;
                              end
                            end comments;
                          with Exit -> is_line_not_colorized := false;
                        end;
                        if !is_line_not_colorized then begin
                          begin
                            try
                              List.iter begin fun {Lex.lexeme=lxm; start=bs; length=len} ->
                                let es = bs + (Glib.Utf8.length lxm) + 2 in
                                if bs <= bol && eol <= es then begin
                                  tbuf#apply_tag_by_name "char" ~start ~stop:start#forward_line;
                                  raise Exit
                                end else if bol < bs && bs < eol && eol < es then begin (* |----|=====| *)
                                  let i = start#forward_chars (bs - bol) in
                                  Lexical.tag (tbuf :> GText.buffer) ~start ~stop:i;
                                  tbuf#apply_tag_by_name "char" ~start:i ~stop:iter#forward_line;
                                  raise Exit
                                end else if bs < bol && bol < es && es < eol then begin (* |=====|----| *)
                                  let i = start#forward_chars (bs + len - bol) in
                                  tbuf#apply_tag_by_name "char" ~start ~stop:i;
                                  Lexical.tag (tbuf :> GText.buffer) ~start:i ~stop:i#forward_to_line_end;
                                  raise Exit
                                end
                              end strings;
                            with Exit -> is_line_not_colorized := false;
                          end;
                          if !is_line_not_colorized then
                            (Lexical.tag (tbuf :> GText.buffer) ~start ~stop:start#forward_to_line_end);
                        end;
                      end;
                      tbuf#apply_tag_by_name "find_text_result_linenum"
                        ~start:(tbuf#get_iter (`LINECHAR (!i, 0)))
                        ~stop:(tbuf#get_iter (`LINECHAR (!i, maxlinenum_length + 2)));
                      List.iter begin fun (start, stop) ->
                        let start = start + maxlinenum_length + 2 - scarto in
                        let stop = stop + maxlinenum_length + 2 - scarto in
                        tbuf#apply_tag_by_name "find_text_result_hit"
                          ~start:(tbuf#get_iter (`LINECHAR (!i, start)))
                          ~stop:(tbuf#get_iter (`LINECHAR (!i, stop)));
                      end hits;
                    with Glib.Convert.Error (Glib.Convert.ILLEGAL_SEQUENCE, _) -> ()
                  end;
                  incr i
                end lines_involved;
                tbuf#place_cursor ~where:tbuf#start_iter;
                current_line_selected <- None;
          with Failure _ -> ()
        end);
      ignore (view#connect#after#row_activated ~callback:begin fun path iter ->
          ignore (self#select_line (tbuf#get_iter `SEL_BOUND));
          preview#misc#grab_focus();
        end);
      ignore (preview#event#connect#focus_in ~callback:begin fun ev ->
          if tbuf#char_count > 0 then begin
            let bgcolor = ColorOps.name_of_gdk (Preferences.editor_tag_bg_color "highlight_current_line") in
            Gmisclib.Util.set_tag_paragraph_background preview#highlight_current_line_tag bgcolor;
            let iter = tbuf#get_iter `INSERT in
            self#select_line iter;
            self#activate ~grab_focus:false iter;
            current_line_selected <- (Some iter#line);
          end;
          false
        end);
      ignore (preview#event#connect#focus_out ~callback:begin fun ev ->
          false
        end);
      let two_button_press = ref false in
      let get_iter ev =
        let x = int_of_float (GdkEvent.Button.x ev) in
        let y = int_of_float (GdkEvent.Button.y ev) in
        let x, y = preview#window_to_buffer_coords ~tag:`TEXT ~x ~y in
        preview#get_iter_at_location ~x ~y
      in
      ignore (preview#event#connect#button_release ~callback:begin fun ev ->
          let iter = get_iter ev in
          match GdkEvent.get_type ev with
          | `BUTTON_RELEASE when !two_button_press ->
              two_button_press := false;
              self#select_line iter;
              self#activate ~grab_focus:true iter;
              preview#buffer#place_cursor ~where:(iter#set_line_index 0);
              false
          | _ -> false
        end);
      ignore (preview#event#connect#button_press ~callback:begin fun ev ->
          let iter = get_iter ev in
          match GdkEvent.get_type ev with
          | `TWO_BUTTON_PRESS ->
              two_button_press := true;
              true
          | `BUTTON_PRESS ->
              self#select_line iter;
              self#activate ~grab_focus:false iter;
              false
          | _ -> false
        end);
      ignore (preview#event#connect#key_press ~callback:begin fun ev ->
          let key = GdkEvent.Key.keyval ev in
          let iter = preview#buffer#get_iter `INSERT in
          if key = _Up then self#prev_line ()
          else if key = _Down then self#next_line ()
          else if List.mem key [_Right; _Left; _Return] then begin
            self#activate ~grab_focus:true iter;
          end;
          true
        end);
      (*  *)
      ignore (preview#event#connect#key_release ~callback:begin fun ev ->
          let key = GdkEvent.Key.keyval ev in
          if key = GdkKeysyms._Tab then ()
          else if key = GdkKeysyms._ISO_Left_Tab then (view#misc#grab_focus());
          true;
        end);
      (*  *)
      ignore (preview#connect#destroy ~callback:begin fun () ->
          List.iter (fun f -> try f () with GText.No_such_mark _ -> ()) delete_marks;
          List.iter editor#connect#disconnect sigids
        end);
      view#selection#select_path (GTree.Path.create [0]);
      if editor_buffer <> None then (lsw#misc#hide ());

    method connect = new signals ~search_started ~search_finished
  end

and search_started () = object (self) inherit [unit] signal () as super end
and search_finished () = object (self) inherit [unit] signal () as super end
and signals ~search_started ~search_finished =
  object (self)
    inherit ml_signals [search_started#disconnect; search_finished#disconnect]
    method search_started = search_started#connect ~after
    method search_finished = search_finished#connect ~after
  end
