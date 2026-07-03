(** Outline view for OCaml code structure visualization.

    This module provides an interactive tree view that displays the structure
    of OCaml source code, including modules, types, values, methods, and comments.
    It integrates with Merlin to extract structural information and provides
    features like fuzzy search, cursor tracking, and sorting. *)

open Oe
open Printf
open Settings_j
open Merlin_j
open Preferences
open Fuzzy_search

module Log = Common.Log.Make(struct let prefix = "OUTLINE" end)
let _ =
  Log.set_print_timestamp true;
  Log.set_verbosity `WARN

(** Exception raised when a matching tree iterator is found during traversal. *)
exception Break of Gtk.tree_iter

(** Exception raised when a fuzzy search match is found.
    Contains the name path and match score. *)
exception Found of string list * float

(** Enable experimental fuzzy search functionality. *)
let enable_fuzzy_search = true

open GUtil

(** Model managing the outline data extracted from OCaml source code.

    This class handles communication with Merlin to extract code structure,
    maintains a cached outline that's periodically refreshed, and notifies
    listeners when the outline changes. The model validates that cached data
    is still current by comparing buffer modification times. *)
class model ~(buffer : Ocaml_text.buffer) () =
  let merlin text func = func ~filename:buffer#filename ~buffer:text in
  object (self)
    (** Current outline structure from Merlin. *)
    val mutable outline = []

    (** Hash of current outline for change detection. *)
    val mutable outline_hash = 0

    (** Timer ID for periodic outline updates. *)
    val mutable timer_id = None

    (** Timestamp of last successful outline refresh. *)
    val mutable last_refresh_time = 0.0

    (** Signal emitted when outline data changes. *)
    val changed = new changed()

    (** Returns the current outline structure. *)
    method get = outline

    (** Starts the automatic refresh timer.
        Updates occur every 300ms while attached. *)
    method attach = self#start_timer

    (** Stops the automatic refresh timer and resets state. *)
    method detach = self#stop_timer

    (** Checks if cached outline is still valid.
        Returns [true] if the buffer hasn't been modified since last refresh. *)
    method is_valid = buffer#last_edit_time < last_refresh_time

    (** Updates the outline from current buffer content.

        @param force If [true], update even if cache is valid

        Queries Merlin for code structure, extracts comments using the lexer,
        and merges them into a single outline. Emits [changed] signal if the
        structure has changed. Updates are skipped if the buffer was modified
        during the async Merlin call (detected via timestamps). *)
    method update ?(force=false) () =
      if not self#is_valid || force then begin
        let source_code = buffer#get_text () in
        last_refresh_time <- Unix.gettimeofday();
        (merlin source_code)@@Merlin.outline
        |> Async.start_with_continuation ~name:__FUNCTION__ begin function
        | Merlin.Ok (ol : Merlin_j.outline list) ->
            (* Extract comments from source and convert to outline entries *)
            let comments =
              let open Location in
              Lex.comments source_code
              |> List.map begin fun (c, loc) ->
                let _, start_ln, start_cn = Location.get_pos_info loc.loc_start in
                let _, stop_ln, stop_cn = Location.get_pos_info loc.loc_end in
                {
                  ol_kind = "Comment";
                  ol_name = "";
                  ol_start = { line = start_ln; col = start_cn };
                  ol_stop = { line = stop_ln; col = stop_cn };
                  ol_parent = None;
                  ol_children = [];
                  ol_level = 0;
                }
              end
            in
            let ol = List.rev_append comments ol in
            let hash = Hashtbl.hash ol in
            (* Only update if content changed and buffer hasn't been modified *)
            if outline_hash <> hash || force then
              if self#is_valid then begin
                outline_hash <- hash;
                outline <- ol;
                changed#call ();
              end else
                Log.println `WARN
                  "*** not up-to-date (%s) %.2f %.2f ***"
                  (Filename.basename buffer#filename)
                  buffer#last_edit_time  last_refresh_time;
        | Merlin.Failure _ | Merlin.Error _ -> ()
        end
      end

    (** Starts the periodic refresh timer if not already running.
        Performs an immediate update followed by updates every 300ms. *)
    method private start_timer () =
      match timer_id with
      | None ->
          self#update() |> ignore;
          timer_id <- Some (GMain.Timeout.add ~ms:300 ~callback:(fun () -> self#update(); true));
      | _ -> ()

    (** Stops the refresh timer and resets timestamps. *)
    method private stop_timer () =
      begin
        match timer_id with
        | None -> ()
        | Some id ->
            timer_id <- None;
            last_refresh_time <- 0.0;
            GMain.Timeout.remove id
      end;

    method connect = new outline_signals ~changed

  end

and changed () = object inherit [unit] signal () end
and outline_signals ~changed =
  object
    inherit ml_signals [changed#disconnect]
    method changed = changed#connect ~after
  end

(** GTK tree model column definitions for outline view. *)
let cols               = new GTree.column_list
let col_markup         = cols#add Gobject.Data.string
let col_data           : Merlin_j.outline GTree.column = cols#add Gobject.Data.caml
let col_name           = cols#add Gobject.Data.string

(** Interactive tree view displaying OCaml code structure.

    Features:
    - Hierarchical display of modules, types, values, and methods
    - Automatic cursor tracking (follows editor cursor position)
    - Sorting by name, kind, or source position
    - Navigation to definitions by clicking tree items
    - Expand/collapse controls
    - Toolbar with various operation buttons *)
class view ~(outline : Oe.outline) ~(source_view : Ocaml_text.view) ?packing () =
  let pref                   = Preferences.preferences#get in
  let show_types             = pref.outline_show_types in
  let vbox                   = GPack.vbox ?packing () in
  let model                  = GTree.tree_store cols in
  let toolbar                = GButton.toolbar ~orientation:`HORIZONTAL ~style:`TEXT ~packing:(vbox#pack ~expand:false ~fill:false) () in
  let sw                     = GBin.scrolled_window ~shadow_type:`NONE ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:vbox#add () in
  let view                   = GTree.view ~model ~headers_visible:false
      ~enable_search:true ~search_column:2
      ~packing:sw#add ~width:350 ~height:500 ()
  in
  let renderer_pixbuf        = GTree.cell_renderer_pixbuf [`YPAD 0; `XPAD 0] in
  let renderer_markup        = GTree.cell_renderer_text [`YPAD 0] in
  let vc                     = GTree.view_column () in
  let _                      = vc#pack ~expand:false renderer_pixbuf in
  let _                      = vc#pack ~expand:false renderer_markup in
  let _                      = vc#add_attribute renderer_markup "markup" col_markup in

  let _                      = view#selection#set_mode `SINGLE in
  let _                      = view#append_column vc in
  let _                      = view#misc#set_name "outline_treeview" in
  let _                      = view#misc#set_property "enable-tree-lines" (`BOOL true) in

  (** Comparison functions for different sorting modes. *)
  let compare_position a b = compare b.ol_start a.ol_start in
  let compare_name a b = compare (String.lowercase_ascii b.ol_name) (String.lowercase_ascii a.ol_name) in
  let compare_kind a b = compare (String.lowercase_ascii b.ol_kind) (String.lowercase_ascii a.ol_kind) in

  object (self)
    inherit GObj.widget vbox#as_widget

    val mutable code_font_family = ""

    (** Signal connection for selection changes (used to block/unblock during updates). *)
    val mutable sig_selection_changed = None

    (** Timer ID for cursor following functionality. *)
    val mutable timer_follow_cursor = None

    (** List of expanded node names (preserved across rebuilds). *)
    val mutable names_expaneded = []

    val view = view
    val model = model
    val vc = vc

    val buffer = source_view#obuffer

    (** Toolbar buttons for various operations. *)
    val tool_refresh = GButton.tool_button ~packing:toolbar#insert ()
    val tool_sort_name = GButton.toggle_tool_button ~packing:toolbar#insert ()
    val tool_sort_kind = GButton.toggle_tool_button ~packing:toolbar#insert ()
    val tool_collapse_all = GButton.tool_button ~packing:toolbar#insert ()
    val tool_goto_cursor_position = GButton.tool_button ~packing:toolbar#insert ()
    val tool_follow_cursor = GButton.toggle_tool_button ~active:true ~packing:toolbar#insert ()

    initializer
      (* Set toolbar button icons *)
      tool_refresh#set_label_widget (Gtk_util.label_icon "\u{f0453}")#coerce;
      tool_collapse_all#set_label_widget (Gtk_util.label_icon "\u{f102}")#coerce;
      tool_goto_cursor_position#set_label_widget (Gtk_util.label_icon "\u{f177}")#coerce;
      tool_follow_cursor#set_label_widget (Gtk_util.label_icon "\u{21c6}")#coerce;
      tool_sort_name#set_label_widget (Gtk_util.label_icon "\u{f05bd}")#coerce;
      tool_sort_kind#set_label_widget (Gtk_util.label_icon "\u{f1385}")#coerce;
      self#update_preferences();
      Preferences.preferences#connect#changed ~callback:(fun _ -> self#update_preferences ()) |> ignore;
      self#set_follow_cursor true;

      view#connect#row_activated ~callback:begin fun _ _ ->
        self#jump_to_definition();
        Gmisclib.Idle.add source_view#misc#grab_focus
      end |> ignore;

      view#event#connect#key_press ~callback:begin fun ev ->
        let key = GdkEvent.Key.keyval ev in
        begin
          match view#selection#get_selected_rows with
          | path :: _ when key = GdkKeysyms._Left -> view#collapse_row path
          | path :: _ when key = GdkKeysyms._Right -> view#expand_row path
          | _ -> ()
        end;
        false
      end |> ignore;

      (* Attach/detach outline updates when source view gains/loses focus *)
      let sig_focus_in =
        source_view#event#connect#focus_in ~callback:(fun _ ->
            self#set_follow_cursor tool_follow_cursor#get_active;
            outline#attach(); false)
      in
      let sig_focus_out =
        source_view#event#connect#focus_out ~callback:(fun _ ->
            self#set_follow_cursor false;
            outline#detach(); false)
      in

      outline#connect#changed ~callback:self#build |> ignore;
      sig_selection_changed <- Some (view#selection#connect#changed ~callback:self#jump_to_definition);

      (* Handle row expansion: lazy-load children *)
      view#connect#row_expanded ~callback:begin fun row path ->
        try
          let ol = model#get ~row ~column:col_data in
          names_expaneded <- ol.ol_name :: names_expaneded;
          self#build_childs row path
        with Gpointer.Null as ex ->
          Printf.eprintf "File \"outline.ml\": **** %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());
      end |> ignore;

      (* Track collapsed rows to preserve state *)
      view#connect#row_collapsed ~callback:begin fun row _ ->
        let ol = model#get ~row ~column:col_data in
        names_expaneded <- names_expaneded |> List.filter (fun n -> ol.ol_name <> n);
      end |> ignore;

      (* Connect toolbar buttons *)
      tool_follow_cursor#connect#clicked ~callback:(fun () ->
          self#set_follow_cursor tool_follow_cursor#get_active) |> ignore;
      tool_goto_cursor_position#connect#clicked ~callback:begin fun () ->
        self#goto_cursor_position (buffer#get_mark `INSERT)
      end |> ignore;

      (* Set tooltips *)
      tool_refresh#misc#set_tooltip_text "Refresh";
      tool_collapse_all#misc#set_tooltip_text "Collapse all";
      tool_goto_cursor_position#misc#set_tooltip_text "Go to cursor position";
      tool_follow_cursor#misc#set_tooltip_text "Follow cursor";
      tool_sort_name#misc#set_tooltip_text "Sort by name";
      tool_sort_kind#misc#set_tooltip_text "Sort by kind";

      (* Handle sort button interactions (mutually exclusive) *)
      let sig_sort_name = ref None in
      let sig_sort_kind = ref None in
      sig_sort_name :=
        Some (tool_sort_name#connect#clicked ~callback:begin fun () ->
            Option.iter tool_sort_kind#misc#handler_block !sig_sort_kind;
            tool_sort_kind#set_active false;
            Option.iter tool_sort_kind#misc#handler_unblock !sig_sort_kind;
            outline#update ~force:true ()
          end);
      sig_sort_kind :=
        Some (tool_sort_kind#connect#clicked ~callback:begin fun () ->
            Option.iter tool_sort_name#misc#handler_block !sig_sort_name;
            tool_sort_name#set_active false;
            Option.iter tool_sort_name#misc#handler_unblock !sig_sort_name;
            outline#update ~force:true ()
          end);

      (* Collapse all with smart re-activation of cursor following *)
      tool_collapse_all#connect#clicked ~callback:begin fun () ->
        if tool_follow_cursor#get_active then begin
          Option.iter GMain.Timeout.remove timer_follow_cursor;
          timer_follow_cursor <- None;
          let sig_mark_set = ref None in
          sig_mark_set := Some (buffer#connect#mark_set ~callback:begin fun _ mark ->
              match GtkText.Mark.get_name mark with
              | Some "insert" ->
                  self#set_follow_cursor tool_follow_cursor#get_active;
                  Option.iter (GtkSignal.disconnect buffer#as_buffer) !sig_mark_set;
              | _ -> ()
            end)
        end;
        view#collapse_all()
      end |> ignore;

      tool_refresh#connect#clicked ~callback:(fun () -> outline#update ~force:true ()) |> ignore;

      (* Cleanup on destroy *)
      view#misc#connect#destroy ~callback:begin fun _ ->
        GtkSignal.disconnect source_view#as_view sig_focus_in;
        GtkSignal.disconnect source_view#as_view sig_focus_out;
        self#set_follow_cursor false
      end |> ignore;

    method outline = outline

    method refresh = outline#update ~force:true

    (** Performs a depth-first fold over the outline structure.

        @param f Folding function that receives parent chain, current node, and accumulator
        @param parent Parent chain (innermost first)
        @param ol Current outline level to traverse
        @param acc Initial accumulator value
        @return Final accumulator after traversing entire structure *)
    method private fold_depth_first f parent ol acc =
      match ol with
      | [] -> acc
      | hd :: tl ->
          let acc = f parent hd acc in
          let acc = self#fold_depth_first f (hd :: parent) hd.ol_children acc in
          let acc = self#fold_depth_first f parent tl acc in
          acc

    (** Gets a GTK text iterator at the start of a line.

        @param pos Position with line number (1-based)
        @raise Invalid_linechar if line number is out of bounds *)
    method private get_iter_at_line pos =
      let ln = pos.line - 1 in
      if pos.line < 0 || ln > buffer#end_iter#line then raise (Invalid_linechar pos);
      buffer#get_iter (`LINE ln)

    (** Gets a GTK text iterator at a specific line and column position.

        @param pos Position with line (1-based) and column (0-based)
        @raise Invalid_linechar if position is out of bounds *)
    method private get_iter_at_linechar pos =
      let it = self#get_iter_at_line pos in
      if pos.col >= it#chars_in_line then raise (Invalid_linechar pos);
      it#set_line_offset pos.col

    (** Jumps to the definition of the currently selected outline item.
        Selects the identifier name in the source buffer and scrolls it into view. *)
    method jump_to_definition () =
      match view#selection#get_selected_rows with
      | [] -> ()
      | path :: _ ->
          begin
            try
              let row = model#get_iter path in
              let ol = model#get ~row ~column:col_data in
              let start = self#get_iter_at_line ol.ol_start in
              let start, stop =
                match start#forward_search ol.ol_name with
                | Some bounds -> bounds
                | _ ->
                    Log.println `WARN "name %S not found on line %d" ol.ol_name (start#line + 1);
                    let start = start#forward_word_end#backward_word_start in
                    start, start
              in
              buffer#select_range start stop;
              source_view#scroll_lazy start;
            with Invalid_linechar pos ->
              Log.println `ERROR "Invalid line/char (file %s, ln %d, cn %d)"
                buffer#filename pos.line pos.col
          end

    (** Selects the entire region of the currently selected outline item in the buffer. *)
    method select_in_buffer () =
      match view#selection#get_selected_rows with
      | [] -> ()
      | path :: _ ->
          begin
            try
              let row = model#get_iter path in
              let ol = model#get ~row ~column:col_data in
              let start = self#get_iter_at_linechar ol.ol_start in
              let stop = self#get_iter_at_linechar ol.ol_stop in
              buffer#select_range start stop
            with Exit ->
              Log.println `ERROR "Invalid line/char"
          end

    (** Selects the outline item containing the cursor position.

        @param mark The text mark to check (typically INSERT for cursor)

        Finds the smallest (most specific) outline item containing the mark position
        and selects it in the tree view. Expands parent nodes and scrolls into view
        if needed. *)
    method goto_cursor_position (mark : Gtk.text_mark) =
      if self#misc#get_flag `VISIBLE then begin
        let iter = buffer#get_iter_at_mark (`MARK mark) in
        let ln = iter#line + 1 in
        let cn = iter#line_offset + 1 in
        let found_paths = ref [] in
        (* Find all outline items containing the cursor *)
        model#foreach begin fun path row ->
          try
            let ol = model#get ~row ~column:col_data in
            if ol.ol_kind <> "Dummy" then begin
              let start, stop =
                if ol.ol_kind = "Method" then
                  (self#get_iter_at_linechar ol.ol_start)#set_line_offset 0,
                  (self#get_iter_at_linechar ol.ol_stop)#set_line_offset 0
                else
                  self#get_iter_at_linechar ol.ol_start,
                  let it = self#get_iter_at_linechar ol.ol_stop in
                  if it#ends_line then it#forward_char else it#forward_to_line_end
              in
              if iter#in_range ~start ~stop then
                found_paths := (path, ol.ol_stop.line - ol.ol_start.line) :: !found_paths;
            end;
            false
          with Invalid_linechar pos ->
            Log.println `ERROR "Invalid line/char (%d, %d)" pos.line pos.col;
            false
        end;
        match !found_paths with
        | [] ->
            view#selection#unselect_all()
        | paths -> begin
            (* Select the smallest (most specific) region *)
            paths
            |> List.fold_left begin fun smallest ((_, d) as x) ->
              match smallest with
              | Some ((_, d') as x') when d' < d -> Some x'
              | _ -> Some x
            end None
            |> Option.iter begin fun (path, _) ->
              match view#selection#get_selected_rows with
              | selected_path :: _ when selected_path = path -> ()
              | _ ->
                  view#expand_to_path path;
                  view#selection#select_path path;
                  if view#misc#get_flag `REALIZED && not (Gmisclib.Util.treeview_is_path_onscreen view path) then
                    Gmisclib.Idle.add ~prio:300 (fun () ->
                        view#scroll_to_cell ~align:(0.38, 0.) path vc);
            end
          end
      end

    (** Returns the current comparison function based on active sort mode. *)
    method private sort_func =
      if tool_sort_name#get_active then compare_name
      else if tool_sort_kind#get_active then compare_kind
      else compare_position

    (** Lazily builds child nodes for an expanded tree row.

        @param prio Priority for idle callbacks (0 = immediate, higher = deferred)
        @param row The parent row being expanded
        @param path The tree path of the parent row

        Replaces the dummy placeholder node with actual child nodes.
        Uses idle callbacks for progressive loading unless prio=0. *)
    method private build_childs ?(prio=300) row path =
      let parent = GTree.Path.copy path in
      GTree.Path.down path;
      let first_child = model#get_iter path in
      let first_child_data = model#get ~row:first_child ~column:col_data in
      if first_child_data.ol_kind = "Dummy" then begin
        model#remove first_child |> ignore;
        let row_data = model#get ~row ~column:col_data in
        let steps =
          row_data.ol_children
          |> List.sort self#sort_func
          |> List.map (fun child () -> self#append ~parent:(model#get_iter parent) child)
        in
        let steps = (fun () -> view#expand_row parent) :: steps in
        match prio with
        | 0 -> steps |> List.rev |> List.iter (fun f -> f())
        | _ -> Gmisclib.Idle.idleize_cascade ~prio steps ();
      end

    (** Rebuilds the entire outline tree from current outline data.
        Preserves expansion state of previously expanded nodes. *)
    method private build () =
      let steps =
        outline#get
        |> List.sort self#sort_func
        |> List.map (fun ol () -> self#append ol)
      in
      let update_expaneded_rows () =
        model#foreach begin fun path row ->
          let ol = model#get ~row ~column:col_data in
          if names_expaneded |> List.exists (fun ne -> ne = ol.ol_name) then begin
            Log.println `DEBUG "%s %s" __FUNCTION__ ol.ol_name;
            view#expand_row path;
          end;
          false
        end;
      in
      let steps = update_expaneded_rows :: steps in
      model#clear();
      Gmisclib.Idle.idleize_cascade ~prio:200 steps ()

    (** Enables or disables automatic cursor following.

        @param active If [true], outline selection follows cursor position

        When enabled, polls cursor position every second and updates selection.
        When disabled, manual navigation button becomes available. *)
    method private set_follow_cursor active =
      tool_goto_cursor_position#misc#set_sensitive (not active);
      if active then
        timer_follow_cursor <- Some begin
            GMain.Timeout.add ~ms:1000 ~callback:begin fun () ->
              let mark = buffer#get_mark `INSERT in
              Gmisclib.Idle.add ~prio:300 begin fun () ->
                if timer_follow_cursor <> None then begin
                  Option.iter view#selection#misc#handler_block sig_selection_changed;
                  self#goto_cursor_position mark;
                  Option.iter view#selection#misc#handler_unblock sig_selection_changed;
                end
              end;
              true
            end
          end
      else begin
        Option.iter begin fun id ->
          timer_follow_cursor <- None;
          GMain.Timeout.remove id
        end timer_follow_cursor
      end

    (** Appends an outline item to the tree model.

        @param parent Optional parent row for nested items
        @param ol Outline item to append

        Creates a tree row with formatted markup including icon, name, and
        optional debug information. Adds a dummy child node if the item has children. *)
    method private append ?parent ol =
      if ol.ol_kind <> "Comment" then
        let row = model#append ?parent () in
        model#set ~row ~column:col_data ol;
        model#set ~row ~column:col_name ol.ol_name;
        let markup =
          sprintf "%s   %s%s %s"
            (Markup.icon_of_kind ol.ol_kind)
            (Glib.Markup.escape_text ol.ol_name)
            ""
            (if !Log.verbosity = `DEBUG then
               sprintf "<span size='x-small' color='#c0c0c0'>[ <i>%d, %d - %d, %d</i> ]</span>"
                 ol.ol_start.line (ol.ol_start.col + 1) ol.ol_stop.line (ol.ol_stop.col + 1) else "")
        in
        model#set ~row ~column:col_markup markup;
        if ol.ol_children <> [] then self#append_dummy row

    (** Appends a dummy placeholder node for lazy loading.

        @param row Parent row that will have children loaded on expansion

        The dummy node is replaced with actual children when the parent is expanded. *)
    method private append_dummy row =
      let dummy = model#append ~parent:row () in
      model#set ~row:dummy ~column:col_markup "";
      model#set ~row:dummy ~column:col_data {
        ol_kind = "Dummy";
        ol_name = "";
        ol_start = { line = 0; col = 0 };
        ol_stop = { line = 0; col = 0 };
        ol_level = 0;
        ol_parent = None;
        ol_children = []
      };

      (** Updates visual appearance from preferences.
          Applies fonts, colors, and theme styling. *)
    method private update_preferences () =
      let pref = Preferences.preferences#get in
      view#misc#modify_font_by_name pref.editor_completion_font;
      view#misc#modify_base [
        `NORMAL,   `NAME ?? (pref.outline_color_nor_bg);
        `SELECTED, `NAME ?? (pref.outline_color_sel_bg);
        `ACTIVE,   `NAME ?? (pref.outline_color_act_bg);
      ];
      view#misc#modify_text [
        `NORMAL,   `NAME ?? (pref.outline_color_nor_fg);
        `SELECTED, `NAME ?? (pref.outline_color_sel_fg);
        `ACTIVE,   `NAME ?? (pref.outline_color_act_fg);
      ];
      let style_outline, apply_outline = Gtk_theme.get_style_outline pref in
      GtkMain.Rc.parse_string (style_outline ^ "\n" ^ apply_outline);
      view#set_rules_hint (pref.outline_color_alt_rows <> None);
      let base_font = pref.editor_base_font in
      code_font_family <-
        String.sub base_font 0 (Option.value (String.rindex_opt base_font ' ') ~default:(String.length base_font));
      GtkBase.Widget.queue_draw view#as_widget;

  end

(** Transient search widget for fuzzy outline navigation.

    Displays a floating entry box near the outline view for typing search queries.
    Automatically closes after a timeout period of inactivity. *)
class search (view : GTree.view) =
  let entry = GEdit.entry () in
  let r0 = view#misc#allocation in
  let wx, wy = Gdk.Window.get_position view#misc#toplevel#misc#window in
  let x = wx + r0.Gtk.x in
  let y = wy + r0.Gtk.y in
  object
    (** Popup window containing the search entry. *)
    val mutable window = Gtk_util.window entry#coerce ~type_hint:`MENU ~focus:false ~x ~y ()

    val mutable entry = entry

    (** Timer ID for auto-close functionality. *)
    val mutable timer = None

    method entry = entry
    method destroy = window#destroy

    (** Resets the auto-close timer.
        Window closes automatically after 4.5 seconds of inactivity. *)
    method add_timer () =
      timer |> Option.iter GMain.Timeout.remove;
      timer <-
        Some (GMain.Timeout.add ~ms:4500 ~callback:begin fun () ->
            window#destroy();
            false
          end)
  end

(** Experimental outline view with fuzzy search capability.

    Extends the basic outline view with keyboard-driven fuzzy search.
    Typing while the view has focus opens a search box that performs
    fuzzy matching against outline item names and navigates to the best match.

    @note This feature is experimental and not fully polished. *)
class view_with_search ~(outline : Oe.outline) ~(source_view : Ocaml_text.view) ?packing () =
  object (self)
    inherit view ~outline ~source_view ?packing ()

    (** Optional search widget for fuzzy navigation. *)
    val mutable search_widget : search option = None

    initializer
      view#event#connect#key_press ~callback:begin fun ev ->
        self#search ev;
        false
      end |> ignore;

      (* Handles user keyboard input for fuzzy searching in the outline.
         Creates/updates search widget, performs fuzzy matching, and navigates to best result *)
    method private search ev =
      (* Finds the best fuzzy match in the outline tree using depth-first traversal.
         Returns the element with the highest match score, or None if no matches found *)
      let find_best_match query outline =
        self#fold_depth_first begin fun path ol acc ->
          let score, _ = FuzzyLetters.compare `Greedy2 query ol.ol_name in
          let path = ol.ol_name :: (path |> List.map (fun x -> x.ol_name)) |> List.rev in
          match acc with
          | None -> Some (path, score)
          | Some (_, score') when score > score' -> Some (path, score)
          | _ -> acc
        end [] outline None
      in
      (* Navigates through the GTK tree model following a hierarchical name path.
         Expands dummy nodes (lazy-loaded placeholders) when encountered.
         Returns the GTree.Path corresponding to the final element in the name path *)
      let find_tree_path (model : GTree.tree_store) name_path =
        (* Inner helper: searches for a child node with the given name under parent *)
        let rec find_child_index_by_name iter name =
          let n_childs = model#iter_n_children iter in
          (* Try each child index until we find a match *)
          List.init n_childs Fun.id
          |> List.find_map begin fun nth ->
            let row = model#iter_children ~nth iter in
            let ol = model#get ~row ~column:col_data in
            (* Direct name match found *)
            if ol.ol_name = name then Some row
            (* Dummy node: expand it and retry the search *)
            else if ol.ol_kind = "Dummy" then begin
              iter |> Option.iter (fun p -> self#build_childs ~prio:0 p (model#get_path p));
              find_child_index_by_name iter name
            end else None
          end
        in
        (* Traverse the name path, building up the tree path incrementally *)
        name_path
        |> List.fold_left begin fun (parent, acc) elm ->
          try
            match find_child_index_by_name parent elm with
            | Some child_iter -> (* Found the child: update parent and accumulator with new path *)
                let child_path = model#get_path child_iter in
                Printf.printf "  %s %s\n%!" elm (GTree.Path.to_string child_path);
                (Some child_iter), child_path
            | _ -> (* Child not found: keep current state *)
                parent, acc
          with ex ->
            Printf.eprintf "File \"outline.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());
            (parent, acc)
        end (None, GTree.Path.create [])
        |> snd (* Extract only the final tree path, discard the parent iterator *)
      in
      (* Appends a character to the search entry and returns the updated query string *)
      let append_to_search sw str =
        sw#entry#set_text (sw#entry#text ^ str);
        sw#entry#text
      in
      let is_printable_key ev =
        let str = GdkEvent.Key.string ev in
        if String.length str > 0 && Glib.Unichar.isprint (Glib.Utf8.first_char str) then Some str else None
      in
      match is_printable_key ev with
      | Some str ->
          let sw =
            match search_widget with
            | Some sw ->
                sw#add_timer();
                sw
            | _ ->
                let sw = new search view in
                search_widget <- Some sw;
                sw
          in
          let query = append_to_search sw str in
          outline#get
          |> find_best_match query
          |> Option.iter begin fun (name_path, score) ->
            try
              Printf.printf "FOUND %s -- %s %f\n%!" sw#entry#text (String.concat "." name_path) score;
              let tree_path = find_tree_path model name_path in
              Printf.printf "tree_path = %s\n%!" (GTree.Path.to_string tree_path);
              view#selection#select_path tree_path;
              view#set_cursor tree_path vc;
              Gmisclib.Idle.add ~prio:300 (fun () ->
                  view#scroll_to_cell ~align:(0.38, 0.) tree_path vc);
            with ex ->
              search_widget |> Option.iter (fun w -> w#destroy());
              search_widget <- None;
              raise ex
          end
      | _ ->
          search_widget |> Option.iter (fun w -> w#destroy());
          search_widget <- None
  end
