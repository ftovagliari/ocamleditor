open Oe
open Printf
open Settings_j
open Preferences
open! Merlin_t
open! Outline_diff

module Log = Common.Log.Make(struct let prefix = "OUTLINE" end)
let _ =
  Log.set_print_timestamp true;
  Log.set_verbosity `WARN

exception Break of Gtk.tree_iter

exception Found of string list * float

let enable_fuzzy_search = true

open GUtil

class model ~(buffer : Ocaml_text.buffer) () : Oe.outline =
  let merlin text func = func ~filename:buffer#filename ~buffer:text in
  object (self)
    val mutable outline = []
    val mutable timer_id = None
    val mutable last_refresh_time = 0.0
    val reset = new reset()
    val changes = new changes()
    method get = outline
    method is_valid = buffer#last_edit_time < last_refresh_time || timer_id = None
    method private update ?(force=false) () =
      (*Log.println `DEBUG "UPDATE is_valid=%b force=%b" self#is_valid force;*)
      if not self#is_valid || force then begin
        let source_code = buffer#get_text () in
        last_refresh_time <- Unix.gettimeofday();
        (merlin source_code)@@Merlin.outline
        |> Async.start_with_continuation ~name:__FUNCTION__ begin function
        | Merlin.Ok (ol : Merlin_t.outline list) ->
            (* Extract comments from source and convert to outline entries *)
            let comments =
              let open Location in
              Lex.comments source_code
              |> List.map begin fun (c, loc) ->
                let _, start_ln, start_cn = Location.get_pos_info loc.loc_start in
                let _, stop_ln, stop_cn = Location.get_pos_info loc.loc_end in
                let ol_start = { line = start_ln; col = start_cn } in
                let ol_stop = { line = stop_ln; col = stop_cn } in
                {
                  ol_kind = "Comment";
                  ol_name = "comment";
                  ol_start;
                  ol_stop;
                  ol_selection = { start = ol_start; stop = ol_stop };
                  ol_type = None;
                  ol_deprecated = false;
                  ol_parent = None;
                  ol_children = [];
                  ol_level = 0;
                }
              end
            in
            if force then begin
              (*outline <- [];*)
              last_refresh_time <- 0.0;
              GtkThread.sync reset#call ();
            end;
            let ol = List.rev_append comments ol in
            let diff = compare_outlines (if force then [] else outline) ol in
            if diff.changed <> [] ||  diff.added <> [] || diff.removed <> [] then begin
              Log.println `DEBUG "%a" (fun oc diff ->
                  output_string oc
                    (sprintf "-------> DIFF (%d, %d, %d), %b, force=%b"
                       (List.length diff.added) (List.length diff.removed) (List.length diff.changed) self#is_valid force))
                diff;
              outline <- ol;
              GtkThread.async changes#call diff;
            end;
        | Merlin.Failure _ | Merlin.Error _ -> ()
        end
      end

    method refresh () = self#update ~force:true ()

    method start_monitor () =
      match timer_id with
      | None ->
          timer_id <- Some (Gmisclib.Timeout.add __FUNCTION__ ~ms:100 ~callback:(fun () -> self#update(); true));
      | _ -> ()

    method stop_monitor () =
      begin
        match timer_id with
        | None -> ()
        | Some id ->
            timer_id <- None;
            last_refresh_time <- 0.0;
            Gmisclib.Timeout.remove id
      end;

    method connect = new outline_signals ~reset ~changes

  end

and reset () = object inherit [unit] signal () end
and changes () = object inherit [Outline_diff.t] signal () end
and outline_signals ~reset ~changes =
  object
    inherit ml_signals [reset#disconnect; changes#disconnect]
    method reset = reset#connect ~after
    method changes = changes#connect ~after
  end

let cols               = new GTree.column_list
let col_markup         = cols#add Gobject.Data.string
let col_data           : Merlin_t.outline GTree.column = cols#add Gobject.Data.caml
let col_name           = cols#add Gobject.Data.string
let col_kind           = cols#add Gobject.Data.string
let col_line           = cols#add Gobject.Data.int
let col_type           = cols#add Gobject.Data.string_option
let col_node_path : node_path GTree.column = cols#add Gobject.Data.caml

class view ~(outline : Oe.outline) ~(source_view : Ocaml_text.view) ?packing () =
  let pref                   = Preferences.preferences#get in
  let show_types             = pref.outline_show_types in
  let vbox                   = GPack.vbox ?packing () in
  let model                  = GTree.tree_store cols in
  let iter_table  = PathHashtbl.create 256 in
  let toolbar                = GButton.toolbar
      ~orientation:`HORIZONTAL ~style:`TEXT ~packing:(vbox#pack ~expand:false ~fill:false) () in
  let sw                     = GBin.scrolled_window
      ~shadow_type:`NONE ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:vbox#add () in
  let view                   = GTree.view ~model ~headers_visible:false
      ~enable_search:true ~search_column:2 ~tooltip_column:col_type.GTree.index
      ~packing:sw#add ()
  in
  let renderer_markup        = GTree.cell_renderer_text [`YPAD 0] in
  let vc                     = GTree.view_column ~renderer:(renderer_markup, ["markup", col_markup]) () in
  let _                      = vc#set_sizing `FIXED in
  let _                      = vc#set_fixed_width 50 in
  let _                      = view#append_column vc in
  let _                      = view#selection#set_mode `SINGLE in
  let _                      = view#misc#set_property "enable-tree-lines" (`BOOL true) in
  let _                      = model#set_sort_column_id col_line.GTree.index `ASCENDING in
  let find_ref (path : node_path) =
    let rref = PathHashtbl.find_opt iter_table path in
    if rref <> None && not (Option.get rref)#valid then
      Log.println `ERROR "%s: row_reference is not valid." __FUNCTION__;
    rref
  in
  let register_ref path rref = PathHashtbl.replace iter_table path rref in
  let unregister_ref (path : node_path) = PathHashtbl.remove iter_table path in
  let clear_ref_table () = PathHashtbl.clear iter_table in
  let print_ref_table () =
    PathHashtbl.iter (fun node_path _ -> Printf.printf "%s\n%!" (string_of_path node_path)) iter_table
  in
  let make_new_path (old_path : node_path) (new_node : outline) : node_path =
    match List.rev old_path with
    | [] -> []
    | last_id :: rest_rev ->
        let parent_path = List.rev rest_rev in
        let new_id = {
          name = new_node.ol_name;
          kind = new_node.ol_kind;
          occurrence = last_id.occurrence; (* keep occurrrence *)
        } in
        parent_path @ [new_id]
  in
  let children_with_node_ids (children : outline list) =
    let module KeyMap = Map.Make(struct
        type t = string * string
        let compare = compare
      end) in
    let _, indexed =
      List.fold_left (fun (counts, acc) o ->
          let key = (o.ol_name, o.ol_kind) in
          let count = Option.value (KeyMap.find_opt key counts) ~default:0 in
          let id = { name = o.ol_name; kind = o.ol_kind; occurrence = count } in
          let new_counts = KeyMap.add key (count + 1) counts in
          (new_counts, (id, o) :: acc)
        ) (KeyMap.empty, []) children
    in
    List.rev indexed
  in
  object (self)
    inherit GObj.widget vbox#as_widget
    val mutable code_font_family = ""
    val mutable sig_cursor_changed = None
    val mutable timer_follow_cursor = None
    val mutable nodes_expanded = []
    val view = view
    val model = model
    val vc = vc
    val buffer = source_view#obuffer
    val tool_refresh = GButton.tool_button ~packing:toolbar#insert ()
    val tool_show_nested_defs = GButton.toggle_tool_button ~active:Preferences.preferences#get.outline_show_nested_defs ~packing:toolbar#insert ()
    val tool_sort_name = GButton.toggle_tool_button ~packing:toolbar#insert ()
    val tool_sort_kind = GButton.toggle_tool_button ~packing:toolbar#insert ()
    val tool_collapse_all = GButton.tool_button ~packing:toolbar#insert ()
    val tool_goto_cursor_position = GButton.tool_button ~packing:toolbar#insert ()
    val tool_follow_cursor = GButton.toggle_tool_button ~active:true ~packing:toolbar#insert ()

    initializer
      self#update_preferences();
      Preferences.preferences#connect#changed ~callback:(fun _ -> self#update_preferences ()) |> ignore;
      self#toolbar_setup();
      self#set_follow_cursor tool_follow_cursor#get_active;

      view#connect#row_activated ~callback:begin fun _ _ ->
        self#jump_to_definition();
        source_view#misc#grab_focus()
      end |> ignore;

      sig_cursor_changed <- Some (view#connect#cursor_changed ~callback:self#jump_to_definition);

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
      (* Outline events *)
      let sig_reset = outline#connect#reset ~callback:self#reset in
      let sig_changes = outline#connect#changes ~callback:self#apply_diff in
      (* Handle row expansion: lazy-load children *)
      view#connect#row_expanded ~callback:begin fun row path ->
        try
          let node_path = model#get ~row ~column:col_node_path in
          nodes_expanded <- node_path :: nodes_expanded;
          self#insert_childs row path
        with Gpointer.Null as ex ->
          Printf.eprintf "File \"outline.ml\": **** %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());
      end |> ignore;
      (* Track collapsed rows to preserve state *)
      view#connect#row_collapsed ~callback:begin fun row _ ->
        let node_path = model#get ~row ~column:col_node_path in
        nodes_expanded <- nodes_expanded |> List.filter ((<>) node_path);
      end |> ignore;
      (* Focus *)
      source_view#event#connect#focus_in ~callback:(fun _ ->
          outline#start_monitor();
          false) |> ignore;
      source_view#event#connect#focus_out ~callback:(fun _ ->
          outline#stop_monitor();
          false) |> ignore;
      let sig_focus_in =
        source_view#event#connect#focus_in ~callback:(fun _ ->
            self#set_follow_cursor tool_follow_cursor#get_active;
            false) in
      let sig_focus_out =
        source_view#event#connect#focus_out ~callback:(fun _ ->
            self#set_follow_cursor false;
            false) in
      (* Cleanup on destroy *)
      view#misc#connect#destroy ~callback:begin fun _ ->
        Log.println `DEBUG "DESTROY";
        outline#connect#disconnect sig_reset;
        outline#connect#disconnect sig_changes;
        GtkSignal.disconnect source_view#as_view sig_focus_in;
        GtkSignal.disconnect source_view#as_view sig_focus_out;
        self#set_follow_cursor false;
      end |> ignore;

    method outline = outline

    method refresh = outline#refresh

    method private fold_depth_first f parent ol acc =
      match ol with
      | [] -> acc
      | hd :: tl ->
          let acc = f parent hd acc in
          let acc = self#fold_depth_first f (hd :: parent) hd.ol_children acc in
          let acc = self#fold_depth_first f parent tl acc in
          acc

    method private get_iter_at_line pos =
      let ln = pos.line - 1 in
      if pos.line < 0 || ln > buffer#end_iter#line then raise (Invalid_linechar pos);
      buffer#get_iter (`LINE ln)

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
              let node = model#get ~row ~column:col_data in
              let start = self#get_iter_at_linechar node.ol_selection.start in
              let stop = self#get_iter_at_linechar node.ol_selection.stop in
              buffer#select_range start stop;
              source_view#scroll_aligned start;
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
              let node = model#get ~row ~column:col_data in
              let start = self#get_iter_at_linechar node.ol_start in
              let stop = self#get_iter_at_linechar node.ol_stop in
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
      if self#visible then begin
        let iter = buffer#get_iter_at_mark (`MARK mark) in
        let ln = iter#line + 1 in
        let cn = iter#line_offset + 1 in
        let found_paths = ref [] in
        (* Find all outline items containing the cursor *)
        model#foreach begin fun path row ->
          try
            let node = model#get ~row ~column:col_data in
            if node.ol_kind <> "Placeholder" then begin
              let start, stop =
                if node.ol_kind = "Method" then
                  (self#get_iter_at_linechar node.ol_start)#set_line_offset 0,
                  (self#get_iter_at_linechar node.ol_stop)#set_line_offset 0
                else
                  self#get_iter_at_linechar node.ol_start,
                  let it = self#get_iter_at_linechar node.ol_stop in
                  if it#ends_line then it#forward_char else it#forward_to_line_end
              in
              if iter#in_range ~start ~stop then
                found_paths := (path, node.ol_stop.line - node.ol_start.line) :: !found_paths;
            end;
            false
          with Invalid_linechar pos as ex ->
            (* Outline is not yet up-to-date with the buffer: ignore the exception *)
            (*Printf.eprintf "%s: %s - %s (%d,%d)\n%s\n%s\n%!" (timestamp()) (string_of_path node_path)
              (Printexc.to_string ex) pos.line pos.col __LOC__ (Printexc.get_backtrace());*)
            false
        end;
        match !found_paths with
        | [] -> ()
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
                  (* TODO: Lablgtk3 issue, `get_flag `REALIZED *)
                  let is_realized =
                    try view#misc#window |> ignore; true with Gpointer.Null -> false
                  in
                  if is_realized && not (Gmisclib.Util.treeview_is_path_onscreen view path) then
                    Gmisclib.Idle.add ~prio:300 (fun () ->
                        view#scroll_to_cell ~align:(0.38, 0.) path vc);
            end
          end
      end

    method private set_follow_cursor active =
      tool_goto_cursor_position#misc#set_sensitive (not active);
      if active then begin
        if timer_follow_cursor = None then
          timer_follow_cursor <- Some begin
              let name = sprintf "timer_follow_cursor-%s" source_view#obuffer#filename in
              Gmisclib.Timeout.add name ~ms:1000 ~callback:begin fun () ->
                let mark = buffer#get_mark `INSERT in
                if timer_follow_cursor <> None then begin
                  Option.iter view#misc#handler_block sig_cursor_changed;
                  self#goto_cursor_position mark;
                  Option.iter view#misc#handler_unblock sig_cursor_changed;
                end;
                true
              end
            end;
      end else begin
        Option.iter begin fun id ->
          timer_follow_cursor <- None;
          Gmisclib.Timeout.remove id;
        end timer_follow_cursor
      end

    method private create_markup node =
      (Markup.icon_of_kind node.ol_kind) ^ "   " ^
      (Glib.Markup.escape_text node.ol_name) ^ "" ^
      (match node.ol_type with
       | Some typ ->
           let sep = if String.length typ <= 50 then ": " else "\n\t" in
           let flatten = String.length typ >= 100 in
           "<span size='x-small' alpha='62%'> " ^ sep ^
           (if flatten then Markup.type_info typ |> String.replace_all ~sub:"\n" ~by:" "
            else Markup.type_info typ |> String.replace_all ~sub:"\n" ~by:"\n\t") ^
           "</span><span alpha='100%'> </span>" (* Fix for GTK/Cairo *)
       | _ -> "")

    method private reset () =
      Log.println `DEBUG "***** RESET BEGIN *****";
      view#set_model None;
      model#clear();
      clear_ref_table ();
      view#set_model (Some model#coerce);
      Log.println `DEBUG "***** RESET END   *****";

    method private insert_node ?parent ?pos child_path node =
      if node.ol_kind <> "Comment" then begin
        Option.iter view#misc#handler_block sig_cursor_changed;
        let row =
          match pos with
          | Some pos -> model#insert ?parent pos
          | _ -> model#append ?parent ()
        in
        register_ref child_path (model#get_row_reference (model#get_path row));
        model#set ~row ~column:col_data node;
        model#set ~row ~column:col_line node.ol_start.line;
        model#set ~row ~column:col_kind node.ol_kind;
        model#set ~row ~column:col_name node.ol_name;
        model#set ~row ~column:col_type (Option.map Glib.Markup.escape_text node.ol_type);
        model#set ~row ~column:col_node_path child_path;
        let markup = self#create_markup node in
        model#set ~row ~column:col_markup markup;
        Option.iter view#misc#handler_unblock sig_cursor_changed;
        if
          node.ol_children <> [] &&
          (tool_show_nested_defs#get_active ||
           node.ol_kind <> "Method" &&
           (node.ol_kind <> "Value" || node.ol_children |> List.for_all (fun c -> c.ol_kind <> "Value")))
        then self#insert_placeholder row
      end

    method private insert_placeholder row =
      Option.iter view#misc#handler_block sig_cursor_changed;
      let placeholder = model#append ~parent:row () in
      let pos_0 = { line = 0; col = 0 } in
      let id = {
        name = "placeholder";
        kind = "plachehoder";
        occurrence = 0;
      } in
      model#set ~row:placeholder ~column:col_node_path [id];
      model#set ~row:placeholder ~column:col_markup "";
      model#set ~row:placeholder ~column:col_data {
        ol_kind = "Placeholder";
        ol_name = "";
        ol_start = pos_0;
        ol_stop = pos_0;
        ol_selection = { start = pos_0; stop = pos_0 };
        ol_type = None;
        ol_deprecated = false;
        ol_level = 0;
        ol_parent = None;
        ol_children = []
      };
      Option.iter view#misc#handler_unblock sig_cursor_changed;

    method private insert_childs row path =
      let parent_tree_path = GTree.Path.copy path in
      GTree.Path.down path;
      let first_child = model#get_iter path in
      let first_child_data = model#get ~row:first_child ~column:col_data in
      if first_child_data.ol_kind = "Placeholder" then begin
        model#remove first_child |> ignore;
        let parent_node = model#get ~row ~column:col_data in
        let parent_node_path = model#get ~row ~column:col_node_path in
        parent_node.ol_children
        |> children_with_node_ids
        |> List.iter (fun (id, child_node) ->
            let child_path = parent_node_path @ [id] in
            self#insert_node ~parent:row child_path child_node);
        view#expand_row parent_tree_path;
      end

    method private apply_diff { Outline_diff.added; removed; changed } =
      self#set_follow_cursor false;
      changed |> List.iter begin fun (old_path, _, node2) ->
        let new_path = make_new_path old_path node2 in
        (*Log.println `DEBUG "CHANGED: %s -> %s (%d, %d)"
          (string_of_path old_path) (string_of_path new_path)
          node2.ol_start.line node2.ol_start.col;*)
        match find_ref old_path with
        | Some rref ->
            let row = rref#iter in
            model#set ~row ~column:col_line node2.ol_start.line;
            model#set ~row ~column:col_kind node2.ol_kind;
            model#set ~row ~column:col_name node2.ol_name;
            model#set ~row ~column:col_type node2.ol_type;
            model#set ~row ~column:col_markup (self#create_markup node2);
            model#set ~row ~column:col_data node2;
            if old_path <> new_path then begin
              model#set ~row ~column:col_node_path new_path;
              unregister_ref old_path;
              register_ref new_path rref;
              let is_expanded = nodes_expanded |> List.exists ((=) old_path) in
              if is_expanded then nodes_expanded <- new_path :: List.filter ((<>) old_path) nodes_expanded
            end
        | _ -> ()
      end;
      removed |> List.iter begin fun (node_path, _, _) ->
        (*Log.println `DEBUG "REMOVED: %s" (string_of_path node_path);*)
        match find_ref node_path with
        | Some rref ->
            Option.iter view#misc#handler_block sig_cursor_changed;
            model#remove rref#iter |> ignore;
            Option.iter view#misc#handler_unblock sig_cursor_changed;
            unregister_ref node_path;
            nodes_expanded <- List.filter ((<>) node_path) nodes_expanded
        | _ -> ()
        (*Log.println `ERROR "node_path %s not found. Remove failed." (string_of_path node_path);*)
      end;
      added |> List.iter begin fun (node_path, node, pos) ->
        (*Log.println `DEBUG "ADDED  : %s" (string_of_path node_path);*)
        let parent_path = match List.rev node_path with _ :: rest -> List.rev rest | [] -> [] in
        let parent_rref = if parent_path = [] then None else find_ref parent_path in
        if parent_path = [] || Option.is_some parent_rref then
          self#insert_node ?parent:(Option.map (fun r -> r#iter) parent_rref) ~pos node_path node
      end;
      Gmisclib.Idle.add ~prio:300 (fun () ->
          self#set_follow_cursor tool_follow_cursor#get_active);

    method private handle_sort_buttons () =
      let sig_sort_name = ref None in
      let sig_sort_kind = ref None in
      let set_sort_column () =
        let open GTree in
        if tool_sort_name#get_active then
          model#set_sort_column_id col_name.index `ASCENDING
        else if tool_sort_kind#get_active then
          model#set_sort_column_id col_kind.index `ASCENDING
        else
          model#set_sort_column_id col_line.index `ASCENDING;
        model#sort_column_changed();
      in
      sig_sort_name :=
        Some (tool_sort_name#connect#clicked ~callback:begin fun () ->
            Option.iter tool_sort_kind#misc#handler_block !sig_sort_kind;
            tool_sort_kind#set_active false;
            Option.iter tool_sort_kind#misc#handler_unblock !sig_sort_kind;
            set_sort_column ();
          end);
      sig_sort_kind :=
        Some (tool_sort_kind#connect#clicked ~callback:begin fun () ->
            Option.iter tool_sort_name#misc#handler_block !sig_sort_name;
            tool_sort_name#set_active false;
            Option.iter tool_sort_name#misc#handler_unblock !sig_sort_name;
            set_sort_column ();
          end);

      tool_show_nested_defs#connect#clicked ~callback:begin fun () ->
        Async.create ~name:"tool_show_nested_defs" begin fun () ->
          let pref = Preferences.preferences#get in
          pref.outline_show_nested_defs <- tool_show_nested_defs#get_active;
          Preferences.preferences#set pref;
          Preferences.save ();
          Printf.printf "Preferences.preferences#get.outline_show_nested_defs = %b\n%!"
            Preferences.preferences#get.outline_show_nested_defs;
        end
        |> Async.start;
        self#refresh ()
      end |> ignore;

    method private toolbar_setup () =
      (* Toolbar style *)
      tool_refresh#misc#style_context#add_class "outline-button";
      tool_sort_name#misc#style_context#add_class "outline-button";
      tool_collapse_all#misc#style_context#add_class "outline-button";
      tool_sort_kind#misc#style_context#add_class "outline-button";
      tool_show_nested_defs#misc#style_context#add_class "outline-button";
      tool_goto_cursor_position#misc#style_context#add_class "outline-button";
      tool_follow_cursor#misc#style_context#add_class "outline-button";
      (* Toolbar icons *)
      let mk_icon = Gtk_util.label_icon ~width:25 ~height:1 ~font_size:"medium" in
      tool_refresh#set_label_widget (mk_icon "\u{f0453}")#coerce;
      tool_collapse_all#set_label_widget (mk_icon "\u{f102}")#coerce;
      tool_show_nested_defs#set_label_widget (mk_icon "\u{e681}")#coerce;
      tool_goto_cursor_position#set_label_widget (mk_icon "\u{ea9b}")#coerce;
      tool_follow_cursor#set_label_widget (mk_icon "\u{ebcb}")#coerce;
      tool_sort_name#set_label_widget (mk_icon "\u{f05bd}")#coerce;
      tool_sort_kind#set_label_widget (mk_icon "\u{f1385}")#coerce;
      (* Toolbar tooltips *)
      tool_refresh#misc#set_tooltip_text "Refresh";
      tool_collapse_all#misc#set_tooltip_text "Collapse all";
      tool_show_nested_defs#misc#set_tooltip_text "Show Nested Definitions";
      tool_goto_cursor_position#misc#set_tooltip_text "Go to cursor position";
      tool_follow_cursor#misc#set_tooltip_text "Follow cursor";
      tool_sort_name#misc#set_tooltip_text "Sort by name";
      tool_sort_kind#misc#set_tooltip_text "Sort by kind";
      (* Toolbar callbacks *)
      self#handle_sort_buttons();
      tool_show_nested_defs#set_active Preferences.preferences#get.outline_show_nested_defs;
      tool_follow_cursor#connect#clicked ~callback:(fun () ->
          self#set_follow_cursor tool_follow_cursor#get_active) |> ignore;
      tool_goto_cursor_position#connect#clicked ~callback:begin fun () ->
        self#goto_cursor_position (buffer#get_mark `INSERT)
      end |> ignore;
      (*Collapse all with smart re-activation of cursor following *)
      tool_collapse_all#connect#clicked ~callback:begin fun () ->
        if tool_follow_cursor#get_active then begin
          self#set_follow_cursor false;
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
      tool_refresh#connect#clicked ~callback:self#refresh |> ignore;

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
      let base_font = pref.editor_base_font in
      code_font_family <-
        String.sub base_font 0 (Option.value (String.rindex_opt base_font ' ') ~default:(String.length base_font));

  end
