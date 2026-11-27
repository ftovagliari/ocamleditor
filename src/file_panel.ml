(** Project file tree that shows only the source files.

    The tree starts at the project root. Directories are read when they are expanded; build output,
    version control metadata, backups and caches are never listed, and only the files that are worth
    editing are shown (OCaml sources, dune files, the project file, documentation...). *)

open Printf

let (//) = Filename.concat

(* Directories that are never worth showing. Hidden directories (".git", ".build", ".tmp"...) are skipped too. *)
let ignored_dirs = ["_build"; "_opam"; "_esy"; "node_modules"; "bak"; "tmp"]

let is_ignored_dir name =
  (String.length name > 0 && name.[0] = '.') || List.mem name ignored_dirs

let source_suffixes = [".ml"; ".mli"; ".mll"; ".mly"; ".mld"; ".atd"]
let config_suffixes = [".project.json"; ".opam"]
let document_suffixes = [".md"; ".txt"; ".sh"]
let config_names = ["dune"; "dune-project"; "dune-workspace"; "Makefile"; "META"]
let document_names = ["README"; "CHANGELOG"; "LICENSE"; "COPYING"; "INSTALL"; "TODO"; "AUTHORS"]

type kind = Source | Config | Document

let kind_of_file name =
  let has suffixes = List.exists (Filename.check_suffix name) suffixes in
  if has source_suffixes then Some Source
  else if has config_suffixes || List.mem name config_names then Some Config
  else if has document_suffixes || List.mem name document_names then Some Document
  else None

let icon_of_file name =
  match kind_of_file name with
  | Some Source -> Icon_set.file_tree_source
  | Some Config -> Icon_set.file_tree_config
  | Some Document | None -> Icon_set.file_tree_other

let icon_of_dir ~expanded = if expanded then Icon_set.file_tree_folder_open else Icon_set.file_tree_folder

let cols = new GTree.column_list
let col_icon = cols#add Gobject.Data.string
let col_name = cols#add Gobject.Data.string
let col_path = cols#add Gobject.Data.string
let col_dir = cols#add Gobject.Data.boolean
let col_fake = cols#add Gobject.Data.boolean (* placeholder child that makes a directory expandable *)

(** The visible entries of a directory as (name, full path, is_directory): directories first, then
    files, each group sorted without regard to case. *)
let entries dir =
  let names = try Array.to_list (Sys.readdir dir) with Sys_error _ -> [] in
  let items =
    List.filter_map begin fun name ->
      let path = dir // name in
      if (try Sys.is_directory path with Sys_error _ -> false) then
        (if is_ignored_dir name then None else Some (name, path, true))
      else if kind_of_file name <> None then Some (name, path, false)
      else None
    end names
  in
  let order (n1, _, d1) (n2, _, d2) =
    match d1, d2 with
    | true, false -> -1
    | false, true -> 1
    | _ -> compare (String.lowercase_ascii n1) (String.lowercase_ascii n2)
  in
  List.sort order items

class widget ~(editor : Editor.editor) ?packing () =
  let box = GPack.vbox ?packing () in
  let toolbar = GButton.toolbar ~style:`ICONS ~orientation:`HORIZONTAL ~packing:box#pack () in
  let _ = toolbar#set_icon_size `MENU in
  let button_refresh = GButton.tool_button ~packing:toolbar#insert () in
  let _ = button_refresh#set_label_widget (Gtk_util.label_icon Icon_set.refresh)#coerce in
  let _ = button_refresh#misc#set_tooltip_text "Refresh" in
  let button_collapse = GButton.tool_button ~packing:toolbar#insert () in
  let _ = button_collapse#set_label_widget (Gtk_util.label_icon Icon_set.outline_collapse)#coerce in
  let _ = button_collapse#misc#set_tooltip_text "Collapse All" in
  let sw = GBin.scrolled_window ~shadow_type:`NONE ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:box#add () in
  let model = GTree.tree_store cols in
  let view = GTree.view ~model ~headers_visible:false ~enable_search:true ~packing:sw#add () in
  let renderer_icon = Gtk_util.cell_renderer_icon ~xpad:2 () in
  let renderer_name = GTree.cell_renderer_text [] in
  let vc = GTree.view_column () in
  let _ = vc#pack ~expand:false renderer_icon in
  let _ = vc#pack ~expand:true renderer_name in
  let _ = vc#add_attribute renderer_icon "text" col_icon in
  let _ = vc#add_attribute renderer_name "text" col_name in
  let _ = view#append_column vc in
  let _ = view#set_search_column col_name.GTree.index in
  object (self)
    inherit GObj.widget box#as_widget

    val mutable root : string option = None
    (* Expanded directories, to restore them when the tree is rebuilt. *)
    val expanded : (string, unit) Hashtbl.t = Hashtbl.create 17
    val mutable rebuilding = false
    val mutable signature = ""

    method private add_row ?parent (name, path, is_dir) =
      let row = model#append ?parent () in
      model#set ~row ~column:col_name name;
      model#set ~row ~column:col_path path;
      model#set ~row ~column:col_dir is_dir;
      model#set ~row ~column:col_fake false;
      model#set ~row ~column:col_icon (if is_dir then icon_of_dir ~expanded:false else icon_of_file name);
      if is_dir then begin
        let fake = model#append ~parent:row () in
        model#set ~row:fake ~column:col_fake true;
        model#set ~row:fake ~column:col_name "";
        model#set ~row:fake ~column:col_path "";
        model#set ~row:fake ~column:col_dir false;
        model#set ~row:fake ~column:col_icon ""
      end;
      row

    method private add_entries ?parent dir =
      List.iter begin fun ((_, path, is_dir) as entry) ->
        let row = self#add_row ?parent entry in
        if is_dir && Hashtbl.mem expanded path then ignore (view#expand_row (model#get_path row))
      end (entries dir)

    method private signature () =
      match root with
      | None -> ""
      | Some r ->
          let dirs = r :: List.sort compare (Hashtbl.fold (fun d () acc -> d :: acc) expanded []) in
          let mtime d = try (Unix.stat d).Unix.st_mtime with Unix.Unix_error _ -> 0.0 in
          String.concat ";" (List.map (fun d -> sprintf "%s:%.3f" d (mtime d)) dirs)

    method private rebuild () =
      rebuilding <- true;
      Fun.protect ~finally:(fun () -> rebuilding <- false) begin fun () ->
        model#clear ();
        Option.iter (fun dir -> self#add_entries dir) root
      end;
      signature <- self#signature ()

    (** Sets the directory shown by the tree (the project root), or empties it. *)
    method set_root dir =
      root <- dir;
      Hashtbl.reset expanded;
      self#rebuild ()

    (** Reads the expanded directories again, keeping the scroll position. *)
    method refresh () =
      let adjustment = sw#vadjustment in
      let value = adjustment#value in
      self#rebuild ();
      Gmisclib.Idle.add ~prio:300 (fun () -> adjustment#set_value value)

    (** Like [refresh], but only if a directory that is shown has changed on disk. *)
    method refresh_if_changed () =
      if root <> None && self#signature () <> signature then self#refresh ()

    (** Selects the row of [filename], expanding the directories that lead to it. *)
    method reveal filename =
      Option.iter begin fun r ->
        Option.iter begin fun rel ->
          let rec go parent = function
            | [] -> ()
            | name :: rest ->
                let n = model#iter_n_children parent in
                let rec find i =
                  if i >= n then None
                  else begin
                    let child = model#iter_children ~nth:i parent in
                    if (not (model#get ~row:child ~column:col_fake)) && model#get ~row:child ~column:col_name = name
                    then Some child else find (i + 1)
                  end
                in
                Option.iter begin fun child ->
                  let path = model#get_path child in
                  if rest = [] then begin
                    view#selection#select_path path;
                    view#scroll_to_cell path vc
                  end else begin
                    ignore (view#expand_row path);
                    go (Some child) rest
                  end
                end (find 0)
          in
          go None (Utils.filename_split rel)
        end (Utils.filename_relative r filename)
      end root

    initializer
      ignore (view#connect#row_expanded ~callback:begin fun row _ ->
          let dir = model#get ~row ~column:col_path in
          if not rebuilding then Hashtbl.replace expanded dir ();
          model#set ~row ~column:col_icon (icon_of_dir ~expanded:true);
          (* The entries are added before the previous children (at the first expansion: the placeholder)
             are removed: GTK collapses at once a row that is left without children. *)
          let previous = model#iter_n_children (Some row) in
          self#add_entries ~parent:row dir;
          for _ = 1 to previous do
            ignore (model#remove (model#iter_children ~nth:0 (Some row)))
          done
        end);
      ignore (view#connect#row_collapsed ~callback:begin fun row _ ->
          let dir = model#get ~row ~column:col_path in
          if not rebuilding then Hashtbl.remove expanded dir;
          model#set ~row ~column:col_icon (icon_of_dir ~expanded:false)
        end);
      ignore (view#connect#row_activated ~callback:begin fun path _ ->
          let row = model#get_iter path in
          let filename = model#get ~row ~column:col_path in
          if model#get ~row ~column:col_dir then begin
            if view#row_expanded path then ignore (view#collapse_row path) else ignore (view#expand_row path)
          end else ignore (editor#open_file ~active:true ~offset:0 ?remote:None filename)
        end);
      ignore (button_refresh#connect#clicked ~callback:self#refresh);
      ignore (button_collapse#connect#clicked ~callback:(fun () -> view#collapse_all ()))
  end
