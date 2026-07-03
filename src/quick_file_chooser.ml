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
[@@@warning "-48"]

open Printf
open Utils
open Preferences
open Fuzzy_search

type t = {
  source                : [ `path of string list * string list (* roots x filenames *)
                          | `filelist of string list (* filenames *)];
  model                 : GTree.list_store;
  mutable index_icons   : Gtk.tree_path list;
  filenames             : (string, Gtk.tree_path) Hashtbl.t; (* filename x true *)
  rtimes                : (string, float) Hashtbl.t;         (* dirname x last read *)
  mutable length        : int;
}

let limit = 100_000

let models : (string * t) list ref = ref []

let re_dot = !~~ ".";;
let re_question = !~~ "?";;
let re_asterisk = !~~ "*";;
let re_wildcards = !~ "[?*]";;

let crono_sum time f x =
  let finally t =
    time := !time +. (Unix.gettimeofday() -. t);
  in
  let t = Unix.gettimeofday() in
  let result = try f x with e -> begin
      finally t;
      raise e
    end in
  finally t;
  result;;

let cols         = new GTree.column_list
let col_pixbuf   = cols#add (Gobject.Data.gobject_option : (GdkPixbuf.pixbuf option) Gobject.data_conv)
let col_name     = cols#add Gobject.Data.string
let col_path     = cols#add Gobject.Data.string
let col_visible  = cols#add Gobject.Data.boolean
let col_score    = cols#add Gobject.Data.float

(** create_model *)
let create_model ~source ~name =
  let model       = GTree.list_store cols in
  let model       = {
    source        = source;
    model         = model;
    index_icons   = [];
    filenames     = Hashtbl.create 17;
    rtimes        = Hashtbl.create 7;
    length        = 0;
  } in
  begin
    match source with
    | `path _ -> models := (name, model) :: !models;
    | _ -> ()
  end;
  model;;

let regexp_of_pattern pattern =
  let pattern = (Str.global_replace re_dot "\\." pattern) in
  let pattern = (Str.global_replace re_question "." pattern) in
  let pattern = (Str.global_replace re_asterisk "[-+ 'a-zA-Z!^%&$()@#;.,_0-9]*" pattern) in
  Utils.regexp_case_fold pattern;;

(** widget *)
class widget ~source ~name ?filter ?packing () =
  let ebox              = GBin.event_box ?packing () in
  let _                 = ebox#misc#set_property "visible-window" (`BOOL false) in
  let vbox              = GPack.vbox ~spacing:5 ~packing:ebox#add () in
  let box               = GPack.hbox ~spacing:8 ~packing:vbox#pack () in
  let entry             = GEdit.entry ~packing:box#add () in
  let icon_progress     = GMisc.image ~width:16 ~file:(Icon.get_themed_filename "spinner_16.gif") ~packing:box#pack () in
  let label_count       = GMisc.label ~packing:box#pack () in
  let model, filelist =
    match source with
    | `path (roots, filelist) ->
        (try List.assoc name !models with Not_found -> create_model ~source ~name), filelist
    | `filelist filelist -> (create_model ~source ~name), filelist
  in
  let model_sort        = GTree.model_sort model.model in
  let model_filter      = GTree.model_filter model_sort in
  let _                 = model_filter#set_visible_column col_visible in
  (* View *)
  let renderer          = GTree.cell_renderer_text [] in
  let renderer_basename = GTree.cell_renderer_text [] in
  let renderer_score    = GTree.cell_renderer_text [] in
  let renderer_pixbuf   = GTree.cell_renderer_pixbuf [] in
  let _                 = renderer_basename#set_properties [] in
  let vc_pixbuf         = GTree.view_column ~title:"" ~renderer:(renderer_pixbuf, ["pixbuf", col_pixbuf]) () in
  let vc_score          = GTree.view_column ~title:"Score" ~renderer:(renderer_score, ["text", col_score]) () in
  let vc_name           = GTree.view_column ~title:"File" ~renderer:(renderer_basename, ["text", col_name]) () in
  let vc_path           = GTree.view_column ~title:"Path" ~renderer:(renderer, ["text", col_path]) () in
  let sw                = GBin.scrolled_window ~shadow_type:`IN ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
      ~packing:vbox#add () in
  let view              = GTree.view ~headers_visible:false ~reorderable:true ~width:130
      ~enable_search:false ~packing:sw#add () in
  let _                 = view#append_column vc_pixbuf in
  let _                 = view#append_column vc_score in
  let _                 = view#append_column vc_name in
  let _                 = view#append_column vc_path in
  let _                 = view#selection#set_mode `MULTIPLE in
  let _                 = view#set_model (Some model_filter#coerce) in
  let _                 = model_sort#set_sort_column_id col_score.GTree.index `DESCENDING in
  let _                 = vc_score#set_visible false in
  object (self)
    inherit GObj.widget ebox#as_widget
    val is_filelist = match source with `filelist _ -> true | _ -> false
    val mutable visible_paths = []
    val mutable choose_func = fun ~filename ~has_cursor -> `ignore
    val mutable queue = Queue.create ()

    initializer self#init()

    method source = source
    method model = model.model
    method view = view
    method renderer = renderer_basename
    method select_path path =
      path |> model_sort#convert_child_path_to_path |> model_filter#convert_child_path_to_path |> view#selection#select_path

    method set_cursor path =
      let path = path |> model_sort#convert_child_path_to_path |> model_filter#convert_child_path_to_path in
      view#set_cursor path vc_name;

    method get_paths_with_icon () = model.index_icons

    method get_path ~filename =
      try Some (Hashtbl.find model.filenames filename) with Not_found -> None

    method get_basename ~path =
      let row = model.model#get_iter path in
      row, model.model#get ~row ~column:col_name

    method get_dirname ~row = model.model#get ~row ~column:col_path

    method set_default_choose_func f = choose_func <- f

    method set_cell_data_func = vc_name#set_cell_data_func renderer_basename

    method set_icon ~row pixbuf =
      model.model#set ~row ~column:col_pixbuf pixbuf;
      let path = model.model#get_path row in
      match pixbuf with
      | None -> model.index_icons <- List.filter (fun x -> x <> path) model.index_icons
      | _ -> model.index_icons <- path :: model.index_icons

    method reset_icons () =
      List.iter begin fun path ->
        let row = model.model#get_iter path in
        model.model#set ~row ~column:col_pixbuf None
      end model.index_icons;
      model.index_icons <- []

    method update_model ?blocking () =
      let scan () =
        self#scan ?blocking ();
        view#selection#select_path (GTree.Path.create [0]);
      in
      match blocking with
      | Some true -> (*crono ~label:"scan"*) scan ();
      | _ -> Gmisclib.Idle.add ~prio:300 scan

    method activate :
      ?f:(filename:string -> has_cursor:bool -> [`clear | `set of GdkPixbuf.pixbuf | `ignore]) ->
      unit -> unit = fun ?f () ->
      let path_cursor, _ = view#get_cursor () in
      let has_cursor =
        match path_cursor with None -> (fun _ -> false) | Some p ->
          let p, _ = self#convert_path_to_child_path p in
          let p = GTree.Path.to_string p in
          fun x -> p = GTree.Path.to_string x
      in
      let f = match f with Some f -> f | _ -> choose_func in
      List.iter begin fun path ->
        let path, row = self#convert_path_to_child_path path in
        let dirname = model.model#get ~row ~column:col_path in
        let basename = model.model#get ~row ~column:col_name in
        let pixbuf = f ~filename:(dirname // basename) ~has_cursor:(has_cursor path) in
        match pixbuf with
        | `ignore -> ()
        | `clear ->
            model.model#set ~row ~column:col_pixbuf None;
            model.index_icons <- List.filter (fun x -> x <> path) model.index_icons
        | `set pixbuf ->
            model.model#set ~row ~column:col_pixbuf (Some pixbuf);
            model.index_icons <- path :: model.index_icons
      end view#selection#get_selected_rows

    method finalize () =
      Gmisclib.Idle.add self#clear_model_filter;
      vc_name#unset_cell_data_func renderer_basename

    method private convert_path_to_child_path path =
      if view#model#misc#get_oid = model_filter#misc#get_oid then
        let path = model_filter#convert_path_to_child_path path in
        let row = path |> model_sort#get_iter |> model_sort#convert_iter_to_child_iter in
        let path = model_sort#convert_path_to_child_path path in
        path, row
      else
        let row = model.model#get_iter path in
        path, row

    method private visible_func ~row pattern =
      entry#text <> "" &&
      let basename = model.model#get ~row ~column:col_name in
      Str.string_match pattern basename 0

    method private apply_pattern () =
      let len = String.length entry#text in
      if len = 0 then begin
        if is_filelist then (self#display_all()) else (self#clear_model_filter ());
      end else
        let re = regexp_string_case_fold entry#text in
        Hashtbl.fold begin fun filename tpath acc ->
          let name = Filename.basename filename in
          let score, _ = FuzzyLetters.compare `Greedy2 entry#text name in
          if score > 0. then
            let score = if Str.string_partial_match re name 0 then score +. 1. else score in
            if score > 0.84 then (score, tpath) :: acc else acc
          else acc
        end model.filenames []
        |> self#display

    method display paths =
      view#set_model None;
      List.iter begin fun path ->
        let row = model.model#get_iter path in
        model.model#set ~row ~column:col_visible false;
      end visible_paths;
      visible_paths <- [];
      let count = ref (List.length visible_paths) in
      let tmp = ref [] in
      begin
        try
          List.iter begin fun (score, path) ->
            let add = not (List.mem path visible_paths) in
            if add then begin
              tmp := path :: !tmp;
              let row = model.model#get_iter path in
              model.model#set ~row ~column:col_visible true;
              model.model#set ~row ~column:col_score score;
              incr count;
              if !count >= limit then (raise Exit)
            end;
          end paths;
        with Exit -> ()
      end;
      visible_paths <- List.rev_append visible_paths !tmp;
      model_filter#refilter ();
      view#set_model (Some model_filter#coerce);
      Gmisclib.Idle.add (fun () -> view#scroll_to_point 0 0);
      self#display_summary ();
      Gmisclib.Idle.add begin fun () ->
        match model_filter#get_iter_first with
        | Some row ->
            let path = model_filter#get_path row in
            view#selection#select_path path;
            view#set_cursor path vc_name;
        | _ -> ()
      end

    method display_all () =
      self#clear_model_filter();
      if model.length <= limit then begin
        view#set_model (Some model.model#coerce);
        self#display_summary();
      end;

    method private display_summary () =
      ksprintf label_count#set_text "%d of %d" (List.length visible_paths) model.length;

    method private clear_model_filter () =
      view#set_model None;
      List.iter begin fun path ->
        model.model#set ~row:(model.model#get_iter path) ~column:col_visible false;
      end visible_paths;
      visible_paths <- [];
      self#display_summary ()

    method private append pattern filename =
      if not (Hashtbl.mem model.filenames filename) then begin
        let dirname = Filename.dirname filename in
        let basename = Filename.basename filename in
        let row = model.model#append() in
        let path = model.model#get_path row in
        model.length <- model.length + 1;
        model.model#set ~row ~column:col_name basename;
        model.model#set ~row ~column:col_path dirname;
        model.model#set ~row ~column:col_score 0.;
        (*  *)
        let is_visible = self#visible_func ~row pattern in
        model.model#set ~row ~column:col_visible is_visible;
        if is_visible then (visible_paths <- path :: visible_paths);
        Hashtbl.add model.filenames filename path;
        self#display_summary ()
      end;

    method private scan ?(blocking=false) () =
      let queue = Queue.create () in
      let filter_dir dir =
        let path = Str.split (!~~ Filename.dir_sep) dir in
        let exclude_dir_rec = [".tmp"; "bak"; "_build"] in
        not (path |> List.exists (fun part -> List.mem part exclude_dir_rec))
      in
      let do_search dirname =
        if filter_dir dirname then begin
          let filenames = File_util.readdirs ~recursive:false filter dirname in
          List.iter (fun x ->  Queue.add x queue) filenames;
          Hashtbl.replace model.rtimes dirname (Unix.gettimeofday());
        end
      in
      let scan_tree roots =
        match !roots with
        | root :: tl ->
            roots := tl;
            let dirs = File_util.readtree root in
            List.iter begin fun dirname ->
              try
                let stat = Unix.stat dirname in
                let rtime = Hashtbl.find model.rtimes dirname in
                if stat.Unix.st_mtime > rtime then do_search dirname;
              with Not_found -> (do_search dirname)
            end dirs;
            true
        | [] -> false
      in
      let append roots =
        let pattern = regexp_of_pattern entry#text in
        (*try*)
        begin
          let i = ref 10 in
          while !i > 0 && Queue.length queue > 0 do
            self#append pattern (Queue.take queue);
            decr i;
          done;
          if !roots = [] && Queue.is_empty queue then begin
            self#prune ();
            if is_filelist then (self#apply_pattern());
            self#display_summary ();
            icon_progress#set_pixbuf (??? Icons.find_16);
            false
          end else true
        end;
        (*with ex ->
          Printf.eprintf "File \"dialog_find_file.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());
          false*)
      in
      let filelist =
        match model.source with
        | `path (roots, filelist) ->
            let roots = ref roots in
            if blocking then begin
              while scan_tree roots do () done;
              while append roots do () done;
            end else begin
              GMain.Timeout.add ~ms:200 ~callback:(fun () -> scan_tree roots) |> ignore;
              GMain.Timeout.add ~ms:15 ~callback:(fun () -> append roots) |> ignore;
            end;
            filelist
        | `filelist x -> x
      in
      let pattern = regexp_of_pattern entry#text in
      List.iter (self#append pattern) filelist;
      (*self#prune()*)

    method private prune () =
      let count = ref 0 in
      Hashtbl.iter begin fun filename path ->
        if not (Sys.file_exists filename) then begin
          Hashtbl.remove model.filenames filename;
          incr count;
        end;
      end model.filenames;
      model.length <- model.length - !count;
      self#display_summary ()

    method private init () =
      ignore (view#misc#connect#destroy ~callback:self#finalize);
      (* Entry key press *)
      entry#connect#changed ~callback:begin fun () ->
        Gmisclib.Idle.add ~prio:200 self#apply_pattern;
      end |> ignore;
      ignore (ebox#event#connect#key_press ~callback:begin fun ev ->
          let key = GdkEvent.Key.keyval ev in
          if key = GdkKeysyms._Up || key = GdkKeysyms._Down then begin
            let clean = view#selection#get_selected_rows = [] in
            if clean then begin
              (try view#selection#select_path (GTree.Path.create [0]) with _ -> ());
            end else begin
              match view#selection#get_selected_rows with
              | path :: _ ->
                  let path_0 = GTree.Path.copy path in
                  if key = GdkKeysyms._Up && GTree.Path.prev path then ()
                  else if key = GdkKeysyms._Down then (GTree.Path.next path);
                  view#set_cursor path vc_name;
                  if GdkEvent.Key.state ev = [`SHIFT] then (view#selection#select_range path_0 path)
                  else (view#selection#select_path path);
              | [] -> ()
            end;
            view#misc#grab_focus();
            not clean
          end else false
        end);
      (* View key press *)
      ignore (view#event#connect#key_release ~callback:begin fun ev ->
          let key = GdkEvent.Key.keyval ev in
          if key = GdkKeysyms._BackSpace then begin
            (try entry#set_text (String.sub entry#text 0 (entry#text_length - 1)) with _ -> ());
            entry#misc#grab_focus();
            entry#set_position entry#text_length;
            true
          end else if key = GdkKeysyms._Left then begin
            entry#misc#grab_focus();
            entry#set_position (entry#text_length - 1);
            true
          end else if key = GdkKeysyms._Right then begin
            entry#misc#grab_focus();
            entry#set_position entry#text_length;
            true
          end else if key = GdkKeysyms._F3 then begin
            entry#misc#grab_focus(); true
          end else begin
            let str = GdkEvent.Key.string ev in
            if String.length str > 0 && Glib.Unichar.isprint (Glib.Utf8.first_char str) then
              entry#set_text (entry#text ^ str);
            false
          end
        end);
      (*  *)
      entry#misc#grab_focus();
      if is_filelist then (self#display_all())
  end

let init ~roots ~filter =
  let widget = new widget ~source:(`path (roots, [])) ~name:"" ~filter () in
  widget#update_model ~blocking:true ();;

let add_roots ~roots ~filter =
  let name = "" in
  match List.assoc_opt name !models with
  | Some model ->
      models := List.remove_assoc name !models;
      begin
        match model.source with
        | `path (rr, filelist) ->
            let roots = rr @ roots in
            init ~roots ~filter
        | _ -> assert false
      end
  | _ -> init ~roots ~filter

