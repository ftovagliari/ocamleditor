open GUtil
open Utils
open Merlin
module ColorOps = Color
open Printf
open Markup

module Log = Common.Log.Make(struct let prefix = "COMPLETION" end)
let _ =
  Log.set_print_timestamp true;
  Log.set_verbosity `WARN

module String_utils = struct
  let rec locate_intersection left right =
    let len_right = String.length right in
    if len_right = 0 then String.length left, 0
    else
      let re = Str.regexp (Printf.sprintf "%s$" (Str.quote right)) in
      try
        Str.search_backward re left (String.length left),
        len_right
      with Not_found ->
        locate_intersection left (Str.first_chars right (len_right - 1))
end
let m_names = ref []

class widget ~project ~(page : Editor_page.page) ?packing () =
  let ebox = GBin.event_box ?packing () in
  let vbox = GPack.vbox ~spacing:5 ~border_width:0 ~packing:ebox#add () in
  let spinner_box = GPack.hbox ~spacing:5 ~border_width:0 ~packing:(vbox#pack ~expand:false) () in
  let spinner = GMisc.spinner ~active:true ~packing:(spinner_box#pack ~expand:false) () in
  let _ = GMisc.label ~text:"Loading..." ~xalign:0.0 ~packing:spinner_box#add () in
  let cols = new GTree.column_list in
  let col_is_exp = cols#add Gobject.Data.boolean in
  let col_prio = cols#add Gobject.Data.float in
  let col_source = cols#add Gobject.Data.string in
  let col_kind = cols#add Gobject.Data.string in
  let col_name = cols#add Gobject.Data.string in
  let col_desc = cols#add Gobject.Data.string in
  let col_info = cols#add Gobject.Data.string in
  let model = GTree.list_store cols in
  let model_sort = GTree.model_sort model in
  let renderer_icon = GTree.cell_renderer_text [
      `FONT Preferences.preferences#get.editor_completion_font; `XPAD 5; `YPAD 0;
    ] in
  let renderer = GTree.cell_renderer_text [
      `FONT Preferences.preferences#get.editor_completion_font; `XPAD 0; `YPAD 0
    ] in
  let renderer_score = GTree.cell_renderer_text [
      `FONT Preferences.preferences#get.editor_completion_font; `XPAD 0; `YPAD 0;
      `FOREGROUND "#707070";
      `SCALE `X_SMALL
    ] in
  let vc_kind = GTree.view_column ~renderer:(renderer_icon, ["markup", col_kind]) () in
  let vc_source = GTree.view_column ~renderer:(renderer_score, ["text", col_source]) () in
  let vc_name = GTree.view_column ~renderer:(renderer, ["text", col_name]) () in
  let sw = GBin.scrolled_window ~shadow_type:`NONE ~hpolicy:`NEVER ~vpolicy:`AUTOMATIC ~packing:vbox#add () in
  let lview = GTree.view ~model:model_sort ~headers_visible:false ~reorderable:false ~packing:sw#add () in
  let _ = lview#set_enable_search false in
  let _ = lview#set_search_column 1 in
  let _ = lview#append_column vc_source in
  let _ = lview#append_column vc_kind in
  let _ = lview#append_column vc_name in
  let _ = lview#selection#set_mode `SINGLE in
  let _ = model_sort#set_sort_column_id col_prio.GTree.index `ASCENDING in
  let _ = vc_source#set_visible true in
  let _ =
    model_sort#set_sort_func col_prio.GTree.index begin fun model r1 r2 ->
      let p1 = model#get ~row:r1 ~column:col_prio in
      let p2 = model#get ~row:r2 ~column:col_prio in
      compare p2 p1
    end
  in
  let view = page#ocaml_view in
  let buffer = view#buffer in
  let merlin merlin_func cont name =
    let filename = match (view#obuffer#as_text_buffer :> Text.buffer)#file with Some file -> file#filename | _ -> "" in
    let buffer = buffer#get_text () in
    (Merlin.as_cps merlin_func ~name ~filename ~buffer) cont
  in
  let markup_odoc = new Markup.odoc() in
  let window_compl =
    let window = GWindow.window
        ~decorated:false
        ~modal:false
        ~border_width:2
        ~deletable:true
        ~resizable:true
        ~kind:`POPUP
        ~type_hint:`TOOLTIP
        ~focus_on_map:false
        ~show:false ()
    in
    window#set_skip_pager_hint true;
    window#set_skip_taskbar_hint true;
    window#set_urgency_hint false;
    Gaux.may (GWindow.toplevel page#view) ~f:(fun x -> window#set_transient_for x#as_window);
    window#add ebox#coerce;
    window
  in
  let label_type = GMisc.label ~markup:"" ~xalign:0.0 ~yalign:0.0 ~xpad:0 ~ypad:0 ~line_wrap:false () in
  let separator = GMisc.separator `HORIZONTAL ~show:true () in
  let label_doc = GMisc.label ~markup:"" ~xalign:0.0 ~yalign:0.0 ~xpad:0 ~ypad:0 ~line_wrap:true ~show:false () in
  let vbox_window_info = GPack.vbox ~spacing:5 ~border_width:5 () in
  let window_info =
    let window = GWindow.window
        ~decorated:false
        ~modal:false
        ~border_width:2
        ~deletable:true
        ~resizable:true
        ~kind:`POPUP
        ~type_hint:`TOOLTIP
        ~focus_on_map:false
        ~show:false ()
    in
    window#set_skip_pager_hint true;
    window#set_skip_taskbar_hint true;
    window#set_urgency_hint false;
    Gaux.may (GWindow.toplevel page#view) ~f:(fun x -> window#set_transient_for x#as_window);
    vbox_window_info#pack ~expand:false label_type#coerce;
    vbox_window_info#pack ~expand:false separator#coerce;
    vbox_window_info#pack ~expand:true label_doc#coerce;
    window#add vbox_window_info#coerce;
    window
  in
  object (self)
    inherit GObj.widget ebox#as_widget as super

    val mutable current_prefix = ""
    val mutable current_prefix_offset_start = 0
    val mutable count = 0
    val mutable is_destroyed = false
    val mutable has_scroll = false
    val mutable buffer_signals = []
    val mutable adjustment_signals = []
    val mutable view_signals = []
    val mutable current_window_info = []
    val mutable model_entries : (float ref * Gtk.tree_path ref * Merlin_t.entry) list = []
    val load_start = new load_start()
    val load_complete = new load_complete()

    method on_load_complete ~callback = self#connect#load_complete ~callback

    method private get_completion_geometry (view : GText.view) =
      let x, y, lh = Gtk_util.get_location_at_cursor view `BELOW in
      match GWindow.toplevel view with
      | Some toplevel ->
          let _, ht = toplevel#misc#allocated_width, toplevel#misc#allocated_height in
          let _, hc = window_compl#misc#allocation.Gtk.width, window_compl#misc#allocation.Gtk.height in
          let bottom_overflow = max 0 (y + hc - ht) in
          let top_overflow = max 0 (lh + hc - y) in
          let y, hc =
            if bottom_overflow = 0 then y, 0
            else if top_overflow = 0 then (y - lh - hc), 0
            else if bottom_overflow <= top_overflow then y, (hc - bottom_overflow)
            else (y - lh - hc + top_overflow), (hc - top_overflow)
          in
          x, y, hc
      | _ -> x, y, 0

    method private get_info_geometry path =
      let r0 = self#misc#allocation in
      let wx, wy = Gdk.Window.get_position self#misc#toplevel#misc#window in
      let x = wx + r0.Gtk.width in
      let y = wy (*+ Gdk.Rectangle.y (lview#get_cell_area ~path ())*) in
      let _, hi = window_info#misc#allocation.Gtk.width, window_info#misc#allocation.Gtk.height in
      match GWindow.toplevel view with
      | Some toplevel ->
          let _, ht = toplevel#misc#allocated_width, toplevel#misc#allocated_height in
          let bottom_overflow = max 0 (y + hi - ht) in
          let top_overflow = max 0 (hi - y) in
          let y, hc =
            if bottom_overflow = 0 then y, 0
            else if top_overflow = 0 then (y - bottom_overflow), 0
            else if bottom_overflow <= top_overflow then y, (hi - bottom_overflow)
            else (y - hi + top_overflow), (hi - top_overflow)
          in
          x, y, hc
      | _ -> x, y, 0

    method complete () =
      load_start#call();
      self#disconnect_signals();
      model_entries <- [];
      model#clear();
      count <- 0;
      let position = buffer#get_iter_at_mark `INSERT in
      let word_start, word_end =
        page#buffer#as_text_buffer#select_word ~pat:Ocaml_word_bound.longid ~select:false ~search:false () in
      let is_sharp uc = Glib.Utf8.from_unichar uc = "#" in
      let start = position#backward_find_char ~limit:word_start is_sharp in
      let is_method_compl = is_sharp start#char in
      let current_prefix_start = if is_method_compl then start#forward_char else start in
      let prefix = page#buffer#get_text ~start:current_prefix_start ~stop:position () in
      current_prefix_offset_start <- current_prefix_start#offset;
      current_prefix <- prefix;
      self#invoke_merlin ~prefix ~position ~expand:(not is_method_compl) ();
      self#connect_signals word_end;

    method private connect_signals word_end =
      buffer_signals <- [
        view#buffer#connect#mark_set ~callback:begin fun it mark ->
          match GtkText.Mark.get_name mark with
          | Some "insert" ->
              let ins = buffer#get_iter `INSERT in
              if
                ins#offset < current_prefix_offset_start ||
                ins#compare word_end > 0
              then self#destroy();
          | _ -> ()
        end;
        view#buffer#connect#after#changed ~callback:self#complete; (* For when typing with completion active *)
      ];
      adjustment_signals <- [
        view#vadjustment#connect#value_changed ~callback:(fun _ -> self#destroy());
      ];
      view_signals <-
        [
          view#event#connect#key_press ~callback:begin fun ev ->
            let keyval = GdkEvent.Key.keyval ev in
            if keyval = GdkKeysyms._Escape then begin
              self#destroy();
              true
            end else if keyval = GdkKeysyms._Up then begin
              self#select_row `PREV;
              true
            end else if keyval = GdkKeysyms._Down then begin
              self#select_row `NEXT;
              true
            end else if keyval = GdkKeysyms._Return then begin
              self#selected_path
              |> Option.iter (fun path -> Gmisclib.Idle.add (fun () -> self#apply path));
              true
            end else false
          end;
          view#event#connect#focus_out ~callback:(fun _ -> self#destroy(); false);
          view#event#connect#scroll ~callback:(fun _ -> self#destroy(); false);
        ]

    method private disconnect_signals () =
      buffer_signals |> List.iter (GtkSignal.disconnect buffer#as_buffer);
      buffer_signals <- [];
      adjustment_signals |> List.iter (GtkSignal.disconnect view#vadjustment#as_adjustment);
      adjustment_signals <- [];
      view_signals |> List.iter (GtkSignal.disconnect view#as_view);
      view_signals <- [];

    method private invoke_merlin ~prefix ~position ?(expand=true) () =
      let position = position#line + 1, position#line_offset in
      (* steps: names, complete_prefix and expand_prefix *)
      let steps = ref 3 in
      let sync entries name =
        GtkThread.sync begin fun name ->
          try
            decr steps;
            self#add_entries name !steps entries;
          with ex ->
            Printf.eprintf "%s\n%s\n%s\n%!" __LOC__ (Printexc.to_string ex) (Printexc.get_backtrace());
        end name
      in
      if Oe_config.completion_name_table_enabled then begin
        if String.length (String.trim prefix) >= 2 then begin
          Async.create ~name:"compl-names" begin fun () ->
            project |> Names.update_all
              ~cont:begin fun db ->
                let entries = Names.filter prefix db in
                sync entries "N"
              end;
          end |> Async.start
        end else sync [] "N"
      end else sync [] "N";
      merlin@@complete_prefix ~position ~prefix |=> begin function
        | Merlin.Ok complete_prefix ->
            let entries = complete_prefix.Merlin_j.entries |> List.map (fun x -> 1., x) in
            sync entries "C";
        | Error _ | Failure _ -> sync [] "C"
        end |=> "complete_prefix";
      if expand then begin
        merlin@@expand_prefix ~position ~prefix |=> begin function
          | Ok expand_prefix ->
              let entries =
                expand_prefix.Merlin_j.entries
                |> List.take 1000
                |> List.map (fun x ->
                    match Names.FS.compare Greedy prefix x.Merlin_j.name with
                    | Some res -> res.Names.FS.score, x
                    | _ -> 1., x)
                |> List.take 100
              in
              sync entries "E";
          | Error _ | Failure _ -> sync [] "E"
          end |=> "expand_prefix"
      end else sync [] "E"

    method private apply path =
      let row = model#get_iter path in
      let name = model#get ~row ~column:col_name in
      let is_expand = model#get ~row ~column:col_is_exp in
      page#view#tbuffer#undo#begin_block ~name:"compl";
      let _, stop = page#buffer#as_text_buffer#select_word ~pat:Ocaml_word_bound.regexp ~select:false ~search:false () in
      self#disconnect_signals();
      if is_expand then begin
        let start = buffer#get_iter_at_char current_prefix_offset_start in
        buffer#delete_interactive ~start ~stop () |> ignore;
        buffer#insert_interactive name |> ignore;
      end else begin
        let a, b = String_utils.locate_intersection current_prefix name in
        let substitute = Str.string_after name b in
        let start = buffer#get_iter `INSERT in
        let stop = if start#compare stop >= 0 then start else stop in
        buffer#delete_interactive ~start ~stop () |> ignore;
        buffer#insert_interactive substitute |> ignore;
      end;
      page#view#tbuffer#undo#end_block();
      self#destroy();

    method private select_row direction =
      try
        let path =
          match lview#selection#get_selected_rows with
          | [ path ] when direction = `NEXT -> GTree.Path.next path; path
          | [ path ] when direction = `PREV ->
              if GTree.Path.prev path then path else raise Gpointer.Null
          | _  -> GTree.Path.create [0]
        in
        match GTree.Path.get_indices path |> Array.to_list with
        | index :: _ when index >= count -> ()
        | _ ->
            lview#selection#unselect_all();
            lview#scroll_to_cell path vc_kind;
            lview#selection#select_path path;
      with Gpointer.Null -> ()

    method private show_info () =
      try
        if is_destroyed then raise Gpointer.Null;
        match lview#selection#get_selected_rows with
        | [] -> ()
        | spath :: _ ->
            let path = model_sort#convert_path_to_child_path spath in
            let row = model#get_iter path in
            let desc = model#get ~row ~column:col_desc in
            let info = model#get ~row ~column:col_info in
            let info = String.trim info in
            if String.trim desc <> "" || String.trim info <> "" then begin
              Gmisclib.Idle.add begin fun () ->
                let markup_type =
                  Printf.sprintf "<span font='%s'>%s</span>"
                    (Preferences.preferences#get.Settings_j.editor_completion_font)
                    (Markup.type_info desc)
                in
                let markup_doc =
                  if info <> "" then
                    Printf.sprintf "<span font='%s'>%s</span>"
                      (Preferences.preferences#get.Settings_j.editor_completion_font)
                      (markup_odoc#convert info)
                  else ""
                in
                self#display_window_info spath markup_type markup_doc
              end
            end else begin
              let name = model#get ~row ~column:col_name in
              let path = model#get_path row in
              merlin@@Merlin.type_expression ~position:(1,1) ~expression:name |=> begin function
                | Ok res ->
                    GtkThread.async begin fun () ->
                      begin
                        try [@warning "-42"]
                          let row = model#get_iter path in
                          model#set ~row ~column:col_desc res;
                          self#show_info()
                        with Failure _ -> () (* "GtkTree.TreeModel.get_iter" *)
                      end
                    end ()
                | Error _ | Failure _ -> ()
                end |=> "type_expression"
            end
      with
      | Gpointer.Null -> ()
      | ex -> Printf.eprintf "%s\n%s\n%s\n%!" __LOC__ (Printexc.to_string ex) (Printexc.get_backtrace());

    method private display_window_info path markup_type markup_doc =
      try
        if is_destroyed then raise Gpointer.Null;
        label_doc#misc#hide();
        window_info#misc#hide();
        self#remove_window_info_scrollbar ();
        label_type#set_label markup_type;
        if markup_doc <> "" then begin
          label_doc#set_label markup_doc;
          label_doc#misc#show();
          separator#misc#show();
        end;
        window_info#show();
        self#add_window_info_scollbar ();
        Gmisclib.Idle.add ~prio:300 begin fun () ->
          try
            let x, y, height = self#get_info_geometry path in
            if not has_scroll then
              window_info#resize ~width:1 ~height:(max 1 height)
            else if height > 0 then
              window_info#resize ~width:window_info#misc#allocated_width ~height;
            window_info#move ~x ~y;
          with Gpointer.Null -> ()
        end
      with ex ->
        Log.println `ERROR "%s\n%s" (Printexc.to_string ex) (Printexc.get_backtrace());

    method private selected_path =
      try
        match lview#selection#get_selected_rows with
        | [ spath ] -> Some (model_sort#convert_path_to_child_path spath)
        | _ -> None
      with Gpointer.Null -> None

    method private add_entries source step entries =
      let is_expand = source = "E" || source = "N" in
      let add_score = match source with "C" -> 2. | "E" -> 0. | _ -> 0. in
      let trigger_load_complete is_last =
        if step = 0 && is_last && not is_destroyed then
          load_complete#call count
      in
      let add_row (entry : Merlin_j.entry) score is_last =
        let score = score +. add_score in
        let row = model#append () in
        Gmisclib.Idle.add ~prio:300 begin fun () -> (* let the spinner spin *)
          m_names := entry.Merlin_j.name :: !m_names;
          model#set ~row ~column:col_is_exp is_expand;
          model#set ~row ~column:col_source (sprintf "%s %6.2f" source score);
          model#set ~row ~column:col_prio score;
          model#set ~row ~column:col_kind (icon_of_kind entry.Merlin_j.kind);
          model#set ~row ~column:col_name (List.hd !m_names);
          model#set ~row ~column:col_desc entry.Merlin_j.desc;
          model#set ~row ~column:col_info entry.Merlin_j.info;
          let path = model#get_path row in (* TODO Crash here. Gpointer.Null *)
          model_entries <- (ref score, ref path, entry) :: model_entries;
          trigger_load_complete is_last
        end;
        count <- count + 1;
        row
      in
      try
        let last_index = List.length entries - 1 in
        match entries with
        | [] -> trigger_load_complete (last_index < 0)
        | entries ->
            entries
            |> List.iteri begin fun i (score, (entry : Merlin_j.entry)) ->
              if is_destroyed then raise Exit;
              model_entries
              |> List.find_opt (fun (_, _, e) -> e.Merlin_j.name = entry.Merlin_j.name)
              |> function
              | Some (s, path, _) when score > !s ->
                  s := score;
                  let row = model#get_iter !path in
                  if model#remove row then begin
                    count <- count - 1;
                    let row = add_row entry score (i = last_index) in
                    path := model#get_path row
                  end else
                    trigger_load_complete (i = last_index)
              | None ->
                  add_row entry score (i = last_index) |> ignore
              | _ -> trigger_load_complete (i = last_index)
            end
      with Exit -> ()

    method! destroy () =
      window_info#destroy();
      window_compl#destroy();
      self#disconnect_signals();
      super#destroy();

    method private add_window_info_scollbar () =
      let limit = 300 in
      if not has_scroll && window_info#misc#allocated_height > limit then begin
        let width = window_info#misc#allocated_width in
        window_info#remove vbox_window_info#coerce;
        let sw = GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:window_info#add () in
        sw#add_with_viewport vbox_window_info#coerce;
        window_info#resize ~width ~height:limit;
        has_scroll <- true;
        sw#misc#show();
      end

    method private remove_window_info_scrollbar () =
      if has_scroll then begin
        window_info#remove window_info#child;
        vbox_window_info#misc#reparent window_info#coerce;
        has_scroll <- false;
      end

    initializer
      label_doc#set_width_chars 40;
      ebox#set_visible_window false;
      view#misc#connect#after#realize ~callback:(fun _ -> vc_name#set_sizing `GROW_ONLY) |> ignore;
      self#misc#connect#destroy ~callback:begin fun () ->
        window_compl#destroy();
        is_destroyed <- true
      end |> ignore;

      self#connect#load_start ~callback:begin fun () ->
        sw#misc#hide();
        window_compl#resize ~width:1 ~height:1;
        window_compl#show();
        let x, y, height = self#get_completion_geometry page#view#as_gtext_view in
        if height > 0 then window_compl#resize ~width:1 ~height;
        window_compl#move ~x ~y;
      end |> ignore;

      self#connect#load_complete ~callback:begin fun count ->
        sw#misc#show();
        spinner_box#misc#hide();
        if count > 0 && not is_destroyed then begin
          window_compl#resize ~width:1 ~height:200;
          self#show_info();
        end else self#destroy()
      end |> ignore;

      lview#selection#connect#changed ~callback:self#show_info |> ignore

    method connect = new signals ~load_start ~load_complete
  end

and load_start () = object inherit [unit] signal () end
and load_complete () = object inherit [int] signal () end
and signals ~load_start ~load_complete =
  object
    inherit ml_signals [load_start#disconnect; load_complete#disconnect]
    method load_start = load_start#connect ~after
    method load_complete = load_complete#connect ~after
  end

let single_instance = ref None

let create ~project ~page =
  !single_instance |> Option.iter (fun instance -> instance#destroy());
  let compl = new widget ~project ~page () in
  single_instance := Some compl;
  compl#complete()

