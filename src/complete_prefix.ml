open GUtil
open Utils
open Merlin
module ColorOps = Color
open Printf
open Markup

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

class virtual completion =
  object
    method virtual complete : GWindow.window -> unit
    method virtual coerce : GObj.widget
    method virtual destroy : unit -> unit
  end

let single_instance : completion option ref = ref None

let create ~compl ~x ~y ~project ~page =
  begin
    match !single_instance with
    | Some instance -> instance#destroy();
    | _ -> ()
  end;
  let window = Gtk_util.window_tooltip compl#coerce ~parent:page ~x ~y () in
  window#misc#hide();
  compl#complete window;
  single_instance := Some compl

class widget ~project ~(page : Editor_page.page) ~x ~y ?packing () =
  let vbox = GPack.vbox ~spacing:5 ~border_width:0 ?packing () in
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
  let renderer = GTree.cell_renderer_text [
      `FONT Preferences.preferences#get.editor_completion_font; `XPAD 0; `YPAD 0
    ] in
  let vc_kind = GTree.view_column ~renderer:(renderer, ["markup", col_kind]) () in
  let vc_source = GTree.view_column ~renderer:(renderer, ["text", col_source]) () in
  let vc_name = GTree.view_column ~renderer:(renderer, ["text", col_name]) () in
  let sw = GBin.scrolled_window ~shadow_type:`NONE ~hpolicy:`NEVER ~vpolicy:`AUTOMATIC ~packing:vbox#add () in
  let lview = GTree.view ~model:model_sort ~headers_visible:false ~reorderable:false ~height:200 ~packing:sw#add () in
  let _ = lview#set_enable_search false in
  let _ = lview#set_search_column 1 in
  let _ = lview#append_column vc_kind in
  let _ = lview#append_column vc_source in
  let _ = lview#append_column vc_name in
  let _ = lview#selection#set_mode `SINGLE in
  let _ = model_sort#set_sort_column_id col_prio.GTree.index `ASCENDING in
  let _ = vc_source#set_visible true in
  let _ =
    model_sort#set_sort_func col_prio.GTree.index begin fun model r1 r2 ->
      let p1 = model#get ~row:r1 ~column:col_prio in
      let p2 = model#get ~row:r2 ~column:col_prio in
      let cmpp = compare p2 p1 in
      if cmpp = 0 then begin
        let s1 = model#get ~row:r1 ~column:col_source in
        let s2 = model#get ~row:r2 ~column:col_source in
        let cmps = compare s1 s2 in
        if cmps = 0 then begin
          let n1 = model#get ~row:r1 ~column:col_name in
          let n2 = model#get ~row:r2 ~column:col_name in
          compare n1 n2
        end else cmps
      end else cmpp
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
  let mx_model_entries = Mutex.create() in
  object (self)
    inherit GObj.widget vbox#as_widget
    inherit completion

    val mutable current_prefix = ""
    val mutable current_prefix_offset_start = 0
    val mutable count = 0
    val first_entry_available = new first_entry_available()
    val loading_complete = new loading_complete()
    val mutable is_destroyed = false
    val mutable page_signals = []
    val mutable buffer_signals = []
    val mutable view_signals = []
    val mutable current_window_info = []

    method complete (window : GWindow.window) =
      let position = buffer#get_iter_at_mark `INSERT in
      let word_start, word_end = page#buffer#as_text_buffer#select_word ~pat:Ocaml_word_bound.longid ~select:false ~search:false () in
      let is_sharp uc = Glib.Utf8.from_unichar uc = "#" in
      let start = position#backward_find_char ~limit:word_start is_sharp in
      let is_method_compl = is_sharp start#char in
      let current_prefix_start = if is_method_compl then start#forward_char else start in
      let prefix = page#buffer#get_text ~start:current_prefix_start ~stop:position () in
      current_prefix_offset_start <- current_prefix_start#offset;
      current_prefix <- prefix;
      count <- 0;
      self#invoke_merlin ~prefix ~position ~expand:(not is_method_compl) window;
      self#connect_signals word_end;
      self#misc#connect#destroy ~callback:begin fun () ->
        self#disconnect_signals();
        window#destroy();
      end |> ignore;
      window#move ~x ~y

    method private connect_signals word_end =
      page_signals <- [
        page#connect#scroll_changed ~callback:(fun _ -> self#destroy());
      ];
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
        view#buffer#connect#changed ~callback:begin fun () ->
          let x, y = view#get_location_at_cursor () in
          let compl = new widget ~project ~page ~x ~y () in
          create ~compl:(compl :> completion) ~x ~y ~project ~page;
        end;
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
      page_signals |> List.iter page#disconnect;
      page_signals <- [];
      buffer_signals |> List.iter (GtkSignal.disconnect buffer#as_buffer);
      buffer_signals <- [];
      view_signals |> List.iter (GtkSignal.disconnect view#as_view);
      view_signals <- [];

    method private invoke_merlin ~prefix ~position ?(expand=true) window =
      let position = position#line + 1, position#line_offset in
      if Oe_config.completion_name_table_enabled then begin
        if String.length (String.trim prefix) >= 2 then begin
          project |> Names.update_all
            ~cont:begin fun db ->
              db |> Names.filter prefix |> self#add_entries "N";
              GtkThread.sync (fun () -> loading_complete#call count) ()
            end;
        end
      end;
      merlin@@complete_prefix ~position ~prefix |=> begin function
        | Merlin.Ok complete_prefix ->
            window#show(); (* NB The window is shown here *)
            complete_prefix.Merlin_j.entries |> List.map (fun x -> 1., x) |> self#add_entries "C";
            if count = 0 || expand then begin
              merlin@@expand_prefix ~position ~prefix |=> begin function
                | Ok expand_prefix ->
                    expand_prefix.Merlin_j.entries |> List.map (fun x -> 1., x) |> self#add_entries "E";
                    loading_complete#call count;
                | Error _ | Failure _ -> ()
                end |=> "expand_prefix"
            end else loading_complete#call count;
        | Error _ | Failure _ -> ()
        end |=> "complete_prefix"

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
          | [ path ] when direction = `PREV -> if GTree.Path.prev path then path else GTree.Path.create [0]
          | _  -> GTree.Path.create [0]
        in
        lview#selection#unselect_all();
        lview#scroll_to_cell path vc_kind;
        lview#selection#select_path path;
      with Gpointer.Null -> ()

    method private show_info () =
      try
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
                    (Preferences.preferences#get.editor_completion_font)
                    (Markup.type_info desc)
                in
                let markup_doc =
                  if info <> "" then
                    Printf.sprintf "<span font='%s'>%s</span>"
                      (Preferences.preferences#get.editor_completion_font)
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
                    begin
                      try [@warning "-42"]
                        let row = model#get_iter path in
                        model#set ~row ~column:col_desc res;
                        self#show_info()
                      with Failure _ -> () (* "GtkTree.TreeModel.get_iter" *)
                    end;
                | Error _ | Failure _ -> ()
                end |=> "type_expression"
            end
      with Gpointer.Null -> ()

    method private display_window_info path markup_type markup_doc =
      try
        let create_window ~x ~y ?width ?height ?show child =
          current_window_info |> List.iter (fun w -> (*Gmisclib.Idle.add*) w#destroy());
          let window = Gtk_util.window_tooltip child ~parent:page ~x ~y ?width ?height ?show () in
          current_window_info <- window :: current_window_info;
          self#misc#connect#destroy ~callback:window#destroy |> ignore;
          window
        in
        let row_area = lview#get_cell_area ~path () in
        let r0 = self#misc#allocation in
        let wx, wy = Gdk.Window.get_position self#misc#toplevel#misc#window in
        let x0 = wx + r0.Gtk.width in
        let y0 = wy + Gdk.Rectangle.y row_area in
        let vbox = GPack.vbox ~spacing:5 ~border_width:5 () in
        let label_type = GMisc.label ~markup:markup_type ~xalign:0.0 ~yalign:0.0 ~xpad:0 ~ypad:0 ~line_wrap:true ~packing:vbox#add () in
        let _ = GMisc.separator `HORIZONTAL ~packing:vbox#add ~show:(markup_doc <> "") () in
        let label_doc = GMisc.label ~markup:markup_doc ~xalign:0.0 ~yalign:0.0 ~xpad:0 ~ypad:0 ~line_wrap:true ~packing:vbox#add ~show:(markup_doc <> "") () in
        let window_info = create_window ~x:x0 ~y:y0 ~show:false vbox#coerce in
        window_info#resize ~width:1 ~height:1;
        Gmisclib.Idle.add ~prio:100 begin fun () ->
          window_info#show();
          let _ = Gtk_util.move_window_within_screen_bounds window_info x0 y0 in
          let r = window_info#misc#allocation in
          if r.height > Gdk.Screen.height() then begin
            let sw = GBin.scrolled_window ~hpolicy:`AUTOMATIC () in
            let vp = GBin.viewport ~packing:sw#add () in
            vbox#misc#reparent vp#coerce;
            let width = r.width + 21 in
            let height = Gdk.Screen.height() - y0 - 13 in
            create_window ~x:x0 ~y:y0 ~width ~height sw#coerce |> ignore;
          end
        end
      with ex ->
        Log.println `ERROR "%s\n%s" (Printexc.to_string ex) (Printexc.get_backtrace());

    method private selected_path =
      try
        match lview#selection#get_selected_rows with
        | [ spath ] -> Some (model_sort#convert_path_to_child_path spath)
        | _ -> None
      with Gpointer.Null -> None

    val mutable model_entries : (float ref * Gtk.tree_path ref * Merlin_t.entry) list = []

    method private add_entries source entries =
      let last_count = count in
      let is_expand = source = "E" || source = "N" in
      let add_row (entry : Merlin_t.entry) score =
        let row = model#append () in
        model#set ~row ~column:col_is_exp is_expand;
        model#set ~row ~column:col_source (sprintf "%s %6.2f" source score);
        model#set ~row ~column:col_prio score;
        model#set ~row ~column:col_kind (icon_of_kind entry.kind);
        model#set ~row ~column:col_name entry.name;
        model#set ~row ~column:col_desc entry.desc;
        model#set ~row ~column:col_info entry.info;
        count <- count + 1;
        let path = model#get_path row in
        model_entries <- (ref score, ref path, entry) :: model_entries;
        row
      in
      begin
        try
          entries
          |> List.iter begin fun (score, (entry : Merlin_t.entry)) ->
            if is_destroyed then raise Exit;
            model_entries
            |> List.find_opt (fun (_, _, e) -> e.Merlin_t.name = entry.Merlin_t.name)
            |> function
            | Some (s, path, _) when score > !s ->
                Mutex.protect mx_model_entries begin fun () ->
                  s := score;
                  let row = model#get_iter !path in
                  if model#remove row then begin
                    let row = add_row entry score in
                    path := model#get_path row
                  end
                end;
            | None ->
                Mutex.protect mx_model_entries
                  (fun () -> add_row entry score |> ignore);
            | _ -> ()
          end
        with Exit -> ()
      end;
      if last_count = 0 && count > 0 then first_entry_available#call();
      lview#scroll_to_cell (GTree.Path.create [0]) vc_kind;

    initializer
      view#misc#connect#after#realize ~callback:(fun _ -> vc_name#set_sizing `GROW_ONLY) |> ignore;
      self#misc#connect#destroy ~callback:begin fun () ->
        single_instance := None;
        is_destroyed <- true
      end |> ignore;
      self#connect#loading_complete ~callback:begin fun count ->
        if count > 0 then Gmisclib.Idle.add self#show_info
        else self#destroy()
      end |> ignore;
      lview#selection#connect#changed ~callback:self#show_info |> ignore;

    method connect = new signals ~first_entry_available ~loading_complete
  end

and first_entry_available () = object inherit [unit] signal () end
and loading_complete () = object inherit [int] signal () end
and signals ~first_entry_available ~loading_complete =
  object
    inherit ml_signals [first_entry_available#disconnect; loading_complete#disconnect]
    method first_entry_available = first_entry_available#connect ~after
    method loading_complete = loading_complete#connect ~after
  end

let create_window ~project ~page =
  let view = page#ocaml_view in
  let x, y = view#get_location_at_cursor () in
  let compl = new widget ~project ~page ~x ~y () in
  create ~compl:(compl :> completion) ~x ~y ~project ~page
