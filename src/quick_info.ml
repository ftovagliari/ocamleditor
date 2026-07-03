open Merlin_j
open Printf
open Utils
open Oe

module Log = Common.Log.Make(struct let prefix = "QUICK-INFO" end)
let _ =
  Log.set_print_timestamp true;
  Log.set_verbosity `DEBUG

let merlin (buffer : Ocaml_text.buffer) func cont =
  let filename = match buffer#file with Some file -> file#filename | _ -> "" in
  let buffer = buffer#get_text () in
  (Merlin.as_cps func ~filename ~buffer) cont

module SignalId = struct
  let create () = ref None
  let save cell id = cell := Some id
  let disconnect cell widget =
    match !cell with
    | Some id -> cell := None; GtkSignal.disconnect widget id
    | None -> ()
end

let mx_index = Mutex.create()
let mx_wininfo = Mutex.create()


let index = ref 10_000
let new_index () =
  Mutex.protect mx_index begin fun () ->
    index := !index + 1;
    !index
  end

(** The type of quick info. *)
type t = {
  mutable markup_odoc : Markup.odoc;
  view : Ocaml_text.view;
  filename : string;
  tag : GText.tag;
  mutable is_active : bool;
  mutable current_x : int;
  mutable current_y : int;
  mutable show_at : (int * int) option;
  (** Contains the location to display quick information when you want to
      override the default location, which is the mouse pointer. *)

  mutable windows : wininfo list;
}

and wininfo = {
  window : GWindow.window;
  mutable range : (Gtk.text_mark * Gtk.text_mark) option;
  (** It is the range of the buffer that contains the expression for which quick
      info is currently shown. *)

  mutable is_pinned : bool;
  mutable index : int;
}

(** Returns the last open quick info window. This function is not thread safe. *)
let get_current_window_unsafe qi =
  match qi.windows with
  | wininfo :: _ -> Some wininfo
  | _ -> None

(** Returns the last open quick info window. *)
let get_current_window qi = Mutex.protect mx_wininfo (fun () -> get_current_window_unsafe qi)

let remove_highlight qi wi =
  match wi.range with
  | Some (m_start, m_stop) ->
      Fun.protect begin fun () ->
        let start =
          match Gmisclib.Util.get_iter_at_mark_opt qi.view#buffer#as_buffer m_start with
          | Some it -> new GText.iter it
          | _ -> qi.view#buffer#start_iter
        in
        let stop =
          match Gmisclib.Util.get_iter_at_mark_opt qi.view#buffer#as_buffer m_stop with
          | Some it -> new GText.iter it
          | _ -> qi.view#buffer#end_iter
        in
        qi.view#buffer#remove_tag qi.tag ~start ~stop;
      end ~finally:begin fun () ->
        wi.range <- None;
        if not (GtkText.Mark.get_deleted m_start) then qi.view#buffer#delete_mark (`MARK m_start);
        if not (GtkText.Mark.get_deleted m_stop) then qi.view#buffer#delete_mark (`MARK m_stop);
      end
  | _ -> ()

let add_wininfo qi callback wi =
  Mutex.protect mx_wininfo (fun () -> qi.windows <- wi :: qi.windows; callback())

let is_poiter_over (wi : wininfo) =
  try
    let px, py = Gdk.Window.get_pointer_location wi.window#misc#window in
    px >= 0 && py >= 0
  with Gpointer.Null -> false

(** Starts a timer that closes the specified quick-info window and removes
    expression highlighting in the editor. An exception is the case
    in which the window is pinned when the timer expires.
    Delayed close allows the user to move the mouse pointer over the window
    to pin it before it closes. *)
let remove_wininfo qi wi =
  GMain.Timeout.add ~ms:300 ~callback:begin fun () ->
    if not wi.is_pinned && not (is_poiter_over wi) then begin
      Mutex.protect mx_wininfo begin fun () ->
        if not wi.is_pinned then remove_highlight qi wi;
        qi.windows <- qi.windows |> List.filter (fun x -> x.window#misc#get_oid <> wi.window#misc#get_oid);
        wi.window#destroy();
      end;
    end;
    false
  end |> ignore

(** [is_pinned qi] is [true] iff the last open quick info window is pinned. *)
let is_pinned qi =
  match get_current_window qi with Some wi -> wi.is_pinned | _ -> false

(** Immediately hides the last open quick-info window and all the others
    previously open and in timed closure. The pinned window is an exception
    and is not hidden. *)
let hide qi =
  Mutex.protect mx_wininfo begin fun () ->
    qi.windows |> List.iter begin fun wi ->
      if not wi.is_pinned then begin
        remove_highlight qi wi;
        wi.window#misc#hide()
      end
    end;
  end

let unpin qi =
  Mutex.protect mx_wininfo (fun () -> List.iter (fun w -> w.is_pinned <- false) qi.windows);
  hide qi

(** Closes the last quick information opened and all other timed closing windows. *)
let close qi cause =
  (*if cause <> "" then Log.println `DEBUG "CLOSE %s" cause;*)
  qi.windows |> List.iter (remove_wininfo qi)

let make_pinnable wininfo =
  wininfo.window#event#connect#button_press ~callback:begin fun _ ->
    wininfo.is_pinned <- true;
    wininfo.window#misc#modify_bg [`NORMAL, `COLOR (Preferences.editor_tag_bg_color "selection")];
    true
  end |> ignore

let (!=) (p1 : Merlin_j.pos) (p2 : Merlin_j.pos) =
  p1.col <> p2.col || p1.line <> p2.line

(** Displays the quick info popup window.  *)
let display qi start stop =
  let rstart = qi.view#get_iter_location start in
  let _, ystart = qi.view#buffer_to_window_coords ~tag:`WIDGET
      ~x:(Gdk.Rectangle.x rstart) ~y:(Gdk.Rectangle.y rstart) in
  let open Preferences in
  let open Settings_j in
  let vbox = GPack.vbox ~border_width:5 ~spacing:5 () in
  let vbox1 = GPack.vbox ~border_width:0 ~spacing:0 ~packing:vbox#add () in
  let label_fn = GMisc.label ~xpad:0 ~ypad:0 ~xalign:0.0 ~yalign:0.0 ~line_wrap:false ~packing:vbox1#add () in
  let label_typ = GMisc.label ~xpad:10 ~ypad:0 ~xalign:0.0 ~yalign:0.0 ~line_wrap:false ~packing:vbox1#add () in
  let label_vars = GMisc.label ~xpad:0 ~ypad:0 ~xalign:0.0 ~yalign:0.0 ~packing:vbox#add ~show:false () in
  let _ = GMisc.separator `HORIZONTAL ~packing:vbox#add () in
  let label_doc = GMisc.label ~xpad:0 ~ypad:0 ~xalign:0.0 ~yalign:0.0 ~line_wrap:true ~packing:vbox#add () in
  label_typ#set_use_markup true;
  label_fn#set_use_markup true;
  label_vars#set_use_markup true;
  label_doc#set_use_markup true;
  label_doc#misc#modify_font_by_name preferences#get.editor_completion_font;
  label_vars#misc#modify_font_by_name preferences#get.editor_completion_font;
  label_typ#misc#modify_font_by_name preferences#get.editor_completion_font;
  label_fn#misc#modify_font_by_name preferences#get.editor_base_font;
  let x, y =
    let pX, pY = Gdk.Window.get_pointer_location (Gdk.Window.root_parent ()) in
    let win = (match qi.view#get_window `WIDGET with None -> assert false | Some w -> w) in
    let px, py = Gdk.Window.get_pointer_location win in
    match qi.show_at with
    | Some (x, y) ->
        qi.show_at <- None;
        pX - px + x, pY - py + y
    | _ ->
        let _, lh = qi.view#get_line_yrange start in
        pX (*- px + xstart*), pY - py + ystart + lh
  in
  let create_range () =
    Some (qi.view#buffer#create_mark ~name:"qi-start" start,
          qi.view#buffer#create_mark ~name:"qi-stop" stop)
  in
  let window = Gtk_util.window_tooltip vbox#coerce ~fade:false ~x ~y ~show:false () in
  let wininfo = {
    window;
    range = create_range ();
    is_pinned = false;
    index = new_index();
  } in
  let callback () =
    match wininfo.range with
    | Some (m_start, m_stop) ->
        GtkThread.async
          begin fun () ->
            try
              let start = Gmisclib.Util.get_iter_at_mark_safe qi.view#buffer#as_buffer m_start in
              let stop = Gmisclib.Util.get_iter_at_mark_safe qi.view#buffer#as_buffer m_stop in
              GtkText.Buffer.apply_tag qi.view#buffer#as_buffer qi.tag#as_tag start stop
            with Gmisclib_util.Mark_deleted -> Log.println `WARN "Mark_deleted"
          end ()
    | _ -> ()
  in
  add_wininfo qi callback wininfo;
  make_pinnable wininfo;
  Gmisclib.Idle.add begin fun () ->
    window#present();
    let r = vbox#misc#allocation in
    if r.Gtk.height > 200 then begin
      let sw = GBin.scrolled_window ~hpolicy:`AUTOMATIC () in
      let vp = GBin.viewport ~packing:sw#add () in
      if not Oe_config.use_theme_colors_when_possible then begin
        sw#misc#modify_bg [`NORMAL, `NAME ?? (Preferences.preferences#get.editor_bg_color_popup)];
        vp#misc#modify_bg [`NORMAL, `NAME ?? (Preferences.preferences#get.editor_bg_color_popup)];
      end;
      vbox#misc#reparent vp#coerce;
      hide qi;
      close qi "";
      let window = Gtk_util.window_tooltip sw#coerce ~fade:false ~x ~y ~width:700 ~height:300 ~show:false () in
      let wininfo = {
        window;
        range = create_range ();
        is_pinned = false;
        index = new_index();
      } in
      add_wininfo qi callback wininfo;
      make_pinnable wininfo;
      window#present()
    end
  end;
  label_fn, label_typ, label_vars, label_doc

let build_content qi (entry : type_enclosing_value) (entry2 : type_enclosing_value option) =
  (*Printf.printf "merlin(1): %s\n%!" entry.Merlin_t.te_type;
    Printf.printf "merlin(2): %s\n%!" (match entry2 with Some e -> e.Merlin_t.te_type | _ -> "NONE");*)
  let tail, (type_expr, type_params) =
    let tail, (type_expr, varmap) =
      match entry2 with
      | Some entry2 ->
          begin
            try
              entry.Merlin_t.te_tail, Type_expr.find_substitutions entry.Merlin_t.te_type entry2.Merlin_t.te_type
            with Syntaxerr.Error _ -> entry.Merlin_t.te_tail, (entry.Merlin_t.te_type, [])
          end;
      | _ -> entry.Merlin_t.te_tail, (entry.Merlin_t.te_type, [])
    in
    let info = varmap |> List.map (fun (n, v) -> sprintf "  %s is %s" (Markup.type_info n) (Markup.type_info v)) in
    tail, (type_expr,
           if info <> [] then "In this context\n" ^ (info |> String.concat "\n") else "")
  in
  let tail_info =
    match tail with
    | Merlin_t.No -> ""
    | Merlin_t.Position -> "\nTail Position"
    | Merlin_t.Call -> "\nTail Call"
  in
  tail_info, Markup.type_info type_expr, type_params

let get_iter_at_line buffer pos =
  let ln = pos.line - 1 in
  if pos.line < 0 || ln > buffer#end_iter#line then raise (Invalid_linechar pos);
  buffer#get_iter (`LINE ln)

let get_iter_at_linechar buffer pos =
  let it = get_iter_at_line buffer pos in
  if pos.col >= it#chars_in_line then raise (Invalid_linechar pos);
  it#set_line_offset pos.col (*buffer#get_iter (`LINECHAR (pos.line - 1, pos.col))*)

(** Opens a new quick information window with the information received from merlin.
    This function is applied in a separate thread. *)
let spawn_window qi position (entry : type_enclosing_value) (entry2 : type_enclosing_value option) =
  if qi.view#misc#get_flag `HAS_FOCUS then begin
    let start = get_iter_at_linechar qi.view#buffer entry.te_start in
    let stop = get_iter_at_linechar qi.view#buffer entry.te_stop in
    let tail_info, type_expr, type_params = build_content qi entry entry2 in
    let label_fn, label_typ, label_vars, label_doc = display qi start stop in
    let ident = qi.view#obuffer#get_text ~start ~stop () in
    let context = qi.filename, Some (start#line + 1), Some (start#line_offset + 1) in
    (* TODO: Avoid looking up the fullname for local identifiers.
       If the type_expr value returned by ocp_index is equal to the type_expr value returned
       by merlin, then the fullname returned by ocp_index is correct;
       otherwise, do not display the fullname. *)
    Ocp_index.fullname_async ~context ident
    |> Async.map ~name:"spawn_window" begin fun fullname ->
      try
        match fullname with
        | Some fullname ->
            label_fn#misc#show();
            label_fn#set_label (sprintf "<span size='small'>%s</span>" (Markup.type_info fullname));
        | None -> label_fn#misc#hide()
      with ex ->
        Log.println `ERROR "%s\n\t%s\n%s" __FUNCTION__ (Printexc.to_string ex) (Printexc.get_backtrace())
    end
    |> Async.run_synchronously;
    label_typ#set_label (sprintf "%s%s" type_expr tail_info);
    if type_params <> "" then begin
      label_vars#misc#show();
      label_vars#set_label type_params
    end;
    merlin qi.view#obuffer @@ Merlin.document ~position () |=> begin function
      | Merlin.Ok doc ->
          GtkThread.async begin fun () ->
            let markup = qi.markup_odoc#convert doc in
            label_doc#set_label markup;
          end ()
      | Merlin.Failure msg | Merlin.Error msg -> ()
      end
  end

let invoke_merlin qi (iter : GText.iter) ~continue_with =
  let position = iter#line + 1, iter#line_index in
  merlin qi.view#obuffer @@ Merlin.type_enclosing ~position () |=> begin function
    | Merlin.Ok types ->
        GtkThread.async begin fun () ->
          match types with
          | [] -> close qi "no-type"
          | fst :: snd :: _ -> continue_with position fst (Some snd)
          | fst :: _ -> continue_with position fst None
        end ()
    | Merlin.Failure _ | Merlin.Error _ -> ()
    end

let is_iter_in_comment (buffer : Ocaml_text.buffer) iter =
  Comments.enclosing (Comments.scan (buffer#get_text ())) iter#offset

let get_typeable_iter_at_coords qi iter =
  if iter#ends_line
  || Glib.Unichar.isspace iter#char
  || (match is_iter_in_comment qi.view#obuffer iter with None -> false | _ -> true)
  then None else Some iter

let in_range buffer iter ~start ~stop =
  try
    let start = Gmisclib.Util.get_iter_at_mark_safe buffer start in
    let stop = Gmisclib.Util.get_iter_at_mark_safe buffer stop in
    GtkText.Iter.in_range iter start stop
  with Gmisclib_util.Mark_deleted -> false

let process_location qi ?(is_at_iter=false) x y =
  let current_window = get_current_window qi in
  let current_range = Option.bind current_window (fun w -> w.range) in
  let bx, by = qi.view#window_to_buffer_coords ~tag:`WIDGET ~x ~y in
  if bx > 0 then begin
    let iter = qi.view#get_iter_at_location ~x:bx ~y:by in
    match current_range with
    | Some (start, stop) when in_range qi.view#buffer#as_buffer iter#as_iter ~start ~stop -> ()
    | _ when is_pinned qi -> ()
    | _ when qi.view#buffer#has_selection -> ()
    | _ ->
        let is_immobile = x = qi.current_x && y = qi.current_y in
        qi.current_x <- x;
        qi.current_y <- y;
        let is_mouse_over =
          match current_window with
          | Some wi ->
              begin
                try
                  let root_window = Gdk.Window.root_parent () in
                  let r = wi.window#misc#allocation in
                  let wx, wy = Gdk.Window.get_position wi.window#misc#window in
                  let px, py = Gdk.Window.get_pointer_location root_window in
                  wx <= px && px <= wx + r.Gtk.width && wy <= py && py <= wy + r.Gtk.height
                with Gpointer.Null -> false
              end
          | _ -> false
        in
        if is_mouse_over then ()
        else if is_immobile || is_at_iter then begin
          match get_typeable_iter_at_coords qi iter with
          | Some iter ->
              hide qi;
              close qi "before-invoke-merlin";
              invoke_merlin qi iter ~continue_with:(spawn_window qi);
          | _ -> close qi "not-typeable"
        end else close qi ""
  end

(** Displays quick info about the expression at the specified iter. *)
let at_iter qi (iter : GText.iter) () =
  close qi "at_iter";
  let rect = qi.view#get_iter_location iter in
  let x = Gdk.Rectangle.x rect in
  let y = Gdk.Rectangle.y rect in
  let x, y = qi.view#buffer_to_window_coords ~x ~y ~tag:`WIDGET in
  qi.show_at <- Some (x, y + Gdk.Rectangle.height rect);
  process_location qi ~is_at_iter:true x y

let query_tooltip qi ~x ~y ~kbd _ =
  (*Log.println `DEBUG "%d %d %f" x y (Unix.gettimeofday());*)
  begin
    try process_location qi x y;
    with ex ->
      Printf.eprintf "File \"quick_info.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());
  end;
  false

let set_active qi value =
  qi.is_active <- value;
  if not qi.is_active then hide qi

let connect_to_view qi (view : Ocaml_text.view) =
  let motion_notify = SignalId.create() in
  view#event#connect#key_press ~callback:begin fun ev ->
    view#misc#set_has_tooltip false;
    view#event#connect#motion_notify ~callback:begin fun _ ->
      view#misc#set_has_tooltip qi.is_active;
      SignalId.disconnect motion_notify view#as_widget;
      false
    end |> SignalId.save motion_notify;
    hide qi;
    close qi "key-press";
    false
  end |> ignore;
  view#event#connect#button_press ~callback:begin fun _ ->
    view#misc#set_has_tooltip false;
    unpin qi;
    close qi "button-press";
    Gmisclib.Idle.add ~prio:300 (fun () -> view#misc#set_has_tooltip qi.is_active);
    false
  end |> ignore;
  view#event#connect#scroll ~callback:begin fun _ ->
    unpin qi;
    hide qi;
    close qi "scroll";
    false
  end |> ignore;
  view#event#connect#focus_in ~callback:begin fun _ ->
    GMain.Timeout.add ~ms:500 ~callback:begin fun () ->
      view#misc#set_has_tooltip qi.is_active;
      false
    end |> ignore;
    false
  end |> ignore;
  view#event#connect#focus_out ~callback:begin fun _ ->
    view#misc#set_has_tooltip false;
    unpin qi;
    close qi "focus_out";
    false
  end |> ignore;
  view#event#connect#leave_notify ~callback:begin fun _ ->
    close qi "leave_notify";
    false
  end |> ignore;
  view#misc#set_has_tooltip qi.is_active;
  view#misc#connect#query_tooltip ~callback:(query_tooltip qi) |> ignore

let create (view : Ocaml_text.view) =
  let open Preferences in
  let bg_color = ?? (Preferences.preferences#get.Settings_t.editor_bg_color_popup) in
  let filename = match view#obuffer#file with Some file -> file#filename | _ -> "" in
  let qi =
    {
      markup_odoc = new Markup.odoc();
      is_active = true;
      view = view;
      filename = filename;
      tag = view#buffer#create_tag ~name:"quick-info" [`BACKGROUND bg_color];
      current_x = 0;
      current_y = 0;
      show_at = None;
      windows = [];
    }
  in
  Preferences.preferences#connect#changed ~callback:begin fun pref ->
    qi.view#misc#set_has_tooltip (qi.is_active && pref.Settings_j.editor_quick_info_enabled);
    qi.markup_odoc <- new Markup.odoc()
  end |> ignore;
  if view#obuffer#is_ocaml_file filename then
    connect_to_view qi view;
  qi
