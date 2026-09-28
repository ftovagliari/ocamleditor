open Merlin_j
open Printf
open Utils
open Oe

module Log = Common.Log.Make(struct let prefix = "QUICK-INFO" end)
let _ =
  Log.set_print_timestamp true;
  Log.set_verbosity `DEBUG

type effct = Motion (* Not tested *) | Fade

let effct : effct option = (*None*) Some Fade

let merlin (buffer : Ocaml_text.buffer) func cont =
  let filename = match buffer#file with Some file -> file#filename | _ -> "" in
  let buffer = buffer#get_text () in
  (Merlin.as_cps func ~filename ~buffer) cont

module OneShotSignal = struct
  let create () = ref []
  let save cell id = cell := id :: !cell
  let disconnect cell widget =
    match !cell with
    | [] -> ()
    | ids -> cell := []; List.iter (GtkSignal.disconnect widget) ids
end

type t = {
  view : Ocaml_text.view;
  filename : string;
  tag : GText.tag;
  motion_notify_signal : GtkSignal.id list ref;
  key_press_signal : GtkSignal.id list ref;
  mutable is_active : bool;
  mutable show_at : (int * int) option;
  mutable markup_odoc : Markup.odoc;
}

type hover = {
  window : GWindow.window;
  fullname_label : GMisc.label;
  typexpr_label : GMisc.label;
  vars_label : GMisc.label;
  doc_label : GMisc.label;
  mutable last_pos : (int * int);
  mutable range : (GText.view * GText.tag * Gtk.text_mark * Gtk.text_mark) option;
  mutable is_pointer_over : bool;
  mutable is_pinned : bool;
  mutable has_scroll : bool;
  mutable effect_timer : Glib.Timeout.id option;
}

let hover : hover option ref = ref None

let move_popup ~(main_window : GWindow.window) ~(popup : GWindow.window) x y =
  let mw, mh = main_window#get_size() in
  let p_alloc = popup#misc#allocation in
  let px, py = x, y (*Gdk.Window.get_origin popup#misc#window*) in
  let pw = p_alloc.Gtk.width in
  let ph = p_alloc.Gtk.height in
  let x = if px + pw > mw then max 0 (mw - pw) else max 0 px in
  let y = if py + ph > mh then max 0 (py - ph - 20) else max 0 py in
  (*Printf.printf "MOVE %d,%d -> %d,%d ph=%d %d mh=%d\n%!" x0 y0 x y ph (py+ph) mh;*)
  popup#move ~x ~y

module Effect = struct
  let motion hover x y k =
    let x0, y0 = hover.last_pos in
    let dx = x - x0 in
    let dy = y - y0 in
    if abs dx > 100 || abs dy > 50 || not hover.window#misc#visible then begin
      hover.window#present();
      hover.window#move ~x:x ~y:y;
      k();
      None
    end else begin
      hover.window#present();
      let duration_steps = 10 in
      let step = ref 0 in
      let update () =
        if !step < duration_steps then begin
          incr step;
          let t = float_of_int !step /. float_of_int duration_steps in
          let x = float_of_int x0 +. t *. (float_of_int x -. float_of_int x0) |> int_of_float in
          let y = float_of_int y0 +. t *. (float_of_int y -. float_of_int y0) |> int_of_float in
          Gmisclib.Idle.add ~prio:300 (fun () -> hover.window#move ~x ~y);
          true
        end else begin
          hover.window#move ~x:x ~y:y;
          k();
          false
        end
      in
      Some (Glib.Timeout.add ~ms:15 ~callback:update)
    end

  let fade window k =
    let duration_steps = 10 in
    let step = ref 0 in
    let update () =
      if !step < duration_steps then begin
        incr step;
        let opa = float !step /. float duration_steps in
        Gmisclib.Idle.add ~prio:300 (fun () -> window#set_opacity opa);
        true
      end else begin
        window#set_opacity 1.0;
        k();
        false
      end
    in
    Some (Glib.Timeout.add ~ms:25 ~callback:update)
end

let pin () =
  !hover |> Option.iter begin fun hover ->
    hover.is_pinned <- true;
    hover.window#misc#style_context#add_class "dialog-window";
  end

let unpin () =
  !hover |> Option.iter begin fun hover ->
    hover.is_pinned <- false;
    hover.window#misc#style_context#remove_class "dialog-window";
  end

let remove_highlight hover =
  match hover.range with
  | Some (view, tag, m_start, m_stop) ->
      Fun.protect begin fun () ->
        let start =
          match Gmisclib.Util.get_iter_at_mark_opt view#buffer#as_buffer m_start with
          | Some it -> new GText.iter it
          | _ -> view#buffer#start_iter
        in
        let stop =
          match Gmisclib.Util.get_iter_at_mark_opt view#buffer#as_buffer m_stop with
          | Some it -> new GText.iter it
          | _ -> view#buffer#end_iter
        in
        view#buffer#remove_tag tag ~start ~stop;
      end ~finally:begin fun () ->
        hover.range <- None;
        if not (GtkText.Mark.get_deleted m_start) then view#buffer#delete_mark (`MARK m_start);
        if not (GtkText.Mark.get_deleted m_stop) then view#buffer#delete_mark (`MARK m_stop);
      end
  | _ -> ()

let reset ?(hide=true) ?(ms=0) () =
  let callback () =
    !hover |> Option.iter begin fun hover ->
      if hover.range <> None && not hover.is_pointer_over then begin
        remove_highlight hover;
        if hide then hover.window#misc#hide();
        hover.fullname_label#set_label "";
        hover.typexpr_label#set_label "";
        hover.vars_label#set_label "";
        hover.doc_label#set_label "";
        hover.fullname_label#misc#hide();
        hover.vars_label#misc#hide();
        hover.doc_label#misc#hide();
        hover.range <- None;
        hover.is_pointer_over <- false;
        unpin();
        hover.doc_label#set_width_chars 45;
      end
    end;
    false
  in
  if ms > 0 then Gmisclib.Timeout.add __FUNCTION__ ~ms ~callback |> ignore
  else callback () |> ignore

let set_active qi value = qi.is_active <- value;;

let setup_hover () =
  let window = GWindow.window
      ~decorated:false
      ~modal:false
      ~border_width:0
      ~deletable:true
      ~resizable:true
      ~kind:`POPUP
      ~type_hint:`TOOLTIP
      ~focus_on_map:false
      ~show:false ()
  in
  Gmisclib.Util.esc_destroy_window window;
  window#set_skip_pager_hint true;
  window#set_skip_taskbar_hint true;
  window#set_urgency_hint false;
  window#set_accept_focus false;
  window#misc#set_can_focus false;
  (*let sw = GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~show:true ~packing:window#add () in*)
  let vbox = GPack.vbox ~border_width:5 ~spacing:5 ~packing:window#add(*sw#add_with_viewport*) () in
  let fullname_label =
    GMisc.label ~xpad:0 ~ypad:0 ~xalign:0.0 ~yalign:0.0 ~line_wrap:false ~packing:(vbox#pack ~expand:false) () in
  let typexpr_label =
    GMisc.label ~xpad:10 ~ypad:0 ~xalign:0.0 ~yalign:0.0 ~line_wrap:false ~packing:(vbox#pack ~expand:false) () in
  let vars_label =
    GMisc.label ~xpad:0 ~ypad:0 ~xalign:0.0 ~yalign:0.0 ~packing:(vbox#pack ~expand:false) ~show:false () in
  let _ = GMisc.separator `HORIZONTAL ~packing:(vbox#pack ~expand:false) () in
  let doc_label =
    GMisc.label ~xpad:0 ~ypad:0 ~xalign:0.0 ~yalign:0.0 ~line_wrap:true ~packing:(vbox#pack ~expand:false) () in
  typexpr_label#set_use_markup true;
  fullname_label#set_use_markup true;
  vars_label#set_use_markup true;
  doc_label#set_use_markup true;
  let open Preferences in
  let open Settings_j in
  doc_label#misc#modify_font_by_name preferences#get.editor_completion_font;
  vars_label#misc#modify_font_by_name preferences#get.editor_completion_font;
  typexpr_label#misc#modify_font_by_name preferences#get.editor_completion_font;
  fullname_label#misc#modify_font_by_name preferences#get.editor_base_font;
  let hov =
    {
      window; fullname_label; typexpr_label; vars_label; doc_label;
      range = None; last_pos = (0, 0); effect_timer = None;
      is_pointer_over = false; is_pinned = false; has_scroll = false }
  in
  window#event#connect#enter_notify ~callback:begin fun _ ->
    hov.is_pointer_over <- true;
    (*Log.println `DEBUG "ENTER %d\n%!" wininfo.index;*)
    false
  end |> ignore;
  window#event#connect#leave_notify ~callback:begin fun ev ->
    let detail = GdkEvent.Crossing.detail ev in
    hov.is_pointer_over <- detail = `INFERIOR;
    (*if not wininfo.is_pointer_over then
      Log.println `DEBUG "LEAVE %d\n%!" wininfo.index;*)
    false
  end |> ignore;
  window#event#connect#button_press ~callback:begin fun _ ->
    pin();
    true
  end |> ignore;
  window#misc#connect#after#show ~callback:begin fun () ->
    let width = window#misc#allocated_width in
    if window#misc#allocated_height > 200 then begin
      window#remove vbox#coerce;
      let sw = GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:window#add () in
      sw#add_with_viewport vbox#coerce;
      window#resize ~width ~height:400;
      hov.has_scroll <- true;
      sw#misc#show();
    end
  end |> ignore;
  window#misc#connect#hide ~callback:begin fun () ->
    if hov.has_scroll then begin
      window#remove window#child;
      vbox#misc#reparent window#coerce;
      hov.has_scroll <- false;
      window#resize ~width:1 ~height:1;
    end
  end |> ignore;
  reset();
  hover := Some hov

let show qi hover x y =
  match effct with
  | None ->
      hover.window#set_opacity 0.0;
      hover.window#show();
      hover.doc_label#misc#show(); (* Resize the window to fit content *)
      Gmisclib.Idle.add ~prio:300 begin fun () ->
        GWindow.toplevel qi.view |> Option.iter (fun main_window ->
            move_popup ~main_window ~popup:hover.window x y |> ignore);
        hover.window#set_opacity 1.0;
        hover.last_pos <- (x, y);
      end
  | Some Motion -> (* TODO Not tested *)
      hover.effect_timer |> Option.iter Gmisclib.Timeout.remove;
      hover.effect_timer <- Effect.motion hover x y begin fun () ->
          hover.last_pos <- (x, y);
          hover.effect_timer <- None
        end;
      hover.doc_label#misc#show(); (* Resize the window to fit content *)
  | Some Fade ->
      hover.effect_timer |> Option.iter Gmisclib.Timeout.remove;
      hover.window#set_opacity 0.0;
      hover.window#show();
      hover.doc_label#misc#show(); (* Resize the window to fit content *)
      hover.window#move ~x ~y;
      Gmisclib.Idle.add ~prio:300 begin fun () ->
        GWindow.toplevel qi.view |> Option.iter (fun main_window ->
            move_popup ~main_window ~popup:hover.window x y |> ignore);
        hover.effect_timer <- Effect.fade hover.window (fun () -> hover.effect_timer <- None);
        hover.last_pos <- (x, y);
      end

let display qi tooltip_x tooltip_y start stop =
  !hover |> Option.iter begin fun hover ->
    try
      let rstart = qi.view#get_iter_location start in
      let _, ystart = qi.view#buffer_to_window_coords ~tag:`WIDGET
          ~x:(Gdk.Rectangle.x rstart) ~y:(Gdk.Rectangle.y rstart) in
      let x, y =
        let pX, pY = Gdk.Window.get_pointer_location qi.view#misc#toplevel#misc#window in
        let win = (match qi.view#get_window `WIDGET with None -> assert false | Some w -> w) in
        let px, py = Gdk.Window.get_pointer_location win in
        let enable_check = false in
        let t = 15 in
        if enable_check &&
           not (px - t <= tooltip_x && tooltip_x <= px + t && py - t <= tooltip_y && tooltip_y <= py + t)
        then raise (Printf.ksprintf invalid_arg "%d,%d %d,%d" tooltip_x tooltip_y px py);
        match qi.show_at with
        | Some (x, y) ->
            qi.show_at <- None;
            pX - px + x, pY - py + y
        | _ ->
            let _, lh = qi.view#get_line_yrange start in
            pX (*- px + xstart*) - 13, pY - py + ystart + lh - qi.view#pixels_below_lines
      in
      hover.range <-
        Some (qi.view#as_gtext_view,
              qi.tag,
              qi.view#buffer#create_mark ~name:"qi-start" start,
              qi.view#buffer#create_mark ~name:"qi-stop" stop);
      begin
        match hover.range with
        | Some (view, tag, m_start, m_stop) ->
            begin
              try
                let start = Gmisclib.Util.get_iter_at_mark_safe view#buffer#as_buffer m_start in
                let stop = Gmisclib.Util.get_iter_at_mark_safe view#buffer#as_buffer m_stop in
                GtkText.Buffer.apply_tag view#buffer#as_buffer tag#as_tag start stop
              with Gmisclib_util.Mark_deleted -> Log.println `WARN "Mark_deleted"
            end;
        | _ -> ()
      end;
      Gaux.may (GWindow.toplevel qi.view) ~f:(fun x -> hover.window#set_transient_for x#as_window);
      if not hover.has_scroll then
        hover.window#resize ~width:1 ~height:1;
      show qi hover x y;
      qi.view#misc#set_has_tooltip qi.is_active;
    with Invalid_argument _ as ex ->
      qi.view#misc#set_has_tooltip qi.is_active;
      Printf.printf " %s\n%!" (Printexc.to_string ex);
  end

let get_iter_at_line (buffer : GText.buffer) pos =
  let ln = pos.line - 1 in
  if pos.line < 0 || ln > buffer#end_iter#line then raise (Invalid_linechar pos);
  buffer#get_iter (`LINE ln)

let get_iter_at_linechar buffer pos =
  let it = get_iter_at_line buffer pos in
  if pos.col >= it#chars_in_line then raise (Invalid_linechar pos);
  it#set_line_offset pos.col (*buffer#get_iter (`LINECHAR (pos.line - 1, pos.col))*)

let get_type_info qi (entry : type_enclosing_value) (entry2 : type_enclosing_value option) =
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
  let start = get_iter_at_linechar qi.view#buffer entry.te_start in
  let stop = get_iter_at_linechar qi.view#buffer entry.te_stop in
  start, stop, tail_info, Markup.type_info type_expr, type_params

let merlin_type qi position k =
  (*let l, c = position in Log.println `DEBUG "%s %d,%d" __FUNCTION__ l c;*)
  merlin qi.view#obuffer @@ Merlin.type_enclosing ~position () |=> begin function
    | Merlin.Ok types ->
        begin
          match types with
          | [] -> ()
          | fst :: snd :: _ -> GtkThread.async k (get_type_info qi fst (Some snd))
          | fst :: _ -> GtkThread.async k (get_type_info qi fst None)
        end;
    | Merlin.Failure _ | Merlin.Error _ -> ()
    end

let merlin_doc qi position k =
  merlin qi.view#obuffer @@ Merlin.document ~position () |=> begin function
    | Merlin.Ok doc -> GtkThread.async k doc
    | Merlin.Failure msg | Merlin.Error msg -> ()
    end

let ocp_index_fullname qi start stop k =
  let ident = qi.view#obuffer#get_text ~start ~stop () in
  let context = qi.filename, Some (start#line + 1), Some (start#line_offset + 1) in
  Ocp_index.fullname_async ~context ident
  |> Async.start_with_continuation ~name:__FUNCTION__ begin fun fullname ->
    try
      match fullname with
      | Some fullname -> GtkThread.async k fullname
      | None -> GtkThread.async k ""
    with ex ->
      Log.println `ERROR "%s\n\t%s\n%s" __FUNCTION__ (Printexc.to_string ex) (Printexc.get_backtrace())
  end

let collect_info qi (iter : GText.iter) ~continue_with =
  if qi.view#has_focus then begin
    !hover |> Option.iter begin fun hover ->
      let position = iter#line + 1, iter#line_index in
      let count = ref 3 in
      let cont start stop =
        decr count;
        if !count = 0 then continue_with start stop;
      in
      merlin_type qi position begin fun (start, stop, tail_info, type_expr, type_params) ->
        (* ocp-index needs start and stop from merlin *)
        ocp_index_fullname qi start stop begin fun fullname ->
          if fullname <> "" then begin
            hover.fullname_label#set_label (Markup.type_info fullname);
            hover.fullname_label#misc#show();
          end else hover.fullname_label#misc#hide();
          cont start stop
        end;
        hover.typexpr_label#set_label (sprintf "%s%s" type_expr tail_info);
        if type_params <> "" then begin
          hover.vars_label#set_label type_params;
          hover.vars_label#misc#show();
        end;
        cont start stop;
        merlin_doc qi position begin fun doc ->
          let markup = qi.markup_odoc#convert doc in
          hover.doc_label#set_label markup;
          hover.doc_label#misc#hide();
          cont start stop
        end;
      end;
    end
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

let process_location qi x y =
  qi.view#misc#set_has_tooltip false;
  !hover |> Option.iter begin fun hover ->
    let current_range = hover.range in
    let bx, by = qi.view#window_to_buffer_coords ~tag:`WIDGET ~x ~y in
    if bx > 0 then begin
      let iter = qi.view#get_iter_at_location ~x:bx ~y:by in
      match current_range with
      | Some (_, _, start, stop) when in_range qi.view#buffer#as_buffer iter#as_iter ~start ~stop ->
          qi.view#misc#set_has_tooltip qi.is_active;
      | _ when hover.is_pinned -> ()
      (*| _ when qi.view#buffer#has_selection -> ()*)
      | _ ->
          begin
            match get_typeable_iter_at_coords qi iter with
            | Some iter ->
                reset (*~hide:(effct = None) *)();
                collect_info qi iter ~continue_with:(display qi x y)
            | _ ->
                reset ~ms:100 ();
                qi.view#misc#set_has_tooltip qi.is_active;
          end
    end else
      qi.view#misc#set_has_tooltip qi.is_active;
  end

let setup_tooltip_toggle qi =
  let rec enable_key_press qi =
    qi.view#event#connect#key_press ~callback:begin fun _ ->
      OneShotSignal.disconnect qi.key_press_signal qi.view#as_widget;
      reset();
      enable_motion_notify qi;
      qi.view#misc#set_has_tooltip false;
      false
    end |> OneShotSignal.save qi.key_press_signal
  and enable_motion_notify (qi : t) =
    qi.view#event#connect#motion_notify ~callback:begin fun _ ->
      OneShotSignal.disconnect qi.motion_notify_signal qi.view#as_widget;
      qi.view#misc#set_has_tooltip qi.is_active;
      enable_key_press qi;
      false
    end |> OneShotSignal.save qi.motion_notify_signal
  in
  enable_key_press qi

let at_iter qi (iter : GText.iter) () =
  let rect = qi.view#get_iter_location iter in
  let x = Gdk.Rectangle.x rect in
  let y = Gdk.Rectangle.y rect in
  let x, y = qi.view#buffer_to_window_coords ~x ~y ~tag:`WIDGET in
  qi.show_at <- Some (x, y + Gdk.Rectangle.height rect);
  setup_tooltip_toggle qi;
  process_location qi x y

let debouncer = Debouncer.create ~ms:250

let query_tooltip qi ~x ~y ~kbd _ =
  (*Log.println `DEBUG "%d %d %f" x y (Unix.gettimeofday());*)
  begin
    try Debouncer.schedule debouncer (fun () -> process_location qi x y);
    with ex ->
      Printf.eprintf "File \"quick_info.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());
  end;
  false

let connect_to_view qi (view : Ocaml_text.view) =
  setup_tooltip_toggle qi;
  (*  *)
  view#event#connect#button_press ~callback:begin fun _ ->
    view#misc#set_has_tooltip false;
    reset ();
    Gmisclib.Idle.add ~prio:300 (fun () -> view#misc#set_has_tooltip qi.is_active);
    false
  end |> ignore;
  view#event#connect#scroll ~callback:begin fun _ ->
    Gmisclib.Timeout.add __FUNCTION__ ~ms:200 ~callback:begin fun () ->
      reset ();
      false
    end  |> ignore;
    false
  end |> ignore;
  view#event#connect#focus_in ~callback:begin fun _ ->
    Gmisclib.Timeout.add __FUNCTION__ ~ms:500 ~callback:begin fun () ->
      view#misc#set_has_tooltip qi.is_active;
      false
    end |> ignore;
    false
  end |> ignore;
  view#event#connect#focus_out ~callback:begin fun _ ->
    view#misc#set_has_tooltip false;
    reset ();
    false
  end |> ignore;
  view#event#connect#leave_notify ~callback:begin fun _ ->
    Gmisclib.Timeout.add __FUNCTION__ ~ms:500 ~callback:begin fun () ->
      reset ();
      false
    end |> ignore;
    false
  end |> ignore;
  view#misc#set_has_tooltip qi.is_active;
  view#misc#connect#query_tooltip ~callback:(query_tooltip qi) |> ignore

let create (view : Ocaml_text.view) =
  if !hover = None then setup_hover();
  let open Preferences in
  let bg_color = ?? (Preferences.preferences#get.Settings_t.editor_bg_color_popup) in
  let filename = match view#obuffer#file with Some file -> file#filename | _ -> "" in
  let qi =
    {
      markup_odoc = new Markup.odoc();
      is_active = Preferences.preferences#get.Settings_j.editor_quick_info_enabled;
      view;
      filename;
      tag = view#buffer#create_tag ~name:"quick-info" [`BACKGROUND bg_color];
      show_at = None;
      motion_notify_signal = OneShotSignal.create();
      key_press_signal = OneShotSignal.create();
    }
  in
  Preferences.preferences#connect#changed ~callback:begin fun pref ->
    qi.is_active <- pref.Settings_j.editor_quick_info_enabled;
    qi.view#misc#set_has_tooltip qi.is_active;
    qi.markup_odoc <- new Markup.odoc(); (* Reset preferences *)
  end |> ignore;
  if view#obuffer#is_ocaml_file filename then
    connect_to_view qi view;
  qi
