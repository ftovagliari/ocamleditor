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
module ColorOps = Color
open Preferences
open Cairo_drawable

let forward_non_blank iter =
  let rec f it =
    let stop = it#forward_char in
    let ch = it#get_text ~stop in
    if List.for_all ((<>) ch) [" "; "\t"; "\r"] then it
    else f stop
  in
  f iter

class manager (view : Ocaml_text.view) =
  let buffer = view#tbuffer in
  let tag_table = new GText.tag_table buffer#tag_table in
  let create_tags () =
    let ts = Unix.gettimeofday() in
    let tag_error = buffer#create_tag ~name:(sprintf "tag_error-%f" ts) [`LEFT_MARGIN view#left_margin] in
    begin
      match Oe_config.error_underline_mode with
      | `CUSTOM -> ()
      | `GTK ->
          Gobject.Property.set tag_error#as_tag {Gobject.name="underline"; conv=Gobject.Data.int} 4;
    end;
    let tag_warning = buffer#create_tag ~name:(sprintf "tag_warning-%f" ts) [`LEFT_MARGIN view#left_margin] in
    let tag_warning_unused = buffer#create_tag ~name:Oe_config.warning_unused_tag_name
        [`FOREGROUND (?? Oe_config.warning_unused_color); `STYLE `ITALIC] in
    tag_error, tag_warning, tag_warning_unused
  in
  let tag_error, tag_warning, tag_warning_unused = create_tags () in
  let is_warning_unused = function [@warning "-4"]
    | Oe.Warning (20, _) | Oe.Warning (26, _) | Oe.Warning (27, _) -> true
    | _ -> false
  in
  object (self)
    val tag_applied = new tag_applied ()
    val tag_removed = new tag_removed ()
    val mutable tag_error_bounds : Oe.error_indication list = []
    val mutable tag_warning_bounds : Oe.error_indication list = []
    val mutable tag_popup = []
    val mutable enabled = true
    val has_errors = new GUtil.variable false
    val has_warnings = new GUtil.variable false
    val has_errors_or_warnings = new GUtil.variable false
    val mutable sticky_popup = false
    val mutable table = []
    val mutable error_gutter_markers = []
    val mutable flag_underline = Preferences.preferences#get.editor_err_underline
    val mutable flag_tooltip = Preferences.preferences#get.editor_err_tooltip
    val mutable flag_gutter = Preferences.preferences#get.editor_err_gutter
    val mutable tag_error = tag_error
    val mutable tag_warning = tag_warning
    val mutable tag_warning_unused = tag_warning_unused
    val mutable phase = 3

    method enabled = enabled
    method set_enabled x = enabled <- x

    method set_phase () = phase <- if view#pixels_below_lines <= 2 then 3 else 3

    method flag_underline = flag_underline
    method flag_tooltip = flag_tooltip
    method flag_gutter = flag_gutter
    method set_flag_underline x =
      flag_underline <- x;
      self#remove_tag();
      match Oe_config.error_underline_mode with
      | `CUSTOM -> ()
      | `GTK ->
          Gobject.Property.set tag_error#as_tag {Gobject.name="underline"; conv=Gobject.Data.int}
            (if flag_underline then 4 else 0)

    method create_tags () =
      tag_table#remove tag_error#as_tag;
      tag_table#remove tag_warning#as_tag;
      tag_table#remove tag_warning_unused#as_tag;
      let tg_error, tg_warning, tg_warning_unused = create_tags () in
      tag_error <- tg_error;
      tag_warning <- tg_warning;
      tag_warning_unused <- tg_warning_unused

    method tag_of_error error =
      match [@warning "-4"] error.Oe.er_level with
      | Oe.Error -> tag_error
      | Oe.Warning (20, _) | Oe.Warning (26, _) | Oe.Warning (27, _) -> tag_warning_unused
      | _ -> tag_warning

    method set_flag_tooltip x = flag_tooltip <- x
    method set_flag_gutter x =
      flag_gutter <- x;
      self#remove_tag();

    method has_errors = has_errors
    method first_error = match tag_error_bounds with x :: _-> Some x | _ -> None

    method has_warnings = has_warnings
    method first_warning = match tag_warning_bounds with x :: _ -> Some x | _ -> None

    method has_errors_or_warnings = has_errors_or_warnings
    method first_error_or_warning = match self#first_error with None -> self#first_warning | x -> x
    method scroll_to_first_error () =
      let iter =
        match self#first_error_or_warning with
        | None -> buffer#start_iter
        | Some { Oe.ei_start; _ } -> buffer#get_iter_at_mark (`MARK ei_start)
      in
      view#scroll_aligned iter;
      buffer#place_cursor ~where:iter

    method private do_apply_tag messages kind =
      (*Prf.crono Prf.prf_error_indication_appy_tag begin fun () ->*)
      let tview = (view :> Text.view) in
      let messages = List.sort (fun e1 e2 -> Stdlib.compare e1.Oe.er_lines e2.Oe.er_lines) messages in
      List.map begin fun error ->
        let l1, l2 = error.Oe.er_lines in
        let l1, l2 = l1 - 1, l2 - 1 in
        let c1, c2 = error.Oe.er_characters in

        let iter = buffer#get_iter (`LINE l1) in
        let text = iter#get_text ~stop:buffer#end_iter in
        let offset = Convert.offset_from_pos text ~pos:c1 in
        let start = iter#forward_chars offset in

        let iter = buffer#get_iter (`LINE l2) in
        let text = iter#get_text ~stop:buffer#end_iter in
        let offset = Convert.offset_from_pos text ~pos:c2 in
        let stop = iter#forward_chars offset in

        let tag = self#tag_of_error error in
        buffer#apply_tag tag ~start ~stop;
        let mark_start = buffer#create_mark(* ~name:(Gtk_util.create_mark_name "Error_indication.do_apply_tag1")*) start in
        (*if flag_gutter then begin
          let kind, icon, color =
            match kind with
            | `Warning -> `Warning error.Oe.er_message, "\u{f071}", "darkorange"
            | `Error -> `Error error.Oe.er_message, "\u{f0159}", "red" (*   ⛔ *)
            | _ -> assert false
          in
          let marker = Gutter.create_marker ~kind
              ~mark:mark_start ~icon:(icon, color) ~callback:self#callback_gutter_marker ()
          in
          tview#margin_manager#gutter.Gutter.markers <- marker :: tview#margin_manager#gutter.Gutter.markers;
          error_gutter_markers <- marker :: error_gutter_markers;
          end;*)
        { Oe.ei_start=mark_start; ei_stop=(buffer#create_mark stop); ei_error=error }
      end messages
    (*end ()*)

    method apply_tag messages =
      Gmisclib.Idle.add begin fun () ->
        if enabled then begin
          let tview = (view :> Text.view) in
          self#remove_tag();
          let errors = messages.Oe.er_errors in
          let warnings = messages.Oe.er_warnings in
          tag_error_bounds <- self#do_apply_tag errors `Error;
          tag_warning_bounds <- self#do_apply_tag warnings `Warning;
          has_errors#set (tag_error_bounds <> []);
          has_warnings#set (tag_warning_bounds <> []);
          let has_messages = has_errors#get || has_warnings#get in
          has_errors_or_warnings#set has_messages;
          if has_messages then (tview#build_gutter());
          tag_applied#call (tag_warning_bounds, tag_error_bounds);
        end;
      end;

    method private do_remove_tag tags =
      List.iter begin fun { Oe.ei_start; ei_stop; ei_error } ->
        let start = `MARK ei_start in
        let stop = `MARK ei_stop in
        let tag = self#tag_of_error ei_error in
        buffer#remove_tag tag ~start:(buffer#get_iter start) ~stop:(buffer#get_iter stop)#forward_line;
        buffer#delete_mark start;
        buffer#delete_mark stop;
      end tags;

    method remove_tag () =
      if has_errors_or_warnings#get then begin
        let had_messages = has_errors_or_warnings#get in
        self#do_remove_tag tag_error_bounds;
        self#do_remove_tag tag_warning_bounds;
        tag_error_bounds <- [];
        tag_warning_bounds <- [];
        has_errors#set false;
        has_warnings#set false;
        has_errors_or_warnings#set false;
        if had_messages then tag_removed#call();
      end

    method hide_tooltip ?(force=true) () =
      if force || not sticky_popup then begin
        List.iter (fun (_, _, w) -> w#destroy()) tag_popup;
        tag_popup <- [];
        sticky_popup <- false
      end

    method private filter_messages iter tags =
      match
        List.filter (fun { Oe.ei_start; ei_stop; _ } -> iter#equal (buffer#get_iter_at_mark (`MARK ei_start))) tags
      with
      | [] ->
          List.filter begin fun { Oe.ei_start; ei_stop; _ } ->
            iter#in_range ~start:(buffer#get_iter_at_mark (`MARK ei_start)) ~stop:(buffer#get_iter_at_mark (`MARK ei_stop))
          end tags
      | x -> x

    method tooltip ?(sticky=false) ?(need_focus=true) (location : [`ITER of GText.iter | `XY of int * int]) =
      if enabled && (not need_focus || view#has_focus) then begin
        let iter = match location with `XY (x, y) -> view#get_iter_at_location ~x ~y | `ITER it -> it in
        if not iter#ends_line then begin
          try
            (* If the tooltip is already displayed, take no action to prevent flickering. *)
            ignore (List.find begin fun (start, stop, _popup) ->
                try
                  let start = buffer#get_iter_at_mark (`MARK start) in
                  let stop = buffer#get_iter_at_mark (`MARK stop) in
                  iter#in_range ~start ~stop
                with Gmisclib.Util.Mark_deleted -> false
              end tag_popup);
            sticky_popup <- sticky;
          with Not_found -> begin
              try
                (*Prf.crono Prf.prf_error_indication_tooltip begin fun () ->*)
                (* Find message by iter *)
                let messages, border_color, bg_color =
                  match
                    (self#filter_messages iter tag_error_bounds),
                    ?? (Oe_config.error_popup_border_color), ?? (Oe_config.error_popup_bg_color)
                  with
                  | [], _, _ ->
                      if Oe_config.warning_tootip_enabled || not sticky then (raise Not_found);
                      (self#filter_messages iter tag_warning_bounds),
                      ?? (Oe_config.warning_popup_border_color), ?? (Oe_config.warning_popup_bg_color)
                  | x -> x
                in
                (* Create popup *)
                self#hide_tooltip();
                let create_popup start stop error displacement =
                  let markup = (*(error.Oe.er_location) ^*)
                    (Print_type.markup3 error.Oe.er_message) in
                  let label = GMisc.label ~markup ~xpad:5 ~ypad:5 (*~packing:ebox#add*) () in
                  let popup = Gtk_util.window_tooltip label#coerce ~parent:view#coerce ~x:0 ~y:0 () in
                  tag_popup <- (start, stop, popup) :: tag_popup;
                  sticky_popup <- sticky;
                  popup#misc#modify_bg [`NORMAL, border_color];
                  popup#child#misc#modify_bg [`NORMAL, bg_color];
                  label#misc#modify_font_by_name Preferences.preferences#get.editor_completion_font;
                  label#misc#modify_fg [`NORMAL, `BLACK];
                  (* Positioning *)
                  begin
                    popup#move ~x:(-1000) ~y:(-1000);
                    match location with
                    | `ITER _ ->
                        let x, y = view#get_location_at_iter iter in
                        popup#show();
                        let y = y - popup#misc#allocation.Gtk.height - 5 - displacement in
                        popup#move ~x ~y;
                    | `XY _ ->
                        let x, y = Gdk.Window.get_pointer_location (Window.root_window view) in
                        popup#show();
                        popup#move ~x ~y:(y - popup#misc#allocation.Gtk.height - 12 - displacement);
                  end;
                  let incr = if Preferences.preferences#get.editor_annot_type_tooltips_delay = 0 then 0.106 else 0.479 in
                  Gmisclib.Util.fade_window ~incr popup;
                  popup#misc#allocation.Gtk.height
                  (*end ()*)
                in
                messages
                |> Utils.ListExt.group_by (fun { Oe.ei_start; ei_stop; ei_error } ->
                    (buffer#get_iter_at_mark (`MARK ei_start))#offset, (buffer#get_iter_at_mark (`MARK ei_stop))#offset)
                |> List.iter begin fun ((start, stop), messages) ->
                  messages |>
                  List.fold_left begin fun displacement { Oe.ei_start; ei_stop; ei_error } ->
                    displacement + create_popup ei_start ei_stop ei_error displacement;
                  end 0 |> ignore
                end
              with Not_found -> (if not sticky_popup then (self#hide_tooltip()))
            end
        end else (if not sticky_popup then (self#hide_tooltip()));
      end

    method private draw_underline drawable top bottom x0 y0 offset = function
      | { Oe.ei_start; ei_stop; ei_error } when (not (is_warning_unused ei_error.Oe.er_level)) ->
          let start = ref (buffer#get_iter_at_mark (`MARK ei_start)) in
          let stop = buffer#get_iter_at_mark (`MARK ei_stop) in
          let stop = if bottom#compare stop < 0 then bottom else stop in
          let phase2 = phase * 2 in
          while !start#compare stop < 0 do
            begin
              try
                if top#compare !start <= 0 && !start#chars_in_line > 1 then begin
                  start := forward_non_blank !start;
                  let iter =
                    if !start#ends_line then raise Exit
                    else
                      let line_end = !start#forward_to_line_end in
                      if stop#compare line_end <= 0 then stop else line_end
                  in
                  let ys, h = view#get_line_yrange !start in
                  let y = ys + h - y0 in
                  let x = ref (Gdk.Rectangle.x (view#get_iter_location !start) - x0) in
                  let x2 = Gdk.Rectangle.x (view#get_iter_location iter) - x0 in
                  let yu = y - phase in
                  let segments = ref [] in
                  while !x <= x2 do
                    segments := (!x + phase, y + offset) :: (!x, yu + offset) :: !segments; x := !x + phase2;
                  done;
                  Cairo_drawable.lines drawable !segments;
                end;
              with Exit | Invalid_argument _ -> ()
            end;
            start := !start#forward_line
          done;
      | _ -> ()

    method private expose _ =
      if flag_underline then begin
        match view#get_window `TEXT with
        | Some window ->
            let drawable = Gdk.Cairo.create window in
            let vrect = view#visible_rect in
            let x0 = Gdk.Rectangle.x vrect in
            let y0 = Gdk.Rectangle.y vrect in
            (* Draw exposed area only *)
            let expose_area = Cairo.clip_extents drawable in
            let ya = y0 + int_of_float expose_area.Cairo.y in
            let top, _ = view#get_line_at_y ya in
            let bottom, _ = view#get_line_at_y (ya + (int_of_float expose_area.Cairo.h)) in
            (*  *)
            let width = 1 in
            set_line_attributes drawable ~width ~style:`SOLID ~join:`MITER ();
            let f = self#draw_underline drawable top bottom x0 y0 in
            (* Underline warnings *)
            set_foreground drawable (?? Oe_config.warning_underline_shadow);
            List.iter (f width) tag_warning_bounds;
            set_foreground drawable (?? Oe_config.warning_underline_color);
            List.iter (f 0) tag_warning_bounds;
            (* Underline errors *)
            begin
              match Oe_config.error_underline_mode with
              | `CUSTOM ->
                  set_foreground drawable (?? Oe_config.error_underline_shadow);
                  List.iter (f width) tag_error_bounds;
                  set_foreground drawable (?? Oe_config.error_underline_color);
                  List.iter (f 0) tag_error_bounds;
              | _ -> ()
            end;
            (*Gdk.GC.set_fill drawable#gc `SOLID;*)
            false
        | _ -> false
      end else false

    initializer
      self#set_phase();
      self#set_flag_underline flag_underline;
      (* Hide tooltip *)
      let unsticky _ = if sticky_popup then (self#hide_tooltip()); false in
      ignore (view#event#connect#focus_out ~callback:unsticky : GtkSignal.id);
      ignore (view#event#connect#button_press ~callback:unsticky : GtkSignal.id);
      ignore (view#event#connect#leave_notify ~callback:unsticky : GtkSignal.id);
      (* View: on expose draw underline for warnings *)
      let signal_expose =
        ref (view#misc#connect#after#draw ~callback:self#expose)
      in
      ignore (view#vadjustment#connect#value_changed ~callback:(fun () -> view#misc#handler_block !signal_expose));
      ignore (view#vadjustment#connect#after#value_changed ~callback:(fun () ->
          Gmisclib.Idle.add ~prio:300 (fun () -> view#misc#handler_unblock !signal_expose)));

    method connect = new signals ~tag_applied ~tag_removed

  end

and signals ~tag_applied ~tag_removed = object
  inherit GUtil.ml_signals [ tag_applied#disconnect; tag_removed#disconnect ]
  method tag_applied = tag_applied#connect ~after
  method tag_removed = tag_removed#connect ~after
end

and tag_applied () = object inherit [(Oe.error_indication list) * (Oe.error_indication list)] GUtil.signal () end
and tag_removed () = object inherit [unit] GUtil.signal () end
