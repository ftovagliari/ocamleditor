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
open Text_util
open Utils
module ColorOps = Color
open Preferences

type bound = Gtk.text_mark * Gtk.text_mark

(** Buffer *)
class buffer =
  let word_bound = Utils.regexp "\\b" in
  fun ?project ?buffer ?file () ->
    let buffer = match buffer with None -> GText.buffer () | Some b -> b in
    let create_tmp_filename () = Filename.temp_file "buffer-" ".tmp", None in
    (*  let tmp_filename, project_tmp_path =
        match project with
          | Some project ->
            begin
              match file with
                | Some file ->
                  begin
                    match Project.tmp_of_abs project file#filename with
                      | None -> create_tmp_filename ()
                      | (Some (tmp, relname) as temp) -> tmp // relname, temp
                  end;
                | _ -> create_tmp_filename ()
            end;
          | _ -> create_tmp_filename ()
        in*)
    object (self)
      inherit GText.buffer buffer#as_buffer

      val undo = Gmisclib.Text.undo_manager ~buffer
      val mutable tab_width = 2
      val mutable tab_spaces = true
      val mutable signal_handlers = []
      val mutable tag_ocamldoc_paragraph : GText.tag option = None
      val mutable tmp_filename = "" (*tmp_filename*)
      val mutable orig_filename = ""
      val mutable project_tmp_path = None (*project_tmp_path*)
      val mutable last_edit_time = Unix.gettimeofday()

      initializer
        buffer#connect#changed ~callback:(fun () -> last_edit_time <- Unix.gettimeofday()) |> ignore;
        self#reset_tmp_filename ();
        orig_filename <- tmp_filename ^ ".orig";
        at_exit begin fun () ->
          match project with
          | None ->
              if Sys.file_exists orig_filename then Sys.remove orig_filename;
              if Sys.file_exists tmp_filename then Sys.remove tmp_filename
          | _ -> ()
        end

      method tmp_filename = tmp_filename
      method orig_filename = orig_filename
      method last_edit_time = last_edit_time
      method set_last_edit_time time = last_edit_time <- time

      method reset_tmp_filename () =
        let a, b =
          match project with
          | Some project ->
              begin
                match file with
                | Some file ->
                    begin
                      match Project.tmp_of_abs project file#filename with
                      | None -> create_tmp_filename ()
                      | (Some (tmp, relname) as temp) -> tmp // relname, temp
                    end;
                | _ -> create_tmp_filename ()
              end;
          | _ -> create_tmp_filename ()
        in
        tmp_filename <- a;
        project_tmp_path <- b


      method tag_ocamldoc_paragraph = tag_ocamldoc_paragraph

      method init_tags ?tags ?colors
          ?ocamldoc_paragraph_enabled
          ?ocamldoc_paragraph_bgcolor_1
          ?ocamldoc_paragraph_bgcolor_2 () =
        Lexical.init_tags ?tags ?colors
          ?ocamldoc_paragraph_enabled
          ?ocamldoc_paragraph_bgcolor_1
          ?ocamldoc_paragraph_bgcolor_2 self#as_gtext_buffer;
        let table = new GText.tag_table self#tag_table in
        tag_ocamldoc_paragraph <- Gaux.may_map ~f:(new GText.tag) (table#lookup "ocamldoc-paragraph");

      method as_gtext_buffer = (self :> GText.buffer)

      method undo = undo

      method add_signal_handler id = signal_handlers <- id :: signal_handlers
      method remove_signal_handler id = signal_handlers <- List.filter (fun x -> x != id) signal_handlers
      method block_signal_handlers () =
        List.iter (GtkSignal.handler_block buffer#as_buffer) signal_handlers;
      method unblock_signal_handlers () =
        List.iter (GtkSignal.handler_unblock buffer#as_buffer) signal_handlers;

      method tab_width = tab_width
      method set_tab_width x = tab_width <- x

      method tab_spaces = tab_spaces
      method set_tab_spaces x = tab_spaces <- x

      method file : Editor_file.file option = file

      method select_all () = self#select_range self#start_iter self#end_iter

      (* Deprecated: use get_iter_at_mark_opt *)
      method get_iter_at_mark mark =
        new GText.iter (Gmisclib.Util.get_iter_at_mark_safe self#as_buffer (self#get_mark mark))

      method get_iter_at_mark_opt mark =
        match Gmisclib.Util.get_iter_at_mark_opt self#as_buffer (self#get_mark mark) with
        | Some i -> Some (new GText.iter i)
        | _ -> None

      method select_marks ~start ~stop =
        self#select_range (self#get_iter_at_mark stop) (self#get_iter_at_mark start);

      method selection_text () =
        let start, stop = self#selection_bounds in
        self#get_text ~start ~stop ()

      method get_line_at_iter (it : GText.iter) =
        self#get_text ~start:(it#set_line_index 0) ~stop:it#forward_line ()

      method indent = Alignment.indent ~buffer:self

      method select_word ?(iter=(self#get_iter `INSERT)) ?(pat=word_bound) ?(select=true) ?(search=true) () =
        let line = self#get_line_at_iter iter in
        let pos = iter#line_index in
        let start = try Str.search_backward pat line pos with Not_found -> 0 in
        if start = pos && pos > 0 then begin
          if Glib.Unichar.isspace iter#backward_char#char then begin
            if search then
              self#select_word ~iter:iter#backward_char ~pat ~select ~search ()
            else (iter, iter)
          end else self#select_word ~iter:iter#backward_char ~pat ~select ~search ()
        end else begin
          let start = if start = 0 then start else start + 1 in
          let stop = try Str.search_forward pat line pos with Not_found -> Glib.Utf8.length line in
          let start = iter#set_line_index start in
          let stop = iter#set_line_index stop in
          if select then self#select_range start stop;
          (start, stop)
        end

      method toggle_case () =
        undo#begin_block ~name:"toggle_case";
        let start, stop = self#get_iter `SEL_BOUND, self#get_iter `INSERT in
        let text = self#get_text ~start ~stop () in
        let uppercase = Glib.Utf8.uppercase text in
        let text = if text = uppercase then Glib.Utf8.lowercase text else uppercase in
        self#delete ~start ~stop;
        self#insert text;
        let stop = self#get_iter `INSERT in
        self#select_range (stop#backward_chars (Glib.Utf8.length text)) stop;
        undo#end_block ();

      method save_buffer ?(filename=tmp_filename) () =
        let text = buffer#get_text () in
        Utils.mkdir_p (Filename.dirname filename);
        let chan = open_out_bin filename in
        begin
          try
            output_string chan text;
            close_out chan;
            filename, project_tmp_path
          with ex -> (close_out chan; raise ex)
        end;
    end

(** View *)
and view ?project ?buffer () =
  let buffer = match buffer with None -> new buffer ?project () | Some b -> b in
  let view = GText.view ~buffer:buffer#as_gtext_buffer () in
  let create_highlight_current_line_tag () =
    buffer#create_tag ~name:(sprintf "highlight_current_line_tag_%f" (Unix.gettimeofday())) []
  in
  let margin = new Margin.container view in
  let margin_line_numbers = new Margin.line_numbers view in
  let margin_markers = new Margin.markers margin#gutter margin_line_numbers in
  object (self)
    inherit GText.view view#as_view as super

    val mutable options = new Text_options.options ()
    val mutable tbuffer = buffer
    val mutable hadjustment : GData.adjustment option = None
    val mutable vadjustment : GData.adjustment option = None
    val mutable prev_line_background = 0
    val mutable highlight_current_line_tag = create_highlight_current_line_tag ()
    val mutable current_matching_tag_bounds = []
    val mutable text_outline = ([] : [ `Ref of bound | `Word of bound | `Delim of bound ] list)
    val mutable approx_char_width = 0
    val visible_height = new GUtil.variable 0
    val mutable signal_expose : GtkSignal.id option = None
    val hyperlink = Gmisclib.Text.hyperlink ~view ()
    val mutable signal_id_highlight_current_line = None
    val mutable mark_occurrences_manager = None
    val mutable current_line_border_x1 = 0
    val mutable current_line_border_x2 = 0

    method set_current_line_border_x1 x = current_line_border_x1 <- x

    method set_current_line_border_x2 x = current_line_border_x2 <- x

    method project = project

    method mark_occurrences_manager = match mark_occurrences_manager with Some x -> x | _ -> assert false

    method as_gtext_view = (self :> GText.view)
    method tbuffer = buffer

    method options = options

    method hyperlink = hyperlink

    method signal_expose = signal_expose
    method gutter = margin#gutter (* Legacy *)
    method margin = margin

    method hadjustment = hadjustment
    method vadjustment = vadjustment

    method set_buffer buf =
      let tbuf = new buffer ~buffer:buf () in
      tbuffer <- tbuf;
      super#set_buffer buf

    method approx_char_width = approx_char_width

    method add_outline_text bounds = text_outline <- List.rev_append text_outline bounds
    method filter_outline_text filter = text_outline <- List.filter filter text_outline;

    method modify_font fontname =
      self#misc#modify_font_by_name fontname;
      approx_char_width <- GPango.to_pixels (self#misc#pango_context#get_metrics())#approx_digit_width;
      Gmisclib.Idle.add self#draw_gutter

    method create_highlight_current_line_tag () =
      let tag_table = new GText.tag_table buffer#tag_table in
      tag_table#remove highlight_current_line_tag#as_tag;
      highlight_current_line_tag <- create_highlight_current_line_tag();
      self#options#set_highlight_current_line options#highlight_current_line

    method highlight_current_line_tag = highlight_current_line_tag

    method scroll_lazy iter =
      Gmisclib.Idle.add ~prio:300 begin fun () ->
        (* TODO Crashed here
           Gtk-CRITICAL **: 10:34:26.095: IA__gtk_text_buffer_remove_tag: assertion 'gtk_text_iter_get_buffer (start) == buffer' failed
           #5  0x00007f78ae7f4721 in gtk_text_view_scroll_to_iter () at /lib/x86_64-linux-gnu/libgtk-x11-2.0.so.0
           #6  0x00005650c6c5fe1e in ml_gtk_text_view_scroll_to_iter ()
           #7  0x00005650c6ca6d1b in <signal handler called> ()
           #8  0x00005650c6b6866c in camlGText.fun_inner_5511 ()
           #9  0x00005650c665e811 in camlText.fun_4581 () at text.ml:278 (self#scroll_to_iter)
        *)
        self#scroll_to_iter ~use_align:(self#scroll_to_iter iter) ~xalign:1.0 ~yalign:0.38 iter |> ignore;
        Gmisclib.Idle.add ~prio:200 begin fun () ->
          self#draw_gutter();
          GtkBase.Widget.queue_draw self#as_widget;
        end
      end;

    method scroll_iter_onscreen iter =
      self#scroll_to_iter ~within_margin:0.1 iter |> ignore;

    method scroll dir =
      let rect = self#visible_rect in
      let it = match dir with
        | `UP ->
            let y = Gdk.Rectangle.y rect in
            let it, _ = self#get_line_at_y y in
            it#backward_lines 2;
        | `DOWN ->
            let y = Gdk.Rectangle.y rect + Gdk.Rectangle.height rect + 2 in (* 2 = pixels below ocamldoc paragraphs *)
            let it, _ = self#get_line_at_y y in
            it#forward_line
      in
      ignore (self#scroll_to_iter it);
      self#place_cursor_onscreen() |> ignore;
      Gmisclib.Idle.add ~prio:300 (fun () -> GtkBase.Widget.queue_draw self#as_widget)

    method goto () = Dialog_goto.show ~view:self ()

    method private matching_delim_apply_tag (text : string) (offset : int) = (*(None : (int * int * int * int) option)*)
      match Delimiters.find_match ~utf8:false text offset with
      | None -> None
      | Some (lstart, lstop, rstart, rstop) as delim when lstop <> rstart ->
          let lstart = self#buffer#get_iter (`OFFSET lstart) in
          let lstop = self#buffer#get_iter (`OFFSET lstop) in
          let rstart = self#buffer#get_iter (`OFFSET rstart) in
          let rstop = self#buffer#get_iter (`OFFSET rstop) in
          (* To satisfy code folding, if the right delim. is not visible
             we do not even draw the background of the left delim.
             See also "Border around matching delimiters" *)
          let rtext = rstart#get_visible_text ~stop:rstop in
          if String.length rtext > 0 then begin
            current_matching_tag_bounds <-
              ((self#buffer#create_mark ~name:"delim_left_start" lstart),
               (self#buffer#create_mark ~name:"delim_left_stop" lstop)) :: [];
            self#buffer#apply_tag_by_name "tag_matching_delim" ~start:lstart ~stop:lstop;
            current_matching_tag_bounds <-
              ((self#buffer#create_mark ~name:"delim_right_start" rstart),
               (self#buffer#create_mark ~name:"delim_right_stop" rstop)) :: current_matching_tag_bounds;
            self#buffer#apply_tag_by_name "tag_matching_delim" ~start:rstart ~stop:rstop;
            self#add_outline_text (current_matching_tag_bounds |> List.map (fun x -> `Delim x));
            GtkBase.Widget.queue_draw self#as_widget;
            delim
          end else None
      | Some (_, lstop, rstart, _) (*as delim*) when lstop = rstart ->
          ignore (self#innermost_enclosing_delim text lstop);
          None
      | _ -> None

    method matching_delim_remove_tag () =
      List.iter begin function (start, stop) ->
        let mstart = (`MARK start) in
        let mstop = (`MARK stop) in
        let start = self#buffer#get_iter_at_mark mstart in
        let stop = self#buffer#get_iter_at_mark mstop in
        self#buffer#remove_tag_by_name "tag_matching_delim" ~start ~stop;
        self#buffer#delete_mark mstart;
        self#buffer#delete_mark mstop;
      end current_matching_tag_bounds;
      text_outline <- text_outline |> List.filter (function `Word _ -> true | `Ref _ -> true | `Delim _ -> false);
      current_matching_tag_bounds <- [];

    method matching_delim () =
      self#matching_delim_remove_tag();
      let pos = self#buffer#get_iter `INSERT in
      let start = pos#backward_chars 5 in
      let stop = pos#forward_chars 5 in
      let text55 = self#buffer#get_text ~start ~stop () in
      let text = self#buffer#get_text () in
      if Delimiters.is_delimiter ~utf8:false text55 (pos#offset - start#offset) then begin
        ignore (self#matching_delim_apply_tag text pos#offset)
      end else begin
        try
          ignore (self#innermost_enclosing_delim text pos#offset);
        with ex -> eprintf "%s\n%!" (Printexc.to_string ex);
      end

    method matching_delim_goto ?(select=false) ?(strict=true) () =
      match self#buffer#selection_bounds with
      | b1, b2 when (not (b1#equal b2)) ->
          self#buffer#place_cursor ~where:b1;
          self#matching_delim_goto ~select ~strict:false ()
      (*let b1 = if List.mem b1#char blanks then
        b1#forward_find_char not_blank else b1 in
        let b2 = if List.mem b2#char blanks then
        (b2#backward_find_char not_blank)#forward_to_line_end else b2 in
        self#buffer#select_range b1 b2*)
      | _ -> begin
          match current_matching_tag_bounds with
          | [(rstart, rstop); (lstart, lstop)] ->
              let ins = self#buffer#get_iter `INSERT in
              let right_start = self#buffer#get_iter_at_mark (`MARK rstart) in
              let left_stop = self#buffer#get_iter_at_mark (`MARK lstop) in
              if select then begin
                let start =
                  if strict then begin
                    let start = left_stop#forward_find_char not_blank in
                    let start = start#set_line_index 0 in
                    if start#compare left_stop <= 0 then left_stop else start
                  end else (self#buffer#get_iter_at_mark (`MARK lstart))
                in
                let stop =
                  if strict then begin
                    let stop = right_start#backward_find_char not_blank in
                    let stop = stop#forward_line in
                    if stop#compare right_start <= 0 then stop else right_start
                  end else (self#buffer#get_iter_at_mark (`MARK rstop))
                in
                self#buffer#select_range start stop;
              end else begin
                (*let right_stop = Gtk_util.get_iter_mark self#buffer rstop in
                  let left_start = Gtk_util.get_iter_mark self#buffer lstart in*)
                let right_start, left_stop = if right_start#compare left_stop <= 0
                  then (right_start, left_stop) else (left_stop, right_start) in
                let left_is_nearest = abs (ins#offset - right_start#offset) > abs (ins#offset - left_stop#offset)
                in
                if left_is_nearest
                then (self#buffer#place_cursor ~where:right_start)
                else (self#buffer#place_cursor ~where:left_stop);
                ignore (self#scroll_to_mark `INSERT);
              end
          | _ -> ()
        end

    method private innermost_enclosing_delim (text : string) (offset : int) = (*(None : (int * int * int * int) option)*)
      (*Prf.crono Prf.innermost_enclosing_delim (fun () ->*)
      match Delimiters.find_innermost_enclosing_delim ~utf8:false text offset with
      | ((start, stop) :: _) (*as stack*)  ->
          if stop = offset
          then (self#innermost_enclosing_delim text start)
          else (self#matching_delim_apply_tag text start);
      | _ -> None (* ) () *)

    method current_matching_tag_bounds = current_matching_tag_bounds

    method get_location_at_cursor () =
      let iter = self#buffer#get_iter `INSERT in
      let rect_it = self#get_iter_location iter in
      let rect_vis = self#visible_rect in
      let win = (match self#get_window `WIDGET
                 with None -> failwith "Text.text#get_location_at_cursor `WIDGET = None" | Some w -> w) in
      let x, y = Gdk.Window.get_position win in
      let win = Gdk.Window.get_parent win in
      let x1, y1 = Gdk.Window.get_position win in
      let vxs, vys = x + x1, y + y1 in
      let xb, yb = Gdk.Rectangle.x rect_it, Gdk.Rectangle.y rect_it in
      let vxb, vyb = Gdk.Rectangle.x rect_vis, Gdk.Rectangle.y rect_vis in
      (vxs + xb - vxb + (self#get_border_window_size `LEFT)),
      (vys + yb - vyb + Gdk.Rectangle.height rect_it)

    method get_location_at_iter iter =
      let rect = view#get_iter_location iter in
      let x = Gdk.Rectangle.x rect in
      let y = Gdk.Rectangle.y rect in
      let x0, y0 =
        let pX, pY = Gdk.Window.get_pointer_location (Gdk.Window.root_parent ()) in
        let win = (match view#get_window `TEXT with None -> assert false | Some w -> w) in
        let px, py = Gdk.Window.get_pointer_location win in
        (pX - px), (pY - py)
      in
      let vrect = view#visible_rect in
      let x = x - Gdk.Rectangle.x vrect in
      let y = y - Gdk.Rectangle.y vrect in
      let x = x0 + x in
      let y = y0 + y in
      x, y

    method get_scroll_top () =
      let vrect = self#visible_rect in
      let y0 = Gdk.Rectangle.y vrect in
      let start, _ = self#get_line_at_y y0 in
      start#offset

    method draw_current_line_background ?(force=false) (ins : GText.iter) =
      let cur_line = ins#line in
      if cur_line <> prev_line_background || ins#ends_line || force then begin
        let prev = self#buffer#get_iter (`LINE prev_line_background) in
        let prev_0 = prev#set_line_offset 0 in
        self#buffer#remove_tag highlight_current_line_tag
          ~start:prev_0#backward_line ~stop:prev#forward_line#forward_to_line_end;
        let ins_0 = ins#set_line_offset 0 in
        let ins_1 = ins_0#backward_char in
        let start = if ins_1#starts_line then ins_0 else ins_1 in
        let stop = ins_0#forward_char in
        self#buffer#apply_tag highlight_current_line_tag ~start ~stop;
        prev_line_background <- cur_line;
      end

    method draw_gutter () = (* 0.008 *) margin#draw ();

    method private expose drawable ev =
      try
        let vrect       = self#visible_rect in
        let h0          = Gdk.Rectangle.height vrect in
        let w0          = Gdk.Rectangle.width vrect in
        let y0          = Gdk.Rectangle.y vrect in
        let start, _    = self#get_line_at_y y0 in
        let stop, _     = self#get_line_at_y (y0 + h0) in
        visible_height#set h0;
        (* Expose area *)
        let expose_area = GdkEvent.Expose.area ev in
        let expose_y    = y0 + Gdk.Rectangle.y expose_area in
        let expose_top, _ = self#get_line_at_y expose_y in
        let expose_bottom, _ = self#get_line_at_y (expose_y + (Gdk.Rectangle.height expose_area)) in
        (*  *)
        let adjust      = Oe_config.current_line_border_adjust in
        let hadjust     = match hadjustment with Some adj -> int_of_float adj#value - self#left_margin | _ -> 0 in
        (* Indentation guidelines *)
        if options#show_indent_lines && not options#show_whitespace_chars
        then (Text_indent_lines.draw_indent_lines self drawable) start stop y0;
        (* Right margin line *)
        begin
          match options#visible_right_margin with
          | Some (column, color) ->
              let x = approx_char_width * column - hadjust - 1 in (* -1 per evitare sovrapposizione col cursore *)
              drawable#set_line_attributes ~style:`SOLID ();
              drawable#set_foreground color;
              drawable#line ~x ~y:0 ~x ~y:h0;
          | _ -> ()
        end;
        (* ocamldoc_paragraph_bgcolor_enabled *)
        if Oe_config.ocamldoc_paragraph_border_enabled
        then (self#draw_paragraph_border drawable start stop y0 w0);
        (* Whitespace characters *)
        if options#show_whitespace_chars then begin
          let iter        = ref expose_top in
          let pango       = self#misc#pango_context in
          let layout      = pango#create_layout in
          let draw iter text =
            let rect = self#get_iter_location iter in
            let x = Gdk.Rectangle.x rect - hadjust in
            let y = Gdk.Rectangle.y rect - y0 in
            Pango.Layout.set_text layout text;
            drawable#put_layout ~x ~y ~fore:options#base_color layout;
            drawable#put_layout ~x ~y ~fore:options#indent_lines_color_solid layout;
          in
          while !iter#compare expose_bottom < 0 do
            let line_num = !iter#line in
            while !iter#line = line_num do
              let char = !iter#char in
              begin
                match char with
                | 32 ->
                    let start = !iter in
                    let pos = start#line_index in
                    iter := !iter#forward_find_char not_blank;
                    let len = !iter#line_index - pos in
                    if len > 0 then
                      draw start (create_middot_string len);
                | 13 -> draw !iter whitespace_crlf
                | 9 -> draw !iter whitespace_tab
                | _ when !iter#ends_line -> draw !iter whitespace_lf
                | _ -> ()
              end;
              iter := !iter#forward_char
            done;
          done
        end;
        (* Dot leaders *)
        if options#show_dot_leaders && not options#show_whitespace_chars then begin
          (*Prf.crono Prf.prf_draw_dot_leaders begin fun () ->*)
          Gdk.GC.set_fill drawable#gc `SOLID;
          drawable#set_line_attributes ~width:1 ~style:Oe_config.dash_style ();
          drawable#set_foreground options#text_color;
          let offset = self#left_margin - hadjust in
          Alignment.iter ~start:expose_top ~stop:expose_bottom begin fun _ _ start stop _ ->
            let start = start#forward_char in
            let len = stop#line_index - start#line_index in
            if len > 2 then begin
              let x1 = approx_char_width * start#line_index + offset in
              let x2 = approx_char_width * (stop#line_index - 1) + offset in
              let y, h = self#get_line_yrange start in
              let y = y - y0 + h - 3 (*(min 3 (h / 5))*) in
              (*Gdk.GC.set_dashes drawable#gc ~offset:(x2 - 6 (*- x1*)) [1; approx_char_width - 1];*)
              Gdk.GC.set_dashes drawable#gc ~offset:(x2 - approx_char_width - 2) [1; approx_char_width - 1];
              drawable#line ~x:x1 ~y ~x:x2 ~y;
            end
          end
          (*end;*)
        end (*()*);
        (* Current line border *)
        begin
          if self#misc#get_flag `HAS_FOCUS && options#current_line_border_enabled then begin
            match options#highlight_current_line with
            | Some _ ->
                let iter = buffer#get_iter `INSERT in
                let y, h = view#get_line_yrange iter in
                let y = y - y0 in
                if iter#equal buffer#end_iter && iter#line_index = 0 then begin
                  (* Fix for draw_current_line_background *)
                  drawable#set_foreground options#current_line_bg_color;
                  drawable#rectangle ~x:self#left_margin ~y ~filled:true ~width:w0 ~height:h ();
                end;
                drawable#set_line_attributes ~join:Oe_config.current_line_join ~width:Oe_config.current_line_width ~style:Oe_config.current_line_style ();
                drawable#set_foreground options#current_line_border_color;
                drawable#rectangle ~x:current_line_border_x1 ~y ~filled:false
                  ~width:(w0 - current_line_border_x2) ~height:(h - adjust) ();
            | _ -> ()
          end;
        end;
        (* Border around matching delimiters *)
        text_outline |> List.iter (Text_outline.draw self drawable approx_char_width hadjust y0);
        false;
      with ex ->
        Printf.eprintf "File \"text.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());
        false

    method private draw_paragraph_border drawable start stop y0 w0 =
      match buffer#tag_ocamldoc_paragraph with
      | Some tag ->
          begin
            let start = ref (start#set_line_index 0) in
            let stop = stop#forward_line#set_line_index 0 in
            match ?? (Preferences.preferences#get.editor_ocamldoc_paragraph_bgcolor_1) with
            | Some color ->
                drawable#set_foreground (*(`NAME "red")*) (`NAME (ColorOps.modify color ~sat:0.1 ~value:0.1));
                drawable#set_line_attributes ~width:1 ~style:`SOLID ();
                let hadjust = match hadjustment with Some adj -> int_of_float adj#value | _ -> 0 in
                while !start#forward_line#compare stop <= 0 && not (!start#equal self#buffer#end_iter) do
                  if !start#has_tag tag then begin
                    let x = 0 - hadjust + self#left_margin in
                    let y1, _ = view#get_line_yrange !start in
                    let y = y1 - y0 in
                    let width = w0 + hadjust - self#left_margin - 2 in
                    let stop_tag = !start#forward_to_tag_toggle (Some tag) in
                    let y2, h = view#get_line_yrange stop_tag in
                    let height = y2 - y0 + h - y in
                    drawable#rectangle ~x ~y ~width ~height ();
                    start := stop_tag#set_line_index 0;
                  end;
                  start := !start#forward_line;
                done
            | _ -> ()
          end
      | _ -> ()

    initializer
      view#event#add [`FOCUS_CHANGE];
      margin#add (margin_line_numbers :> Margin.margin);
      margin#add (margin_markers :> Margin.margin);
      margin#connect#update ~callback:(fun () -> approx_char_width <- margin#approx_char_width) |> ignore;
      view#misc#connect#style_set ~callback:begin fun () ->
        let fd = self#misc#pango_context#font_description in
        margin_line_numbers#resize ~desc:fd ();
        (* Applies the new font size to labels that have been created after
           the number of lines of text has increased. *)
        Gmisclib.Idle.add ~prio:300 begin fun () ->
          margin_line_numbers#resize ~desc:fd ();
          Gmisclib.Idle.add self#draw_gutter
        end;
      end |> ignore;
      ignore (options#connect#mark_occurrences_changed ~callback:(fun _ -> self#mark_occurrences_manager#mark()));
      ignore (options#connect#after#mark_occurrences_changed ~callback:begin function
        | true, _, color ->
            self#mark_occurrences_manager#tag#set_property (`BACKGROUND_GDK (GDraw.color (`NAME color)));
        | _ -> ()
        end);
      ignore (options#connect#after#line_numbers_changed ~callback:begin fun visible ->
          margin_line_numbers#set_is_visible visible;
          margin_markers#set_size (if visible then 0 else margin_markers#icon_size);
          margin_line_numbers#reset();
          Gmisclib.Idle.add self#draw_gutter
        end);
      ignore (options#connect#line_numbers_font_changed ~callback:begin fun fontname ->
          margin_line_numbers#resize ~desc:(GPango.font_description fontname) ()
        end);
      options#set_line_numbers_font view#misc#pango_context#font_name;
      ignore (options#connect#after#show_markers_changed ~callback:(fun _ ->
          Gmisclib.Idle.add self#draw_gutter));
      ignore (options#connect#word_wrap_changed ~callback:(fun x -> self#set_wrap_mode (if x then `WORD else `NONE)));
      ignore (options#connect#after#highlight_current_line_changed ~callback:begin fun x ->
          prev_line_background <- 0;
          match x with
          | None ->
              Gaux.may signal_id_highlight_current_line ~f:begin fun id ->
                self#tbuffer#remove_signal_handler id;
                GtkSignal.disconnect self#tbuffer#as_buffer id;
                signal_id_highlight_current_line <- None
              end
          | Some (fg_color, bg_color) ->
              options#set_current_line_bg_color (`NAME bg_color);
              options#set_current_line_border_color (`NAME fg_color);
              Gmisclib.Util.set_tag_paragraph_background highlight_current_line_tag bg_color;
              let id = self#buffer#connect#mark_set ~callback:begin fun iter mark ->
                  match GtkText.Mark.get_name mark with
                  | Some name when name = "insert" -> self#draw_current_line_background iter
                  | _ -> ()
                end in
              signal_id_highlight_current_line <- Some id;
              self#tbuffer#add_signal_handler id;
              self#draw_current_line_background ~force:true (self#buffer#get_iter `INSERT)
        end);
      Text_init.key_press ?project self;
      Text_init.select_lines_from_gutter self;
      (** Margin and line spacings *)
      (* To avoid strange application crash, avoid to draw the border of
         matching delimiters when we are in the middle of an insert_text event.
         This is done by setting current_matching_tag_bounds_draw to [], still
         keeping marks in current_matching_tag_bounds to be used for syntax
         coloring after the insert_text event. *)
      buffer#connect#insert_text ~callback:(fun _ _ -> text_outline <- []) |> ignore;
      (** Expose *)
      view#misc#connect#realize ~callback:begin fun () ->
        match view#get_window `TEXT with
        | Some window ->
            let drawable = new GDraw.drawable window in
            signal_expose <- Some (self#event#connect#after#expose ~callback:(self#expose drawable));
        | _ -> failwith "realize"
      end |> ignore;
      (*  *)
      ignore (visible_height#connect#changed ~callback:(fun _ -> self#draw_gutter()));
      (** Refresh gutter and right margin line when scrolling *)
      ignore (self#connect#set_scroll_adjustments ~callback:begin fun h v ->
          hadjustment <- h;
          vadjustment <- v;
          match h, v with
          | (Some h), (Some v) ->
              (* Redraw the entire window on horizontal scroll to refresh right margin *)
              h#connect#after#value_changed
                ~callback:(fun () -> GtkBase.Widget.queue_draw self#as_widget) |> ignore;
              (* Update gutter on vertical scroll changed *)
              v#connect#value_changed
                ~callback:begin fun () ->
                  Gmisclib.Idle.add begin fun () ->
                    self#draw_gutter();
                    GtkBase.Widget.queue_draw self#as_widget;
                  end |> ignore;
                end |> ignore;
          | _ -> ()
        end);
      (** Fix bug in draw_current_line_background *)
      let before, after =
        let old_mark_occurrences = ref None in
        let mark = ref None in
        begin fun ~name ->
          old_mark_occurrences := Some self#options#mark_occurrences;
          self#options#set_mark_occurrences (false, false, "");
          mark := Some (buffer#create_mark(* ~name:(Gtk_util.create_mark_name "Text.initializer")*) (buffer#get_iter `INSERT))
        end,
        begin fun ~name ->
          Option.iter self#options#set_mark_occurrences !old_mark_occurrences;
          match !mark with
          | Some m ->
              let iter = ref (buffer#get_iter_at_mark (`MARK m)) in
              let stop = buffer#get_iter `INSERT in
              while !iter#compare buffer#end_iter < 0 && !iter#compare stop <= 0 do
                self#draw_current_line_background ~force:true !iter;
                iter := !iter#forward_line;
              done;
              buffer#delete_mark (`MARK m);
              mark := None
          | _ -> ()
        end
      in
      ignore (buffer#undo#connect#undo ~callback:before);
      ignore (buffer#undo#connect#after#undo ~callback:after);
      ignore (buffer#undo#connect#redo ~callback:before);
      ignore (buffer#undo#connect#after#redo ~callback:after);
      Gmisclib.Idle.add (fun () -> self#draw_current_line_background ~force:true (buffer#get_iter `INSERT));
      mark_occurrences_manager <- Some (new Mark_occurrences.manager ~view:self);
  end
