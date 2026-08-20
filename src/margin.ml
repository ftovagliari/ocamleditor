open Gutter
module ColorOps = Color
open Preferences

type kind = FOLDING | LINE_NUMBERS | MARKERS | DIFF [@@deriving show]

class virtual margin () =
  object (self)
    val mutable is_visible = true
    method virtual kind : kind
    method is_visible = is_visible
    method set_is_visible x = is_visible <- x
    method virtual size : int (* width of the margin in pixels *)
    method virtual index : int
    method virtual build : start:GText.iter -> stop:GText.iter -> unit

    (** Called whenever the margin needs to be redrawn.
        @param top The top edge of the margin area to be drawn, in buffer coordinates.
    *)
    method virtual draw_margin :
      view:GText.view ->
      top:int -> left:int -> height:int ->
      start:GText.iter -> stop:GText.iter -> unit
  end

class line_numbers (view : GText.view) =
  object (self)
    inherit margin ()
    val mutable size = 0
    val mutable color = ?? Oe_config.warning_unused_color
    val mutable font_desc = Pango.Font.from_string Preferences.preferences#get.Settings_j.editor_base_font
    val mutable numbers = []

    initializer () (* TODO: notify pref changes *)

    method kind = LINE_NUMBERS
    method size = size
    method index = 0

    method build ~(start : GText.iter) ~(stop : GText.iter) =
      numbers <- [];
      let iter = start#backward_line in
      let stop = stop#forward_line in
      let y = ref 0 in
      let h = ref 0 in
      let num = ref 0 in
      while not (iter#equal stop) do
        num := iter#line + 1;
        let yl, hl = view#get_line_yrange iter in
        y := yl (*- top*) + view#pixels_above_lines;
        h := hl;
        iter#nocopy#forward_line |> ignore; (* TODO Crashed here *)
        numbers <- (!num, !y) :: numbers
      done;

    method draw_margin ~view ~top ~left ~height ~start ~stop =
      match view#get_window `LEFT with
      | Some window ->
          (* TODO: Optimize without List.rev *)
          numbers |> List.rev |> List.iter begin fun (num, y) ->
            let drawable = Gdk.Cairo.create window in
            let layout = Cairo_pango.create_layout drawable in
            Pango.Layout.set_font_description layout font_desc;
            if size = 0 then begin
              Pango.Layout.set_text layout "0000";
              let rect = Pango.Layout.get_pixel_extent layout in
              size <- rect.Pango.width
            end;
            ColorOps.rgb (fun r g b -> Cairo.set_source_rgb drawable r g b) color;
            Pango.Layout.set_text layout (string_of_int num);
            let rect = Pango.Layout.get_pixel_extent layout in
            let x = size - rect.Pango.width in
            Cairo.move_to drawable (float x) (float (y - top));
            Cairo_pango.show_layout drawable layout;
          end;
      | _ -> ()

  end

class markers gutter margin_line_numbers =
  object (self)
    inherit margin ()
    val mutable positions = []
    val mutable size = 0 (* visible line number => size = 0; hidden => size > 0 *)
    method kind = MARKERS
    method icon_size = 15
    method index = 10
    method size = size
    method set_size x = size <- x

    method build ~start ~stop = ()

    method draw_margin ~view ~top ~left ~height ~start ~stop =
      let left = (if size = 0 then left else left + size) - self#icon_size in (* icon right aligned *)
      positions <- [];
      gutter.markers
      |> List.iter begin fun mark ->
        match mark.icon_pixbuf with
        | Some pixbuf ->
            begin
              match Gmisclib.Util.get_iter_at_mark_opt view#buffer#as_buffer mark.mark with
              | Some mark_iter ->
                  let ym, h = view#get_line_yrange (new GText.iter mark_iter) in
                  let y = ym - top in
                  (*margin_line_numbers#hide_label (y + view#pixels_above_lines);*)
                  let y = y + (h - self#icon_size) / 2 in
                  begin
                    match mark.icon_obj with
                    | None ->
                        let ebox = GBin.event_box () in
                        ebox#misc#set_property "visible-window" (`BOOL false);
                        let _ = GMisc.image ~pixbuf ~packing:ebox#add () in
                        Gaux.may mark.callback ~f:begin fun callback ->
                          ebox#event#connect#enter_notify ~callback:begin fun ev ->
                            let window = GdkEvent.get_window ev in
                            Gdk.Window.set_cursor window (Gdk.Cursor.create `HAND2);
                            true
                          end |> ignore;
                          ebox#event#connect#leave_notify ~callback:begin fun ev ->
                            let window = GdkEvent.get_window ev in
                            Gdk.Window.set_cursor window (Gdk.Cursor.create `ARROW);
                            true
                          end |> ignore;
                          ebox#event#connect#button_press ~callback:begin fun _ ->
                            view#misc#grab_focus();
                            callback mark.mark
                          end
                        end;
                        let child = ebox#coerce in
                        view#add_child_in_window ~child ~which_window:`LEFT ~x:left ~y;
                        positions <- (y, child) :: positions;
                        mark.icon_obj <- Some child;
                    | Some child ->
                        view#move_child ~child ~x:left ~y;
                        positions <- (y, child) :: positions;
                  end;
              | _ -> ()
            end;
        | _ -> ()
      end;
      (* Spread markers *)
      positions |> Utils.ListExt.group_assoc
      |> List.iter begin fun (y, childs) ->
        childs
        |> List.fold_left begin fun x child ->
          view#move_child ~child ~x ~y;
          x - (if self#size = 0 then self#icon_size - 4 else 0 )
        end left |> ignore
      end
  end

class container (view : GText.view) =
  let gutter = Gutter.create () in
  let left_spacing = 5 in
  let right_spacing = 0 in
  object (self)
    val update = new update
    val mutable childs : margin list = []
    val mutable width = 0
    val mutable approx_char_width = 0

    (*initializer*)
    (*view#misc#connect#realize ~callback:begin fun _ ->
      self#draw_margin_container();
      end |> ignore;*)
    (*view#set_border_window_size ~typ:`TOP ~size:50;*)
    (*view#buffer#connect#changed ~callback:begin fun () ->
      Printf.printf "************8\n%!" ;
      self#test ();
      end |> ignore;*)
    (*view#event#connect#button_press ~callback:begin fun _ ->
      (*self#test ();*)
      false
      end |> ignore;
      view#misc#connect#after#draw ~callback:begin fun cr ->
      self#test ();
      false
      end |> ignore;*)

    (*method private test drawable =
      match view#get_window `TOP with
      | Some window ->
          let open Cairo_drawable in
          let drawable = GDraw.Cairo.create window in
          let x = 0 and y = 0 in
          let x1, y1 = view#buffer_to_window_coords ~x ~y ~tag:`TOP in
          let x2, y2 = view#window_to_buffer_coords ~x ~y ~tag:`TOP in
          Printf.printf "----> test %d %d -- %d %d\n%!" x1 y1 x2 y2;
          set_foreground drawable (`NAME "red");
          rectangle drawable ~x ~y ~width:10 ~height:10 ();
          set_foreground drawable (`NAME "green");
          rectangle drawable ~x:x1 ~y:y1 ~width:10 ~height:10 ();
          set_foreground drawable (`NAME "blue");
          rectangle drawable ~x:x2 ~y:y2 ~width:10 ~height:10 ();
      | _ -> ()*)

    method gutter = gutter
    method approx_char_width = approx_char_width

    method add margin =
      match margin#kind with
      | LINE_NUMBERS ->
          (childs <- margin :: childs |> List.sort (fun m1 m2 -> Stdlib.compare m1#index m2#index))
      | _ -> ()

    method remove margin = childs <- childs |> List.filter ((<>) margin)
    method list = childs

    method build () =
      if view#visible then begin (* TODO: Lablgtk3 issue, check is_realized, not visible *)
        let vrect = view#visible_rect in
        let height = Gdk.Rectangle.height vrect in
        let top = Gdk.Rectangle.y vrect in
        let start, _ = view#get_line_at_y top in
        let stop, _ = view#get_line_at_y (top + height) in
        List.iter (fun child -> child#build ~start ~stop) childs
      end

    method draw () =
      (* Check `REALIZED to avoid caching line numbers without parent. *)
      if view#visible then begin (* TODO: Lablgtk3 issue, check is_realized, not visible *)
        Prf.register Prf.draw_margins begin fun () ->
          let vrect = view#visible_rect in
          let height = Gdk.Rectangle.height vrect in
          let top = Gdk.Rectangle.y vrect in
          let start, _ = view#get_line_at_y top in
          let stop, _ = view#get_line_at_y (top + height) in
          view#set_border_window_size ~typ:`LEFT ~size:(max 50 gutter.size); (* dummy initial size *)
          let size =
            childs
            |> List.fold_left begin fun left margin ->
              if margin#is_visible then begin
                margin#draw_margin ~view ~top ~left ~height ~start ~stop;
                left + margin#size
              end else left
            end left_spacing
          in
          (* TODO Optimize. There is no need to resize with every draw *)
          let size = size + right_spacing in
          gutter.size <- size;
          view#set_border_window_size ~typ:`LEFT ~size;
          approx_char_width <- GPango.to_pixels (view#misc#pango_context#get_metrics())#approx_digit_width;
        end ();
        update#call ();
      end

    method connect = new container_signals ~update
  end

and container_signals ~update = object
  inherit GUtil.ml_signals [ update#disconnect ]
  method update = update#connect ~after
end

and update = object inherit [unit] GUtil.signal () end

