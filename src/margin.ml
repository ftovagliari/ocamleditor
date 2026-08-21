open Printf
open Gutter
module ColorOps = Color
open Preferences

type kind = FOLDING | LINE_NUMBERS | MARKERS | DIFF [@@deriving show]
class virtual margin () =
  object
    val mutable is_visible = true
    method virtual index : int
    method virtual kind : kind
    method virtual build : start:GText.iter -> stop:GText.iter -> unit
    method is_visible = is_visible
    method set_is_visible x = is_visible <- x
    method virtual size : int (* width of the margin in pixels *)
    (** Called whenever the margin needs to be redrawn.
        @param top The top edge of the margin area to be drawn, in buffer coordinates.
    *)
    method virtual draw_margin :
      view:GText.view ->
      drawable:Gdk.cairo ->
      top:int -> left:int -> height:int ->
      start:GText.iter -> stop:GText.iter -> unit
  end

class virtual ['a] widget () =
  object
    inherit margin ()
    val mutable model : (int * ' a) list = []
  end

class markers gutter (view : GText.view) =
  let icon_size = 12 in
  let size_px = icon_size * 1024 * 72 / 96 in
  object
    inherit [int * GObj.widget] widget ()
    val mutable size = icon_size * 3 / 2
    val mutable size_extent = 0
    method kind = MARKERS
    method index = 0
    method size = size
    method model = model
    method set_size_extent x = size_extent <- x

    method build ~start ~stop =
      model <- [];
      gutter.markers |> List.iter begin fun mark ->
        mark.icon
        |> Option.iter begin fun icon ->
          Gmisclib.Util.get_iter_at_mark_opt view#buffer#as_buffer mark.mark
          |> Option.iter begin fun mark_iter ->
            let iter = new GText.iter mark_iter in
            let line = iter#line + 1 in
            let y, h = view#get_line_yrange iter in
            let y = y + (h - icon_size) / 2 - 1 in
            match mark.icon_obj with
            | None ->
                let ebox = GBin.event_box ~show:false () in
                ebox#misc#set_property "visible-window" (`BOOL true);
                let _ = GMisc.label
                    ~xpad:0 ~ypad:0
                    ~markup:(sprintf "<span size='%d' face='FiraCode OCamlEditor'>%s</span>" size_px icon)
                    ~packing:ebox#add ()
                in
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
                mark.icon_obj <- Some child;
                view#add_child_in_window ~child ~which_window:`LEFT ~x:0 ~y;
                model <- (line, (y, child)) :: model;
            | Some child ->
                model <- (line, (y, child)) :: model;
          end
        end
      end

    method draw_margin ~view ~drawable ~top ~left ~height ~start ~stop =
      Prf.register Prf.draw_margin_markers begin fun () ->
        let left = left + size + size_extent - icon_size in (* icon right aligned *)
        model
        |> Utils.ListExt.group_assoc
        |> List.iter begin fun (_, childs) ->
          let displacement = min (icon_size * 4 / (List.length childs)) (icon_size + 2) in
          childs |> List.fold_left begin fun x (y, child) ->
            child#misc#show();
            view#move_child ~child ~x ~y;
            x - displacement
          end left |> ignore
        end
      end ()
  end

class line_numbers (view : GText.view) (markers : markers) =
  let padding_left = 0 in
  let padding_right = 0 in
  object (self)
    inherit [int] widget ()
    val mutable size = 0
    val mutable color = ?? Oe_config.warning_unused_color
    val mutable font_desc = Pango.Font.from_string Preferences.preferences#get.Settings_j.editor_base_font

    method kind = LINE_NUMBERS
    method size = size
    method index = 10

    initializer
      view#misc#connect#realize ~callback:begin fun () ->
        match view#get_window `LEFT with
        | Some window ->
            let drawable = Gdk.Cairo.create window in
            let layout = Cairo_pango.create_layout drawable in
            Pango.Layout.set_font_description layout font_desc;
            ColorOps.rgb (fun r g b -> Cairo.set_source_rgb drawable r g b) color;
            Pango.Layout.set_text layout "0000";
            let rect = Pango.Layout.get_pixel_extent layout in
            size <- rect.Pango.width + padding_right;
            markers#set_size_extent size;
        | _ -> ()
      end |> ignore;

    method build ~start:iter ~stop =
      model <- [];
      let buffer = view#buffer in
      let start_line = iter#line in
      let stop_line = stop#line in
      for line_idx = start_line to stop_line do
        let num = line_idx + 1 in
        if not (List.exists (fun (ln, _) -> ln = num) markers#model) then begin
          let line_iter = buffer#get_iter (`LINE line_idx) in
          let yl, _ = view#get_line_yrange line_iter in
          let y = yl + view#pixels_above_lines in
          model <- (num, y) :: model
        end
      done

    method draw_margin ~view ~drawable ~top ~left ~height ~start ~stop =
      Prf.register Prf.draw_margin_ln begin fun () ->
        let layout = Cairo_pango.create_layout drawable in
        Pango.Layout.set_font_description layout font_desc;
        ColorOps.rgb (fun r g b -> Cairo.set_source_rgb drawable r g b) color;
        Pango.Layout.set_text layout "0";
        let digit_width = (Pango.Layout.get_pixel_extent layout).Pango.width in
        model |> List.iter begin fun (num, y) ->
          let s_num = string_of_int num in
          let text_width = String.length s_num * digit_width in
          let x = left + size - text_width - padding_right in (* right aligned *)
          let y = y - top in
          Pango.Layout.set_text layout (string_of_int num);
          Cairo.move_to drawable (float x) (float y);
          Cairo_pango.show_layout drawable layout;
        end
      end ()

  end

class container (view : GText.view) =
  let gutter = Gutter.create () in
  let padding_left = 5 in
  let padding_right = 0 in
  object (self)
    val update = new update
    val mutable childs : margin list = []
    val mutable width = 0
    val mutable approx_char_width = 0

    initializer
      view#misc#connect#realize ~callback:begin fun () ->
        view#set_border_window_size ~typ:`LEFT ~size:(max 50 gutter.size); (* dummy initial size *)
        self#build ();
      end |> ignore;
      view#misc#connect#after#draw ~callback:begin fun _ ->
        self#draw ();
        false
      end |> ignore;
      view#vadjustment#connect#value_changed ~callback:self#build |> ignore;

    method gutter = gutter
    method approx_char_width = approx_char_width

    method add margin =
      match margin#kind with
      | LINE_NUMBERS | MARKERS | DIFF ->
          (childs <- margin :: childs |> List.sort (fun m1 m2 -> Stdlib.compare m1#index m2#index))
      | _ -> ()

    method remove margin = childs <- childs |> List.filter ((<>) margin)
    method list = childs

    method build () =
      let vrect = view#visible_rect in
      let height = Gdk.Rectangle.height vrect in
      let top = Gdk.Rectangle.y vrect in
      let start, _ = view#get_line_at_y top in
      let stop, _ = view#get_line_at_y (top + height) in
      List.iter (fun child -> child#build ~start ~stop) childs

    method draw () =
      Prf.register Prf.draw_margins begin fun () ->
        match view#get_window `LEFT with
        | Some window ->
            let vrect = view#visible_rect in
            let height = Gdk.Rectangle.height vrect in
            let top = Gdk.Rectangle.y vrect in
            let start, _ = view#get_line_at_y top in
            let stop, _ = view#get_line_at_y (top + height) in
            let size =
              childs
              |> List.fold_left begin fun left margin ->
                if margin#is_visible then begin
                  let drawable = Gdk.Cairo.create window in
                  margin#draw_margin ~view ~drawable ~top ~left ~height ~start ~stop;
                  left + margin#size
                end else left
              end padding_left
            in
            (* TODO Optimize. There is no need to resize with every draw *)
            let size = size + padding_right in
            gutter.size <- size;
            view#set_border_window_size ~typ:`LEFT ~size;
            approx_char_width <- GPango.to_pixels (view#misc#pango_context#get_metrics())#approx_digit_width;
        | _ -> ()
      end ();
      update#call ();

    method connect = new container_signals ~update
  end

and container_signals ~update = object
  inherit GUtil.ml_signals [ update#disconnect ]
  method update = update#connect ~after
end

and update = object inherit [unit] GUtil.signal () end

