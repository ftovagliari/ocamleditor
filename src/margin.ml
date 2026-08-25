open Printf
open Gutter
module ColorOps = Color
open Preferences

type scope = Local | Global [@@deriving show]

type kind = FOLDING | LINE_NUMBERS | MARKERS | DIFF | GLOBAL_DIFF [@@deriving show]

let text_pixel_size ?layout font_desc text =
  let layout =
    match layout with
    | Some layout -> layout
    | None ->
        let surface = Cairo.Image.create Cairo.Image.ARGB32 ~w:1 ~h:1 in
        let dummy_cr = Cairo.create surface in
        Cairo_pango.create_layout dummy_cr
  in
  Pango.Layout.set_font_description layout font_desc;
  Pango.Layout.set_text layout text;
  let rect = Pango.Layout.get_pixel_extent layout in
  let width = rect.Pango.x + rect.Pango.width in
  let height = rect.Pango.y + rect.Pango.height in
  (width, height)

class virtual margin () =
  object (self)
    val mutable is_visible = true
    method virtual index : int
    method virtual scope : scope
    method virtual kind : kind
    method virtual color : string
    method virtual build : start:GText.iter -> stop:GText.iter -> unit
    method is_visible = is_visible
    method set_is_visible x = is_visible <- x
    method name = show_kind self#kind
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
  let font = "FiraCode OCamlEditor 12" in
  object
    inherit [(int * GObj.widget * int) list] widget ()
    val mutable size = 18
    val mutable size_extent = 0
    val mutable avail_width = 0
    val font_desc = Pango.Font.from_string font
    val layout = view#misc#create_pango_context#create_layout#as_layout

    method kind = MARKERS
    method scope = Local
    method color = "#500050"
    method index = 0
    method size = size
    method model = model
    method set_size_extent x = size_extent <- x

    method build ~start ~stop =
      avail_width <- size + size_extent;
      model <- [];
      let tmp_model = ref [] in
      gutter.markers |> List.iter begin fun mark ->
        mark.icon
        |> Option.iter begin fun (icon, color) ->
          Gmisclib.Util.get_iter_at_mark_opt view#buffer#as_buffer mark.mark
          |> Option.iter begin fun mark_iter ->
            let iter = new GText.iter mark_iter in
            let line = iter#line + 1 in
            let y, h = view#get_line_yrange iter in
            let icon_width, icon_height = text_pixel_size ~layout font_desc icon in
            let y = y + (h - icon_height) / 2 - 1 in
            match mark.icon_obj with
            | None ->
                let ebox = GBin.event_box ~show:false () in
                ebox#misc#set_property "visible-window" (`BOOL true);
                let _ = GMisc.label
                    ~xpad:0 ~ypad:0 ~xalign:0.0 ~width:icon_width
                    ~markup:(sprintf "<span face='%s' color='%s'>%s</span>" font color icon)
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
                tmp_model := (line, (y, child, icon_width)) :: !tmp_model
            | Some child ->
                tmp_model := (line, (y, child, icon_width)) :: !tmp_model
          end
        end
      end;
      model <- Utils.ListExt.group_assoc !tmp_model

    method draw_margin ~view ~drawable ~top ~left ~height ~start ~stop =
      Prf.register Prf.draw_margin_markers begin fun () ->
        let left = left + avail_width in
        model
        |> List.iter begin fun (ln, childs) ->
          let icons_width = List.fold_left (fun sum (_, _, w) -> w + sum) 0 childs in
          let n_childs = List.length childs in
          let offset =
            if icons_width > avail_width && n_childs > 1
            then (avail_width - icons_width) / (n_childs - 1) else 0
          in
          childs |> List.fold_left begin fun x (y, child, icon_width) ->
            child#misc#show();
            let x = x - icon_width in
            view#move_child ~child ~x ~y;
            x - offset
          end left |> ignore;
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
    val mutable font_family = "Monospace"
    val mutable font_size_px = 10.0
    val mutable digit_width = 0
    val layout = view#misc#create_pango_context#create_layout#as_layout

    method scope = Local
    method kind = LINE_NUMBERS
    method color = "#505000"
    method size = size
    method index = 10

    initializer
      font_family <- Pango.Font.get_family font_desc;
      let pango_size = Pango.Font.get_size font_desc in
      let is_absolute = Pango.Font.get_size_is_absolute font_desc in
      let size_pt = float pango_size /. float Pango.scale in
      font_size_px <-
        if is_absolute then size_pt
        else size_pt *. (96.0 /. 72.0);
      let max_digits = 4 in
      let width, _ = text_pixel_size ~layout font_desc (String.make max_digits '0') in
      size <- width;
      digit_width <- int_of_float (ceil (float size /. (float max_digits)));
      markers#set_size_extent size;

    method build ~start:iter ~stop =
      Prf.register Prf.build_margin_ln begin fun () ->
        model <- [];
        let buffer = view#buffer in
        let start_line = iter#line in
        let stop_line = stop#line in
        let marks_by_ln =
          markers#model
          |> Utils.ListExt.group_by (fun (ln, _) -> ln)
          |> List.filter_map (fun (ln, ms) -> if List.length ms > 0 then Some ln else None)
        in
        for line_idx = start_line to stop_line do
          let num = line_idx + 1 in
          if not (List.mem num marks_by_ln) then begin
            let line_iter = buffer#get_iter (`LINE line_idx) in
            let yl, _ = view#get_line_yrange line_iter in
            let y = yl + view#pixels_above_lines in
            model <- (num, y) :: model
          end
        done
      end ()

    method draw_margin ~view ~drawable ~top ~left ~height ~start ~stop =
      Prf.register Prf.draw_margin_ln begin fun () ->
        ColorOps.rgb (fun r g b -> Cairo.set_source_rgb drawable r g b) color;
        Cairo.select_font_face drawable font_family;
        Cairo.set_font_size drawable font_size_px;
        let font_ext = Cairo.font_extents drawable in
        let font_ascent = font_ext.Cairo.ascent in
        model |> List.iter begin fun (num, y) ->
          let s_num = string_of_int num in
          let text_width = String.length s_num * digit_width in
          let x = left + size - text_width - padding_right in (* right aligned *)
          let y = float (y - top) +. font_ascent in
          Cairo.move_to drawable (float x) y;
          Cairo.show_text drawable s_num;
        end
      end ()

  end

class container (view : GText.view) =
  let gutter = Gutter.create () in
  let padding_left = 3 in
  let padding_right = 0 in
  let [@inline] draw_childs childs ~top ~height ~start ~stop = function
    | (`LEFT | `RIGHT) as border ->
        begin
          match view#get_window border with
          | Some window ->
              childs
              |> List.fold_left begin fun left margin ->
                if margin#is_visible then begin
                  let drawable = Gdk.Cairo.create window in
                  (*Cairo_drawable.set_foreground drawable (`NAME margin#color);
                    Cairo_drawable.rectangle drawable ~x:left ~y:0 ~width:(margin#size) ~height ~filled:true ();*)
                  margin#draw_margin ~view ~drawable ~top ~left ~height ~start ~stop;
                  left + margin#size
                end else left
              end padding_left |> ignore;
          | _ -> ()
        end
    | _ -> ()
  in
  object (self)
    val mutable local_childs : margin list = []
    val mutable global_childs : margin list = []

    initializer
      view#misc#connect#after#draw ~callback:begin fun _ ->
        self#draw ();
        false
      end |> ignore;
      view#vadjustment#connect#value_changed ~callback:self#build |> ignore;

    method gutter = gutter

    method add margin =
      match margin#kind with
      | LINE_NUMBERS | MARKERS | DIFF | GLOBAL_DIFF ->
          begin
            let compare m1 m2 = Stdlib.compare m1#index m2#index in
            match margin#scope with
            | Local ->
                local_childs <- margin :: local_childs |> List.sort compare
            | Global ->
                global_childs <- margin :: global_childs |> List.sort compare
          end;
          let size =
            if local_childs = [] then 0 else
              local_childs |> List.fold_left (fun sum m -> sum + m#size) (padding_left + padding_right)
          in
          view#set_border_window_size ~typ:`LEFT ~size;
          let size =
            if global_childs = [] then 0 else
              global_childs |> List.fold_left (fun sum m -> sum + m#size) (padding_left + padding_right)
          in
          view#set_border_window_size ~typ:`RIGHT ~size;
          self#build ();
      | FOLDING -> ()

    method remove margin =
      match margin#scope with
      | Local -> local_childs <- local_childs |> List.filter ((<>) margin)
      | Global -> global_childs <- global_childs |> List.filter ((<>) margin)

    method get kind =
      match local_childs |> List.find_opt (fun child -> child#kind = kind) with
      | None -> global_childs |> List.find_opt (fun child -> child#kind = kind)
      | margin -> margin

    method build () =
      let vrect = view#visible_rect in
      let height = Gdk.Rectangle.height vrect in
      let top = Gdk.Rectangle.y vrect in
      let start, _ = view#get_line_at_y top in
      let stop, _ = view#get_line_at_y (top + height) in
      List.iter (fun child -> child#build ~start ~stop) local_childs;
      List.iter (fun child -> child#build ~start ~stop) global_childs

    method draw () =
      Prf.register Prf.draw_margins begin fun () ->
        let vrect = view#visible_rect in
        let height = Gdk.Rectangle.height vrect in
        let top = Gdk.Rectangle.y vrect in
        let start, _ = view#get_line_at_y top in
        let stop, _ = view#get_line_at_y (top + height) in
        draw_childs local_childs `LEFT ~top ~height ~start ~stop;
        draw_childs global_childs `RIGHT ~top ~height ~start ~stop
      end ();

  end
