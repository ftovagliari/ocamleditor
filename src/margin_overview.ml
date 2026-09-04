open Margin
module ColorOps = Color
open Preferences

[@@@warning "-42"]

type rect = {
  width : int;
  height : int;
  color : GDraw.color;
  filled : bool;
}

type wave = {
  num : int;
  height : int;
  color : GDraw.color;
}

type marker = Rect of rect | Wave of wave | Cursor_indicator of rect | Viewport_indicator

type item = { x : int; marker : marker; offset : int }

let size_errors = 21
let size_occurrences = 13
let size_current_line = 0

class virtual base (view : GText.view) =
  object (self)
    inherit [item] Margin.widget ()
    val buffer = view#buffer

    method color = "#500050"
    method scope = Global
    method build ~start ~stop = ()

    method draw_margin ~view ~drawable ~top:offset_top ~left ~height ~start ~stop =
      Cairo_drawable.set_line_attributes drawable ~width:1 ();
      let line_count = buffer#line_count in
      model
      |> List.iter begin fun (y, { x; marker; offset }) ->
        let y = float y /. float line_count *. float height |> int_of_float in
        match marker with
        | Rect r | Cursor_indicator r ->
            let x = x + left in
            Cairo_drawable.set_foreground drawable r.color;
            Cairo_drawable.rectangle drawable ~x ~y ~width:r.width ~height:r.height ~filled:r.filled ();
        | Wave s ->
            let lines = ref [] in
            let i = ref 0 in
            let h = s.height in
            while !i < s.num do
              lines := (left + (!i+1) * h, y + h/2) :: (left + !i * h, y - h/2) :: !lines;
              incr i; incr i;
            done;
            Cairo_drawable.set_foreground drawable s.color;
            Cairo_drawable.lines drawable !lines
        | Viewport_indicator ->
            let start, _ = view#get_line_at_y offset_top in
            let stop, _ = view#get_line_at_y (offset_top + height) in
            let width = size_current_line + Margin_diff.size + size_errors + size_occurrences - 4 in
            let x = left - width - 2 in
            let scale = float height /. float line_count in
            let y = float (start#line + 1) *. scale |> int_of_float in
            let height = float (stop#line - start#line) *. scale |> int_of_float in
            let color = if Preferences.preferences#get.Settings_j.theme_is_dark then `WHITE else `BLACK in
            Cairo_drawable.set_foreground_a drawable color 0.1;
            Cairo_drawable.rectangle drawable ~x ~y ~width ~height ~filled:true ();

      end

    (** Finds the buffer iterator for the model element nearest to the given
        overview ruler Y-coordinate, within an 8-pixel click tolerance. *)
    method! find_nearest_iter_at_y y =
      let ruler_h = float_of_int (view#misc#allocation).Gtk.height in
      let buffer_h = view#vadjustment#upper in
      if ruler_h <= 0.0 || buffer_h <= 0.0 then None
      else
        let max_dist_px = 8.0 in
        let scale = ruler_h /. buffer_h in
        let step best (_line, { offset; _ }) =
          let it = buffer#get_iter (`OFFSET offset) in
          let yi, hi = view#get_line_yrange it in
          let item_y = (float yi +. float hi /. 2.) *. scale in
          let dist = abs_float (y -. item_y) in
          if dist <= max_dist_px then
            match best with
            | None -> Some (dist, offset)
            | Some (min_dist, _) when dist < min_dist -> Some (dist, offset)
            | Some _ -> best
          else
            best
        in
        match List.fold_left step None model with
        | Some (_, best_offset) -> Some (buffer#get_iter (`OFFSET best_offset))
        | None -> None

  end

class errors (view : GText.view) =
  let is_warning_unused = function [@warning "-4"]
    | Oe.Warning (20, _) | Oe.Warning (26, _) | Oe.Warning (27, _) -> true
    | _ -> false
  in
  object (self)
    inherit base view

    method kind = ERRORS
    method index = 10
    method size = size_errors

    method build_errors ~(warnings : Oe.error_indication list) ~(errors : Oe.error_indication list) =
      model <-
        warnings @ errors
        |> List.map begin fun { Oe.ei_start; ei_stop; ei_error } ->
          let start = buffer#get_iter_at_mark (`MARK ei_start) in
          let color =
            match ei_error.Oe.er_level with
            | Oe.Error | Oe.Alert _ -> ?? Oe_config.error_underline_color
            |  Oe.Warning _ -> ?? Oe_config.warning_popup_border_color
          in
          let marker =
            if is_warning_unused ei_error.Oe.er_level then
              let height = 2 in
              Wave { num = size_errors / height; height; color = `NAME (?? Oe_config.warning_unused_color) }
            else
              Rect { width = size_errors; height = 3; color; filled = true }
          in
          (start#line, { x = 0; marker; offset = start#offset })
        end;

  end

class occurrences (view : GText.view) =
  object (self)
    inherit base view

    method kind = OCCURRENCES
    method index = 20
    method size = size_occurrences

    method build_occurrences ~(words : (GText.mark * GText.mark) list) ~(refs : (GText.mark * GText.mark) list) =
      let visible_lines_before = float in
      model <-
        (words |> List.map begin fun (start, stop) ->
            let start = buffer#get_iter_at_mark start in
            let color = ?? (Preferences.preferences#get.Settings_j.editor_mark_occurrences_bg_color) in
            let factor = if Preferences.preferences#get.theme_is_dark then -0.23 else 0.13 in
            let color = `NAME (ColorOps.add_value color ~sfact:0.75 factor) in
            let marker = Rect { width = size_occurrences * 2; height = 3; color; filled = false } in
            (start#line, { x = -size_occurrences; marker; offset = start#offset })
          end)
        @
        (refs |> List.map begin fun (start, stop) ->
            let start = buffer#get_iter_at_mark start in
            let color = ?? Oe_config.ref_bg_color in
            let marker = Rect { width = size_occurrences; height = 3; color; filled = true } in
            (start#line, { x = 0; marker; offset = start#offset })
          end);

  end

class current_line (view : GText.view) =
  object (self)
    inherit base view

    method kind = CURRENT_LINE
    method index = 30
    method size = size_current_line

    method build_current_line iter =
      let color = if Preferences.preferences#get.Settings_j.theme_is_dark then `WHITE else `BLACK in
      model <-
        begin
          let width = size_current_line + (*Margin_diff.size +*) size_errors + size_occurrences in
          let marker = Cursor_indicator { width; height = 4; color; filled = false } in
          (iter#line, { x = -width; marker; offset = iter#offset })
        end ::
        (0, { x = 0; marker = Viewport_indicator; offset = 0 }) ::
        (model |> List.filter (function [@warning "-4"]
             | (_, { marker=Cursor_indicator _; _ }) | (_, { marker=Viewport_indicator; _ }) -> false
             | _ -> true))

    initializer
      buffer#connect#mark_set ~callback:begin fun iter mark ->
        match GtkText.Mark.get_name mark with
        | Some "insert" -> self#build_current_line iter
        | _ -> ()
      end |> ignore;

  end
