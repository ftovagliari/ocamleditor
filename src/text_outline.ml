open Preferences
open Cairo_drawable

open Preferences
open Cairo_drawable

let draw view (drawable : Gdk.cairo) metrics approx_char_width hadjust y0 bounds =
  let buffer = view#tbuffer in
  let (start, stop), color, line_width =
    match bounds with
    | `Ref x -> x, ?? Oe_config.ref_border_color, 1
    | `Word x -> x, ?? Oe_config.word_border_color, 0
    | `Delim x -> x, ?? Oe_config.matching_delim_border_color, 2
  in
  match buffer#get_iter_at_mark_opt (`MARK start) with
  | Some start_iter ->
      begin
        match buffer#get_iter_at_mark_opt (`MARK stop) with
        | Some stop_iter when start_iter#compare stop_iter < 0 ->
            (* Usa le coordinate native di GtkTextView invece dei cicli di avanzamento caratteri *)
            let rect_start = view#get_iter_location start_iter in
            let rect_stop = view#get_iter_location stop_iter in

            let x = Gdk.Rectangle.x rect_start - hadjust - 1 in
            let y = Gdk.Rectangle.y rect_start - y0 + view#pixels_above_lines in
            let width = (Gdk.Rectangle.x rect_stop) - (Gdk.Rectangle.x rect_start) in
            let height = (metrics#ascent + metrics#descent) / Pango.scale - 1 in

            if width > 0 then begin
              set_foreground drawable color;
              set_line_attributes drawable ~width:line_width ();
              rectangle drawable ~x ~y ~width ~height ();
            end
        | _ -> ()
      end
  | _ -> ()
