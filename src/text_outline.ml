open Preferences
open Cairo_drawable

let draw view (drawable : Gdk.cairo) approx_char_width hadjust y0 bounds =
  let buffer = view#tbuffer in
  let (start, stop), color, line_width =
    match bounds with
    | `Ref x -> x, ?? Oe_config.ref_border_color, 1
    | `Word x -> x, ?? Oe_config.word_border_color, 0
    | `Delim x -> x, ?? Oe_config.matching_delim_border_color, 2
  in
  match buffer#get_iter_at_mark_opt (`MARK start) with
  | Some start ->
      begin
        match buffer#get_iter_at_mark_opt (`MARK stop) with
        | Some stop when start#get_visible_text ~stop |> String.length > 0 ->
            let yl1, hl1 = view#get_line_yrange start in
            let yl1 = yl1 - y0 in
            (* count_displayed_lines *)
            let iter = ref (stop#set_line_index 0) in
            let x_chars = ref 0 in
            let lines_displayed = ref 1 in (* n-th display-line where "start" lies, counting from 1 *)
            if !iter#compare start <= 0 then begin (* `Refs are not always accurate  *)
              while !iter#compare start <> 0 do
                if !iter#char = 9 then (x_chars := ((!x_chars / 8) * 8 + 8)) else (incr x_chars);
                iter := !iter#forward_char;
                if view#starts_display_line !iter then (x_chars := 0; incr lines_displayed)
              done;
              (* count how many display-lines constitute the line *)
              let n_display_lines = ref !lines_displayed in
              let i = stop#copy in
              while view#forward_display_line i && i#line = stop#line do
                incr n_display_lines
              done;
              (*  *)
              let x = approx_char_width * !x_chars - hadjust - 1 in (* -1 per evitare sovrapposizione col cursore *)
              let width_chars = stop#line_index - start#line_index in
              let width = approx_char_width * width_chars in
              let pango = view#misc#pango_context in
              let metrics = pango#get_metrics ?desc:None ?lang:None () in
              let height = (metrics#ascent + metrics#descent) / Pango.scale -  1 in
              let y =
                if !lines_displayed > 1 (*0 ?*)
                then yl1 + ((!lines_displayed - 1) * (hl1 / !n_display_lines))
                else yl1 + view#pixels_above_lines
              in
              set_foreground drawable color;
              set_line_attributes drawable ~width:line_width ();
              rectangle drawable ~x ~y ~width ~height ();
            end
        | _ -> ()
      end
  | _ -> ()

