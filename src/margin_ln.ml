open Margin
open Preferences

class line_numbers (view : GText.view) (markers : Margin_markers.markers) =
  let padding_left = 0 in
  let padding_right = 0 in
  let invisible_tag = ref None in
  let is_invisible_line line_num =
    match !invisible_tag with
    | Some tag ->
        let start_iter = view#buffer#get_iter (`LINE line_num) in
        let end_iter =
          let it = start_iter#copy in
          if not it#ends_line then ignore (it#forward_to_line_end);
          it
        in
        start_iter#has_tag tag || end_iter#has_tag tag (*|| List.mem tag start_iter#tags*)
    | _ -> false
  in
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
      view#misc#connect#map ~callback:begin fun () ->
        invisible_tag :=
          begin
            match GtkText.TagTable.lookup view#buffer#tag_table Oe_config.code_folding_tag_invisible_name with
            | None -> None
            | Some tag -> Some (new GText.tag tag);
          end;
      end |> ignore;
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
          if is_invisible_line line_idx then () else
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
