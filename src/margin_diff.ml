open Margin
open Odiff
module ColorOps = Color
open Preferences
open Cairo_drawable

type diff_item =
  | Bar of { color : GDraw.color; y : int; height : int }
  | Triangle of { color : GDraw.color; y : int }

class widget view =
  let color_add =
    let sat, value = if Preferences.preferences#get.theme_is_dark then 0.2, 0.4 else 0.4, 0.2 in
    `NAME (ColorOps.modify (?? Oe_config.global_gutter_diff_color_add) ~sat ~value)
  in
  let color_del =
    let sat, value = if Preferences.preferences#get.theme_is_dark then 0.2, 0.4 else 0.4, 0.2 in
    `NAME (ColorOps.modify (?? Oe_config.global_gutter_diff_color_del) ~sat ~value)
  in
  let color_change = `NAME (?? Oe_config.global_gutter_diff_color_change) in
  let pad_left = 3 in
  let line_width = 1 in
  let filled = true in
  let area_width = 8 in
  let size = area_width + pad_left in
  let bar_width = area_width / 2 - line_width in
  let tri_extent = 0 (*area_width / 4*) in
  let tri_half_height = (area_width + tri_extent) / 2 in
  object (self)
    inherit [diff_item] Margin.widget ()
    val mutable diffs : Odiff.diffs = []
    val mutable color_base = `COLOR (view#misc#style#bg `NORMAL)
    method kind = DIFF
    method index = 20
    val mutable last_diff_time = view#tbuffer#last_edit_time
    method size = size
    method set_diffs x = diffs <- x
    method is_changed_after_last_diff = last_diff_time < view#tbuffer#last_edit_time
    method sync_diff_time () = last_diff_time <- Unix.gettimeofday()

    initializer
      Preferences.preferences#connect#changed ~callback:begin fun _ ->
        Gmisclib.Idle.add (fun () -> color_base <- `COLOR (view#misc#style#bg `NORMAL))
      end |> ignore;

    method build ~start ~stop =
      model <- [];
      let buffer = view#buffer in
      let process_item line_num item = model <- (line_num, item) :: model  in
      diffs |> List.iter begin function
      | Add (_, ind, _) ->
          self#build_bar buffer color_add ind process_item
      | Delete (_, ind, _) ->
          self#build_triangle buffer color_del ind process_item
      | Change (_, _, ind, _) ->
          self#build_bar buffer color_change ind process_item
      end

    method private build_bar buffer color index yield =
      match index with
      | One ln ->
          let iter = buffer#get_iter (`LINE (ln - 1)) in
          let y, height = view#get_line_yrange iter in
          yield ln (Bar { color; y; height })

      | Many (l1, l2) ->
          let iter1 = buffer#get_iter (`LINE (l1 - 1)) in
          let y1, _ = view#get_line_yrange iter1 in
          let iter2 = buffer#get_iter (`LINE (l2 - 1)) in
          let y2, height2 = view#get_line_yrange iter2 in
          let height = y2 + height2 - y1 in
          yield l1 (Bar { color; y = y1; height })

    method private build_triangle buffer color index yield =
      match index with
      | One ln ->
          let iter = buffer#get_iter (`LINE (ln - 1)) in
          let y, height = view#get_line_yrange iter in
          let y_center = y + height in
          yield ln (Triangle { color; y = y_center })
      | Many _ -> ()

    method draw_margin ~view ~drawable ~top:offset_top ~left ~height ~start ~stop =
      Prf.register Prf.draw_margin_diff begin fun () ->
        let x = left + pad_left in
        let x_bar = x + area_width - bar_width in
        let start_line = start#line + 1 in
        let stop_line = stop#line + 1 in
        model |> List.iter begin fun (ln, item) ->
          if start_line <= ln && ln <= stop_line then
            match item with
            | Bar { color; y; height } ->
                set_foreground drawable color;
                rectangle drawable ~x:x_bar ~y:(y - offset_top) ~width:bar_width ~height ~filled ()
            | Triangle { color; y } ->
                set_foreground drawable color;
                let y_rel = y - offset_top in
                polygon drawable ~filled [
                  x - tri_extent, y_rel - tri_half_height;
                  x - tri_extent, y_rel + tri_half_height;
                  x + area_width, y_rel
                ]
        end
      end ()

  end
