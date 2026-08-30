open Margin
open Odiff
module ColorOps = Color
open Preferences
open Cairo_drawable

type diff_item =
  | Bar of { color : GDraw.color; y : int; height : int }
  | Triangle of { color : GDraw.color; y : int }

let size = 13 

class local (view : Text.view) =
  let color_add =
    let sat, value = if Preferences.preferences#get.theme_is_dark then 0.2, 0.4 else 0.4, 0.2 in
    `NAME (ColorOps.modify (?? Oe_config.global_gutter_diff_color_add) ~sat ~value)
  in
  let color_del =
    let sat, value = if Preferences.preferences#get.theme_is_dark then 0.2, 0.4 else 0.4, 0.2 in
    `NAME (ColorOps.modify (?? Oe_config.global_gutter_diff_color_del) ~sat ~value)
  in
  let color_change = `NAME (?? Oe_config.global_gutter_diff_color_change) in
  object (self)
    inherit [int * diff_item] Margin.widget ()
    val mutable diffs : Odiff.diffs = []
    val mutable color_base = `COLOR (view#misc#style#bg `NORMAL)
    val mutable last_diff_time = view#tbuffer#last_edit_time
    val bar_width = size / 2
    val tri_half_height =size / 2
    val filled = true
    val size = size

    method scope = Local
    method kind = DIFF
    method color = "#005050"
    method index = 20
    method size = size
    method set_diffs x =
      Prf.register Prf.set_margin_diff (fun () ->
          diffs <- x) ()
    method is_changed_after_last_diff = last_diff_time < view#tbuffer#last_edit_time
    method sync_diff_time () = last_diff_time <- Unix.gettimeofday()

    initializer
      Preferences.preferences#connect#changed ~callback:begin fun _ ->
        Gmisclib.Idle.add (fun () -> color_base <- `COLOR (view#misc#style#bg `NORMAL))
      end |> ignore;

    method build ~start ~stop =
      Prf.register Prf.build_margin_diff begin fun () ->
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
      end ()

    method private build_bar buffer color index yield =
      match index with
      | One ln ->
          let iter = buffer#get_iter (`LINE (ln - 1)) in
          let y, height = view#get_line_yrange iter in
          yield ln (ln, Bar { color; y; height })
      | Many (l1, l2) ->
          let iter1 = buffer#get_iter (`LINE (l1 - 1)) in
          let y1, _ = view#get_line_yrange iter1 in
          let iter2 = buffer#get_iter (`LINE (l2 - 1)) in
          let y2, height2 = view#get_line_yrange iter2 in
          let height = y2 + height2 - y1 in
          yield l1 (l2, Bar { color; y = y1; height })

    method private build_triangle buffer color index yield =
      match index with
      | One ln ->
          let iter = buffer#get_iter (`LINE (ln - 1)) in
          let y, height = view#get_line_yrange iter in
          let y_center = y + height in
          yield ln (ln, Triangle { color; y = y_center })
      | Many _ -> ()

    method draw_items drawable x offset_top height start_line stop_line =
      model |> List.iter begin fun (l1, (l2, item)) ->
        if start_line <= l1 && l1 <= stop_line || start_line <= l2 && l2 <= stop_line then
          self#draw_item drawable item x height offset_top
      end

    method draw_item drawable item x height offset_top =
      match item with
      | Bar { color; y; height } ->
          set_foreground drawable color;
          let x = x + size - bar_width in
          rectangle drawable ~x ~y:(y - offset_top) ~width:bar_width ~height ~filled ()
      | Triangle { color; y } ->
          set_foreground drawable color;
          let y_rel = y - offset_top in
          polygon drawable ~filled [
            x, y_rel - tri_half_height;
            x, y_rel + tri_half_height;
            x + size, y_rel
          ]

    method draw_margin ~view ~drawable ~top:offset_top ~left:x ~height ~start ~stop =
      Prf.register Prf.draw_margin_diff begin fun () ->
        let start_line = start#line + 1 in
        let stop_line = stop#line + 1 in
        self#draw_items drawable x offset_top height start_line stop_line
      end ()

  end

class global (view : Text.view) =
  object (self)
    inherit local view
    method! scope = Global
    method! kind = GLOBAL_DIFF
    method! index = 0

    method! draw_items drawable x offset_top height start_line stop_line =
      model |> List.iter (fun (l1, (l2, item)) -> self#draw_item drawable item x height offset_top)

    method! draw_item drawable item x height offset_top =
      let full_height = view#vadjustment#upper in
      let height = float height in
      match item with
      | Bar { color; y; height=bar_height } ->
          set_foreground drawable color;
          let bar_height = float bar_height in
          let y = int_of_float (float y /. full_height *. height) in
          let bar_height = max 1. (bar_height /. full_height *. height) in
          rectangle drawable ~x ~y ~width:bar_width ~height:(int_of_float bar_height) ~filled ()
      | Triangle { color; y } ->
          set_foreground drawable color;
          let y = int_of_float (float y /. full_height *. height) in
          polygon drawable ~filled [
            x + size, y - tri_half_height;
            x + size, y + tri_half_height;
            x, y
          ]

  end
