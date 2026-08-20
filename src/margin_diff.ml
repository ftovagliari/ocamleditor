open Margin
open Odiff
module ColorOps = Color
open Preferences
open Cairo_drawable

class widget view =
  let drawing_area = GMisc.drawing_area () in
  let color_add =
    let sat, value = if Preferences.preferences#get.theme_is_dark then 0.2, 0.4 else 0.4, 0.2 in
    `NAME (ColorOps.modify (?? Oe_config.global_gutter_diff_color_add) ~sat ~value)
  in
  let color_del =
    let sat, value = if Preferences.preferences#get.theme_is_dark then 0.2, 0.4 else 0.4, 0.2 in
    `NAME (ColorOps.modify (?? Oe_config.global_gutter_diff_color_del) ~sat ~value)
  in
  let color_change = `NAME (?? Oe_config.global_gutter_diff_color_change) in
  let spacing = 5 in
  let line_width = 1 in
  let filled = true in
  let size = 13 in
  let area_width = size - spacing in
  let width = area_width - line_width in
  object (self)
    inherit margin ()
    val mutable diffs : Odiff.diffs = []
    val mutable color_base = `COLOR (view#misc#style#bg `NORMAL)
    val mutable start_line = 0
    val mutable stop_line = 0
    val mutable top = 0
    val mutable height = 0
    method kind = DIFF
    method index = 20
    val mutable last_diff_time = view#tbuffer#last_edit_time
    method size = size
    method set_diffs x = diffs <- x
    method is_changed_after_last_diff = last_diff_time < view#tbuffer#last_edit_time
    method sync_diff_time () = last_diff_time <- Unix.gettimeofday()

    initializer
      view#add_child_in_window ~child:drawing_area#coerce ~which_window:`LEFT ~x:0 ~y:0;
      Preferences.preferences#connect#changed ~callback:begin fun _ ->
        Gmisclib.Idle.add (fun () -> color_base <- `COLOR (view#misc#style#bg `NORMAL))
      end |> ignore;
      drawing_area#misc#connect#draw ~callback:begin fun drawable ->
        set_background drawable color_base;
        set_foreground drawable color_base;
        set_line_attributes drawable ~width:line_width ();
        rectangle drawable ~x:0 ~y:0 ~width:area_width ~height ~filled:true ();
        diffs
        |> List.iter begin function
        | Add (_, ind, _) ->
            set_foreground drawable color_add;
            self#draw_bar drawable ind
        | Delete (_, ind, _) ->
            set_foreground drawable color_del;
            self#draw_triangle drawable ind
        | Change (_, _, ind, _) ->
            set_foreground drawable color_change;
            self#draw_bar drawable ind
        end;
        false
      end |> ignore

    method build ~start ~stop = ()

    method draw_bar drawable = function
      | One ln when start_line <= ln && ln <= stop_line ->
          let iter = view#buffer#get_iter (`LINE (ln - 1)) in
          let y, height = view#get_line_yrange iter in
          rectangle drawable ~x:0 ~y:(y - top) ~width ~height ~filled ()
      | Many (l1, l2) when l1 <= stop_line && l2 >= start_line ->
          let iter1 = view#buffer#get_iter (`LINE (l1 - 1)) in
          let y1, _ = view#get_line_yrange iter1 in
          let iter2 = view#buffer#get_iter (`LINE (l2 - 1)) in
          let y2, height2 = view#get_line_yrange iter2 in
          rectangle drawable ~x:0 ~y:(y1 - top) ~width ~height:(y2 + height2 - y1) ~filled ()
      | One _ | Many _ -> ()

    method draw_triangle drawable = function
      | One ln when start_line <= ln && ln <= stop_line ->
          let iter = view#buffer#get_iter (`LINE (ln - 1)) in
          let y, height = view#get_line_yrange iter in
          let y = y - top + height in
          let dy = height / 3 in
          polygon drawable ~filled [ 0, y - dy; 0, y + dy; width, y ];
      | One _ | Many _ -> ()

    method draw_margin ~view ~top:t ~left ~height:h ~start ~stop =
      drawing_area#misc#set_size_request ~width:area_width ~height ();
      start_line <- start#line + 1;
      stop_line <- stop#line + 1;
      top <- t;
      height <- h;
      view#move_child ~child:drawing_area#coerce ~x:(left + spacing) ~y:0;
  end
