open Margin
open Gutter
open Printf

class markers (view : GText.view) =
  let font = "FiraCode OCamlEditor 12" in
  object (self)
    inherit [(int * GObj.widget * int * int) list] widget ()
    val mutable size = 18
    val mutable markers : Gutter.marker list = []
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

    method add ~kind ~mark ~icon ~color =
      let marker = {kind; mark; icon=(Some (icon, color)); icon_obj=None} in
      markers <- marker :: markers;
      marker

    method remove markers_to_remove =
      markers <- List.filter (fun x -> not (List.memq x markers)) markers_to_remove;
      Gutter.destroy_markers markers_to_remove;

    method build ~start ~stop =
      avail_width <- size + size_extent;
      model <- [];
      let tmp_model = ref [] in
      markers |> List.iter begin fun mark ->
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
                let label = GMisc.label
                    ~xpad:0 ~ypad:0 ~xalign:0.0 ~width:icon_width
                    ~markup:(sprintf "<span face='%s' color='%s'>%s</span>" font color icon)
                    ~packing:ebox#add ()
                in
                begin
                  match mark.kind with
                  | `Warning _ | `Error _ ->
                      ebox#event#connect#button_press ~callback:begin fun _ ->
                        view#misc#grab_focus();
                        view#scroll_to_iter ~use_align:(view#scroll_to_iter iter) ~xalign:1.0 ~yalign:0.38 iter |> ignore;
                        view#buffer#place_cursor ~where:iter;
                        self#trigger#click iter;
                        true
                      end |> ignore;
                      ebox#event#connect#enter_notify ~callback:begin fun ev ->
                        self#trigger#mouseover ev;
                        true
                      end |> ignore;
                      ebox#event#connect#leave_notify ~callback:begin fun ev ->
                        self#trigger#mouseout ev;
                        true
                      end |> ignore;
                  | _ -> ()
                end;
                let child = ebox#coerce in
                mark.icon_obj <- Some child;
                view#add_child_in_window ~child ~which_window:`LEFT ~x:0 ~y;
                tmp_model := (line, (y, child, icon_width, iter#offset)) :: !tmp_model
            | Some child ->
                tmp_model := (line, (y, child, icon_width, iter#offset)) :: !tmp_model
          end
        end
      end;
      model <- Utils.ListExt.group_assoc !tmp_model

    method draw_margin ~view ~drawable ~top ~left ~height ~start ~stop =
      Prf.register Prf.draw_margin_markers begin fun () ->
        let left = left + avail_width in
        model
        |> List.iter begin fun (ln, childs) ->
          let icons_width = List.fold_left (fun sum (_, _, w, _) -> w + sum) 0 childs in
          let n_childs = List.length childs in
          let offset =
            if icons_width > avail_width && n_childs > 1
            then (avail_width - icons_width) / (n_childs - 1) else 0
          in
          childs |> List.fold_left begin fun x (y, child, icon_width, _) ->
            child#misc#show();
            let x = x - icon_width in
            view#move_child ~child ~x ~y;
            x - offset
          end left |> ignore;
        end
      end ()

    method! find_nearest_iter_at_y y =
      let ruler_h = float_of_int (view#misc#allocation).Gtk.height in
      let buffer_h = view#vadjustment#upper in
      if ruler_h <= 0.0 || buffer_h <= 0.0 then None
      else
        let vrect = view#visible_rect in
        let top = Gdk.Rectangle.y vrect in
        let max_dist_px = 0. in
        let y = y +. float top in
        let step best (_, _, _, offset) =
          let it = view#buffer#get_iter (`OFFSET offset) in
          let yi, hi = view#get_line_yrange it in
          let item_y = float yi +. float hi /. 2. in
          let dist = abs_float (y -. item_y) in
          if dist <= max_dist_px then
            match best with
            | None -> Some (dist, offset)
            | Some (min_dist, _) when dist < min_dist -> Some (dist, offset)
            | Some _ -> best
          else
            best
        in
        let m = model |> List.map (fun (_, items) -> items) |> List.flatten in
        match List.fold_left step None m with
        | Some (_, best_offset) -> Some (view#buffer#get_iter (`OFFSET best_offset))
        | None -> None

  end
