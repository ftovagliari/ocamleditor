open Margin
open Gutter
open Printf

class markers (view : GText.view) =
  let font = "FiraCode OCamlEditor 12" in
  let last_id = Atomic.make 0 in
  object (self)
    inherit [(int * GObj.widget * Gutter.marker_kind * int) list] widget ()
    val mutable size = 16
    val mutable markers : Gutter.marker list = []
    val font_desc = Pango.Font.from_string font
    val layout = view#misc#create_pango_context#create_layout#as_layout

    method kind = MARKERS
    method scope = Local
    method color = "#500050"
    method index = 0
    method size = size
    method model = model

    method add ~kind ~mark ~icon ~color =
      let marker = {id = Atomic.fetch_and_add last_id 1; kind; mark; icon=(Some (icon, color)); icon_obj=None} in
      markers <- marker :: markers;
      marker

    method remove markers_to_remove =
      markers <- markers |> List.filter (fun m -> not (List.exists (fun r -> r.id = m.id) markers_to_remove)) ;
      Gutter.destroy_markers markers_to_remove;
      self#build ~start:view#buffer#start_iter ~stop:view#buffer#end_iter

    method get_lines_with_markers () =
      model
      |> List.filter_map (fun (ln, ms) -> if List.length ms >= 2 then Some ln else None)

    method build ~start ~stop =
      model <- [];
      let tmp_model = ref [] in
      markers
      |> List.iter begin fun mark ->
        mark.icon
        |> Option.iter begin fun (icon, color) ->
          Gmisclib.Util.get_iter_at_mark_opt view#buffer#as_buffer mark.mark
          |> Option.iter begin fun mark_iter ->
            let iter = new GText.iter mark_iter in
            let line = iter#line + 1 in
            let y, _ = view#get_line_yrange iter in
            match mark.icon_obj with
            | None ->
                let ebox = GBin.event_box ~show:false ~border_width:0 () in
                ebox#misc#set_property "visible-window" (`BOOL false);
                let label = GMisc.label
                    ~xpad:0 ~ypad:0 ~xalign:0.0 ~yalign:0.0 ~width:1
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
                tmp_model := (line, (y, child, mark.kind, iter#offset)) :: !tmp_model
            | Some child ->
                tmp_model := (line, (y, child, mark.kind, iter#offset)) :: !tmp_model
          end
        end
      end;
      model <-
        Utils.ListExt.group_assoc !tmp_model
        |> List.map (fun (x, l) -> x, l |> List.sort (fun (_, _, k1, _) (_, _, k2, _) -> compare k1 k2))

    method draw_margin ~view ~drawable ~top ~left ~height ~start ~stop =
      Prf.register Prf.draw_margin_markers begin fun () ->
        model
        |> List.iter begin fun (ln, childs) ->
          childs |> List.fold_left begin fun x (y, child, _, _) ->
            child#misc#show();
            let child_width = child#misc#allocated_width in
            view#move_child ~child ~x ~y;
            x + child_width + 2
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
