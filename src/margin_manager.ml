open Margin

class manager ?(overview_ruler_mode=Separated) (view : GText.view) =
  let overview_widget = GMisc.drawing_area () in
  let gutter = Gutter.create () in (* tech debt *)
  let rec find_margin x x_offset = function
    | [] -> None
    | m :: rest ->
        let next_offset = x_offset + m#size in
        if x_offset < x && x <= next_offset then Some m
        else find_margin x next_offset rest
  in
  let draw_childs childs ~top ~height ~start ~stop window =
    childs
    |> List.fold_left begin fun left margin ->
      if margin#is_visible then begin
        let drawable = Gdk.Cairo.create window in
        (*Cairo_drawable.set_foreground drawable (`NAME margin#color);
          Cairo_drawable.rectangle drawable ~x:left ~y:0 ~width:(margin#size) ~height ~filled:true ();*)
        margin#draw_margin ~view ~drawable ~top ~left ~height ~start ~stop;
        left + margin#size
      end else left
    end 0 |> ignore;
  in
  object (self)
    val mutable gutter_margins : margin list = []
    val mutable overview_margins : margin list = []

    initializer
      overview_widget#event#add [`BUTTON_PRESS];
      overview_widget#event#connect#button_press ~callback:self#overview_button_press |> ignore;
      (*view#event#connect#button_press ~callback:self#gutter_button_press |> ignore;*)
      view#misc#connect#after#draw ~callback:begin fun _ ->
        self#draw ();
        false
      end |> ignore;
      view#vadjustment#connect#value_changed ~callback:self#build |> ignore;

    method gutter = gutter (* tech debt *)
    method overview_widget = overview_widget

    method add margin =
      match margin#kind with
      | LINE_NUMBERS | MARKERS | DIFF | GLOBAL_DIFF | ERRORS | OCCURRENCES | CURRENT_LINE ->
          begin
            let compare m1 m2 = Stdlib.compare m1#index m2#index in
            match margin#scope with
            | Local ->
                gutter_margins <- margin :: gutter_margins |> List.sort compare
            | Global ->
                overview_margins <- margin :: overview_margins |> List.sort compare
          end;
          let size =
            if gutter_margins = [] then 0 else
              gutter_margins |> List.fold_left (fun sum m -> sum + m#size) 0
          in
          view#set_border_window_size ~typ:`LEFT ~size;
          let size =
            if overview_margins = [] then 0 else
              overview_margins |> List.fold_left (fun sum m -> sum + m#size) 0
          in
          begin
            match overview_ruler_mode with
            | Integrated ->
                view#set_border_window_size ~typ:`RIGHT ~size;
            | Separated ->
                overview_widget#misc#set_size_request ~width:size ();
          end;
          self#build ();
      | FOLDING -> ()

    method remove margin =
      match margin#scope with
      | Local -> gutter_margins <- gutter_margins |> List.filter ((<>) margin)
      | Global -> overview_margins <- overview_margins |> List.filter ((<>) margin)

    method get kind =
      match gutter_margins |> List.find_opt (fun m -> m#kind = kind) with
      | None -> overview_margins |> List.find_opt (fun m -> m#kind = kind)
      | margin -> margin

    method build () =
      let vrect = view#visible_rect in
      let height = Gdk.Rectangle.height vrect in
      let top = Gdk.Rectangle.y vrect in
      let start, _ = view#get_line_at_y top in
      let stop, _ = view#get_line_at_y (top + height) in
      List.iter (fun m -> m#build ~start ~stop) gutter_margins;
      List.iter (fun m -> m#build ~start ~stop) overview_margins

    method draw () =
      Prf.register Prf.draw_margins begin fun () ->
        let vrect = view#visible_rect in
        let height = Gdk.Rectangle.height vrect in
        let top = Gdk.Rectangle.y vrect in
        let start, _ = view#get_line_at_y top in
        let stop, _ = view#get_line_at_y (top + height) in
        (view#get_window `LEFT) |> Option.iter (draw_childs gutter_margins ~top ~height ~start ~stop);
        match overview_ruler_mode with
        | Integrated ->
            (view#get_window `RIGHT) |> Option.iter (draw_childs overview_margins ~top ~height ~start ~stop);
        | Separated ->
            draw_childs overview_margins ~top ~height ~start ~stop overview_widget#misc#window;
      end ();

    method private gutter_button_press ev =
      (* TODO handle size extent *)
      if (GdkEvent.Button.button ev = 1 && GdkEvent.get_type ev = `BUTTON_PRESS) then begin
        let x = GdkEvent.Button.x ev |> int_of_float in
        let callback target =
          let y = GdkEvent.Button.y ev in
          let iter_at_y = gutter_margins |> List.find_map (fun m -> m#find_nearest_iter_at_y y) in
          Printf.printf "%d %f %s\n%!" x y (show_kind target#kind);
          match iter_at_y with
          | Some iter ->
              Printf.printf "iter_at_y = %d\n%!" iter#offset;
              target#trigger#click iter;
          | _ -> ()
        in
        find_margin x 0 gutter_margins |> Option.iter callback
      end;
      false

    method private overview_button_press ev =
      if (GdkEvent.Button.button ev = 1 && GdkEvent.get_type ev = `BUTTON_PRESS) then begin
        let x = GdkEvent.Button.x ev |> int_of_float in
        let callback target =
          let y = GdkEvent.Button.y ev in
          let iter_at_y = overview_margins |> List.find_map (fun m -> m#find_nearest_iter_at_y y) in
          let iter =
            match iter_at_y with
            | Some i -> i
            | _ ->
                let alloc = view#misc#allocation in
                let line_count = view#buffer#line_count in
                let line = int_of_float (y /. float alloc.Gtk.height *. float line_count) in
                view#buffer#get_iter (`LINE line)
          in
          view#scroll_to_iter ~use_align:(view#scroll_to_iter iter) ~xalign:1.0 ~yalign:0.38 iter |> ignore;
          view#buffer#place_cursor ~where:iter;
          target#trigger#click iter;
        in
        find_margin x 0 overview_margins |> Option.iter callback

        (*let height = float (height - 2 * alloc.Gtk.width) in
          let y = y -. (float alloc.Gtk.width) in
          let tooltip, iter =
          try
            let _, mark =
              table
              |> List.rev
              |> List.find (fun (yy, _) -> let yy = float yy in yy -. 4. <= y && y <= yy +. 4.)
            in
            true, (buffer#get_iter_at_mark (`MARK mark))
          with Not_found -> begin
              let line_count = float buffer#line_count in
              let line = int_of_float (y /. height *. line_count) in
              false, buffer#get_iter (`LINE line);
            end
          in
          view#scroll_lazy iter;
          buffer#place_cursor ~where:iter;
          (*if tooltip then begin
          Gmisclib.Idle.add ~prio:300 (fun () -> self#tooltip ~sticky:true (`ITER iter));
          end;*)
          end else begin
          (*let iter =
          match self#first_error_or_warning with
          | None -> buffer#start_iter
          | Some (start, _, _) -> buffer#get_iter_at_mark (`MARK start)
          in
          view#scroll_lazy iter;
          buffer#place_cursor ~where:iter;*)*)
      end;
      false

  end

