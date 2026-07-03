module ColorOps = Color

let global_gutter_diff_size = 8
let global_gutter_diff_sep = 1
let fact = 0.0
open Preferences

let color_add =
  let sat, value = if Preferences.preferences#get.theme_is_dark then 0.2, 0.4 else 0.4, 0.2 in
  `NAME (ColorOps.modify (?? Oe_config.global_gutter_diff_color_add) ~sat ~value)

let color_del =
  let sat, value = if Preferences.preferences#get.theme_is_dark then 0.2, 0.4 else 0.4, 0.2 in
  `NAME (ColorOps.modify (?? Oe_config.global_gutter_diff_color_del) ~sat ~value)

let color_change = `NAME (?? Oe_config.global_gutter_diff_color_change)

let initialized : (int * Margin_diff.widget) list ref = ref []

let wave_line ~(drawable : GDraw.drawable) ~color ~width ~height ~y ~is_add =
  drawable#set_foreground color;
  let ww = width * 2 / 3 in
  let len = 0 (*height / ww*) in
  if len < 4 then
    drawable#rectangle ~filled:is_add ~x:0 ~y ~width:(width - (if is_add then 1 else 2)) ~height ()
  else
    let polyline = ref [] in
    let i = ref 0 in
    while !i < len - 1 do
      polyline := (ww, y + ww * (!i + 1)) :: (0, y + ww * !i) :: !polyline;
      incr i;
      incr i;
    done;
    if !polyline <> [] then begin
      let width = if is_add then 1 else 2 in
      drawable#set_line_attributes ~width ~cap:`ROUND ~style:`SOLID ~join:`ROUND ();
      drawable#lines !polyline
    end;;

let paint_diffs page diffs =
  let window = page#global_gutter#misc#window in
  let drawable = new GDraw.drawable window in
  let alloc = page#vscrollbar#misc#allocation in
  drawable#set_line_attributes ~width:1 ~style:`SOLID ~join:`ROUND ();
  let _, height = drawable#size in
  let width = global_gutter_diff_size in
  let line_count = float page#buffer#line_count in
  let open Odiff in
  drawable#set_foreground (`COLOR (page#view#misc#style#base `NORMAL));
  drawable#rectangle ~filled:true ~x:0 ~y:0 ~width ~height ();
  page#set_global_gutter_tooltips [];
  let black = page#view#gutter.Gutter.marker_color in
  let height = float height in
  let y_of_line ln = int_of_float ((float ln /. line_count) *. height) in
  let line_height = int_of_float (height /. line_count) in
  let wtri = width * 2 / 3 in
  let wtri2 = wtri in
  let paint color elems = function
    | One ln ->
        let x0 = 0 (*width - wtri*) in
        let y = y_of_line ln in
        let height = max 3 line_height in
        begin
          match color with
          | col when col = color_del ->
              begin
                match Oe_config.global_gutter_diff_style with
                | `COLOR with_border ->
                    drawable#set_foreground color;
                    drawable#polygon ~filled:true [x0, y; x0 + wtri, y - wtri2; x0 + wtri, y + wtri2];
                    if (*true ||*) with_border then begin
                      drawable#set_foreground (ColorOps.set_value 0.6 color);
                      drawable#polygon ~filled:false [0, y; wtri, y - wtri2; wtri, y + wtri2]
                    end
                | `BW ->
                    drawable#set_foreground black;
                    let tri = [x0, y; x0 + wtri, y - wtri; x0 + wtri, y + wtri] in
                    drawable#polygon ~filled:true tri;
                    drawable#set_foreground (ColorOps.set_value 0.5 black);
                    drawable#polygon ~filled:false tri;
              end;
          | col when col = color_add ->
              drawable#set_foreground
                (match Oe_config.global_gutter_diff_style with
                 | `COLOR _ -> color;
                 | `BW -> black);
              drawable#rectangle ~filled:true ~x:0 ~y:(y - 2) ~width:(width-2) ~height ();
          | color ->
              begin
                match Oe_config.global_gutter_diff_style with
                | `COLOR with_border ->
                    drawable#set_foreground color;
                    drawable#rectangle ~filled:true ~x:0 ~y:(y - 2) ~width ~height ();
                    if with_border then begin
                      drawable#set_foreground (ColorOps.set_value 0.6 color);
                      drawable#rectangle ~filled:false ~x:0 ~y:(y - 2) ~width:(width-1) ~height ();
                    end
                | `BW ->
                    drawable#set_foreground black;
                    drawable#rectangle ~filled:false ~x:0 ~y:(y - 2) ~width:(width - 1) ~height ();
              end;
        end
    | Many (l1, l2) ->
        let y1 = y_of_line l1 in
        let y2 = y_of_line l2 in
        let height = max 3 (y2 - y1 + line_height) in
        begin
          match Oe_config.global_gutter_diff_style with
          | `COLOR with_border ->
              drawable#set_foreground color;
              drawable#rectangle ~filled:true ~x:0 ~y:y1 ~width ~height ();
              if with_border then begin
                drawable#set_foreground (ColorOps.set_value 0.6 color);
                drawable#rectangle ~filled:false ~x:0 ~y:y1 ~width:(width-1) ~height ();
              end;
          | `BW ->
              wave_line ~drawable ~color:black ~width ~height ~y:y1 ~is_add:(color = color_add);
        end
  in
  let diffs = List.sort begin fun a b ->
      match a with
      | Delete _ -> (match b with Delete _ -> 0 | _ -> 1)
      | _ -> (match b with Delete _ -> -1 | _ -> 0)
    end diffs
  in
  begin
    match List.assoc_opt page#get_oid !initialized with
    | Some margin -> margin#set_diffs diffs
    | _ -> ()
  end;
  List.iter begin fun diff ->
    Gmisclib.Idle.add ~prio:200 begin fun () ->
      match diff with
      | Add (_, ind, a) ->
          paint color_add [color_add, a] ind |> ignore;
      | Delete (_, ind, a) ->
          paint color_del [color_del, a] ind |> ignore;
      | Change (_, a, ind, b) ->
          paint color_change [color_del, a; color_add, b] ind
    end
  end diffs

let compare_with_head page continue_with =
  match Utils.filename_relative (Filename.dirname (Sys.getcwd())) page#get_filename with
  | Some filename ->
      let open Printf in
      let buf = Buffer.create 1024 in
      Spawn.async "git" [| "show"; sprintf "HEAD:%s" filename |]
        ~process_in:(Spawn.loop (fun ic -> Buffer.add_string buf (input_line ic); Buffer.add_char buf '\n'))
        ~continue_with:begin fun _ ->
          let text = page#buffer#get_text ?start:None ?stop:None ?slice:None ?visible:None () in
          try (* TODO Crashed here
                 at parsing.ml:143
                 #1  0x000056469f749302 in camlStdlib__Parsing.loop_521 () at parsing.ml:143
                 #2  0x000056469f749075 in camlStdlib__Parsing.yyparse_515 () at parsing.ml:165
                 #3  0x000056469f6194f7 in camlOdiff.parse_from_lexbuf_281 () at lib/odiff_parser.ml:310
                 #4  0x000056469f619ebb in camlOdiff.try_finalize_636 () at lib/odiff.ml:126
                 #5  0x000056469f619ebb in camlOdiff.try_finalize_636 () at lib/odiff.ml:126
                 #6  0x000056469f1d5f84 in camlGlobal_diff.fun_1904 () at global_diff.ml:153
              *)
            continue_with (Odiff.strings_diffs (Buffer.contents buf) text)
          with ex ->
            Printf.eprintf "File \"global_diff.ml\": %s\n%s\n%!"
              (Printexc.to_string ex) (Printexc.get_backtrace());
        end |> ignore
  | _ -> ()

let try_compare ?(force=false) page =
  let margin = List.assoc_opt page#get_oid !initialized in
  let is_changed = Option.fold ~none:true ~some:(fun m -> m#is_changed_after_last_diff) margin in
  if (is_changed || force) && page#view#misc#get_flag `VISIBLE then begin
    compare_with_head page begin fun diffs ->
      try
        diffs |> paint_diffs page;
        Option.iter (fun m -> m#sync_diff_time()) margin
      with Gpointer.Null as ex ->
        Printf.eprintf "File \"global_diff.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());
    end
  end

let to_buffer (buffer : GText.buffer) ignore_whitespace filename1 filename2 =
  Global_diff_gtext.insert buffer ignore_whitespace filename1 filename2

let init_page page =
  match List.assoc_opt page#get_oid !initialized with
  | None ->
      let change_size () =
        let _, height = (new GDraw.drawable page#global_gutter#misc#window)#size in
        let new_width = Oe_config.global_gutter_size + global_gutter_diff_size + global_gutter_diff_sep in
        page#global_gutter#set_size ~width:new_width ~height;
        page#global_gutter#event#connect#button_release ~callback:begin fun ev ->
          if (GdkEvent.Button.button ev = 3 && GdkEvent.get_type ev = `BUTTON_RELEASE) then begin
            let x = GdkEvent.Button.x ev in
            let y = GdkEvent.Button.y ev in
            if int_of_float x < global_gutter_diff_size then begin
              (*Printf.printf "----> %f, %f\n%!" x y;*)
            end;
          end;
          true
        end |> ignore;
        let margin = new Margin_diff.widget page#view in
        page#view#margin#add (margin :> Margin.margin);
        initialized := (page#get_oid, margin) :: !initialized;
        try_compare ~force:true page;
        Gmisclib.Idle.add ~prio:500 page#view#draw_gutter
      in
      begin
        page#view#event#connect#focus_in ~callback:(fun _ -> try_compare ~force:true page; false) |> ignore;
        try change_size () with Gpointer.Null ->
          page#global_gutter#misc#connect#after#realize ~callback:change_size |> ignore
      end;
  | _ -> ()

let init_editor editor =
  let callback pg = Gmisclib.Idle.add ~prio:300 (fun () -> init_page pg) in
  editor#connect#add_page ~callback |> ignore;
  editor#connect#remove_page ~callback:begin fun page ->
    begin
      match !initialized |> List.assoc_opt page#get_oid with
      | Some margin_diff -> page#view#margin#remove (margin_diff :> Margin.margin)
      | _ -> ()
    end;
    initialized := List.filter (fun (oid, _) -> oid <> page#get_oid) (!initialized);
  end |> ignore;
  (* Timeout *)
  let id_timeout_diff = ref None in
  let create_timeout_diff () =
    Gaux.may !id_timeout_diff ~f:GMain.Timeout.remove;
    id_timeout_diff := None;
    let callback () =
      try
        editor#with_current_page begin fun page ->
          if page#view#misc#get_flag `HAS_FOCUS then (try_compare page);
        end;
        true
      with ex ->
        Printf.eprintf "File \"plugin_diff.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());
        true
    in
    id_timeout_diff := Some (GMain.Timeout.add ~ms:500 ~callback);
  in
  let bind _ =
    create_timeout_diff();
    Gaux.may (GWindow.toplevel editor#coerce) ~f:begin fun (w : GWindow.window) ->
      w#event#connect#focus_in ~callback:begin fun _ ->
        create_timeout_diff();
        false
      end |> ignore;
      w#event#connect#focus_out ~callback:begin fun _ ->
        Gaux.may !id_timeout_diff ~f:GMain.Timeout.remove;
        id_timeout_diff := None;
        false
      end |> ignore;
    end;
  in
  begin
    try bind ()
    with Gpointer.Null -> editor#misc#connect#map ~callback:bind |> ignore;
  end
