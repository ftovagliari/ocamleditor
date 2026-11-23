open Printf

class uss_histogram ?(width=100) ?(height=20) ?(samples=30) ?(interval_ms=5000) ~enabled ?packing () =
  let toggle_signal = new toggle () in
  let pid = Unix.getpid () in
  let cmd = sprintf "awk '/Private_Clean|Private_Dirty/ {sum+=$2} END {print sum/1024}' /proc/%d/smaps" pid in
  let ebox = GBin.event_box ~height ?packing () in
  let area = GMisc.drawing_area ~packing:ebox#add () in
  let history : float Queue.t = Queue.create () in
  let max_val = ref 16.0 in
  let enabled = ref enabled in
  let timer_id = ref None in
  let read_uss_mb () =
    match Shell.get_command_output cmd with
    | line :: _ ->
        (try float_of_string (String.trim line) with Failure _ -> 0.0)
    | [] -> 0.0
  in
  let push_sample v =
    if Queue.length history >= samples then ignore (Queue.take history);
    Queue.add v history;
    let peak = Queue.fold (fun acc x -> if x > acc then x else acc) 1.0 history in
    max_val := peak *. 1.15
  in
  let draw (cr : Cairo.context) =
    let alloc = area#misc#allocation in
    let w = if alloc.Gtk.width > 1 then float_of_int alloc.Gtk.width else float_of_int width in
    let h = if alloc.Gtk.height > 1 then float_of_int alloc.Gtk.height else float_of_int height in
    (* Background *)
    if !enabled then
      Cairo.set_source_rgb cr 0.15 0.15 0.17
    else
      Cairo.set_source_rgb cr 0.22 0.22 0.24;
    Cairo.rectangle cr 0.0 0.0 ~w ~h;
    Cairo.fill cr;
    if !enabled then begin
      (* Bars *)
      let n = Queue.length history in
      if n > 0 then begin
        let slot_w = w /. float_of_int samples in
        let bar_w = max 1.0 (slot_w -. 1.0) in
        let idx = ref 0 in
        Queue.iter
          (fun v ->
             let ratio = min 1.0 (max 0.0 (v /. !max_val)) in
             let bar_h = ratio *. (h -. 2.0) in
             let x = float_of_int !idx *. slot_w in
             let y = h -. bar_h in
             Cairo.set_source_rgb cr 0.25 0.72 0.45;
             Cairo.rectangle cr x y ~w:bar_w ~h:bar_h;
             Cairo.fill cr;
             incr idx)
          history
      end;
      let last_mb =
        match Queue.fold (fun _ x -> Some x) None history with
        | Some v -> v
        | None -> 0.0
      in
      Cairo.set_source_rgb cr 0.95 0.95 0.95;
      Cairo.select_font_face cr "Sans" ~slant:Cairo.Upright ~weight:Cairo.Bold;
      Cairo.set_font_size cr 9.0;
      Cairo.move_to cr 3.0 ((h +. 8.0) /. 2.0);
      Cairo.show_text cr (Printf.sprintf "%.1fM" last_mb)
    end else begin
      (* Disabled *)
      Cairo.set_source_rgb cr 0.65 0.65 0.65;
      Cairo.select_font_face cr "Sans" ~slant:Cairo.Upright ~weight:Cairo.Normal;
      Cairo.set_font_size cr 9.0;
      Cairo.move_to cr 3.0 ((h +. 8.0) /. 2.0);
      Cairo.show_text cr "USS: off"
    end;
    true
  in
  let start_timer () =
    match !timer_id with
    | Some _ -> ()
    | None ->
        push_sample (read_uss_mb ());
        GtkBase.Widget.queue_draw area#as_widget;
        timer_id := Some (GMain.Timeout.add ~ms:interval_ms ~callback:begin fun () ->
            if !enabled then begin
              push_sample (read_uss_mb ());
              GtkBase.Widget.queue_draw area#as_widget;
              true
            end else begin
              timer_id := None;
              false
            end
          end)
  in
  let stop_timer () =
    match !timer_id with
    | Some id ->
        GMain.Timeout.remove id;
        timer_id := None
    | None -> ()
  in
  let toggle () =
    enabled := not !enabled;
    toggle_signal#call !enabled;
    if !enabled then start_timer ()
    else begin
      stop_timer ();
      GtkBase.Widget.queue_draw area#as_widget
    end
  in
  object
    initializer
      ebox#set_width_request width;
      ebox#set_height_request height;
      ebox#set_valign `CENTER;
      area#misc#connect#after#draw ~callback:draw |> ignore;
      ebox#event#connect#button_press ~callback:begin fun ev ->
        if GdkEvent.Button.button ev = 1 then begin
          toggle ();
          true
        end else false
      end |> ignore;
      start_timer ()

    method as_widget = ebox#coerce
    method enabled = !enabled

    method set_enabled b =
      if !enabled <> b then toggle ()

    method refresh () =
      if !enabled then begin
        push_sample (read_uss_mb ());
        GtkBase.Widget.queue_draw area#as_widget
      end

    method connect = new signals ~toggle:toggle_signal

  end

and toggle () = object inherit [bool] GUtil.signal () end
and signals ~toggle =
  object
    inherit GUtil.ml_signals [toggle#disconnect]
    method toggle = toggle#connect ~after
  end
