(*

  OCamlEditor
  Copyright (C) 2010-2014 Francesco Tovagliari

  This file is part of OCamlEditor.

  OCamlEditor is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  OCamlEditor is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.

*)


module SVG = struct

  let zooms = [|(*0.125; 0.25; 0.33;*) 0.5; 0.67; 0.75; 1.0; 1.07; 1.25; 1.5; 1.85; 2.0; 3.0; (*4.0; 6.0; 8.0; 12.0*)|]

  let lang = "svg"

  let have_embedded_viewer = true

  class view ?packing () =
    let hbox              = GPack.hbox ~spacing:5 ?packing ~show:false () in
    let sw                = GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:hbox#add () in
    let viewport          = GBin.viewport ~packing:sw#add () in
    let image             = GMisc.image ~packing:viewport#add () in
    let _                 = viewport#misc#modify_bg [`NORMAL, `WHITE] in
    let _                 = viewport#event#add [`BUTTON_RELEASE; `BUTTON_MOTION] in
    (*  *)
    let cbox              = GPack.vbox ~spacing:8 ~packing:hbox#pack ~show:false () in
    let check_include_all = GButton.check_button ~label:"Include all modules" ~packing:cbox#pack () in
    let check_types       = GButton.check_button ~label:"Type dependency" ~packing:cbox#pack () in
    object (self)
      inherit Dot_viewer_plugin.viewer
      inherit GObj.widget hbox#as_widget

      val mutable buffer = ""
      val mutable zoom = let i = ref (-1) in Array.fold_left (fun acc x -> incr i; if x = 1.0 then !i else acc) !i zooms
      val mutable xi = 0.
      val mutable yi = 0.
      val mutable xp = 0.
      val mutable yp = 0.
      val mutable xm0 = 0.
      val mutable ym0 = 0.
      val mutable xm = 0.
      val mutable ym = 0.
      val mutable pan_tool = false
      val mutable cache = []

      initializer
        (*ignore (check_include_all#connect#toggled ~callback:self#execute);
          ignore (check_types#connect#toggled ~callback:self#execute);*)
        ignore (viewport#event#connect#button_press ~callback:begin fun _ ->
            pan_tool <- true;
            Gdk.Window.set_cursor viewport#misc#window (Gdk.Cursor.create `FLEUR);
            false
          end);
        ignore (viewport#event#connect#button_release ~callback:begin fun _ ->
            Gdk.Window.set_cursor viewport#misc#window (Gdk.Cursor.create `ARROW);
            pan_tool <- false;
            xm0 <- 0.;
            ym0 <- 0.;
            xm <- 0.;
            ym <- 0.;
            false
          end);
        let count = ref 0 in
        ignore (viewport#event#connect#motion_notify ~callback:begin fun ev ->
            if pan_tool then begin
              if xm > 0. && ym > 0. then begin
                if !count = 1 then begin
                  count := 0;
                  xm0 <- xm;
                  ym0 <- ym;
                  xm <- GdkEvent.Motion.x ev;
                  ym <- GdkEvent.Motion.y ev;
                  if xm0 <> xm then begin
                    let dx = xm0 -. xm in
                    let h = sw#hadjustment#value +. dx in
                    let up = sw#hadjustment#upper -. sw#hadjustment#page_size in
                    if h >= 0. && h <= up
                    then sw#hadjustment#set_value h
                    else if h <= 0. then sw#hadjustment#set_value 0.
                    else if sw#hadjustment#value <> up then sw#hadjustment#set_value up
                  end;
                  if ym0 <> ym then begin
                    let dy = ym0 -. ym in
                    let v = sw#vadjustment#value +. dy in
                    let up = sw#vadjustment#upper -. sw#vadjustment#page_size in
                    if v >= 0. && v <= up
                    then sw#vadjustment#set_value v
                    else if v <= 0. then sw#vadjustment#set_value 0.
                    else if sw#vadjustment#value <> up then sw#vadjustment#set_value up
                  end
                end else incr count
              end else begin
                xm <- GdkEvent.Motion.x ev;
                ym <- GdkEvent.Motion.y ev;
              end
            end;
            false
          end);
        ignore (viewport#event#connect#scroll ~callback:begin fun ev ->
            begin
              match GdkEvent.Scroll.direction ev with
              | `UP ->
                  zoom <- min (zoom + 1) (Array.length zooms - 1);
                  self#get_pointer_position (GdkEvent.Scroll.x ev) (GdkEvent.Scroll.y ev);
                  self#redisplay zooms.(zoom)
              | `DOWN ->
                  zoom <- max (zoom - 1) 0;
                  self#get_pointer_position (GdkEvent.Scroll.x ev) (GdkEvent.Scroll.y ev);
                  self#redisplay zooms.(zoom)
              | _ -> ()
            end;
            true
          end);

      method set_buffer b = buffer <- b
      method zoom = zoom
      method clear_cache () = cache <- []

      method display ~filename =
        self#clear_cache ();
        GtkThread2.async begin fun () ->
          self#misc#show();
          self#set_buffer (Buffer.contents (File_util.read filename));
          self#redisplay zooms.(self#zoom);
          Array.iter begin fun zm ->
            Gmisclib.Idle.add ~prio:300 begin fun () ->
              try ignore (self#create_pixbuf zm);
              with ex -> Printf.eprintf "File \"dot.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());
            end
          end zooms;
          if Sys.file_exists filename then (Sys.remove filename);
        end ();

      method private redisplay zm =
        try
          let pixbuf = self#create_pixbuf zm in
          image#misc#hide();
          image#set_pixbuf pixbuf;
          let width = float (GdkPixbuf.get_width image#pixbuf) in
          let height = float (GdkPixbuf.get_height image#pixbuf) in
          sw#hadjustment#set_bounds ~upper:width ();
          sw#vadjustment#set_bounds ~upper:height ();
          sw#hadjustment#set_value (width *. xi -. sw#hadjustment#page_size *. xp);
          sw#vadjustment#set_value (height *. yi -. sw#vadjustment#page_size *. yp);
          image#misc#show();
          xi <- 0.;
          yi <- 0.;
          xp <- 0.;
          xp <- 0.;
        with ex -> Printf.eprintf "File \"dot.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());

      method private get_pointer_position ev_x ev_y =
        let pixbuf = image#pixbuf in
        let width = float (GdkPixbuf.get_width pixbuf) in
        let height = float (GdkPixbuf.get_height pixbuf) in
        xi <- (ev_x +. sw#hadjustment#value) /. width;
        yi <- (ev_y +. sw#vadjustment#value) /. height;
        xp <- ev_x /. sw#hadjustment#page_size;
        yp <- ev_y /. sw#vadjustment#page_size;

      method private create_pixbuf zm =
        match List_opt.assoc zm cache with
        | Some pixbuf -> pixbuf
        | None ->
            let pixbuf = Rsvg.render_from_string ~size_cb:(Rsvg.at_zoom zm zm) buffer in
            cache <- (zm, pixbuf) :: cache;
            pixbuf

    end

  let create ?packing () = Some ((new view ?packing ()) :> Dot_viewer_plugin.viewer)

  let draw ~filename dev =
    match dev with
    | None -> assert false
    | Some dev -> dev#display ~filename
end
