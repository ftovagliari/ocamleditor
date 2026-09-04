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
[@@@warning "-48"]

let icon_normal =
  GdkPixbuf.from_xpm_data [|
    "5 7 2 1";
    " 	c None";
    ".	c #000000";
    "     ";
    "     ";
    ".....";
    " ... ";
    "  .  ";
    "     ";
    "     "|];;

let icon_pressed =
  GdkPixbuf.from_xpm_data [|
    "5 7 2 1";
    " 	c None";
    ".	c #000000";
    "     ";
    "     ";
    "     ";
    "     ";
    ".....";
    " ... ";
    "  .  "|];;

let custom_css = {|#gmisclib_button_menu_right {
  min-width: 12px;
}
|}

let label_icon ?(width=20) ?(height=16) ?(font_name="FiraCode OCamlEditor") ?color ?packing icon =
  let markup = Printf.sprintf "<big>%s</big>" icon in
  let label = GMisc.label ~xalign:0.5 ~yalign:0.5 ~xpad:0 ~ypad:0 ~width ~height ~markup ?packing () in
  label#misc#modify_font_by_name font_name;
  color |> Option.iter begin fun color ->
    label#misc#modify_fg [ `NORMAL, `NAME color; `ACTIVE, `NAME color; `PRELIGHT, `NAME color ];
    label#misc#modify_text [ `NORMAL, `NAME color; `ACTIVE, `NAME color; `PRELIGHT, `NAME color ];
  end;
  label

class button_menu ?(label="") ?(relief=`NORMAL) ?stock ?spacing ?packing () =
  let box = GPack.hbox ?spacing ?packing () in
  let button = GButton.button ~relief ?stock ~packing:box#pack () in
  let button_menu = GButton.button ~relief ~packing:box#pack () in
  let _ = button#misc#set_name "gmisclib_button_menu_left" in
  let _ = button_menu#misc#set_name "gmisclib_button_menu_right" in
  let clicked = new clicked () in
  let show_menu = new show_menu () in
  let label_widget = GMisc.label ~text:label () in
  (*  let _ = GtkMain.Rc.parse_string "
      style \"gmisclib_button_menu_left\" {
      GtkButton::inner-border = { 0, 0, 0, 0 }
      }
      style \"gmisclib_button_menu_right\" {
      GtkButton::inner-border = { 0, 0, 0, 0 }
      }
      widget \"*.gmisclib_button_menu_left\" style \"gmisclib_button_menu_left\"
      widget \"*.gmisclib_button_menu_right\" style \"gmisclib_button_menu_right\"
      " in*)
  object (self)
    inherit GObj.widget box#as_widget
    val relief = relief
    val mutable gmenu = None
    val image_pressed = (label_icon ~width:5 "<span size='small'>\u{f035d}</span>")#coerce
    val image_normal = (label_icon ~width:5 "<span size='small'>\u{f035d}</span>")#coerce
    val mutable tooltip_text = None
    val mutable sigid_button_press = None
    val mutable sigid_button_released = None
    val mutable is_menu_only = false
    val mutable image = None

    initializer
      ignore (button#connect#clicked ~callback:clicked#call);
      button_menu#add image_normal;
      button_menu#set_border_width 0;
      button#set_focus_on_click false;
      button_menu#set_focus_on_click false;
      ignore (button#connect#leave ~callback:begin fun _ ->
          self#set_button_menu_child false;
          button_menu#set_relief relief;
        end);
      ignore (button#connect#pressed ~callback:begin fun _ ->
          self#set_button_menu_child true;
          button_menu#set_relief `HALF;
        end);
      ignore (button#connect#released ~callback:begin fun _ ->
          self#set_button_menu_child false;
        end);
      ignore (button_menu#event#connect#button_press ~callback:begin fun ev ->
          self#popup_menu ev;
          true
        end);
      ignore (button_menu#connect#released ~callback:self#popdown_menu);

    method connect = new signals ~clicked ~show_menu

    method button = button
    method button_menu = button_menu

    method set_image x =
      image <- Some x;
      button#set_image x

    method set_label x =
      label_widget#set_text x

    method private set_button_menu_child pressed =
      if not is_menu_only then (button_menu#set_image (if pressed then image_pressed else image_normal))

    method set_menu_only () =
      if not is_menu_only then begin
        sigid_button_press <- Some (button#event#connect#button_press ~callback:begin fun ev ->
            self#popup_menu ev;
            true
          end);
        sigid_button_released <- Some (button#connect#released ~callback:self#popdown_menu);
        button#misc#hide();
        button_menu#remove button_menu#child;
        let box = GPack.hbox ~spacing:3 ~packing:button_menu#add () in
        begin
          match image with
          | Some image ->
              image#misc#unparent();
              box#pack image;
          | _ -> ()
        end;
        box#add label_widget#coerce;
        let draw_sep = try button#image |> ignore; true with Gpointer.Null -> false in
        let draw_sep = draw_sep || String.trim label <> "" in
        let _ = GMisc.separator `VERTICAL ~packing:box#pack ~show:draw_sep () in
        let arrow = GMisc.arrow ~kind:`DOWN ~width:8 ~height:1 () in
        box#pack arrow#coerce;
        is_menu_only <- true;
      end;

    method clear_menu_only () =
      if is_menu_only then begin
        Gaux.may sigid_button_press ~f:button#misc#disconnect;
        Gaux.may sigid_button_released ~f:button#misc#disconnect;
        begin
          match image with
          | Some image ->
              image#misc#unparent();
              button#set_image image;
          | _ -> ()
        end;
        button#misc#show();
        button_menu#remove button_menu#child;
        button_menu#set_label "";
        self#set_button_menu_child false;
        is_menu_only <- false;
      end;

    method private popup_menu ev =
      (try tooltip_text <- Some (GtkBase.Widget.Tooltip.get_text box#as_widget) with Gpointer.Null -> ());
      box#misc#set_tooltip_text "";
      self#set_button_menu_child true;
      let time = GdkEvent.Button.time ev in
      let pos ~x ~y ~pushed_in =
        let bt = if button#visible then button else button_menu in
        let xP, yP = Gdk.Window.get_pointer_location bt#misc#window in
        let xA, yA = bt#misc#allocation.Gtk.x, bt#misc#allocation.Gtk.y in
        let x' = x - xP + xA in
        let y' = y - yP + yA + bt#misc#allocation.Gtk.height in
        x', y', pushed_in
      in
      let menu = GMenu.menu () in
      gmenu <- Some menu;
      let rlabel = ref (Some label) in
      show_menu#call (rlabel, menu);
      begin
        match !rlabel with
        | Some default_label ->
            if List.length menu#children > 0 then (ignore (GMenu.separator_item ~packing:menu#prepend ()));
            let default_item = GMenu.menu_item ~packing:menu#prepend () in
            default_item#add (GMisc.label ~markup:("<b>"^default_label^"</b>") ~xalign:0.0 ())#coerce;
            ignore (default_item#connect#activate ~callback:button#clicked);
        | None -> ()
      end;
      ignore (menu#connect#deactivate ~callback:begin fun () ->
          self#set_button_menu_child false;
          button_menu#set_relief relief;
          button#set_relief relief;
          Gaux.may tooltip_text ~f:box#misc#set_tooltip_text;
          tooltip_text <- None;
        end);
      let cursor = Gdk.Cursor.create `ARROW in
      menu#event#connect#motion_notify  ~callback:begin fun _ ->
        Gdk.Window.set_cursor menu#misc#window cursor;
        false;
      end |> ignore;
      button#event#connect#motion_notify  ~callback:begin fun _ ->
        Gdk.Window.set_cursor button#misc#window cursor;
        false;
      end |> ignore;
      button_menu#event#connect#motion_notify  ~callback:begin fun _ ->
        Gdk.Window.set_cursor button_menu#misc#window cursor;
        false;
      end |> ignore;
      GtkMenu.Menu.popup_at menu#as_menu ~button:(GdkEvent.Button.button ev) ~time pos;
      button_menu#set_relief `NORMAL;
      button#set_relief `NORMAL;

    method private popdown_menu () =
      Gaux.may tooltip_text ~f:box#misc#set_tooltip_text;
      tooltip_text <- None;
      Gaux.may gmenu ~f:begin fun m ->
        ignore (m#popdown());
        gmenu <- None
      end;
  end

and clicked () = object inherit [unit] GUtil.signal () end
and show_menu () = object inherit [string option ref * GMenu.menu] GUtil.signal () end

and signals ~clicked ~show_menu =
  object
    inherit GUtil.ml_signals [clicked#disconnect; show_menu#disconnect]
    method clicked = clicked#connect ~after
    method show_menu = show_menu#connect ~after
  end

let create = new button_menu










