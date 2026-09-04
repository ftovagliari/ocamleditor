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


open Printf

let _ = Gmisclib.Util.fade_window_enabled := Oe_config.fade_window_enabled

let create_mark_name =
  let count = ref 0 in fun prefix ->
    incr count;
    prefix ^ (string_of_int !count);;

(** window *)
let window widget
    ?type_hint
    ?modal
    ?(decorated=false)
    ?parent
    ?(destroy_on_focus_out=true)
    ?(destroy_child=true)
    ?(fade=false)
    ?(focus=true)
    ?(escape=true)
    ?(border_width=1)
    ?(show=true)
    ~x ~y () =
  let window = GWindow.window
      ~decorated
      ?modal
      ~border_width
      ~deletable:true
      ~focus_on_map:focus
      ?type_hint
      ~show:false ()
  in
  let ebox = GBin.event_box ~packing:window#add () in
  ebox#add widget;
  let color = Color.set_value 0.38 (`COLOR (window#misc#style#base `NORMAL)) (*(`NAME Preferences.preferences#get.Preferences.pref_bg_color_popup)*) in
  let _ = window#misc#modify_bg [`NORMAL, color] in
  (*let _ = ebox#misc#modify_bg [`NORMAL, `NAME Preferences.preferences#get.Preferences.pref_bg_color_popup] in*)
  if destroy_on_focus_out then
    window#event#connect#after#focus_out ~callback:begin fun _ ->
      if not destroy_child then (ebox#remove widget);
      window#destroy();
      true
    end |> ignore;
  if escape then Gmisclib.Util.esc_destroy_window window;
  window#set_skip_pager_hint true;
  window#set_skip_taskbar_hint true;
  window#set_urgency_hint false;
  Gaux.may parent ~f:(fun parent -> Gaux.may (GWindow.toplevel parent) ~f:(fun x -> window#set_transient_for x#as_window));
  window#set_accept_focus focus;
  if show then begin
    if fade then (window#set_opacity 0.0);
    window#move ~x ~y;
    let alloc = window#misc#allocation in
    let x, y =
      (if x + alloc.Gtk.width > (Gdk.Screen.width()) then (Gdk.Screen.width() - alloc.Gtk.width) else x),
      (if y + alloc.Gtk.height > (Gdk.Screen.height()) then (Gdk.Screen.height() - alloc.Gtk.height) else y);
    in
    window#move ~x ~y;
    if fade then (Gmisclib.Util.fade_window window);
    window#present();
  end;
  window

let move_window_within_screen_bounds window x y =
  let alloc = window#misc#allocation in
  let x, y =
    (if x + alloc.Gtk.width > (Gdk.Screen.width()) then max 0 (Gdk.Screen.width() - alloc.Gtk.width) else x),
    (if y + alloc.Gtk.height > (Gdk.Screen.height()) then max 0 (Gdk.Screen.height() - alloc.Gtk.height) else y);
  in
  window#move ~x ~y;
  x, y

(** window_tooltip *)
let window_tooltip widget ?parent ?(fade=false) ~x ~y ?width ?height ?(kind=`POPUP) ?(type_hint=`NORMAL) ?(show=true) () =
  let fade = fade && !Gmisclib.Util.fade_window_enabled in
  let window = GWindow.window
      ~decorated:false
      ~kind (* `POPUP is faster *)
      ~type_hint (*`NORMAL allows wm effects *)
      ~border_width:1
      ?width ?height
      ~show:false ()
  in
  let ebox = GBin.event_box ~packing:window#add () in
  ebox#add widget;
  let open Preferences in
  if not Oe_config.use_theme_colors_when_possible then begin
    window#misc#modify_bg [`NORMAL, `NAME ?? (Preferences.preferences#get.editor_bg_color_popup)];
    ebox#misc#modify_bg [`NORMAL, `NAME ?? (Preferences.preferences#get.editor_bg_color_popup)];
  end;
  window#set_skip_pager_hint true;
  window#set_skip_taskbar_hint true;
  window#set_accept_focus false;
  window#misc#set_can_focus false;
  (*window#set_focus_on_map false;*)
  Gaux.may parent ~f:(fun parent -> Gaux.may (GWindow.toplevel parent) ~f:(fun x -> window#set_transient_for x#as_window));
  if fade then (window#set_opacity 0.0);
  window#move ~x ~y;
  if show then begin
    if fade then (Gmisclib.Util.fade_window window) else window#present();
  end;
  move_window_within_screen_bounds window x y |> ignore;
  window

(** with_tag *)
let with_tag ~(buffer : GText.buffer) tag f =
  let m1 = buffer#create_mark(* ~name:(create_mark_name "Gtk_util.with_tag")*) (buffer#get_iter `INSERT) in
  f ();
  buffer#apply_tag tag ~start:(buffer#get_iter_at_mark (`MARK m1)) ~stop:(buffer#get_iter `INSERT);
  buffer#delete_mark (`MARK m1);;

(** increase_font_size *)
let increase_font_size ?weight ?(increment=3) widget =
  let fd : GPango.font_description = widget#misc#pango_context#font_description in
  if increment <> 0 then begin
    let size = fd#size + increment * Pango.scale in
    if size >= 0 then begin
      let size = Some size in
      fd#modify ?weight ?size ();
      widget#misc#modify_font fd;
    end;
  end;
  fd;;

(** try_font *)
let try_font context family =
  try
    let fd = Printf.ksprintf Pango.Font.from_string "%s 9" family in
    let _ = Pango.Context.load_font context#as_context fd in
    true
  with Gpointer.Null -> false

let label_icon ?(width=20) ?(height=16) ?(font_name="FiraCode OCamlEditor") ?(font_size="larger") ?color ?packing icon =
  let markup = Printf.sprintf "<span size='%s'>%s</span>" font_size icon in
  let label = GMisc.label ~xalign:0.5 ~yalign:0.5 ~xpad:0 ~ypad:0 ~width ~height ~markup ?packing () in
  label#misc#modify_font_by_name font_name;
  (* TODO: Lablgtk3 issue, refactor css into a sigle module. *)
  Option.iter begin fun color ->
    let css_provider = GObj.css_provider () in
    css_provider#load_from_data (sprintf {|
label.%s-button { color: %s; }
label.%s-button:disabled { color: #808080; }
|} color color color);
    label#misc#style_context#add_class (sprintf "%s-button" color);
    label#misc#style_context#add_provider css_provider 600
  end color;

  label

class button_icon ?label ?(icon="") ?(icon_spacing=3) ?icon_width ?icon_height ?relief ?packing () =
  let button = GButton.button ?label ?relief ?packing () in
  let hbox = GPack.hbox ~border_width:0 ~spacing:icon_spacing ~packing:button#add () in
  let icon = label_icon ?width:icon_width ?height:icon_height ~packing:hbox#add icon in
  let label = GMisc.label ~xpad:0 ~ypad:0 ~packing:hbox#add () in
  object (self)
    inherit GObj.widget hbox#as_widget
    method set_icon = icon#set_label
    method set_label = label#set_label
    method button = button
  end

let scroll_aligned (view : GText.view) (where : GText.iter) ~xalign ~yalign =
  Gmisclib.Idle.add begin fun () ->
    let rect = view#get_iter_location where in
    let top = view#vadjustment#page_size *. yalign in
    view#vadjustment#set_value (float (Gdk.Rectangle.y rect) -. top);
  end

let rec scroll_aligned_alt =
  let is_iter_at_yalign (view : GText.view) (iter : GText.iter) (target_yalign : float) : bool =
    let visible_rect = view#visible_rect in
    let iter_rect = view#get_iter_location iter in
    let iter_y = Gdk.Rectangle.y iter_rect in
    let vis_y = Gdk.Rectangle.y visible_rect in
    let vis_h = Gdk.Rectangle.height visible_rect in
    let target_pixel_y = float_of_int vis_y +. (float_of_int vis_h *. target_yalign) in
    let tolerance = 3.0 in
    abs_float (float_of_int iter_y -. target_pixel_y) <= tolerance
  in
  fun (view : GText.view) ?(max_attempts=10) ~xalign ~yalign iter ->
    if max_attempts <= 0 then () else
      view#scroll_to_iter ~use_align:true ~xalign:xalign ~yalign:yalign iter |> ignore;
    if is_iter_at_yalign view iter yalign then ()
    else
      GMain.Timeout.add ~ms:50 ~callback:begin fun () ->
        scroll_aligned_alt view iter ~xalign ~yalign ~max_attempts:(max_attempts - 1);
        false
      end |> ignore
