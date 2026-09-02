open Printf
open Preferences

let default_font_size =
  let font = preferences#get.Settings_j.font in
  Str.string_after font (String.rindex font ' ' + 1) |> int_of_string

let font_size =
  if Oe_config.unify_statusbars then None else Some 9

let set_label_font_size label =
  label#misc#modify_font_by_name (match font_size with Some x -> string_of_int x | _ -> "")

let create_small_button ?tooltip ?icon ?button ?callback ?packing ?show () =
  let button =
    match button with
    | Some b -> b
    | _ -> GButton.button ~relief:`NONE ?packing ?show ()
  in
  button#set_focus_on_click false;
  button#misc#style_context#add_class "statusbar-button";
  icon |> Option.iter begin fun icon ->
    let icon =
      match font_size with
      | Some size -> sprintf "<span font='%d'>%s</span>" size icon
      | _ -> icon
    in
    (Gtk_util.label_icon (*~height:14*) ~packing:button#add icon)#coerce |> ignore
  end;
  Gaux.may tooltip ~f:button#misc#set_tooltip_text;
  Gaux.may callback ~f:(fun callback -> ignore (button#connect#clicked ~callback));
  button;;

let create_small_toggle_button ?tooltip ~icon ?callback ?packing ?show () =
  let button = GButton.toggle_button ~relief:`NONE ?packing ?show () in
  let _ = create_small_button ~button:(button :> GButton.button) ?tooltip ~icon ?callback ?packing () in
  button;;

class editorbar ~view ?packing () =
  let icon_font_name = sprintf "FiraCode OCamlEditor %s" (match font_size with Some x -> string_of_int x | _ -> "") in
  let box = GPack.hbox ~spacing:1 ~border_width:0 ?packing () in
  let _ = GMisc.separator `VERTICAL ~packing:box#pack () in
  let lbox = GPack.hbox ~spacing:1 ~border_width:0 ~packing:box#add () in
  let button_font_incr = create_small_button
      ~icon:"\u{f09f4}"
      ~packing:lbox#pack
      ~callback:begin fun () ->
        let fd : GPango.font_description = view#misc#pango_context#font_description in
        let size = fd#size + Pango.scale in
        fd#modify ~size ();
        view#misc#modify_font fd;
      end ()
  in

  let button_font_decr = create_small_button
      ~icon:"\u{f09f3}"
      ~packing:lbox#pack
      ~callback:begin fun () ->
        let fd : GPango.font_description = view#misc#pango_context#font_description in
        let size = fd#size in
        if size - Pango.scale >= (7 * Pango.scale) then begin
          let size = size - Pango.scale in
          fd#modify ~size ();
          view#misc#modify_font fd;
        end
      end ()
  in
  let button_rowspacing_incr = create_small_button
      ~icon:"\u{f084f}"
      ~packing:lbox#pack
      ~callback:begin fun () ->
        let above, below = Preferences.preferences#get.editor_pixels_lines in
        view#set_pixels_above_lines (min (4 + above) (view#pixels_above_lines + 1));
        view#set_pixels_below_lines (min (4 + below) (view#pixels_below_lines + 1));
        Gmisclib.Idle.add view#build_gutter;
      end ()
  in
  let button_rowspacing_decr = create_small_button
      ~icon:"\u{f084d}"
      ~packing:lbox#pack
      ~callback:begin fun () ->
        view#set_pixels_above_lines (max 0 (view#pixels_above_lines - 1));
        view#set_pixels_below_lines (max 0 (view#pixels_below_lines - 1));
        Gmisclib.Idle.add view#build_gutter;
      end ()
  in

  let button_toggle_wrap = create_small_toggle_button ~icon:"\u{eb80}" ~packing:lbox#pack () in
  let button_toggle_whitespace =
    let icon =
      let icon = "\u{eb7d}" in
      match font_size with
      | Some size -> sprintf "<span font='%d'>%s</span>" size icon
      | _ -> icon
    in
    create_small_toggle_button ~icon ~packing:lbox#pack () in (* f1dd *)

  let button_dotview = create_small_toggle_button
      ~icon:"\u{f104a}"
      ~packing:lbox#pack ()
      ~show:(false (*Oe_config.dot_version <> None*))
  in
  let _ = GMisc.separator `VERTICAL ~show:true ~packing:lbox#pack () in

  let status_filename = GMisc.label ~selectable:true ~xalign:0.0 ~xpad:5 ~ellipsize:`END ~packing:lbox#add () in
  let _ = status_filename#set_use_markup true in
  let _ = set_label_font_size status_filename in
  let _ = GMisc.separator `VERTICAL ~packing:lbox#pack () in
  let status_modified = GMisc.label ~markup:"" ~xpad:0 ~ypad:0 ~xalign:0.5 ~packing:lbox#pack () in
  let _ = status_modified#misc#modify_fg [`NORMAL, `NAME "#1E90FF"] in
  let _ = status_modified#set_use_markup true in
  let _ = set_label_font_size status_modified in

  let pos_box = GPack.hbox ~spacing:3 ~packing:lbox#pack () in

  let _ = GMisc.separator `VERTICAL ~packing:pos_box#pack () in
  let label_pos_lin = GMisc.label ~xalign:0.0 ~yalign:0.5 ~text:"\u{e0a1}\u{2009}" ~packing:pos_box#pack () in
  let _ = label_pos_lin#misc#modify_font_by_name icon_font_name in
  let status_pos_lin = GMisc.label ~xalign:0.0 ~yalign:0.5 ~width:34 ~packing:pos_box#pack () in
  let _ = set_label_font_size status_pos_lin in
  let _ = GMisc.separator `VERTICAL ~packing:pos_box#pack () in

  let label_pos_col = GMisc.label ~xalign:0.0 ~yalign:0.5 ~markup:"<small>\u{e0a3}</small>\u{2009}" ~packing:pos_box#pack () in
  let _ = label_pos_col#misc#modify_font_by_name icon_font_name in
  let status_pos_col = GMisc.label ~xalign:0.0 ~yalign:0.5 ~width:34 ~packing:pos_box#pack () in
  let _ = set_label_font_size status_pos_col in
  let _ = GMisc.separator `VERTICAL ~packing:pos_box#pack () in

  let status_pos_off = GMisc.label ~xalign:0.0 ~yalign:0.5 ~width:55 ~packing:pos_box#pack () in
  let _ = set_label_font_size status_pos_off in
  (*let _ = status_pos_off#misc#set_tooltip_text "Character offset from start" in*)
  let _ = GMisc.separator `VERTICAL ~packing:pos_box#pack () in

  let status_pos_sel = GMisc.label ~xalign:0.0 ~yalign:0.5 ~width:34 ~text:"0" ~packing:pos_box#pack () in
  (*let _ = status_pos_sel#misc#set_tooltip_text "Selected lines" in*)
  let _ = set_label_font_size status_pos_sel in
  let _ = GMisc.separator `VERTICAL ~packing:pos_box#pack () in
  let status_pos_sel_chars = GMisc.label ~xalign:0.0 ~yalign:0.5 ~width:55 ~text:"0" ~packing:pos_box#pack () in
  let _ = set_label_font_size status_pos_sel_chars in
  (*let _ = status_pos_sel_chars#misc#set_tooltip_text "Selected characters" in*)
  let _ = GMisc.separator `VERTICAL ~packing:pos_box#pack () in
  (* TODOL Labltgk3 issue, paned#set_position *)
  (*let _ = paned#set_position 900 in*)
  object (self)
    inherit GObj.widget box#as_widget
    method filename = status_filename
    method modified = status_modified
    method pos_box = pos_box
    method pos_lin = status_pos_lin
    method pos_col = status_pos_col
    method pos_off = status_pos_off
    method pos_sel = status_pos_sel
    method pos_sel_chars = status_pos_sel_chars
    method button_dotview = button_dotview
    method button_font_incr = button_font_incr
    method button_font_decr = button_font_decr
    method button_rowspacing_incr = button_rowspacing_incr
    method button_rowspacing_decr = button_rowspacing_decr
    method button_toggle_wrap = button_toggle_wrap
    method button_toggle_whitespace = button_toggle_whitespace
  end

class gitbar ?packing () =
  let box = GPack.hbox ~spacing:8 ?packing () in
  let create_gitbutton icon =
    let button = new Gtk_util.button_icon (*~icon_width:18*) ~icon_spacing:0 ~icon ~packing:box#add ~relief:`NONE () in
    button#button#set_focus_on_click false;
    button#button#misc#style_context#add_class "statusbar-button";
    button
  in
  let button_gitunpushed = create_gitbutton "\u{eaa1}" in
  let button_gitpending = create_gitbutton "\u{ea73}" in
  let button_gitbranch = create_gitbutton "\u{f062c}" in
  let button_gitpath = create_gitbutton "\u{e65d}" in
  object (self)
    inherit GObj.widget box#as_widget
    initializer
      button_gitpending#button#connect#clicked ~callback:begin fun () ->
        Git.diff_stat (Git.show_diff_stat None);
      end |> ignore;
    method button_gitunpushed = button_gitunpushed
    method button_gitpending = button_gitpending
    method button_gitpath = button_gitpath
    method button_gitbranch = button_gitbranch
  end

class widget ?packing () =
  let ebox = GBin.event_box ~border_width:0 ?packing () in
  let box = GPack.hbox ~spacing:1 ~border_width:0 ~packing:ebox#add () in
  (*let statusbar = GMisc.statusbar (*~spacing:1*) ~border_width:0 ~packing:ebox#add () in*)
  let editorbar_placeholder = GBin.alignment ~packing:(box#pack ~from:`START ~expand:true ~fill:true) () in
  let _ = GMisc.separator `VERTICAL ~packing:(box#pack ~expand:false) () in
  let spinner = GMisc.image ~width:20 ~packing:box#pack () in
  let _ = GMisc.separator `VERTICAL ~packing:(box#pack ~expand:false) () in
  (*let context_flash = statusbar#new_context ~name:"flash-messages" in*)
  object (self)
    inherit GObj.widget ebox#as_widget
    initializer
      self#set_custom_style();
      ebox#misc#toplevel#misc#connect#after#style_set
        ~callback:(fun () -> Gmisclib.Idle.add ~prio:300 self#set_custom_style) |> ignore

    method pack_editorbar (bar : editorbar) =
      if editorbar_placeholder#children <> [] then
        editorbar_placeholder#remove editorbar_placeholder#child#coerce;
      editorbar_placeholder#add bar#coerce;

    method pack ?from widget = box#pack ?from ~expand:false ~fill:false widget

    method spinner = spinner

    method private set_custom_style () =
      ebox#misc#modify_bg [`NORMAL, `COLOR (ebox#misc#style#light `NORMAL)];
      ebox#misc#modify_fg [`NORMAL, `COLOR (ebox#misc#style#fg `NORMAL)];
  end

