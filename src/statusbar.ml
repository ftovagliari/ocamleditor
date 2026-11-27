open Printf
open Preferences

let default_font_size =
  let font = preferences#get.Settings_j.font in
  Str.string_after font (String.rindex font ' ' + 1) |> int_of_string

let font_size =
  if Oe_config.unify_statusbars then None else Some 9

let set_label_font_size label =
  label#misc#modify_font_by_name (match font_size with Some x -> string_of_int x | _ -> "")

let create_small_button ?tooltip ?icon ?text ?button ?callback ?packing ?show () =
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
  text |> Option.iter button#set_label;
  Gaux.may tooltip ~f:button#misc#set_tooltip_text;
  Gaux.may callback ~f:(fun callback -> ignore (button#connect#clicked ~callback));
  button;;

let create_small_toggle_button ?tooltip ?icon ?text ?callback ?packing ?show () =
  let button = GButton.toggle_button ~relief:`NONE ?packing ?show () in
  let _ = create_small_button ~button:(button :> GButton.button) ?tooltip ?icon ?text ?callback ?packing () in
  button;;

let create_button ?packing icon =
  let button = new Gtk_util.button_icon (*~icon_width:18*) ~icon_spacing:0 ~icon ?packing ~relief:`NONE () in
  button#button#set_focus_on_click false;
  button#button#misc#style_context#add_class "statusbar-button";
  button;;

(** Markup of the problems counter: errors and warnings, grayed out when there are none. *)
let problems_markup ~errors ~warnings =
  let item glyph color n =
    let color = if n = 0 then "#808080" else color in
    sprintf "<span foreground='%s'><span font_family='OCamlEditor'>%s</span>   %d</span>" color glyph n
  in
  sprintf "%s  %s" (item Icon_set.error "red" errors) (item Icon_set.warning "darkorange" warnings)

class editorbar ~view ?packing () =
  let icon_font_name = sprintf "OCamlEditor %s" (match font_size with Some x -> string_of_int x | _ -> "") in
  let box = GPack.hbox ~spacing:1 ~border_width:0 ?packing () in
  let _ = GMisc.separator `VERTICAL ~packing:box#pack () in
  let lbox = GPack.hbox ~spacing:1 ~border_width:0 ~packing:box#add () in
  let button_font_incr = create_small_button
      ~icon:Icon_set.editor_font_increase
      ~packing:lbox#pack
      ~callback:begin fun () ->
        let fd : GPango.font_description = view#misc#pango_context#font_description in
        let size = fd#size + Pango.scale in
        fd#modify ~size ();
        view#misc#modify_font fd;
      end ()
  in

  let button_font_decr = create_small_button
      ~icon:Icon_set.editor_font_decrease
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
      ~icon:Icon_set.editor_rowspacing_incr
      ~packing:lbox#pack
      ~callback:begin fun () ->
        let above, below = Preferences.preferences#get.editor_pixels_lines in
        view#set_pixels_above_lines (min (4 + above) (view#pixels_above_lines + 1));
        view#set_pixels_below_lines (min (4 + below) (view#pixels_below_lines + 1));
        Gmisclib.Idle.add view#build_gutter;
      end ()
  in
  let button_rowspacing_decr = create_small_button
      ~icon:Icon_set.editor_rowspacing_decr
      ~packing:lbox#pack
      ~callback:begin fun () ->
        view#set_pixels_above_lines (max 0 (view#pixels_above_lines - 1));
        view#set_pixels_below_lines (max 0 (view#pixels_below_lines - 1));
        Gmisclib.Idle.add view#build_gutter;
      end ()
  in

  let button_toggle_wrap = create_small_toggle_button ~icon:Icon_set.editor_word_wrap ~packing:lbox#pack () in
  let button_toggle_whitespace =
    let icon =
      let icon = Icon_set.editor_whitespace in
      match font_size with
      | Some size -> sprintf "<span font='%d'>%s</span>" size icon
      | _ -> icon
    in
    create_small_toggle_button ~icon ~packing:lbox#pack () in (* f1dd *)

  let button_dotview = create_small_toggle_button
      ~icon:Icon_set.editor_dep_graph
      ~packing:lbox#pack ()
      ~show:(false (*Oe_config.dot_version <> None*))
  in

  let button_switch_diff_base =
    create_small_toggle_button
      ~tooltip:"Toggle diff base (HEAD / Stage)"
      ~text:(Diff.show_base !Diff.base)
      ~packing:lbox#pack ()
      ~show:true
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

  (* Errors and warnings of the file; clicking goes to the next one *)
  let _ = GMisc.separator `VERTICAL ~packing:lbox#pack () in
  let problems = GButton.button ~relief:`NONE ~packing:lbox#pack () in
  let _ = problems#set_focus_on_click false in
  let _ = problems#misc#style_context#add_class "statusbar-button" in
  let problems_label = GMisc.label ~markup:(problems_markup ~errors:0 ~warnings:0) ~packing:problems#add () in
  let _ = set_label_font_size problems_label in

  let pos_box = GPack.hbox ~spacing:3 ~packing:lbox#pack () in

  let _ = GMisc.separator `VERTICAL ~packing:pos_box#pack () in
  let label_pos_lin = GMisc.label ~xalign:0.0 ~yalign:0.5 ~text:Icon_set.editor_line_number ~packing:pos_box#pack () in
  let _ = label_pos_lin#misc#modify_font_by_name icon_font_name in
  let status_pos_lin = GMisc.label ~xalign:0.0 ~yalign:0.5 ~width:34 ~packing:pos_box#pack () in
  let _ = set_label_font_size status_pos_lin in
  let _ = GMisc.separator `VERTICAL ~packing:pos_box#pack () in

  let label_pos_col = GMisc.label ~xalign:0.0 ~yalign:0.5 ~markup:Icon_set.editor_column_number ~packing:pos_box#pack () in
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
    initializer
      button_switch_diff_base#misc#modify_font_by_name "sans 8";
      button_switch_diff_base#misc#set_size_request ~width:50 ();
      button_switch_diff_base#connect#after#clicked ~callback:(fun () ->
          button_switch_diff_base#set_label (Diff.show_base !Diff.base)) |> ignore;
      view#connect#destroy ~callback:self#destroy |> ignore;
    method filename = status_filename
    method modified = status_modified
    method problems = problems
    method set_problems ~errors ~warnings =
      problems_label#set_label (problems_markup ~errors ~warnings);
      problems#misc#set_tooltip_text
        (sprintf "%d error(s), %d warning(s)\nClick to go to the next problem" errors warnings)
    method pos_box = pos_box
    method pos_lin = status_pos_lin
    method pos_col = status_pos_col
    method pos_off = status_pos_off
    method pos_sel = status_pos_sel
    method pos_sel_chars = status_pos_sel_chars
    method button_dotview = button_dotview
    method button_switch_diff_term = button_switch_diff_base
    method button_font_incr = button_font_incr
    method button_font_decr = button_font_decr
    method button_rowspacing_incr = button_rowspacing_incr
    method button_rowspacing_decr = button_rowspacing_decr
    method button_toggle_wrap = button_toggle_wrap
    method button_toggle_whitespace = button_toggle_whitespace
  end

class opambar ?switch ?packing () =
  let box = GPack.hbox ~spacing:8 ?packing () in
  let button_switch = create_button ~packing:box#add "" in
  let switch_changed = new switch_changed () in
  object (self)
    inherit GObj.widget box#as_widget
    method set_switch switch =
      let sw = switch |> Option.value ~default:(Opam.switch_show()) in
      switch_changed#call sw;
      button_switch#set_icon Icon_set.opam_switch;
      button_switch#set_label sw
    initializer
      self#set_switch switch;
      button_switch#button#connect#clicked ~callback:begin fun () ->
        let popover = GBin.popover ~relative_to:button_switch#coerce () in
        let vbox = GPack.vbox ~border_width:8 ~packing:popover#add () in
        let _ = GMisc.label ~markup:"<b>Opam switch</b>" ~packing:vbox#add () in
        let _ = GMisc.separator `HORIZONTAL ~packing:vbox#add () in
        let radio_group = ref None in
        let radios =
          Opam.switch_list()
          |> List.map begin fun switch ->
            let active = switch = button_switch#label in
            let radio = GButton.radio_button ~label:switch ~active ?group:!radio_group ~packing:vbox#add () in
            begin
              match !radio_group with
              | None -> radio_group := Some radio#group;
              | _ -> ()
            end;
            switch, radio
          end
        in
        radios |> List.iter begin fun (sw, radio) ->
          radio#connect#after#clicked ~callback:begin fun () ->
            if radio#active then begin
              self#set_switch (Some sw);
              popover#popdown ();
            end
          end |> ignore;
        end;
        popover#popup ();
      end |> ignore;
    method connect = new opambar_signals ~switch_changed
  end

and switch_changed () = object inherit [string] GUtil.signal () end
and opambar_signals ~switch_changed =
  object
    inherit GUtil.ml_signals [switch_changed#disconnect]
    method switch_changed = switch_changed#connect ~after
  end

class gitbar ?packing () =
  let box = GPack.hbox ~spacing:8 ?packing () in
  let button_gitunpushed = create_button ~packing:box#add Icon_set.git_unpushed in
  let button_gitpending = create_button ~packing:box#add Icon_set.git_pending in
  let button_gitbranch = create_button ~packing:box#add Icon_set.git_branch in
  let button_gitpath = create_button ~packing:box#add Icon_set.git_path in
  object (self)
    inherit GObj.widget box#as_widget
    initializer
      button_gitpending#button#connect#clicked ~callback:begin fun () ->
        Git.diff_stat begin fun widget ->
          let popover = GBin.popover ~relative_to:button_gitpending#coerce () in
          popover#add widget;
          popover#popup()
        end;
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
  let spinner = GMisc.spinner ~width:20 ~packing:box#pack () in
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
      (* Derive the colors from the toplevel: reading them from [ebox] itself would feed the colors
         set by [modify_bg] back into [style#light], making the bar lighter at every "style_set"
         (e.g. each time the window gets or loses the focus). *)
      let base = ebox#misc#toplevel in
      ebox#misc#modify_bg [`NORMAL, `COLOR (base#misc#style#light `NORMAL)];
      ebox#misc#modify_fg [`NORMAL, `COLOR (base#misc#style#fg `NORMAL)];
  end

