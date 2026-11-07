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


module ColorOps = Color

open Pref_page
open Preferences
open Settings_t
open Option_syntax

(** pref_color *)
class pref_color title ?packing () =
  let vbox                = GPack.vbox ~spacing ?packing () in
  let notebook            = GPack.notebook ~packing:vbox#add () in
  let border_width        = 5 in
  (* tags *)
  let cols                = new GTree.column_list in
  let tag_col             = cols#add Gobject.Data.string in
  let lab_col             = cols#add Gobject.Data.string in
  let tag_model           = GTree.list_store cols in
  (* Syntax coloring *)
  let color_ocaml         = GPack.vbox ~border_width ~spacing:8 () in
  let _                   = notebook#append_page ~tab_label:(GMisc.label ~text:"OCaml" ())#coerce color_ocaml#coerce in
  let hbox                = GPack.hbox ~spacing:8 ~packing:color_ocaml#pack () in
  let _                   = GMisc.label ~text:"Default background color:" ~xalign ~packing:hbox#pack () in
  let button_default_bg   = GButton.color_button ~packing:hbox#pack () in
  let _                   = button_default_bg#set_relief `NONE in
  let box_tag             = GPack.hbox ~border_width:0 ~spacing:8 ~packing:(color_ocaml#pack ~expand:true) () in
  let sw                  = GBin.scrolled_window ~shadow_type:`IN ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
      ~packing:(box_tag#pack ~expand:true) () in
  let view_tag            = GTree.view ~headers_visible:false ~model:tag_model ~packing:sw#add () in
  let renderer            = GTree.cell_renderer_text [] in
  let vc_tag              = GTree.view_column ~renderer:(renderer, ["text", tag_col]) () in
  let _                   = view_tag#append_column vc_tag in
  let vc_lab              = GTree.view_column ~title:"Text Elements" ~renderer:(renderer, ["text", lab_col]) () in
  let _                   = view_tag#append_column vc_lab in
  let _                   = vc_tag#set_visible false in
  let prop_box            = GPack.vbox ~border_width:0 ~packing:box_tag#add () in
  let table               = GPack.table ~packing:prop_box#pack () in
  let label_tag_bg        = GMisc.label ~text:"Background:" ~xalign:0.0 ~packing:(table#attach ~top:0 ~left:0) () in
  let button_tag_bg       = GButton.color_button ~packing:(table#attach ~top:0 ~left:1) () in
  let _                   = button_tag_bg#set_relief `NONE in
  let check_tag_bg        = GButton.check_button ~label:"Use default" ~packing:(table#attach ~top:0 ~left:2) () in
  let _                   = GMisc.label ~text:"Foreground:" ~xalign:0.0 ~packing:(table#attach ~top:1 ~left:0) () in
  let button_tag_fg       = GButton.color_button ~packing:(table#attach ~top:1 ~left:1) () in
  let _                   = button_tag_fg#set_relief `NONE in
  let adjustment          = GData.adjustment ~lower:0.0 ~upper:1100.0 ~step_incr:100. ~page_incr:300. () in
  let _                   = GMisc.label ~markup:"Weight:" ~xalign:0.0 ~packing:(table#attach ~top:2 ~left:0) () in
  let scale_tag_weight    = GRange.scale `HORIZONTAL ~digits:0 ~value_pos:`RIGHT ~draw_value:false ~adjustment
      ~packing:(table#attach ~top:2 ~left:1 ~expand:`X ~fill:`X) () in
  let label_weight        = GMisc.label ~xalign:0.0 ~xpad:8 ~width:100 ~packing:(table#attach ~top:2 ~left:2) () in
  let weights = [
    0, "Off";
    100, "Thin";
    200, "Ultralight";
    300, "Light";
    400, "Normal";
    500, "Meduim";
    600, "Semibold";
    700, "Bold";
    800, "Ultrabold";
    900, "Heavy";
    1000, "Ultraheavy";
  ] in
  let clamp_weight x = int_of_float (x /. 100.) * 100 in
  let _                   = adjustment#connect#value_changed ~callback:begin fun _ ->
      if (int_of_float adjustment#value) mod 100 > 0 then
        clamp_weight adjustment#value |> float |> adjustment#set_value
    end in
  let _                   = adjustment#connect#notify_value ~callback:begin fun x ->
      try
        weights |> List.assoc (clamp_weight x) |> label_weight#set_text
      with ex ->
        Printf.eprintf "File \"pref_color.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());
    end in
  let check_tag_style     = GButton.check_button ~label:"Italic" ~packing:(table#attach ~top:3 ~left:0) () in
  let check_tag_underline = GButton.check_button ~label:"Underline" ~packing:(table#attach ~top:4 ~left:0) () in
  let _                   = check_tag_style#set_image (GMisc.image ~stock:`ITALIC ())#coerce in
  let _                   = check_tag_underline#set_image (GMisc.image ~stock:`UNDERLINE ())#coerce in
  let box_odoc_bg         = GPack.hbox ~spacing:5 ~packing:prop_box#pack ~show:false () in
  let _                   = GMisc.label ~xalign:0.0
      ~markup:"Paragraph background colors:" (* \n<small>(only applies to comments preceded\nby a blank line)</small> *)
      ~packing:box_odoc_bg#pack () in
  let button_odoc_bg      = GButton.color_button ~packing:box_odoc_bg#pack () in
  let _                   = button_odoc_bg#set_relief `NONE in
  let button_odoc_bg2     = GButton.color_button ~packing:box_odoc_bg#pack () in
  let _                   = button_odoc_bg2#set_relief `NONE in
  (* OCaml Preview *)
  let osw                 = GBin.scrolled_window ~height:200 ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~shadow_type:`IN () in
  let tag_group           = Text_tag.create_group "preview" in
  let buffer              = new Ocaml_text.buffer ~lexical_enabled:true ~tag_group () in
  let _                   = buffer#set_text ocaml_preview in
  let preview             = new Ocaml_text.view ~buffer () in
  let _                   = Gmisclib.Idle.add (fun () -> preview#buffer#place_cursor ~where:(preview#buffer#start_iter#forward_lines 5)) in
  let _                   = Preferences_apply.apply (preview :> Text.view) Preferences.preferences#get in
  let _                   = preview#set_editable false in
  let _                   = osw#add preview#coerce in
  let _                   = color_ocaml#add osw#coerce in
  (* color_compl *)
  let color_compl         = GPack.vbox ~border_width () in
  let table               = GPack.table ~row_spacings ~col_spacings ~packing:color_compl#pack () in
  let _                   = GMisc.label ~text:"Background color:" ~xalign ~packing:(table#attach ~top:0 ~left:0) () in
  let _                   = GMisc.label ~text:"Foreground color:" ~xalign ~packing:(table#attach ~top:1 ~left:0) () in
  let button_tag_bg_popup = GButton.color_button ~packing:(table#attach ~top:0 ~left:1 ~expand:`X) () in
  let button_tag_fg_popup = GButton.color_button ~packing:(table#attach ~top:1 ~left:1 ~expand:`X) () in
  let _                   = notebook#append_page ~tab_label:(GMisc.label ~text:"Inferred Types" ())#coerce color_compl#coerce in
  (* color_other *)
  let color_other         = GPack.vbox ~border_width () in
  let table               = GPack.table ~row_spacings ~col_spacings ~packing:color_other#pack () in
  let button_bg           = GButton.color_button ~packing:(table#attach ~top:0 ~left:1 ~expand:`X) () in
  let button_fg_stdin     = GButton.color_button ~packing:(table#attach ~top:1 ~left:1 ~expand:`X) () in
  let button_fg_stdout    = GButton.color_button ~packing:(table#attach ~top:2 ~left:1 ~expand:`X) () in
  let button_fg_err       = GButton.color_button ~packing:(table#attach ~top:3 ~left:1 ~expand:`X) () in
  let button_fg_warn      = GButton.color_button ~packing:(table#attach ~top:4 ~left:1 ~expand:`X) () in
  let _                   = GMisc.label ~xalign ~text:"Background color:" ~packing:(table#attach ~top:0 ~left:0) () in
  let _                   = GMisc.label ~xalign ~text:"Standard input:" ~packing:(table#attach ~top:1 ~left:0) () in
  let _                   = GMisc.label ~xalign ~text:"Standard output:" ~packing:(table#attach ~top:2 ~left:0) () in
  let _                   = GMisc.label ~xalign ~text:"Errors:" ~packing:(table#attach ~top:3 ~left:0) () in
  let _                   = GMisc.label ~xalign ~text:"Warnings:" ~packing:(table#attach ~top:4 ~left:0) () in
  (* *)
  let _                   = notebook#append_page ~tab_label:(GMisc.label ~text:"Message Pane" ())#coerce color_other#coerce in
  object (self)
    inherit page title vbox
    val mutable signals = []
    val mutable pref_copy = None

    initializer
      let signal_id = Preferences.preferences#connect#changed ~callback:self#read in
      self#misc#connect#destroy ~callback:(fun () ->
          Preferences.preferences#connect#disconnect signal_id) |> ignore;
      view_tag#selection#connect#after#changed ~callback:self#enable_controls |> ignore;
      signals <- [
        check_tag_bg#coerce,        check_tag_bg#connect#toggled ~callback:self#update_preview;
        button_tag_fg#coerce,       button_tag_fg#connect#color_set ~callback:self#update_preview;
        button_tag_bg#coerce,       button_tag_bg#connect#color_set ~callback:self#update_preview;
        button_default_bg#coerce,   button_default_bg#connect#color_set ~callback:self#update_preview;
        button_odoc_bg#coerce,      button_odoc_bg#connect#color_set ~callback:self#update_preview;
        button_odoc_bg2#coerce,     button_odoc_bg2#connect#color_set ~callback:self#update_preview;
        scale_tag_weight#coerce,    scale_tag_weight#connect#value_changed ~callback:self#update_preview;
        check_tag_style#coerce,     check_tag_style#connect#clicked ~callback:self#update_preview;
        check_tag_underline#coerce, check_tag_underline#connect#clicked ~callback:self#update_preview;
      ];
      check_tag_bg#connect#after#clicked ~callback:(fun () ->
          button_tag_bg#misc#set_sensitive (not check_tag_bg#active)) |> ignore;

    method private copy_pref base =
      pref_copy <- Some begin
          let module C = Preferences.Color in
          { base with
            (* The font preferred by the user, rather than the default one (although they
               are the same initially, they become different when "Reset Page Defaults" is performed) *)
            editor_base_font = preferences#get.editor_base_font;
            editor_bg_color_theme = base.editor_bg_color_theme;
            editor_bg_color_user =
              C.new_themed_color (button_default_bg#color |> ColorOps.name_of_gdk) base.editor_bg_color_user;
            editor_ocamldoc_paragraph_bgcolor_1 =
              C.new_themed_color
                (Some (button_odoc_bg#color |> ColorOps.name_of_gdk)) base.editor_ocamldoc_paragraph_bgcolor_1;
            editor_ocamldoc_paragraph_bgcolor_2 =
              C.new_themed_color
                (Some (button_odoc_bg2#color |> ColorOps.name_of_gdk)) base.editor_ocamldoc_paragraph_bgcolor_2;
          } end

    method write pref =
      let set tc (btn : GButton.color_button) =
        Preferences.Color.set_themed_color tc (btn#color |> ColorOps.name_of_gdk) in
      let set_opt tc (btn : GButton.color_button) =
        Preferences.Color.set_themed_color tc (Some (btn#color |> ColorOps.name_of_gdk)) in
      pref.editor_bg_color_theme <- false;
      set pref.editor_bg_color_popup                  button_tag_bg_popup;
      set pref.editor_fg_color_popup                  button_tag_fg_popup;
      set pref.editor_bg_color_user                   button_default_bg;
      set pref.output_bg_color                        button_bg;
      set pref.output_stdin_fg_color                  button_fg_stdin;
      set pref.output_stdout_fg_color                 button_fg_stdout;
      set pref.output_err_fg_color                    button_fg_err;
      set pref.output_warn_fg_color                   button_fg_warn;
      set_opt pref.editor_ocamldoc_paragraph_bgcolor_1 button_odoc_bg;
      set_opt pref.editor_ocamldoc_paragraph_bgcolor_2 button_odoc_bg2;
      let@ pref_copy = pref_copy in
      pref.editor_tags <- pref_copy.editor_tags;

    method read pref =
      (`NAME ??(pref.editor_bg_color_popup)) |> GDraw.color |> button_tag_bg_popup#set_color;
      (`NAME ??(pref.editor_fg_color_popup)) |> GDraw.color |> button_tag_fg_popup#set_color;
      (`NAME ??(pref.editor_bg_color_user)) |> GDraw.color |> button_default_bg#set_color;
      tag_model#clear();
      let tags = List.sort (fun a b -> compare a.Settings_t.name b.name) pref.editor_tags in
      tags |> List.iter begin fun tag ->
        let tagname = tag.Settings_t.name in
        let row = tag_model#append () in
        let label = Preferences.editor_tag_label tagname in
        tag_model#set ~row ~column:tag_col tagname;
        tag_model#set ~row ~column:lab_col label;
      end;
      view_tag#selection#select_path (GTree.Path.create [0]);
      (`NAME ??(pref.output_bg_color))     |> GDraw.color |> button_bg#set_color;
      (`NAME ??(pref.output_stdin_fg_color))  |> GDraw.color |> button_fg_stdin#set_color;
      (`NAME ??(pref.output_stdout_fg_color)) |> GDraw.color |> button_fg_stdout#set_color;
      (`NAME ??(pref.output_err_fg_color))    |> GDraw.color |> button_fg_err#set_color;
      (`NAME ??(pref.output_warn_fg_color))   |> GDraw.color |> button_fg_warn#set_color;
      ??(pref.editor_ocamldoc_paragraph_bgcolor_1) |> Option.iter (fun c -> (`NAME c) |> GDraw.color |> button_odoc_bg#set_color);
      ??(pref.editor_ocamldoc_paragraph_bgcolor_2) |> Option.iter (fun c -> (`NAME c) |> GDraw.color |> button_odoc_bg2#set_color);
      self#copy_pref pref;
      self#update_preview ();

    method private selected_tag =
      match view_tag#selection#get_selected_rows with
      | path :: _ ->
          let row = tag_model#get_iter path in
          Some (tag_model#get ~row ~column:tag_col)
      | _ -> None

    method private enable_controls () =
      self#selected_tag |> Option.iter begin fun current_tag ->
        List.iter (fun (w, s) -> w#misc#handler_block s) signals;
        begin
          let open Settings_t in
          match preferences#get.editor_tags |> List.find_opt (fun t -> t.name = current_tag) with
          | Some t ->
              (* Enable font controls *)
              (`NAME ??(t.color)) |> GDraw.color |> button_tag_fg#set_color;
              scale_tag_weight#adjustment#set_value (float_of_int t.weight);
              check_tag_style#set_active (t.style <> `NORMAL);
              check_tag_underline#set_active (t.underline <> `NONE);
              check_tag_bg#set_active t.bg_default;
              (`NAME ??(t.bg_color)) |> GDraw.color |> button_tag_bg#set_color;
          | _ -> ()
        end;
        (* Extra controls for ocamldoc *)
        if Oe_config.ocamldoc_paragraph_bgcolor_enabled && current_tag = Text_tag.Name.ocamldoc
        then begin
          box_odoc_bg#misc#show();
          label_tag_bg#misc#set_sensitive false;
          button_tag_bg#misc#set_sensitive false;
          check_tag_bg#misc#set_sensitive false;
        end else begin
          box_odoc_bg#misc#hide();
          label_tag_bg#misc#set_sensitive true;
          button_tag_bg#misc#set_sensitive true;
          check_tag_bg#misc#set_sensitive true;
        end;
        (* Extra controls for current line highlight *)
        if current_tag = Text_tag.Name.current_line_highlight then begin
          check_tag_bg#set_active false;
          check_tag_bg#misc#set_sensitive false;
        end;
        button_tag_bg#misc#set_sensitive (not check_tag_bg#active);
        (*self#update_preview();*)
        List.iter (fun (w, s) -> w#misc#handler_unblock s) signals;
      end

    method private update_preview () =
      self#selected_tag |> Option.iter begin fun selected_tag ->
        try
          begin
            let@ pref_copy = pref_copy in
            let module C = Preferences.Color in
            let color      = button_tag_fg#color |> ColorOps.name_of_gdk in
            let weight     = scale_tag_weight#adjustment#value |> int_of_float in
            let style      = if check_tag_style#active then `ITALIC else `NORMAL in
            let underline  = if check_tag_underline#active then `SINGLE else `NONE in
            let scale      = editor_tag_scale selected_tag in
            let bg_default = check_tag_bg#active in
            let bg_color   = button_tag_bg#color |> ColorOps.name_of_gdk in
            let base_bg_color =
              C.new_themed_color
                (button_default_bg#color |> ColorOps.name_of_gdk)
                preferences#get.editor_bg_color_user in
            let odoc_par_bg_color_1 = C.new_themed_color
                (Some (button_odoc_bg#color |> ColorOps.name_of_gdk)) preferences#get.editor_ocamldoc_paragraph_bgcolor_1 in
            let odoc_par_bg_color_2 = C.new_themed_color
                (Some (button_odoc_bg2#color |> ColorOps.name_of_gdk)) preferences#get.editor_ocamldoc_paragraph_bgcolor_2 in
            match pref_copy.editor_tags |> List.find_opt (fun t -> t.name = selected_tag) with
            | Some tag ->
                let new_editor_tags =
                  { tag with
                    color = C.new_themed_color color tag.color;
                    bg_color = C.new_themed_color bg_color tag.bg_color;
                    scale; weight; style; underline; bg_default;
                  } :: (pref_copy.editor_tags |> List.filter (fun t -> t.name <> selected_tag))
                in
                pref_copy.editor_bg_color_user <- base_bg_color;
                pref_copy.editor_ocamldoc_paragraph_bgcolor_1 <- odoc_par_bg_color_1;
                pref_copy.editor_ocamldoc_paragraph_bgcolor_2 <- odoc_par_bg_color_2;
                pref_copy.editor_tags <- new_editor_tags;
                Text_tag.update_properties tag_group pref_copy;
                Preferences_apply.apply (preview :> Text.view) pref_copy;
                Lexical.tag ~tags:tag_group (preview#buffer :> GText.buffer);
            | _ -> failwith (Printf.sprintf "tag %s not fount \n%!" selected_tag);
          end;
        with ex -> Printf.eprintf "%s\n%s\n%s\n%!" __LOC__ (Printexc.to_string ex) (Printexc.get_backtrace());
      end
  end

(** pref_color_structure *)
and pref_color_structure title ?packing () =
  let vbox                = GPack.vbox ~spacing ?packing () in
  let table               = GPack.table ~row_spacings ~col_spacings ~packing:vbox#pack () in
  let _                   = GMisc.label ~text:"Base background color:" ~xalign ~packing:(table#attach ~top:0 ~left:0) () in
  let _                   = GMisc.label ~text:"Base foreground color:" ~xalign ~packing:(table#attach ~top:1 ~left:0) () in
  let _                   = GMisc.label ~text:"Selected item background color:" ~xalign ~packing:(table#attach ~top:2 ~left:0) () in
  let _                   = GMisc.label ~text:"Selected item foreground color:" ~xalign ~packing:(table#attach ~top:3 ~left:0) () in
  let _                   = GMisc.label ~text:"Active item background color:" ~xalign ~packing:(table#attach ~top:4 ~left:0) () in
  let _                   = GMisc.label ~text:"Active item foreground color:" ~xalign ~packing:(table#attach ~top:5 ~left:0) () in
  let _                   = GMisc.label ~text:"Types foreground color:" ~xalign ~packing:(table#attach ~top:6 ~left:0) () in
  let check_alt_rows      = GButton.check_button ~label:"Draw rows in alternating colors" ~packing:(table#attach ~top:7 ~left:0) () in
  let button_color_nor_bg = GButton.color_button ~packing:(table#attach ~top:0 ~left:1 ~expand:`X) () in
  let button_color_nor_fg = GButton.color_button ~packing:(table#attach ~top:1 ~left:1 ~expand:`X) () in
  let button_color_sel_bg = GButton.color_button ~packing:(table#attach ~top:2 ~left:1 ~expand:`X) () in
  let button_color_sel_fg = GButton.color_button ~packing:(table#attach ~top:3 ~left:1 ~expand:`X) () in
  let button_color_act_bg = GButton.color_button ~packing:(table#attach ~top:4 ~left:1 ~expand:`X) () in
  let button_color_act_fg = GButton.color_button ~packing:(table#attach ~top:5 ~left:1 ~expand:`X) () in
  let button_color_types  = GButton.color_button ~packing:(table#attach ~top:6 ~left:1 ~expand:`X) () in
  object (self)
    inherit page title vbox

    initializer
      let signal_id = Preferences.preferences#connect#changed ~callback:self#read in
      self#misc#connect#destroy ~callback:(fun () -> Preferences.preferences#connect#disconnect signal_id) |> ignore;

    method read pref =
      (`NAME ??(pref.outline_color_types))  |> GDraw.color |> button_color_types#set_color;
      (`NAME ??(pref.outline_color_nor_bg)) |> GDraw.color |> button_color_nor_bg#set_color;
      (`NAME ??(pref.outline_color_nor_fg)) |> GDraw.color |> button_color_nor_fg#set_color;
      (`NAME ??(pref.outline_color_sel_bg)) |> GDraw.color |> button_color_sel_bg#set_color;
      (`NAME ??(pref.outline_color_sel_fg)) |> GDraw.color |> button_color_sel_fg#set_color;
      (`NAME ??(pref.outline_color_act_bg)) |> GDraw.color |> button_color_act_bg#set_color;
      (`NAME ??(pref.outline_color_act_fg)) |> GDraw.color |> button_color_act_fg#set_color;
      check_alt_rows#set_active (pref.outline_color_alt_rows <> None);

    method write pref =
      let set tc btn = Preferences.Color.set_themed_color tc (btn#color |> ColorOps.name_of_gdk) in
      set pref.outline_color_types  button_color_types;
      set pref.outline_color_nor_bg button_color_nor_bg;
      set pref.outline_color_nor_fg button_color_nor_fg;
      set pref.outline_color_sel_bg button_color_sel_bg;
      set pref.outline_color_sel_fg button_color_sel_fg;
      set pref.outline_color_act_bg button_color_act_bg;
      set pref.outline_color_act_fg button_color_act_fg;
      pref.outline_color_alt_rows <- if check_alt_rows#active then Some 0.95 else None

  end
