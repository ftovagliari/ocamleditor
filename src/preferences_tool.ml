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

open Pref_page
open Pref_color
open Printf
open Preferences

(** preferences *)
class preferences ~editor () =
  let original_pref = { Preferences.preferences#get with timestamp = Preferences.preferences#get.timestamp } in
  let initial_gtk_theme = Preferences.preferences#get.theme in
  let initial_compl_decorated = Preferences.preferences#get.editor_completion_decorated in
  let initial_general_font = Preferences.preferences#get.font in
  let window            = GWindow.window ~resizable:true ~width:1200
      ~type_hint:`DIALOG ~modal:true ~title:"Preferences" ~position:`CENTER ~icon:(??? Icons.oe) ~show:false () in
  let _ = Gmisclib.Window.GeometryMemo.add ~key:"dialog-preferences" ~window Preferences.geometry_memo in
  let _                 = Gaux.may (GWindow.toplevel editor) ~f:(fun w -> window#set_transient_for w#as_window) in
  let vbox              = GPack.vbox ~border_width:8 ~spacing:8 ~packing:window#add () in
  let hbox              = GPack.hbox ~spacing:8 ~packing:vbox#add () in
  let cols              = new GTree.column_list in
  let column            = cols#add Gobject.Data.string in
  let model             = GTree.tree_store cols in
  let renderer          = GTree.cell_renderer_text [] in
  let view_column       = GTree.view_column ~title:"File" ~renderer:(renderer, ["text", column]) () in
  let sw                = GBin.scrolled_window ~shadow_type:`IN ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
      ~packing:(hbox#pack ~expand:true) () in
  let view              = GTree.view ~model:model ~headers_visible:false ~reorderable:false ~width:360
      ~height:300 ~packing:sw#add () in
  let _                 = view#append_column view_column in
  let _                 = GMisc.separator `HORIZONTAL ~packing:vbox#pack () in
  let button_box        = GPack.button_box `HORIZONTAL ~layout:`END ~border_width:5
      ~spacing:8 ~packing:(vbox#pack ~expand:false) () in
  let ok_button         = GButton.button ~label:"OK" ~packing:button_box#add () in
  let cancel_button     = GButton.button ~label:"Cancel" ~packing:button_box#add () in
  let reset_button      = GButton.button ~label:"Reset All" ~packing:button_box#add () in
  let _                 = button_box#set_child_secondary reset_button#coerce true in
  let reset_page_button = GButton.button ~label:"Reset Page" ~packing:button_box#add () in
  let _                 = button_box#set_child_secondary reset_page_button#coerce true in
  object (self)
    inherit GObj.widget vbox#as_widget
    val mutable pages : (string * Pref_page.page) list = []
    val mutable current = ""

    method private show_page title =
      let page = List.assoc title pages in
      List.iter (fun (_, p) -> p#misc#hide()) pages;
      page#misc#show();
      current <- title;

    method private write () =
      pages
      |> List.rev
      |> List.iter (fun (_, page) -> page#write Preferences.preferences#get);
      editor#set_tab_pos ?page:None Preferences.preferences#get.tab_pos;
      editor#show_global_gutter#set Preferences.preferences#get.editor_show_global_gutter;
      begin
        match Preferences.preferences#get.theme with
        | Some theme as new_theme when new_theme <> None && new_theme <> initial_gtk_theme ->
            Gtk_theme.set_theme ~theme ~context:self#misc#pango_context ();
        | Some theme as new_theme when new_theme <> None && new_theme = initial_gtk_theme ->
            if initial_general_font <> Preferences.preferences#get.font then
              Gtk_theme.set_theme ~theme ~context:self#misc#pango_context ();
        | _ -> self#reset_theme();
      end;

    method private ok () =
      self#write();
      Preferences.save();
      window#destroy();

    method private cancel () =
      self#reset_theme();
      window#destroy()

    method private reset_defaults () =
      let defaults = Preferences.default_values in
      List.iter (fun (_, page) -> page#read defaults) pages;

    method private reset_page_defaults () =
      let defaults = Preferences.default_values in
      match List.find_opt (fun (t, _) -> t = current) pages with
      | Some (_, p) -> p#read defaults
      | _ -> ()

    method private reset_theme () =
      Gtk_theme.set_theme ?theme:initial_gtk_theme ~context:self#misc#pango_context ()

    method private create ?(idle=true) title row  (page : string -> ?packing:(GObj.widget -> unit) -> unit -> page) =
      model#set ~row ~column title;
      let f () =
        let page = page title ~packing:hbox#add () in
        page#misc#hide();
        page#read Preferences.preferences#get;
        pages <- (title, page) :: pages
      in
      if idle then Gmisclib.Idle.add f else f()

    initializer
      (* Tree *)
      let row = model#append () in self#create ~idle:false "General" row (new Pref_view.pref_view);
      let row = model#append () in self#create "Fonts" row (new pref_fonts);
      let row as parent = model#append () in self#create "Color" row (new pref_color);
      let row = model#append ~parent () in self#create "Module Structure" row (new pref_color_structure);
      let row as parent = model#append () in self#create "Editor" row (new pref_editor);
      let row = model#append ~parent () in self#create "Display" row (new Pref_editor_display.pref_editor_display);
      let row = model#append ~parent () in self#create "Actions" row (new pref_editor_actions);
      let row = model#append ~parent () in self#create "Indentation" row (new Pref_editor_indent.pref_editor_indent);
      let row = model#append ~parent () in self#create "Code Templates" row (new pref_templ);
      let row = model#append () in self#create "Build" row (new pref_build);
      let row = model#append () in self#create "External Programs" row (new pref_program_pdf_viewer);
      view#expand_all();
      view#selection#set_mode `SINGLE;
      ignore (view#selection#connect#changed ~callback:begin fun () ->
          match view#selection#get_selected_rows with
          | path :: _ ->
              let title = model#get ~row:(model#get_iter path) ~column in
              self#show_page title
          | _ -> ()
        end);
      (* Buttons *)
      ignore (cancel_button#connect#clicked ~callback:self#cancel);
      ignore (ok_button#connect#clicked ~callback:self#ok);
      ignore (reset_button#connect#clicked ~callback:self#reset_defaults);
      ignore (reset_page_button#connect#clicked ~callback:self#reset_page_defaults);
      (*  *)
      Gmisclib.Idle.add (fun () -> view#selection#select_path (GTree.Path.create [0]));
      window#present();
  end

(** pref_editor_actions *)
and pref_editor_actions title ?packing () =
  let vbox                        = GPack.vbox ~spacing ?packing () in
  (* Smart Keys *)
  let align                       = create_align ~title:"Smart Keys" ~vbox () in
  let skbox                       = GPack.table ~row_spacings:row_spacings ~col_spacings:col_spacings ~packing:align#add () in
  let _                           = GMisc.label ~text:"Home: " ~xalign:0.0 ~packing:(skbox#attach ~top:0 ~left:0) () in
  let combo_home, _ = GEdit.combo_box_text ~active:0 ~strings:
      ["Move to beginning of first word"; "Move to first column position"]
      ~packing:(skbox#attach ~top:0 ~left:1 ~expand:`X) () in
  let _                           = GMisc.label ~text:"End: " ~xalign:0.0 ~packing:(skbox#attach ~top:1 ~left:0) () in
  let combo_end, _ = GEdit.combo_box_text ~active:0 ~strings:
      ["Move to last column position (includes whitespace)"; "Move to end of last character"]
      ~packing:(skbox#attach ~top:1 ~left:1 ~expand:`X) () in
  (*  (* Type Annotation *)
      let align                       = create_align ~title:"Type Annotations" ~vbox () in
      let box                         = GPack.vbox ~spacing:row_spacings ~packing:align#add () in
      let table_annot_type            = GPack.table ~row_spacings:row_spacings ~col_spacings:col_spacings ~packing:box#add () in
      let check_annot_type_enabled    = GButton.check_button ~label:"Enable tooltips"
      ~packing:(table_annot_type#attach ~top:0 ~left:0) () in
      let combo_annot_type_tooltips_delay, _ = GEdit.combo_box_text ~strings:
      ["Show immediately"; "Show after delay"]
      ~packing:(table_annot_type#attach ~top:0 ~left:1 ~expand:`X) () in
      (*  let _ = GMisc.label ~show:false ~text:"Implementation: " ~xalign:0.0 ~packing:(table_annot_type#attach ~top:1 ~left:0) () in
      let combo_annot_type_tooltips_impl, _ = GEdit.combo_box_text ~show:false ~strings:
      ["Use popups"; "Use GTK tooltips"]
      ~packing:(table_annot_type#attach ~top:1 ~left:1 ~expand:`X) () in*)
      let _                           = check_annot_type_enabled#connect#toggled ~callback:begin fun () ->
      combo_annot_type_tooltips_delay#misc#set_sensitive check_annot_type_enabled#active;
      end in*)
  (* File Saving *)
  let align                       = create_align ~title:"File Saving" ~vbox () in
  let box                         = GPack.vbox ~spacing:row_spacings ~packing:align#add () in
  (*let check_save_all_bef_comp     = GButton.check_button ~label:"Save all before compiling" ~packing:box#pack () in*)
  let check_bak                   = GButton.check_button ~label:"Create a backup copy of files before saving" ~packing:box#pack () in
  let check_trim                  = GButton.check_button ~label:"Strip trailing whitespace" ~packing:box#pack () in
  let check_autoformat            = GButton.check_button ~label:"Auto-format on save" ~packing:box#pack () in
  (* Searching *)
  let align                       = create_align ~title:"Searching" ~vbox () in
  let box                         = GPack.vbox ~spacing:row_spacings ~packing:align#add () in
  let check_search_word_at_cursor = GButton.check_button ~label:"Search word at cursor" ~packing:box#pack () in
  object
    inherit page title vbox

    (*  initializer
        combo_annot_type_tooltips_delay#misc#set_sensitive check_annot_type_enabled#active;*)

    method write pref =
      pref.editor_bak <- check_bak#active;
      pref.editor_trim_lines <- check_trim#active;
      pref.editor_format_on_save <- check_autoformat#active;
      pref.editor_smart_keys_home <- combo_home#active;
      pref.editor_smart_keys_end <- combo_end#active;
      (*    pref.Preferences.pref_annot_type_tooltips_enabled <- check_annot_type_enabled#active;
            pref.Preferences.pref_annot_type_tooltips_delay <- combo_annot_type_tooltips_delay#active;
            (*pref.Preferences.pref_annot_type_tooltips_impl <- combo_annot_type_tooltips_impl#active;*)*)
      pref.editor_search_word_at_cursor <- check_search_word_at_cursor#active;
      (*pref.Preferences.pref_editor_save_all_bef_comp <- check_save_all_bef_comp#active;*)

    method read pref =
      check_bak#set_active pref.editor_bak;
      check_trim#set_active pref.editor_trim_lines;
      check_autoformat#set_active pref.editor_format_on_save;
      combo_home#set_active pref.editor_smart_keys_home;
      combo_end#set_active pref.editor_smart_keys_end;
      (*    check_annot_type_enabled#set_active pref.Preferences.pref_annot_type_tooltips_enabled;
            combo_annot_type_tooltips_delay#set_active pref.Preferences.pref_annot_type_tooltips_delay;
            (*combo_annot_type_tooltips_impl#set_active pref.Preferences.pref_annot_type_tooltips_impl;*)*)
      check_search_word_at_cursor#set_active pref.editor_search_word_at_cursor;
      (*check_save_all_bef_comp#set_active pref.Preferences.pref_editor_save_all_bef_comp*)
  end

(** pref_fonts *)
and pref_fonts title ?packing () =
  let vbox         = GPack.vbox ~spacing ?packing () in
  let notebook     = GPack.notebook ~packing:vbox#add () in
  let border_width = 5 in
  let preview_text = "abcdefghijk ABCDEFGHIJK òàùèéì → •" in
  let font_app     = GMisc.font_selection ~preview_text ~border_width () in
  let font_editor  = GMisc.font_selection ~preview_text ~border_width () in
  let box_compl    = GPack.vbox ~border_width ~spacing:8 () in
  let font_compl   = GMisc.font_selection ~preview_text ~packing:box_compl#add () in
  let button_greek = GButton.check_button ~label:"Use greek letters in types" ~packing:box_compl#pack () in
  let font_other   = GMisc.font_selection ~preview_text ~border_width () in
  let font_odoc    = GMisc.font_selection ~preview_text ~border_width () in
  let _            = notebook#append_page ~tab_label:(GMisc.label ~text:"Application" ())#coerce font_app#coerce in
  let _            = notebook#append_page ~tab_label:(GMisc.label ~text:"Editor" ())#coerce font_editor#coerce in
  let _            = notebook#append_page ~tab_label:(GMisc.label ~text:"Completion" ())#coerce box_compl#coerce in
  let _            = notebook#append_page ~tab_label:(GMisc.label ~text:"Output" ())#coerce font_other#coerce in
  let _            = notebook#append_page ~tab_label:(GMisc.label ~text:"Documentation" ())#coerce font_odoc#coerce in
  let unchanged    = Array.make 5 true in
  object (self)
    inherit page title vbox

    method write pref =
      let open Settings_t in
      if not unchanged.(0) then pref.font <- font_app#font_name;
      if not unchanged.(1) then pref.editor_base_font <- font_editor#font_name;
      if not unchanged.(2) then pref.editor_completion_font <- font_compl#font_name;
      if not unchanged.(3) then pref.output_font <- font_other#font_name;
      if not unchanged.(4) then pref.odoc_font <- font_odoc#font_name;
      pref.editor_completion_greek_letters <- button_greek#active

    method read pref =
      let open Settings_t in
      let idle (f : string -> unit) x =
        Gdk.Window.set_cursor self#misc#window (Gdk.Cursor.create `WATCH);
        Gmisclib.Idle.add ~prio:300 begin fun () ->
          f x;
          Gdk.Window.set_cursor self#misc#window (Gdk.Cursor.create `ARROW)
        end
      in
      self#misc#connect#after#show ~callback:begin fun () ->
        if unchanged.(0) then idle font_app#set_font_name pref.font;
        unchanged.(0) <- false
      end |> ignore;
      let on_first_switch_page page_num f x =
        notebook#connect#after#switch_page ~callback:begin function
        | n when n = page_num && unchanged.(page_num) -> idle f x; unchanged.(page_num) <- false
        | _ -> ()
        end |> ignore;
      in
      on_first_switch_page 1 font_editor#set_font_name pref.editor_base_font;
      on_first_switch_page 2 font_compl#set_font_name pref.editor_completion_font;
      on_first_switch_page 3 font_other#set_font_name pref.output_font;
      on_first_switch_page 4 font_odoc#set_font_name pref.odoc_font;
      button_greek#set_active pref.editor_completion_greek_letters;
  end

(** pref_editor *)
and pref_editor title ?packing () =
  let vbox                  = GPack.vbox ~spacing ?packing () in
  let align                 = create_align ~title:"Tab Settings/Word Wrap" ~vbox () in
  let box                   = GPack.vbox ~spacing:row_spacings ~packing:align#add () in
  let tbox                  = GPack.hbox ~spacing:8 ~packing:box#pack () in
  let _                     = GMisc.label ~text:"Tab width:" ~packing:tbox#pack () in
  let adjustment            = GData.adjustment ~page_size:0.0 () in
  let entry_tab_width       = GEdit.spin_button ~adjustment ~rate:1.0 ~digits:0 ~numeric:true ~packing:tbox#add () in
  let check_tab_spaces      = GButton.check_button ~label:"Use spaces instead of tabs" ~packing:tbox#add () in
  let _                     = check_tab_spaces#misc#set_sensitive false in
  let check_wrap            = GButton.check_button ~label:"Wrap text, breaking lines between words" ~packing:box#pack () in
  (* Line spacing *)
  let align                 = create_align ~title:"Spacing" ~vbox () in
  let box                   = GPack.table ~row_spacings ~col_spacings ~packing:align#add () in
  let _                     = GMisc.label ~text:"Left margin (pixels):" ~xalign:0. ~packing:(box#attach ~top:0 ~left:0) () in
  let adjustment            = GData.adjustment ~lower:1.0 ~page_size:0.0 () in
  let entry_left_margin     = GEdit.spin_button ~adjustment ~rate:1.0 ~digits:0 ~numeric:true ~packing:(box#attach ~top:0 ~left:1) () in
  let _                     = GMisc.label ~text:"Pixels above lines:" ~xalign:0. ~packing:(box#attach ~top:1 ~left:0) () in
  let adjustment            = GData.adjustment ~page_size:0.0 () in
  let entry_pixels_above    = GEdit.spin_button ~adjustment ~rate:1.0 ~digits:0 ~numeric:true ~packing:(box#attach ~top:1 ~left:1) () in
  let _                     = GMisc.label ~text:"Pixels below lines:" ~xalign:0. ~packing:(box#attach ~top:2 ~left:0) () in
  let adjustment            = GData.adjustment ~page_size:0.0 () in
  let entry_pixels_below    = GEdit.spin_button ~adjustment ~rate:1.0 ~digits:0 ~numeric:true ~packing:(box#attach ~top:2 ~left:1) () in
  (* Error indication *)
  let align                 = create_align ~title:"Error Indication" ~vbox () in
  let box                   = GPack.table ~row_spacings ~col_spacings ~packing:align#add () in
  let check_error_underline = GButton.check_button ~label:"Underline errors" ~packing:(box#attach ~top:0 ~left:0) () in
  let check_error_gutter    = GButton.check_button ~label:"Show in gutter" ~packing:(box#attach ~top:0 ~left:1) () in
  let check_error_tooltip   = GButton.check_button ~label:"Show tooltip" ~packing:(box#attach ~top:1 ~left:0) () in
  object
    inherit page title vbox

    method write pref =
      pref.editor_tab_width <- entry_tab_width#value_as_int;
      pref.editor_tab_spaces <- check_tab_spaces#active;
      pref.editor_wrap <- check_wrap#active;
      pref.editor_left_margin <- entry_left_margin#value_as_int;
      pref.editor_pixels_lines <- entry_pixels_above#value_as_int, entry_pixels_below#value_as_int;
      pref.editor_err_underline <- check_error_underline#active;
      pref.editor_err_tooltip <- check_error_tooltip#active;
      pref.editor_err_gutter <- check_error_gutter#active;

    method read pref =
      entry_tab_width#set_value (float pref.editor_tab_width);
      check_tab_spaces#set_active pref.editor_tab_spaces;
      check_wrap#set_active pref.editor_wrap;
      entry_left_margin#set_value (float pref.editor_left_margin);
      let above, below = pref.editor_pixels_lines in
      entry_pixels_above#set_value (float above);
      entry_pixels_below#set_value (float below);
      check_error_underline#set_active (pref.editor_err_underline);
      check_error_tooltip#set_active (pref.editor_err_tooltip);
      check_error_gutter#set_active (pref.editor_err_gutter);
  end

(** pref_program_pdf_viewer *)
and pref_program_pdf_viewer title ?packing () =
  let vbox       = GPack.vbox ~spacing ?packing () in
  let align      = create_align ~vbox () in
  let box        = GPack.vbox ~spacing:1 ~packing:align#add () in
  let label_text = "PDF viewer" in
  let _          = GMisc.label ~xalign:0.0 ~markup:(label_text ^ ":") ~packing:box#pack () in
  let hbox       = GPack.hbox ~spacing:3 ~packing:box#pack () in
  let entry_pdf  = GEdit.entry ~editable:true ~packing:hbox#add () in
  let button_pdf = GButton.button ~label:" ... " ~packing:hbox#pack () in
  let label_text = "Diff command" in
  let _          = GMisc.label ~xalign:0.0 ~markup:(label_text ^ ":") ~packing:box#pack () in
  let hbox       = GPack.hbox ~spacing:3 ~packing:box#pack () in
  let entry_diff = GEdit.entry ~editable:true ~packing:hbox#add () in
  let button_diff = GButton.button ~label:" ... " ~packing:hbox#pack () in
  let label_text = "Graphical diff command" in
  let _          = GMisc.label ~xalign:0.0 ~markup:(label_text ^ ":") ~packing:box#pack () in
  let hbox       = GPack.hbox ~spacing:3 ~packing:box#pack () in
  let entry_gdiff = GEdit.entry ~editable:true ~packing:hbox#add () in
  let button_gdiff = GButton.button ~label:" ... " ~packing:hbox#pack () in
  let _          =
    let bind button entry =
      button#connect#clicked ~callback:begin fun () ->
        let chooser    = GWindow.file_chooser_dialog ~action:`OPEN ~title:label_text
            ~icon:(??? Icons.oe) ~position:`CENTER ~modal:true () in
        ignore (chooser#set_filename entry#text);
        chooser#add_button_stock `OK `OK;
        chooser#add_button_stock `CANCEL `CANCEL;
        match chooser#run () with
        | `OK ->
            Gaux.may chooser#filename ~f:(fun name -> entry#set_text name);
            chooser#destroy()
        | _ -> chooser#destroy()
      end |> ignore
    in
    bind button_pdf entry_pdf;
    bind button_diff entry_diff;
    bind button_gdiff entry_gdiff;
  in
  object
    inherit page title vbox
    method write pref =
      pref.program_pdf_viewer <- entry_pdf#text;
      pref.program_diff <- entry_diff#text;
      pref.program_diff_graphical <- entry_gdiff#text;
    method read pref =
      ignore (entry_pdf#set_text pref.program_pdf_viewer);
      ignore (entry_diff#set_text pref.program_diff);
      ignore (entry_gdiff#set_text pref.program_diff_graphical);
  end

(** pref_build *)
and pref_build title ?packing () =
  let vbox                    = GPack.vbox ~spacing ?packing () in
  let check_save_all_bef_comp = GButton.check_button ~label:"Save all files before building" ~packing:vbox#pack () in
  let box                     = GPack.vbox ~spacing:3 ~packing:vbox#pack () in
  let check_build_parallel    = GButton.check_button ~label:"Enable parallel compilation" ~packing:box#pack () in
  let align                    = GBin.alignment ~padding:(0, 0, 25, 0) ~packing:box#pack () in
  let jbox                    = GPack.hbox ~spacing:8 ~packing:align#add () in
  let _                       = GMisc.label ~text:"Max. number of processes (0 for unlimited):" ~packing:jbox#pack () in
  let adjustment              = GData.adjustment ~lower:0.0 ~upper:1000. ~page_size:0. () in
  let entry_jobs              = GEdit.spin_button ~adjustment ~numeric:true ~digits:0 ~value:0.0 ~packing:jbox#pack () in
  let hbox                    = GPack.hbox ~spacing:8 ~packing:vbox#pack () in
  let _                       = GMisc.label ~text:"Verbosity level:" ~packing:hbox#pack () in
  let combo_verbose, _        = GEdit.combo_box_text ~strings:[
      "0 - Quiet";
      "1 - Print build summary";
      "2 - Print compiler commands";
      "3 - Print times";
      "4 - Print detailed information about compilation process";
      "5 - Pass -verbose to OCaml tools"
    ] ~packing:hbox#pack () in
  object
    inherit page title vbox

    initializer
      check_build_parallel#connect#toggled ~callback:begin fun () ->
        jbox#misc#set_sensitive check_build_parallel#active
      end |> ignore;
      jbox#misc#set_sensitive check_build_parallel#active;

    method write pref =
      pref.editor_save_all_bef_comp <- check_save_all_bef_comp#active;
      pref.build_verbosity <- combo_verbose#active;
      pref.build_parallel <-
        if check_build_parallel#active then Some (entry_jobs#value_as_int) else None;

    method read pref =
      check_save_all_bef_comp#set_active pref.editor_save_all_bef_comp;
      combo_verbose#set_active pref.build_verbosity;
      match pref.build_parallel with
      | None -> check_build_parallel#set_active false
      | Some jobs ->
          check_build_parallel#set_active true;
          entry_jobs#set_value (float jobs);
  end

(** pref_templ *)
and pref_templ title ?packing () =
  let vbox       = GPack.vbox ~spacing ?packing () in
  let align      = create_align ~title:"Custom Templates" ~vbox () in
  let box        = GPack.vbox ~spacing:1 ~packing:align#add () in
  let label_text = "Select an OCaml library containing custom templates" in
  let _          = GMisc.label ~xalign:0.0
      ~markup:(label_text ^ ":")
      ~packing:box#pack ()
  in
  let hbox       = GPack.hbox ~spacing:3 ~packing:box#pack () in
  let entry      = GEdit.entry ~editable:false ~packing:hbox#add () in
  let button     = GButton.button ~label:" ... " ~packing:hbox#pack () in
  let _          =
    ignore (button#connect#clicked ~callback:begin fun () ->
        let chooser    = GWindow.file_chooser_dialog ~action:`OPEN ~title:(label_text ^ " (.cma)")
            ~icon:(??? Icons.oe) ~position:`CENTER ~modal:true () in
        chooser#set_filter (GFile.filter ~patterns:["*.cma"] ());
        ignore (chooser#set_filename entry#text);
        chooser#add_button_stock `OK `OK;
        chooser#add_button_stock `CANCEL `CANCEL;
        match chooser#run () with
        | `OK ->
            Gaux.may chooser#filename ~f:(fun name -> entry#set_text name);
            chooser#destroy()
        | _ -> chooser#destroy()
      end)
  in
  let label      = GMisc.label ~markup:"<b>How to create custom templates</b>" () in
  let expander   = GBin.expander ~packing:vbox#add () in
  let _          = expander#set_label_widget label#coerce in
  let sw         = GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:expander#add () in
  let markup     = Template.help in
  let label      = GMisc.label ~xalign:0.0 ~yalign:0.0 ~xpad:3 ~ypad:3 ~markup ~line_wrap:false ~selectable:true ~packing:sw#add_with_viewport () in
  object
    inherit page title vbox
    method write pref =
      pref.editor_custom_templ_filename <- entry#text;
      Templ.load_custom `user;

    method read pref =
      ignore (entry#set_text pref.editor_custom_templ_filename);
  end

(** create *)
let create ~editor () = new preferences ~editor () |> ignore
