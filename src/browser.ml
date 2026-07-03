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

open Prj
open Printf
open Utils
open GUtil
open Oe
open Preferences

class browser window =
  (* signals *)
  let startup                            = new startup () in
  let switch_project                     = new switch_project () in
  let project_history_changed            = new project_history_changed () in
  (*  *)
  let get_menu_item_nav_history_backward = ref (fun () -> failwith "get_menu_item_nav_history_backward") in
  let get_menu_item_nav_history_forward  = ref (fun () -> failwith "get_menu_item_nav_history_forward") in
  let get_menu_item_nav_history_last     = ref (fun () -> failwith "get_menu_item_nav_history_last") in
  let get_menu_item_undo                 = ref (fun () -> failwith "get_menu_item_undo") in
  let get_menu_item_redo                 = ref (fun () -> failwith "get_menu_item_redo") in
  (* Packing *)
  let editor = new Editor.editor () in
  let _ = window#add Messages.hpaned#coerce in
  let vbox = GPack.vbox ~packing:Messages.hpaned#add1 () in
  let menubarbox = GPack.hbox ~spacing:0 ~packing:vbox#pack () in
  (* Menubar icon displayed full-screen mode *)
  let window_title_menu_icon = GBin.event_box ~packing:menubarbox#pack ~show:false () in
  let icon = Gtk_util.label_icon ~width:32 ~height:32 "<span font='18'>\u{e10e}</span>" ~packing:window_title_menu_icon#add in
  let _ = icon#misc#modify_fg [`NORMAL, `COLOR (icon#misc#style#fg `INSENSITIVE) ] in
  let _ = window_title_menu_icon#misc#set_property "visible-window" (`BOOL false) in

  (* Menubar *)
  let menubar = GMenu.menu_bar ~border_width:0 ~packing:menubarbox#add () in
  let cursor = Gdk.Cursor.create `ARROW in
  let _ = menubar#event#connect#motion_notify  ~callback:begin fun _ ->
      Gdk.Window.set_cursor menubar#misc#window cursor;
      false;
    end in
  (* Standard Toolbar *)
  let toolbar = new Toolbar.toolbar ~messages:Messages.vmessages ~hmessages:Messages.hmessages ~editor () in
  let _ = vbox#pack toolbar#coerce in
  let paned = Messages.vpaned in
  let _ = vbox#add paned#coerce in
  let toolbox = GPack.hbox ~packing:menubarbox#add ~show:false () in
  let _ = GMisc.separator `VERTICAL ~packing:toolbox#pack () in
  (*  *)
  let ebox_project_name = GBin.event_box ~packing:(menubarbox#pack ~expand:false) () in
  let label_project_name = GMisc.label ~markup:"" ~xpad:5 ~packing:ebox_project_name#add () in
  (* Status bar *)
  let statusbar = new Statusbar.widget ~packing:vbox#pack () in
  let gitbar = new Statusbar.gitbar ~packing:(statusbar#pack ~from:`END) () in
  let _ =
    if Oe_config.unify_statusbars then
      editor#connect#switch_page ~callback:(fun page -> statusbar#pack_editorbar page#statusbar) |> ignore;
    editor#connect#notification ~callback:statusbar#flash_message |> ignore;
  in
  (** Spinner *)
  let activate_spinner (active : Activity.t list) =
    match active with
    | [] ->
        statusbar#spinner#set_pixbuf (??? Icons.empty_16);
        statusbar#spinner#misc#set_tooltip_text "";
    | msgs ->
        let msgs = snd (List.split msgs) in
        statusbar#spinner#set_file (Icon.get_themed_filename "spinner_16.gif");
        statusbar#spinner#misc#set_tooltip_text (String.concat "\n" (List.rev msgs));
  in
  let _ =
    Activity.table#connect#changed ~callback:activate_spinner |> ignore;
    activate_spinner Activity.table#get;
  in
  (*  *)
  let vbox_menu_buttons = GPack.vbox ~border_width:0 ~packing:menubarbox#pack ~show:false () in
  let align = GBin.aspect_frame ~yalign:0.0 ~shadow_type:`NONE ~packing:vbox_menu_buttons#add () in
  let hbox_menu_buttons = GPack.hbox ~border_width:0 ~spacing:0 ~packing:align#add () in

  let button_menu_iconify = GButton.button ~relief:`NONE ~packing:hbox_menu_buttons#pack () in
  let _ = GMisc.image ~pixbuf:(??? Icons.minimize_window) ~packing:button_menu_iconify#add () in
  let _ = button_menu_iconify#misc#set_name "windowbutton" in
  let _ = button_menu_iconify#set_focus_on_click false in

  let button_menu_reset = GButton.button ~relief:`NONE ~packing:hbox_menu_buttons#pack () in
  let _ = GMisc.image ~pixbuf:(??? Icons.restore_window) ~packing:button_menu_reset#add () in
  let _ = button_menu_reset#misc#set_name "windowbutton" in
  let _ = button_menu_reset#set_focus_on_click false in

  let button_menu_exit = GButton.button ~relief:`NONE ~packing:hbox_menu_buttons#pack () in
  let _ = GMisc.image ~pixbuf:(??? Icons.close_window) ~packing:button_menu_exit#add () in
  let _ = button_menu_exit#misc#set_name "windowbutton" in
  let _ = button_menu_exit#set_focus_on_click false in
  let tout_low_prio = Timeout.create ~delay:0.75 () in

  object (self)
    val mutable finalize = fun _ -> ()
    val mutable projects = []
    val mutable current_project = new GUtil.variable None
    val menubar_visible = new GUtil.variable true;
    val toolbar_visible = new GUtil.variable true;
    val tabbar_visible = new GUtil.variable true;
    val outline_visible = new GUtil.variable true;
    val mutable maximized_view_action = `NONE
    val mutable geometry = "";
    val mutable project_history =
      File_history.create
        ~filename:Oe_config.project_history_filename
        ~max_length:Oe_config.project_history_max_length
    val mutable cache_dialog_project_properties = []
    val mutable menu = None
    val mutable pref_max_view_2 = false
    val mutable max_height_prev_h = -1
    val mutable max_height_prev_y = -1
    val mutable is_fullscreen = false
    val mutable is_decorated = true
    val mutable maximized_view_actions = [
      `NONE, (fun () -> {
            mva_menubar    = true; (* dummies *)
            mva_toolbar    = true;
            mva_tabbar     = true;
            mva_messages   = false;
            mva_fullscreen = false;
            mva_decorated  = false;
          });
      `FIRST, (fun () -> {
            mva_menubar    = true(*Preferences.preferences#get.Preferences.pref_max_view_1_menubar*);
            mva_toolbar    = Preferences.preferences#get.max_view_1_toolbar;
            mva_tabbar     = Preferences.preferences#get.max_view_1_tabbar;
            mva_messages   = Preferences.preferences#get.max_view_1_messages;
            mva_fullscreen = false;
            mva_decorated  = false;
          });
      `SECOND, (fun () -> {
            mva_menubar    = true(*Preferences.preferences#get.Preferences.pref_max_view_2_menubar*);
            mva_toolbar    = Preferences.preferences#get.max_view_2_toolbar;
            mva_tabbar     = Preferences.preferences#get.max_view_2_tabbar;
            mva_messages   = Preferences.preferences#get.max_view_2_messages;
            mva_fullscreen = true;
            mva_decorated  = true; (* not used if fullscreen *)
          })
    ];

    method toolbar = toolbar

    method editor = editor
    (*method vmessages = Messages.vmessages
      method hmessages = Messages.hmessages*)
    method shell () =
      self#with_current_project (fun project -> Ocaml_shell.append_page ~project Messages.vmessages);
      Messages.vmessages#set_visible true;

    method save_all () =
      editor#save_all();
      self#with_current_project(fun p -> Project.save ~editor p)

    method refresh () = self#with_current_project Project.refresh
    method clear_cache () = self#with_current_project begin fun project ->
        Project.clear_cache project |> ignore;
      end

    method project_clean () =
      self#with_current_project (fun project ->
          self#with_default_target (fun target ->
              ignore (Task_console.exec ~editor:self#editor `CLEANALL target);
              Project.clean_tmp project))

    method current_project = current_project

    method project_history = project_history

    method private project_write_history () =
      File_history.write project_history;
      project_history_changed#call project_history;

    method project_close () =
      self#with_current_project begin fun project ->
        Project.save ~editor project;
        self#project_write_history();
        Names.Cache.save ~project;
        Project.unload project;
        List.iter (fun p -> Autosave.delete ~filename:p#get_filename ()) editor#pages;
      end

    method project_open filename =
      self#project_close();
      let proj = Project.load filename in
      current_project#set (Some proj);
      Project.load_path proj;
      editor#set_history_switch_page_locked true;
      (*crono ~label:"close_all" *)editor#close_all ();
      editor#set_project proj;
      Sys.chdir (proj.root // Prj.default_dir_src);
      Gmisclib.Idle.add ~prio:300 self#set_title;
      (File_history.add project_history) filename;
      Names.Cache.load ~project:proj;
      Autosave.recover ();
      Gmisclib.Idle.add ~prio:300(*crono ~label:"dialog_project_properties" (fun () -> *)(self#dialog_project_properties ~show:false);
      begin
        try Templ.load_custom (`project proj)
        with Templ.Error (msg, details) ->
          (Dialog.message ~title:"Warning" ~message:(sprintf "%s\n\n\"%s\"" msg details) `WARNING);
      end;
      (*Project.load_rc_icons proj;*)
      switch_project#call();
      (* Load files *)
      let active_exists = ref false in
      let i = ref 0 in
      let _ =
        List.map (begin fun (filename, scroll_offset, offset, active) ->
            let filename = List.fold_left (//) "" (filename_split filename) in
            incr i;
            active_exists := !active_exists || active;
            let active = active || (List.length proj.open_files = !i && not !active_exists) in
            editor#open_file ~active ~scroll_offset ~offset ?remote:None filename;
          end) proj.open_files
      in
      editor#set_history_switch_page_locked false;
      proj.open_files <- [];
      proj.modified <- false;
      self#update_git_status();
      proj


    method dialog_project_open () =
      let project_home =
        match self#current_project#get with Some p -> p.Prj.root | _ -> App_config.user_home
      in
      let pat1 = "*"^Prj.default_extension in
      let pat2 = "*"^Prj.old_extension in
      let dialog = GWindow.file_chooser_dialog ~action:`OPEN ~width:600 ~height:600
          ~title:"Open project..." ~icon:(??? Icons.oe) ~position:`CENTER ~show:false () in
      dialog#add_filter (GFile.filter
                           ~name:(sprintf "%s projects (%s)" About.program_name pat1) ~patterns:[pat1] ());
      dialog#add_filter (GFile.filter
                           ~name:(sprintf "%s projects old version (%s)" About.program_name pat2) ~patterns:[pat2] ());
      dialog#add_select_button_stock `OK `OK;
      dialog#add_button_stock `CANCEL `CANCEL;
      dialog#set_select_multiple false;
      dialog#set_current_folder (Filename.dirname project_home) |> ignore;
      match dialog#run () with
      | `OK ->
          List.iter begin fun filename ->
            let filename, save = if filename ^^^ Prj.old_extension then (Filename.chop_extension filename) ^ Prj.default_extension, true else filename, false in
            let proj = self#project_open filename in
            Quick_file_chooser.add_roots ~roots:[Filename.dirname filename] ~filter:Dialog_find_file.filter;
            if save then (Project.save ~editor proj);
          end dialog#get_filenames;
          dialog#destroy()
      | _ -> dialog#destroy()

    method dialog_project_properties ?page_num ?(show=true) () =
      self#with_current_project begin fun project ->
        let id = Project.filename project in
        let remove_from_cache () =
          cache_dialog_project_properties <- List.filter (fun (x, _) -> x <> id) cache_dialog_project_properties
        in
        try
          begin
            let create_first_target widget =
              if List.length project.Prj.targets = 0 then (ignore (widget#target_list#add_target()));
            in
            match List.assoc id cache_dialog_project_properties with
            | window, widget, `ICONIFIED ->
                remove_from_cache();
                cache_dialog_project_properties <- (id, (window, widget, `VISIBLE)) :: cache_dialog_project_properties;
                if show then begin
                  Gaux.may page_num ~f:widget#goto_page;
                  create_first_target widget;
                  window#set_modal false;
                  window#present();
                end
            | window, widget, _state ->
                (*widget#reset();*)
                if show then begin
                  Gaux.may page_num ~f:widget#goto_page;
                  create_first_target widget;
                  window#set_modal false;
                  window#present();
                end
          end;
        with Not_found ->
          let window, widget = Project_properties.create ~editor ?page_num ~show () in
          (* "widget" (the "Project Properties" dialog) is cached, then
             browser#current_project and the project instance stored in the dialog can
             be different objects (i.e. after a project switch in the IDE).  *)
          ignore (widget#connect#project_changed ~callback:(fun proj -> current_project#set (Some proj)));
          ignore (window#connect#destroy ~callback:(fun _ -> remove_from_cache()));
          ignore (window#misc#connect#hide ~callback:begin fun () ->
              toolbar#update current_project#get;
              widget#reset();
            end);
          ignore (window#event#connect#delete ~callback:begin fun _ ->
              window#misc#hide();
              true
            end);
          ignore (window#event#connect#window_state ~callback:begin fun ev ->
              begin
                match GdkEvent.WindowState.new_window_state ev with
                | [`ICONIFIED] ->
                    remove_from_cache();
                    cache_dialog_project_properties <- (id, (window, widget, `ICONIFIED)) :: cache_dialog_project_properties;
                | _ ->
                    remove_from_cache();
                    cache_dialog_project_properties <- (id, (window, widget, `VISIBLE)) :: cache_dialog_project_properties;
              end;
              false
            end);
          cache_dialog_project_properties <- (id, (window, widget, `VISIBLE)) :: cache_dialog_project_properties;
      end

    method dialog_project_new () =
      let rec mkname n =
        if n = 100 then (failwith "dialog_project_new (mkname)");
        let name = sprintf "Untitled_%d" n in
        if not (Sys.file_exists (App_config.user_home // name)) then name else (mkname (n + 1))
      in
      let name = mkname 0 in
      let filename = App_config.user_home // name // (name^Prj.default_extension) in
      let new_project = Project.create ~filename () in
      let window, widget =
        Project_properties.create ~show:true ~editor ~new_project ~callback:begin fun proj ->
          self#project_close();
          Project.save ~editor new_project;
          ignore (self#project_open (Project.filename proj));
        end ()
      in
      window#set_modal true;
      ignore (widget#connect#project_changed ~callback:(fun proj -> current_project#set (Some proj)));

    method dialog_file_new = Dialog_file_new.show ~editor:self#editor;

    method dialog_external_tools () =
      self#with_current_project begin fun project ->
        External_tools.create
          ~get_editor:(fun () -> self#editor)
          ~get_current_project:(fun () -> project) ()
      end

    method dialog_find_file ?all () =
      self#with_current_project begin fun project ->
        let current = project.Prj.root in
        let roots = project_history.File_history.content in (* project filenames .xml *)
        let roots = List.filter ((<>) current) (List.map Filename.dirname roots) in
        let roots = current :: roots in
        let dialog = Dialog_find_file.create ?all ~roots ~editor () in
        dialog#set_transient_for window#as_window;
      end

    method menubar_visible = menubar_visible

    method toolbar_visible = toolbar_visible

    method tabbar_visible = tabbar_visible

    method outline_visible = outline_visible

    method private set_geometry () =
      let alloc = window#misc#allocation in
      geometry <- sprintf "%d\n%d\n%d\n%d\n%b\n%b\n%b\n%b\n"
          (alloc.Gtk.width) (alloc.Gtk.height) (alloc.Gtk.x) (alloc.Gtk.y)
          menubar_visible#get editor#show_tabs toolbar_visible#get outline_visible#get;

    method update_git_status () =
      Timeout.set tout_low_prio 0 begin fun () ->
        let with_project f =
          match current_project#get with
          | Some proj -> f proj
          | _ -> ()
        in
        with_project begin fun proj ->
          ksprintf label_project_name#set_label "<b>%s</b>" proj.Prj.name;
          label_project_name#misc#set_tooltip_text (Project.filename proj);
        end;
        Git.toplevel begin function
        | Some toplevel ->
            gitbar#button_gitpath#set_label (Filename.basename toplevel);
            toplevel |> Filename.dirname |> gitbar#button_gitpath#misc#set_tooltip_text;
        | _ ->
            gitbar#button_gitpath#set_label "";
            gitbar#button_gitpath#misc#set_tooltip_text "";
        end;
        Git.status begin function
        | Some s ->
            gitbar#misc#show();
            gitbar#button_gitbranch#set_label s.Git.branch;
            let changes =
              s.Git.added + s.Git.modified + s.Git.deleted + s.Git.renamed + s.Git.copied + s.Git.untracked + s.Git.ignored
            in
            gitbar#button_gitpending#set_label (sprintf "%3d" changes);
            Git.markup_of_status s |> gitbar#button_gitpending#misc#set_tooltip_markup;
            gitbar#button_gitunpushed#set_label (sprintf "%3d" s.Git.ahead);
            s.Git.ahead |> sprintf "%d unpushed commits" |> gitbar#button_gitunpushed#misc#set_tooltip_markup;
        | _ ->
            gitbar#misc#hide();
        end;
      end

    method set_title () =
      let filename, modified =
        match editor#get_page `ACTIVE with
        | None -> "", false
        | Some page -> page#get_title, page#buffer#modified
      in
      match current_project#get with
      | Some proj ->
          let projectname = proj.Prj.name in
          [projectname; " - "; filename]
          |> String.concat ""
          |> window#set_title;
      | _ ->
          window#set_title filename;

    val mutable busy = false

    method set_maximized_view (view : [ `FIRST | `NONE | `SECOND ]) =
      let mb = menubar_visible#get in
      let tb = toolbar_visible#get in
      let tab = tabbar_visible#get in
      let ms = Messages.vmessages#visible in
      let fs = is_fullscreen in
      let dc = is_decorated in
      let save_default () =
        self#set_geometry();
        maximized_view_actions <- (`NONE, (fun () -> {
              mva_menubar    = mb;
              mva_toolbar    = tb;
              mva_tabbar     = tab;
              mva_messages   = ms;
              mva_fullscreen = fs;
              mva_decorated  = dc;
            })) :: (List.remove_assoc `NONE maximized_view_actions);
      in
      let reset_default () =
        let original = (List.assoc `NONE maximized_view_actions) () in
        self#menubar_visible#set original.mva_menubar;
        self#toolbar_visible#set original.mva_toolbar;
        self#tabbar_visible#set original.mva_tabbar;
        Messages.vmessages#set_visible original.mva_messages;
        self#set_fullscreen original.mva_fullscreen;
        window#set_decorated original.mva_decorated;
        toolbox#misc#hide();
        maximized_view_action <- `NONE
      in
      begin
        match view with
        | `FIRST when maximized_view_action = `NONE ->
            save_default();
            let first = (List.assoc `FIRST maximized_view_actions) () in
            self#menubar_visible#set first.mva_menubar;
            self#toolbar_visible#set first.mva_toolbar;
            self#tabbar_visible#set first.mva_tabbar;
            if Messages.vmessages#visible then (Messages.vmessages#set_visible first.mva_messages);
            if first.mva_fullscreen then
              self#set_fullscreen first.mva_fullscreen
            else begin
              window#set_decorated first.mva_decorated;
              if not first.mva_decorated then toolbox#misc#show()
            end;
            maximized_view_action <- `FIRST;
        | `SECOND when maximized_view_action = `NONE ->
            save_default();
            let second = (List.assoc `SECOND maximized_view_actions) () in
            self#menubar_visible#set second.mva_menubar;
            self#toolbar_visible#set second.mva_toolbar;
            self#tabbar_visible#set second.mva_tabbar;
            if Messages.vmessages#visible then (Messages.vmessages#set_visible second.mva_messages);
            if second.mva_fullscreen then
              self#set_fullscreen second.mva_fullscreen
            else
              window#set_decorated second.mva_decorated;
            maximized_view_action <- `SECOND;
        | `FIRST when maximized_view_action = `SECOND ->
            reset_default();
            self#set_maximized_view view
        | `SECOND when maximized_view_action = `FIRST -> reset_default();
        | `FIRST when maximized_view_action = `FIRST ->
            reset_default();
            self#set_maximized_view `SECOND;
        | `SECOND when maximized_view_action = `SECOND ->
            reset_default();
            self#set_maximized_view `FIRST;
        | `NONE when maximized_view_action = `FIRST -> reset_default();
        | `NONE when maximized_view_action = `SECOND -> reset_default();
        | `NONE when maximized_view_action = `NONE -> ()
        | _ -> assert false
      end;
      editor#with_current_page (fun p -> p#view#misc#grab_focus())

    method set_fullscreen want_fullscreen =
      if want_fullscreen && (not is_fullscreen) then begin
        (* pref_max_view_fullscreen = "Prefer fullscreen over maximize window" *)
        let prefer_maximized_window = not Preferences.preferences#get.max_view_prefer_fullscreen in
        if prefer_maximized_window then window#maximize() else window#fullscreen();
        window#set_decorated (not prefer_maximized_window);
        window_title_menu_icon#misc#show();
        vbox_menu_buttons#misc#show();
        menubar#misc#set_name "oe_menubar";
        menubarbox#set_child_packing ~expand:false ~fill:false menubar#coerce;
        toolbox#misc#show();
      end else if (not want_fullscreen) && is_fullscreen then begin
        window_title_menu_icon#misc#hide();
        vbox_menu_buttons#misc#hide();
        menubar#misc#set_name "";
        menubarbox#set_child_packing ~expand:true ~fill:true menubar#coerce;
        toolbox#misc#hide();
        window#unfullscreen();
        window#unmaximize();
        window#set_decorated true;
      end;
      self#set_title ();
      is_fullscreen <- want_fullscreen;

    method set_menu_item_nav_history_sensitive () =
      let back, forward, last = editor#location_history_is_empty () in
      (*Location_history.print editor#location_history;
        eprintf "%d, %d, %d, %b\n%!"
        (List.length (Location_history.get_history_backward editor#location_history))
        (List.length (Location_history.get_history_forward editor#location_history))
        (Location_history.current_index editor#location_history)
        last;*)
      (!get_menu_item_nav_history_backward())#misc#set_sensitive (not back);
      (!get_menu_item_nav_history_forward())#misc#set_sensitive (not forward);
      (!get_menu_item_nav_history_last())#misc#set_sensitive (not last);
      toolbar#tool_back#misc#set_sensitive (not back);
      toolbar#tool_forward#misc#set_sensitive (not forward);
      toolbar#tool_last_edit_loc#misc#set_sensitive (not last);

    method goto_location dir =
      let move = function
        | `NEXT ->
            begin
              match Location_history.next editor#location_history with
              | None -> ()
              | Some entry -> editor#location_history_goto entry
            end
        | `PREV ->
            begin
              match Location_history.previous editor#location_history with
              | None -> ()
              | Some entry -> editor#location_history_goto entry
            end
        | `LOCATION location ->
            Location_history.goto editor#location_history ~location;
            editor#location_history_goto location
        | `LAST ->
            begin
              match Location_history.goto_last_edit_location editor#location_history with
              | None -> ()
              | Some loc -> editor#location_history_goto loc
            end
      in
      begin
        match dir with
        | `LAST -> move dir
        | `LOCATION _ -> move dir
        | _ ->
            begin
              match editor#get_page `ACTIVE with
              | None -> move dir
              | Some page ->
                  let iter = page#buffer#get_iter `INSERT in
                  let current_editor_location = iter#offset in
                  begin
                    match Location_history.current_location editor#location_history with
                    | Some cur_hist_loc when (cur_hist_loc.Location_history.filename = page#get_filename) ->
                        let already_there =
                          match cur_hist_loc.Location_history.mark with
                          | None -> current_editor_location = cur_hist_loc.Location_history.offset
                          | Some cur_hist_mark ->
                              let hmn = match GtkText.Mark.get_name cur_hist_mark with None -> assert false | Some x -> x in
                              List.exists begin fun m ->
                                match GtkText.Mark.get_name m with
                                | None -> false
                                | Some name -> hmn = name
                              end iter#marks
                        in
                        if already_there then (move dir) else (move (`LOCATION cur_hist_loc));
                    | Some chl -> move (`LOCATION chl);
                    | None -> ()
                  end;
            end;
      end;
      self#set_menu_item_nav_history_sensitive();

    method build_all configs =
      let tasks = List.map begin fun target ->
          let cmd, args = Target.create_cmd_line target in
          let name = sprintf "Build \xC2\xAB%s\xC2\xBB" (Filename.basename target.Target.name) in
          let task = Task.create ~name ~env:[] ~dir:"" ~cmd ~args () in
          `COMPILE, task
        end configs in
      self#with_current_project (fun _ -> ignore (Task_console.exec_sync ~editor [tasks]))

    method quick_info_at_cursor () =
      editor#with_current_page begin fun page ->
        let iter = page#buffer#get_iter `INSERT in
        page#quick_info_at_iter iter
      end

    method create_menu_history dir ~menu =
      let history = match dir with
        | `BACK -> Location_history.get_history_backward editor#location_history
        | `FORWARD -> Location_history.get_history_forward editor#location_history
      in
      try
        List.iter begin (*let i = ref 0 in*) fun entry ->
          let label = Location_history.string_of_location entry in
          let label = if entry.Location_history.kind = `EDIT then label ^ "*" else label in
          let mi = GMenu.menu_item ~label ~packing:menu#add () in
          ignore (mi#connect#activate ~callback:(fun () -> self#goto_location (`LOCATION entry)));
          (*if !i >= 30 then (raise Exit) else (incr i)*)
        end history;
        if List.length history = 0 then begin
          try
            let sep = List.hd (List.tl menu#all_children) in
            menu#remove sep
          with _ -> ()
        end
      with Exit -> ()

    method with_current_project f = match current_project#get with Some p -> f p | _ -> ()

    method with_default_target f =
      self#with_current_project begin fun project ->
        match Project.default_target project with
        | Some bc -> f bc
        | _ -> ()
      end

    method with_default_runtime_config ~open_dialog f =
      self#with_current_project begin fun project ->
        match List.find_opt (fun x -> x.Rconf.default) project.Prj.executables with
        | Some rconf -> f rconf
        | None when open_dialog -> self#dialog_project_properties ~page_num:2 ()
        | _ -> ()
      end

    method window : GWindow.window = window

    method exit (editor : Editor.editor) () =
      try
        Preferences.preferences#get.hmessages_width <- Messages.hmessages#position;
        Preferences.preferences#get.vmessages_height <- Messages.vmessages#position;
        Preferences.preferences#get.outline_width <- editor#paned#position;
        ignore(Messages.vmessages#remove_all_tabs());
        if maximized_view_action = `NONE then (self#set_geometry());
        (*Save geometry*)
        let chan = open_out (Filename.concat App_config.ocamleditor_user_home "geometry") in
        fprintf chan "%s" geometry;
        close_out_noerr chan;
        window#misc#hide();
        (*  *)
        let finalize () =
          (* Save project *)
          begin
            try self#with_current_project begin fun project ->
                Project.save ~editor project;
                Names.Cache.save ~project;
              end with Gpointer.Null -> ();
          end;
          (* Save project and file history *)
          File_history.write editor#file_history;
          self#project_write_history();
          Preferences.save();
          (*  *)
          finalize();
          GMain.Main.quit();
          Stdlib.exit 0
        in
        let pages = List.filter (fun p -> p#buffer#modified) editor#pages in
        let pages = List.map (fun x -> true, x) pages in
        editor#dialog_save_modified ~close:false ~callback:finalize pages
      with Messages.Cancel_process_termination -> (GtkSignal.stop_emit())

    method startup = startup#call

    method menu =
      match menu with
      | Some menu -> menu.Menu_types.menus
      | _ -> assert false

    method private init () =
      window#event#connect#window_state ~callback:begin fun ev ->
        match GdkEvent.WindowState.new_window_state ev with
        | [`MAXIMIZED] when maximized_view_action <> `NONE && not window#decorated ->
            Gmisclib.Idle.add ~prio:300 begin fun () ->
              let alloc = window#misc#allocation in
              window#resize ~width:alloc.Gtk.width ~height:(alloc.Gtk.height - 2);
            end;
            false
        | _ -> false
      end |> ignore;
      let find_custom_button = Toolbox.populate ~browser:self ~packing:toolbox#pack in
      let _ = Editor.set_menu_item_nav_history_sensitive := self#set_menu_item_nav_history_sensitive in
      (* Menubar items *)
      let open! Menu_types in
      let menu_item_view_menubar = ref [] in
      let menu_item_view_toolbar = ref [] in
      let menu_item_view_tabbar = ref [] in
      let menu_item_view_outline = ref [] in
      let menu_item_view_messages = ref [] in
      let menu_item_view_hmessages = ref [] in
      let accel_group = GtkData.AccelGroup.create () in
      let group = accel_group in
      let menu_items = Menu.create ~browser:self ~group
          ~get_menu_item_undo
          ~get_menu_item_redo
          ~get_menu_item_nav_history_backward
          ~get_menu_item_nav_history_forward
          ~get_menu_item_nav_history_last
          ~menu_item_view_menubar
          ~menu_item_view_toolbar
          ~menu_item_view_tabbar
          ~menu_item_view_outline
          ~menu_item_view_messages ~menu_item_view_hmessages () in
      menu <- Some menu_items;
      List.iter menubar#append menu_items.menu_items;
      (* Update Window menu with files added to the editor *)
      ignore (editor#connect#add_page ~callback:begin fun page ->
          begin
            if outline_visible#get then page#show_outline();
            match page#file with None -> () | Some file ->
              let offset = page#initial_offset in
              let scroll_offset = page#view#get_scroll_top () in
              self#with_current_project (fun project -> Project.add_file project ~scroll_offset ~offset file);
          end;
          Gaux.may menu ~f:begin fun menu ->
            menu.window_signal_locked <- true;
            let basename = Filename.basename page#get_filename in
            let label = sprintf "%s%s" basename (if page#buffer#modified then "*" else "") in
            let group = menu.window_radio_group in
            let item = GMenu.radio_menu_item ?group ~active:true
                ~label ~packing:(menu.window#insert ~pos:menu.window_n_childs) ()
            in
            menu.window_n_childs <- menu.window_n_childs + 1;
            let _ = item#connect#toggled ~callback:begin fun () ->
                if not menu.window_signal_locked then begin
                  ignore (editor#open_file ~active:true ~scroll_offset:0 ~offset:0 ?remote:None page#get_filename)
                end
              end in
            menu.window_pages <- (page#misc#get_oid, item) :: menu.window_pages;
            menu.window_signal_locked <- false;
            menu.window_radio_group <- Some item#group;
          end;
        end);
      (* Update Window menu with files removed from the editor *)
      ignore (editor#connect#remove_page ~callback:begin fun page ->
          self#with_current_project begin fun project ->
            Project.remove_file project page#get_filename;
            self#set_title ();
            Gaux.may menu ~f:begin fun menu ->
              try
                let item = List.assoc page#misc#get_oid menu.window_pages in
                item#destroy();
                menu.window_pages <- List.remove_assoc page#misc#get_oid menu.window_pages;
                menu.window_n_childs <- menu.window_n_childs - 1;
              with Not_found -> ()
            end;
          end
        end);
      (*  *)
      ignore (editor#connect#switch_page ~callback:begin fun page ->
          Gaux.may menu ~f:begin fun menu ->
            let basename = Filename.basename page#get_filename in
            ksprintf (Menu.set_label menu.file_rename) "Rename \xC2\xAB%s\xC2\xBB" basename;
            menu.file_switch#misc#set_sensitive
              ((basename ^^^ ".ml") || (basename ^^^ ".mli"));
            ksprintf (Menu.set_label menu.file_close) "Close \xC2\xAB%s\xC2\xBB" basename;
            ksprintf (Menu.set_label menu.file_close_all) "Close All Except \xC2\xAB%s\xC2\xBB" basename;
            ksprintf (Menu.set_label menu.file_revert) "Revert to Saved \xC2\xAB%s\xC2\xBB" basename;
            ksprintf (Menu.set_label menu.file_delete) "Delete \xC2\xAB%s\xC2\xBB" basename;
            match List.assoc_opt page#misc#get_oid menu.window_pages with
            | Some item ->ksprintf (Menu.set_label item) "%s" basename
            | _ -> ()
          end;
        end);
      (* Update Project menu with project history *)
      ignore (self#connect#project_history_changed ~callback:begin fun history ->
          Gaux.may menu ~f:begin fun menu ->
            Gmisclib.Idle.add ~prio:600 begin fun () ->
              List.iter (fun (_, i) -> menu.project#remove (i :> GMenu.menu_item)) menu.project_history;
              menu.project_history <- []
            end;
            let project_names = List.map (fun x -> x, Filename.chop_extension (Filename.basename x)) history.File_history.content in
            let project_names = List.sort (fun (_, x1) (_, x2) ->
                compare (String.lowercase_ascii x1) (String.lowercase_ascii x2)) project_names in
            List.iter begin fun (filename, label) ->
              Gmisclib.Idle.add ~prio:600 begin fun () ->
                let item = GMenu.check_menu_item ~label ~packing:menu.project#add () in
                item#misc#set_tooltip_text (Filename.dirname filename);
                ignore (item#connect#after#toggled ~callback:(fun () ->
                    if not menu.project_history_signal_locked && item#active then (ignore (self#project_open filename))));
                menu.project_history <- (filename, item) :: menu.project_history;
              end;
            end project_names;
          end
        end);

      Editor_menu.menu_item_view_menubar := (fun () -> menu_item_view_menubar);

      (* Load current project *)
      let rec load_current_proj history =
        match history with [] -> () | filename :: _ ->
          if Sys.file_exists filename then (ignore (self#project_open filename))
          else (load_current_proj (List.tl history))
      in
      File_history.read project_history;
      project_history.File_history.content <- List.filter (fun x -> (x ^^^ Prj.default_extension)) project_history.File_history.content;
      project_history_changed#call project_history;
      load_current_proj project_history.File_history.content;

      (* Toolbar signals *)
      toolbar#bind_signals self;

      (*  *)
      self#set_menu_item_nav_history_sensitive();
      menubar_visible#connect#changed ~callback:begin fun visible ->
        if visible then menubarbox#misc#show() else (menubarbox#misc#hide());
        List.iter begin fun (mi, sign) ->
          mi#misc#handler_block sign;
          mi#set_active visible;
          mi#misc#handler_unblock sign;
        end !menu_item_view_menubar
      end |> ignore;
      toolbar_visible#connect#changed ~callback:begin fun visible ->
        if visible then toolbar#misc#show_all() else (toolbar#misc#hide());
        List.iter begin fun (mi, sign) ->
          mi#misc#handler_block sign;
          mi#set_active visible;
          mi#misc#handler_unblock sign;
        end !menu_item_view_toolbar
      end |> ignore;
      tabbar_visible#connect#changed ~callback:begin fun visible ->
        editor#set_show_tabs visible;
        List.iter begin fun (mi, sign) ->
          mi#misc#handler_block sign;
          mi#set_active visible;
          mi#misc#handler_unblock sign;
        end !menu_item_view_tabbar
      end |> ignore;
      outline_visible#connect#changed ~callback:begin fun visible ->
        editor#set_show_outline visible;
        List.iter begin fun (mi, sign) ->
          mi#misc#handler_block sign;
          mi#set_active visible;
          mi#misc#handler_unblock sign;
        end !menu_item_view_outline
      end |> ignore;
      let update_view_vmessages_items visible =
        List.iter begin fun (mi, sign) ->
          toolbar#tool_messages_handler_block ();
          mi#misc#handler_block sign;
          mi#set_active visible;
          toolbar#tool_messages_set_active visible;
          mi#misc#handler_unblock sign;
          toolbar#tool_messages_handler_unblock ();
        end !menu_item_view_messages
      in
      Messages.vmessages#connect#visible_changed ~callback:update_view_vmessages_items |> ignore;
      let update_view_hmessages_items visible =
        List.iter begin fun (mi, sign) ->
          toolbar#tool_hmessages_handler_block ();
          mi#misc#handler_block sign;
          mi#set_active visible;
          toolbar#tool_hmessages_set_active visible;
          mi#misc#handler_unblock sign;
          toolbar#tool_hmessages_handler_unblock ();
        end !menu_item_view_hmessages
      in
      Messages.hmessages#connect#visible_changed ~callback:update_view_hmessages_items |> ignore;

      (* Editor *)
      paned#pack1 ~resize:true ~shrink:true editor#coerce;
      let update_toolbar_save () =
        let exists_unsaved = List.exists (fun p -> p#view#buffer#modified) editor#pages in
        Gmisclib.Idle.add ~prio:300 (fun () -> toolbar#tool_save_all#misc#set_sensitive exists_unsaved);
        Gaux.may (editor#get_page `ACTIVE) ~f:begin fun page ->
          Gmisclib.Idle.add ~prio:300 (fun () ->
              toolbar#tool_save#misc#set_sensitive page#buffer#modified);
          Gmisclib.Idle.add ~prio:300 begin fun () ->
            let button = find_custom_button `SAVE in
            Option.iter
              begin fun button ->
                button#misc#set_sensitive page#buffer#modified;
                button#misc#set_state `NORMAL
              end
              button
          end;
          Gmisclib.Idle.add ~prio:300 begin fun () ->
            let button = find_custom_button `SAVE_ALL in
            Option.iter
              begin fun button ->
                button#misc#set_sensitive exists_unsaved;
                button#misc#set_state `NORMAL
              end
              button
          end;
          Gmisclib.Idle.add ~prio:300 self#set_title;
        end;
      in
      let update_toolbar_undo () =
        Gaux.may (editor#get_page `ACTIVE) ~f:begin fun page ->
          let can_undo = (*page#buffer#modified &&*) page#buffer#undo#can_undo in
          let can_redo = (*page#buffer#modified &&*) page#buffer#undo#can_redo in
          toolbar#tool_undo#misc#set_sensitive can_undo;
          toolbar#tool_redo#misc#set_sensitive can_redo;
          (!get_menu_item_undo())#misc#set_sensitive can_undo;
          (!get_menu_item_redo())#misc#set_sensitive can_redo;
        end;
      in
      let callback _ =
        toolbar#update current_project#get;
        update_toolbar_save();
        update_toolbar_undo();
        Gaux.may (editor#get_page `ACTIVE) ~f:(fun _ -> self#set_title ());
        self#set_menu_item_nav_history_sensitive();
      in
      ignore (editor#connect#switch_page ~callback);
      ignore (editor#connect#remove_page ~callback);
      ignore (editor#connect#add_page ~callback);
      ignore (editor#connect#after#changed ~callback:update_toolbar_undo);
      editor#connect#after#modified_changed ~callback:update_toolbar_save |> ignore;
      editor#connect#file_saved ~callback:(fun _ -> self#update_git_status()) |> ignore;
      ignore (editor#connect#file_history_changed ~callback:begin fun fh ->
          Gaux.may menu ~f:begin fun menu ->
            let f =
              if List.length fh.File_history.content > 0
              then (fun x -> x#misc#show()) else (fun x -> x#misc#hide())
            in
            List.iter f [
              menu.file_recent_select;
              menu.file_recent_clear;
              menu.file_recent_sep;
            ]
          end;
        end);
      ignore (self#connect#switch_project ~callback:begin fun () ->
          toolbar#update current_project#get;
          self#set_menu_item_nav_history_sensitive()
        end);
      List.iter (fun c -> c#misc#set_sensitive (current_project#get <> None)) toolbar#children;
      callback();
      (* Initialize Quick_file_chooser *)
      let roots = List.map Filename.dirname project_history.File_history.content in
      Quick_file_chooser.init ~roots ~filter:Dialog_find_file.filter;
      (* Geometry settings *)
      let height = ref 700 in
      let width = ref 1052 in
      let pos_x = ref None in
      let pos_y = ref None in
      let is_menubar_visible = ref true in
      let is_toolbar_visible = ref true in
      let is_tabbar_visible = ref true in
      let is_outline_visible = ref true in
      begin
        try
          let chan = open_in (Filename.concat App_config.ocamleditor_user_home "geometry") in
          width := int_of_string (input_line chan);
          height := int_of_string (input_line chan);
          pos_x := Some (int_of_string (input_line chan));
          pos_y := Some (int_of_string (input_line chan));
          (try is_menubar_visible := (bool_of_string (input_line chan)) with End_of_file -> ());
          (try is_tabbar_visible := (bool_of_string (input_line chan)) with End_of_file -> ());
          (try is_toolbar_visible := (bool_of_string (input_line chan)) with End_of_file -> ());
          (try is_outline_visible := (bool_of_string (input_line chan)) with End_of_file -> ());
          close_in chan;
        with _ -> ()
      end;
      self#menubar_visible#set !is_menubar_visible;
      self#toolbar_visible#set !is_toolbar_visible;
      self#tabbar_visible#set !is_tabbar_visible;
      outline_visible#set !is_outline_visible;
      window#resize ~width:!width ~height:!height;
      Gmisclib.Idle.add ~prio:300 begin fun () ->
        Messages.vmessages#set_position (Preferences.preferences#get.vmessages_height);
      end;
      Gmisclib.Idle.add ~prio:300 (fun () -> Messages.hmessages#set_position (Preferences.preferences#get.hmessages_width));
      ignore (window#event#connect#after#delete ~callback:(fun _ -> self#exit editor (); true));
      button_menu_exit#connect#clicked ~callback:(fun () -> self#exit editor ()) |> ignore;
      button_menu_reset#connect#clicked ~callback:(fun () -> self#set_maximized_view `NONE) |> ignore;
      button_menu_iconify#connect#clicked ~callback:window#iconify |> ignore;
      (*  *)
      Ocaml_text.create_shell := self#shell;
      (* Load custom templates *)
      begin
        try Templ.load_custom `user;
        with Templ.Error (msg, details) ->
          (Dialog.message ~title:"Warning" ~message:(sprintf "%s\n\n\"%s\"" msg details) `WARNING);
      end;
      (*  *)
      self#set_geometry();
      window#add_accel_group accel_group;
      (* Listen for launcher *)
      let launcher_list = App_config.launcher_filename in
      let id_timeout = ref None in
      let check_launcher () =
        if Sys.file_exists launcher_list then begin
          let text = File_util.read launcher_list in
          let filenames = Str.split (Utils.regexp "\n") (Buffer.contents text) in
          let filenames = List.map String.trim filenames in
          let filenames = Utils.ListExt.remove_dupl filenames in
          List.iter begin fun filename ->
            editor#open_file ~active:true ~scroll_offset:0 ~offset:0 filename |> ignore;
          end filenames;
          let mv = maximized_view_action in
          window#set_modal true;
          window#present();
          if Sys.file_exists launcher_list then Sys.remove launcher_list;
          window#set_modal false;
          self#set_maximized_view mv;
          self#set_maximized_view mv;
        end;
      in
      window#event#connect#focus_out ~callback:begin fun _ ->
        Timeout.destroy tout_low_prio;
        id_timeout := Some (GMain.Timeout.add ~ms:300 ~callback:begin fun () ->
            check_launcher();
            true
          end);
        false
      end |> ignore;
      window#event#connect#focus_in ~callback:begin fun _ ->
        self#set_title();
        self#update_git_status();
        Timeout.start tout_low_prio;
        (match !id_timeout with Some id -> GMain.Timeout.remove id | _ -> ());
        false
      end |> ignore;
      (* Key sequences ("chords") *)
      window#event#connect#key_press ~callback:begin fun ev ->
        let keyval = GdkEvent.Key.keyval ev in
        let state = GdkEvent.Key.state ev in
        if state = [`CONTROL] && keyval = GdkKeysyms._k then begin
          let window = GWindow.window
              ~position:`CENTER
              ~modal:true
              ~decorated:false
              ~width:1 ~height:1
              ~show:true ()
          in
          let ms = 5000 in
          statusbar#flash_message ~delay:ms "Ctrl+K was pressed. Waiting for a second key...";
          GMain.Timeout.add ~ms ~callback:(fun () -> window#destroy(); false) |> ignore;
          window#event#connect#key_press ~callback:begin fun ev ->
            let state = GdkEvent.Key.state ev in
            let keyval = GdkEvent.Key.keyval ev in
            let result =
              if state = [`CONTROL] && keyval = GdkKeysyms._c then begin
                editor#with_current_page (fun page -> ignore (page#ocaml_view#toggle_comment false));
                true
              end else if keyval = GdkKeysyms._0 then begin
                editor#with_current_page Margin_fold.collapse_to_definitions;
                true
              end else if keyval = GdkKeysyms._j then begin
                editor#with_current_page Margin_fold.expand_all;
                true
              end else if keyval = GdkKeysyms._Escape then begin
                self#set_maximized_view `NONE;
                true
              end else false
            in
            statusbar#flash_message ~delay:ms "";
            window#destroy ();
            result
          end |> ignore;
          true
        end else false
      end |> ignore;
      (*  *)
      check_launcher()

    method connect = new signals ~startup ~switch_project ~project_history_changed

    initializer
      self#connect#startup ~callback:self#init |> ignore;

  end

and startup () = object inherit [unit] signal () end
and switch_project () = object inherit [unit] signal () end
and project_history_changed () = object inherit [File_history.t] signal () end
and signals ~startup ~switch_project ~project_history_changed =
  object
    inherit ml_signals [startup#disconnect; switch_project#disconnect; project_history_changed#disconnect ]
    method startup = startup#connect ~after
    method switch_project = switch_project#connect ~after
    method project_history_changed = project_history_changed#connect ~after
  end

let browser = ref None

(** create *)
let create window =
  let widget = new browser window in
  browser := Some widget;
  widget

(** splashscreen *)
let splashscreen () =
  let pref = Preferences.preferences#get in
  if pref.splashscreen_enabled then begin
    let decorated = false in
    let pixbuf = ??? Icons.logo in
    let image = GMisc.image ~pixbuf () in
    let window = GWindow.window
        ~title:About.program_name
        ~type_hint:`SPLASHSCREEN
        ~position:`CENTER_ALWAYS
        ~border_width:(if decorated then 0 else 1)
        ~focus_on_map:true
        ~decorated
        ~resizable:false
        ~urgency_hint:false
        ~show:false ()
    in
    window#misc#modify_bg [`NORMAL, `NAME (if decorated then "#ffffff" else "#c0c0c0")];
    window#add image#coerce;
    window#set_skip_pager_hint true;
    window#set_skip_taskbar_hint true;
    Some window
  end else None

