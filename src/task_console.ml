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
open Utils
open Target
open Preferences

let re_error_line = Str.regexp_case_fold
    ".*\"\\(.+\\)\", line \\([0-9]+\\), characters \\([0-9]+\\)-\\([0-9]+\\):?"

let re_assert_failure = Str.regexp ".*\\(Assert_failure(\"\\(.+\\)\", \\([0-9]+\\), \\([0-9]+\\))\\)"

class view ~(editor : Editor.editor) ?(task_kind=(`OTHER : Task.kind)) ~task ?packing () =
  let project           = editor#project in
  let vbox              = GPack.vbox ?packing () in
  let hbox              = GPack.hbox ~packing:vbox#add () in
  let tooltips          = GData.tooltips () in
  let toolbar           = GButton.toolbar ~orientation:`VERTICAL ~style:`ICONS ~packing:hbox#pack () in
  let _                 = toolbar#set_icon_size `MENU in
  let sw                = GBin.scrolled_window ~shadow_type:`NONE ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:hbox#add () in
  let button_detach     = GButton.tool_button ~label:"Detach" ~packing:toolbar#insert () in
  let _                 = button_detach#set_icon_widget (Gtk_util.label_icon "\u{eb23}")#coerce in
  let _                 = button_detach#misc#set_name "menubarbutton" in
  let _                 = GButton.separator_tool_item ~packing:toolbar#insert () in
  (*  *)
  let button_stop       = GButton.tool_button ~packing:toolbar#insert () in
  let _                 = button_stop#set_icon_widget (Gtk_util.label_icon ~color:"red" "\u{f04d}")#coerce in
  let _                 = button_stop#misc#set_name "menubarbutton" in
  let _                 = tooltips#set_tip ~text:"Kill Process" button_stop#coerce in
  let button_run        = GButton.tool_button ~packing:toolbar#insert () in
  let _                 = button_run#misc#set_name "menubarbutton" in
  let _                 = button_run#set_icon_widget begin match task_kind with
      | `OTHER | `RUN ->
          tooltips#set_tip ~text:"Start" button_run#coerce;
          (Gtk_util.label_icon ~color:"forestgreen" "\u{f04b}")#coerce;
      | `CLEAN | `CLEANALL ->
          tooltips#set_tip ~text:task.Task.et_name button_run#coerce;
          (Icons.create (??? Icons.clear_build_16))#coerce;
      | `ANNOT | `COMPILE ->
          tooltips#set_tip ~text:task.Task.et_name button_run#coerce;
          (Gtk_util.label_icon ~color:"forestgreen" "\u{eba2}")#coerce;
    end#coerce in
  let _                 = GButton.separator_tool_item ~packing:toolbar#insert () in
  (*  *)
  let button_clear      = GButton.tool_button ~packing:toolbar#insert () in
  let _                 = button_clear#misc#set_name "menubarbutton" in
  let _                 = tooltips#set_tip ~text:"Clear Messages" button_clear#coerce in
  let _                 = button_clear#set_icon_widget (Gtk_util.label_icon "\u{eabf}")#coerce in
  let button_incr_font  = GButton.tool_button ~packing:toolbar#insert () in
  let _                 = button_incr_font#misc#set_name "menubarbutton" in
  let _                 = tooltips#set_tip ~text:"Increase Font Size" button_incr_font#coerce in
  let _                 = button_incr_font#set_icon_widget (Gtk_util.label_icon "\u{f09f4}")#coerce in
  let button_decr_font  = GButton.tool_button ~packing:toolbar#insert () in
  let _                 = button_decr_font#misc#set_name "menubarbutton" in
  let _                 = tooltips#set_tip ~text:"Decrease Font Size" button_decr_font#coerce in
  let _                 = button_decr_font#set_icon_widget (Gtk_util.label_icon "\u{f09f3}")#coerce in
  let button_wrap       = GButton.toggle_tool_button ~packing:toolbar#insert () in
  let _                 = button_wrap#misc#set_name "menubarbutton" in
  let _                 = tooltips#set_tip ~text:"Word Wrap" button_wrap#coerce in
  let _                 = button_wrap#set_icon_widget (Gtk_util.label_icon "\u{eb80}")#coerce in
  (*  *)
  let view              = GText.view ~editable:(task_kind = `RUN) ~cursor_visible:true ~packing:sw#add () in
  let _                 = view#set_right_margin 2 in
  let _                 = view#set_left_margin 2 in
  let _                 = button_wrap#connect#toggled ~callback:begin fun () ->
      view#set_wrap_mode (if button_wrap#get_active then `WORD else `NONE)
    end in
  let _                 = button_clear#connect#clicked ~callback:(fun () -> view#buffer#set_text "") in
  let _                 = button_wrap#set_active true in
  object (self)
    inherit GObj.widget vbox#as_widget
    inherit Messages.page ~role:"task-console" as super

    val working_status_changed = new working_status_changed ()
    val m_write = Mutex.create ()
    val mutable seq_tag_location = 0
    val mutable tag_locations = [];
    val mutable process = None
    val mutable signal_enabled = true
    val mutable process_outchan = None
    val mutable thread_run = None
    val mutable has_errors = false
    val mutable killed = false
    val mutable task = task
    val mutable button_run_signals = []
    val mutable current_run_cb = None
    val mutable current_use_thread = None
    val mutable tab_label : GMisc.label option = None

    method set_tab_label label = tab_label <- Some label
    method tab_label = match tab_label with Some x -> x | _ -> assert false

    method! parent_changed messages =
      super#parent_changed messages;
      toolbar#misc#hide();
      if messages = Messages.vmessages then begin
        toolbar#set_orientation `VERTICAL;
        toolbar#misc#reparent hbox#coerce;
        hbox#set_child_packing ~expand:false ~fill:false toolbar#coerce;
        hbox#reorder_child ~pos:0 toolbar#coerce;
      end else begin
        toolbar#set_orientation `HORIZONTAL;
        toolbar#misc#reparent vbox#coerce;
        vbox#set_child_packing ~expand:false ~fill:false toolbar#coerce;
      end;
      hbox#set_child_packing ~expand:true ~fill:true sw#coerce;
      toolbar#misc#show();

    method button_run = button_run
    method set_task x = task <- x
    method set_has_errors x = has_errors <- x
    method has_errors = has_errors
    method killed = killed
    method buffer = view#buffer
    method view : GText.view = view
    method has_process = process <> None
    method clear () =
      let buffer = view#buffer in
      buffer#delete ~start:(buffer#get_iter `START) ~stop:(buffer#get_iter `END)

    method close () =
      begin
        match process with
        | None -> ()
        | Some proc ->
            begin
              let pid, status = Unix.waitpid [] proc.Spawn.pid in
              match status with
              | Unix.WEXITED code
              | Unix.WSIGNALED code
              | Unix.WSTOPPED code -> has_errors <- code <> 0
            end;
      end;
      button_stop#misc#set_sensitive false;
      button_run#misc#set_sensitive true;
      self#view#set_editable false;
      process <- None;

    method stop () =
      match process with None -> () | Some proc ->
        Unix.kill proc.Spawn.pid 9 |> ignore;
        killed <- true;
        self#close()

    method restart () =
      self#stop();
      self#run ?run_cb:current_run_cb ?use_thread:current_use_thread ();

    method iter_chan f inchan =
      set_binary_mode_in inchan false;
      try while true do f inchan done with End_of_file -> ()

    method run ?(run_cb : (unit -> unit) option) ?(use_thread=true) () =
      current_run_cb <- run_cb;
      current_use_thread <- Some use_thread;
      let callback () =
        match process with
        | None ->
            if use_thread then begin
              let th = Thread.create (fun () -> self#do_run task) () in
              thread_run <- Some th;
              Some th
            end else begin
              self#do_run task;
              None
            end
        | Some _ ->
            GtkThread2.async self#present ();
            if use_thread then
              (match thread_run with None -> assert false | Some th -> Some th)
            else None
      in
      (* Disconnect previous callback assiciated with button_run and connect with a new one. *)
      GtkThread2.sync begin fun () ->
        begin
          try
            let sid = List.assoc button_run#misc#get_oid button_run_signals in
            GtkSignal.disconnect button_run#as_widget sid;
            button_run_signals <- List.remove_assoc button_run#misc#get_oid button_run_signals;
          with Not_found -> ()
        end;
        let sid = button_run#connect#clicked ~callback:begin fun () ->
            match (*task_kind,*) run_cb with
            | (*`RUN,*) Some f -> ignore (f ())
            | _ -> ignore (callback())
          end in
        button_run_signals <- (button_run#misc#get_oid, sid) :: button_run_signals;
      end ();
      callback()

    method private do_run task =
      let finally () =
        GtkThread2.sync begin fun () ->
          self#close();
          if has_errors then begin
            (*play "error.wav";*)
            Gmisclib.Idle.add begin fun () ->
              try view#scroll_to_mark (`NAME "first_error_line")
              with GText.No_such_mark _ -> ()
            end;
          end else begin
            (*play "success.wav";*)
            Gmisclib.Idle.add (fun () -> ignore (view#scroll_to_mark `INSERT));
          end;
          working_status_changed#call false;
          Activity.remove task.Task.et_name;
        end ()
      in
      has_errors <- false;
      GtkThread2.async begin fun () ->
        (try view#buffer#delete_mark (`NAME "first_error_line");
         with GText.No_such_mark("first_error_line") -> ());
        tag_locations <- [];
        view#buffer#remove_all_tags ~start:(view#buffer#get_iter `START) ~stop:(view#buffer#get_iter `END);
      end ();
      (** Process start *)
      let proc, cmd_line = Task_process.create task in
      (** Print command line *)
      GtkThread2.async begin fun () ->
        self#view#set_editable true;
        button_run#misc#set_sensitive false;
        button_stop#misc#set_sensitive true;
        working_status_changed#call true;
        (*Activity.add Activity.Task task.Task.name;*)
        Mutex.lock m_write;
        signal_enabled <- false;
        self#clear();
        (*      ksprintf (view#buffer#insert ~tag_names:["bold"; "output"]) "Environment: %s" (String.concat "; " task.Task.env);
                ksprintf (view#buffer#insert ~tag_names:["bold"; "output"]) "\nWorking directory: %s\n" task.Task.dir;
                ksprintf (view#buffer#insert ~tag_names:["bold"; "output"]) "Command:\n%s\n" (Process.cmd_line proc);*)
        (*ksprintf (view#buffer#insert ~tag_names:["bold"; "output"]) "%s\n" (Cmd.expand (project.Prj.autocomp_compiler ^ " -v"));*)
        let args = String.concat " " (List.map (fun (_ , x) -> x) (List.filter (fun (e, _) -> e) task.Task.et_args)) in
        ksprintf (view#buffer#insert ~tag_names:["bold"; "output"]) "%s %s\n" (Filename.quote task.Task.et_cmd) args;
        signal_enabled <- true;
        Mutex.unlock m_write;
        self#present ();
      end ();
      process <- Some proc;
      (*try*)
      let inchan = proc.Spawn.inchan in
      let outchan = proc.Spawn.outchan in
      let errchan = proc.Spawn.errchan in
      process_outchan <- Some outchan;
      (** Thread looping over the standard output of the process *)
      let th_in =
        Thread.create begin self#iter_chan
            begin fun ic ->
              let line = input_line ic in
              let line = if Glib.Utf8.validate line then line else Convert.to_utf8 line in
              GtkThread2.async begin fun line ->
                Mutex.lock m_write;
                signal_enabled <- false;
                view#buffer#insert ~iter:(self#buffer#get_iter `END) ~tag_names:["output"] (line ^ "\n");
                view#scroll_to_mark `INSERT;
                signal_enabled <- true;
                Mutex.unlock m_write;
              end line;
            end
        end inchan
      in
      (** Thread looping over the standard error of the process *)
      let is_before_first_error = ref true in
      let pending_loc_line = ref None in
      let check_first_error tag iter =
        if !is_before_first_error && (tag = "error" || tag = "warning") then begin
          ignore (view#buffer#create_mark ~name:"first_error_line" iter);
          (*view#buffer#insert ~iter ("####\n");*)
          is_before_first_error := false;
        end;
      in
      let th_err =
        Thread.create begin self#iter_chan
            begin fun ic ->
              let line = input_line ic in
              let line = if Glib.Utf8.validate line then line else Convert.to_utf8 line in
              let error_line = try Str.string_before line 6 = "Error:"
                with Invalid_argument _ -> false in
              let warning_line = try Str.string_before line 7 = "Warning"
                with Invalid_argument _ -> false in
              has_errors <- has_errors || error_line;
              let tag = if warning_line then "warning" else "error" in
              let is_location_line = Str.string_match re_error_line line 0 in
              let is_location_line = is_location_line || (Str.string_match re_assert_failure line 0) in
              GtkThread2.async begin fun line ->
                Mutex.lock m_write;
                signal_enabled <- false;
                let stop = self#buffer#get_iter `END in
                if is_location_line then begin
                  (* Links *)
                  let tag_location_name = sprintf "loc-%d" seq_tag_location in
                  let tag_location = view#buffer#create_tag ~name:tag_location_name [] in
                  tag_locations <- (tag_location#get_oid, (tag_location, line)) :: tag_locations;
                  seq_tag_location <- seq_tag_location + 1;
                  pending_loc_line := Some (line, tag_location_name, ref None);
                  view#buffer#insert ~iter:stop ~tag_names:["error"; tag_location_name] (line ^ "\n");
                end else begin
                  let tag_names =
                    match !pending_loc_line with
                    | None -> [tag]
                    | Some (_(*pl*), _, pending_tag) ->
                        let tag, apply_pending =
                          match !pending_tag with
                          | Some t ->
                              pending_tag := if String.trim line = "" then None else Some t;
                              t, false
                          | _ ->
                              pending_tag := Some tag;
                              tag, true
                        in
                        if apply_pending then begin
                          let start = stop#backward_lines 1 in
                          view#buffer#apply_tag_by_name tag ~start ~stop;
                          check_first_error tag start;
                        end;
                        [tag]
                  in
                  check_first_error tag stop;
                  view#buffer#insert ~iter:stop ~tag_names (line ^ "\n");
                end;
                view#scroll_to_mark `INSERT;
                signal_enabled <- true;
                Mutex.unlock m_write;
              end line;
            end
        end errchan
      in
      Thread.join th_in;
      Thread.join th_err;
      (*begin
        try ignore (Spawn.Parallel_process.close proc);
        with ex -> (printf "%s\n%!" (Printexc.to_string ex))
        end;*)
      finally()
    (*with Spawn.Parallel_process.Not_started -> (finally())*)

    initializer
      ignore (button_detach#connect#clicked ~callback:(fun () -> self#detach button_detach));
      ignore (button_incr_font#connect#clicked ~callback:(fun () -> Gtk_util.increase_font_size ~increment:1 view |> ignore));
      ignore (button_decr_font#connect#clicked ~callback:(fun () -> Gtk_util.increase_font_size ~increment:(-1) view |> ignore));
      ignore (Messages.vmessages#connect#remove_page ~callback:begin fun child ->
          if child#misc#get_oid = vbox#misc#get_oid then begin
            match process with None -> () | Some _ ->
              Dialog.process_still_active ~name:task.Task.et_name
                ~ok:self#stop ~cancel:(fun () -> raise Messages.Cancel_process_termination) ()
          end
        end);
      (** Input of the user redirected to the outchan of the process *)
      if task_kind = `RUN then begin
        ignore (view#buffer#connect#after#insert_text ~callback:begin fun it txt ->
            if process <> None && signal_enabled then (GtkThread2.sync begin fun () ->
                let start = it#backward_chars (String.length txt) in
                view#buffer#apply_tag_by_name "input" ~start ~stop:(view#buffer#get_iter `INSERT);
                match process_outchan with
                | None -> ()
                | Some ochan ->
                    output_string ochan txt;
                    flush ochan;
              end ())
          end);
      end;
      (** Locations links are activated by button_press *)
      let _ = view#event#connect#button_press ~callback:begin fun ev ->
          let x = int_of_float (GdkEvent.Button.x ev) in
          let y = int_of_float (GdkEvent.Button.y ev) in
          let x, y = view#window_to_buffer_coords ~tag:`WIDGET ~x ~y in
          let iter = view#get_iter_at_location ~x ~y in
          let find t = List.assoc t#get_oid tag_locations in
          let line = ref "" in
          List.iter (fun t -> try line := (snd (find t)) with Not_found -> ()) iter#tags;
          let group = if Str.string_match re_error_line !line 0 then 4 - 3
            else if Str.string_match re_assert_failure !line 0 then 2
            else 0 in
          if group > 0 then begin
            let basename = Str.matched_group group !line in  (* 2 *)
            if basename <> "_none_" then begin
              let linenum = int_of_string (Str.matched_group (group + 1) !line) in (* 3 *)
              let start = int_of_string (Str.matched_group (group + 2) !line) in  (* 4 *)
              let len = (* Length from the start of line *)
                try int_of_string (Str.matched_group (group + 3) !line)
                with Invalid_argument _ -> start
              in
              let parent = project.Prj.root // Prj.default_dir_src in
              let filename = List.fold_left (fun acc x -> acc // x) parent (Utils.filename_split basename) in
              ignore (editor#open_file ~active:true ~scroll_offset:0 ~offset:0 ?remote:None filename);
              match editor#get_page (`FILENAME filename) with
              | None -> false
              | Some page ->
                  editor#goto_view page#view;
                  let buf = (page#buffer :> Text.buffer) in
                  let lines = buf#line_count in
                  if linenum < lines - 1 then begin
                    Gmisclib.Idle.add begin fun () ->
                      let it = buf#get_iter (`LINE (linenum - 1)) in
                      let it = it#set_line_index 0 in
                      let line = it#get_text ~stop:it#forward_to_line_end in
                      let c = Convert.offset_from_pos line ~pos:start in
                      let where = it#forward_chars c in
                      let len = Convert.offset_from_pos (buf#get_text ~start:it ~stop:buf#end_iter ()) ~pos:len in
                      buf#select_range where (it#forward_chars len);
                      if (buf#get_iter `INSERT)#compare (buf#get_iter `SEL_BOUND) = 0 then (ignore(buf#select_word()));
                      ignore (page#view#scroll_lazy where);
                      page#view#misc#grab_focus();
                    end;
                    true
                  end else false;
            end else false;
          end else false
        end in
      (** Change the cursor and underline the links *)
      let _ = view#event#connect#motion_notify ~callback:begin fun ev ->
          let x = int_of_float (GdkEvent.Motion.x ev) in
          let y = int_of_float (GdkEvent.Motion.y ev) in
          let x, y = view#window_to_buffer_coords ~tag:`WIDGET ~x ~y in
          let iter = view#get_iter_at_location ~x ~y in
          List.iter (fun (_, (t, _)) -> t#set_properties [`UNDERLINE `NONE]) tag_locations;
          Gaux.may (view#get_window `TEXT) ~f:(fun w -> Gdk.Window.set_cursor w (Gdk.Cursor.create `ARROW));
          List.iter begin fun t ->
            try
              let _ = List.assoc t#get_oid tag_locations in
              (*let start = iter#backward_to_tag_toggle (Some t) in
                let stop = iter#forward_to_tag_toggle (Some t) in*)
              t#set_properties [`UNDERLINE `LOW];
              Gaux.may (view#get_window `TEXT) ~f:(fun w -> Gdk.Window.set_cursor w (Gdk.Cursor.create `HAND2));
            with Not_found -> () (* The cursor is not inside a tag_location *)
          end iter#tags;
          false
        end in
      (*  *)
      (*    button_run#connect#clicked ~callback:(fun () -> ignore (self#run ()));*)
      ignore (button_stop#connect#clicked ~callback:self#stop);
      view#misc#modify_font_by_name Preferences.preferences#get.output_font;
      view#misc#modify_base [`NORMAL, `NAME ?? (Preferences.preferences#get.output_bg_color)];
      ignore (view#buffer#create_tag ~name:"input"
                [`FOREGROUND ?? (Preferences.preferences#get.output_stdin_fg_color)]);
      ignore (view#buffer#create_tag ~name:"error"
                [`FOREGROUND ?? (Preferences.preferences#get.output_err_fg_color)]);
      ignore (view#buffer#create_tag ~name:"warning"
                [`FOREGROUND ?? (Preferences.preferences#get.output_warn_fg_color)]);
      ignore (view#buffer#create_tag ~name:"output"
                [`FOREGROUND ?? (Preferences.preferences#get.output_stdout_fg_color)]);
      ignore (view#buffer#create_tag ~name:"bold" [`WEIGHT `BOLD]);
      view#misc#grab_focus()

    method vbox = vbox

    method connect = new signals ~working_status_changed
  end
and signals ~working_status_changed =
  object
    inherit GUtil.ml_signals [working_status_changed#disconnect]
    method working_status_changed = working_status_changed#connect ~after
  end
and working_status_changed () = object inherit [bool] GUtil.signal () end

let views : (string * (view * GObj.widget)) list ref = ref []

(** create *)
let create ~editor task_kind task =
  let console_id =
    task.Task.et_args
    |> List.filter_map (fun (e, v) -> if e then Some (Shell.parse_args v) else None)
    |> List.flatten
    |> String.concat " "
    |> sprintf "%s %s %s" task.Task.et_name task.Task.et_cmd
  in
  try
    let (console, _) = List.assoc console_id !views in
    console#set_task task;
    console
  with Not_found -> begin
      let label_widget, set_active_func, label, icon =
        match task_kind with
        | `RUN (*| `OTHER*) ->
            let box = GPack.hbox ~spacing:3 () in
            let icon = (Icons.create (??? Icons.start_10)) in
            box#pack icon#coerce;
            let label = GMisc.label ~text:task.Task.et_name ~packing:box#pack () in
            box#coerce, Some begin fun active ->
              (*if finished then (icon#misc#hide()) else (icon#misc#show());*)
              if not active then (icon#misc#set_sensitive false) else (icon#misc#set_sensitive true);
            end, label, Some icon
        | _ ->
            let label = GMisc.label ~text:task.Task.et_name () in
            label#coerce, None, label, None
      in
      let page = new view ~editor ~task_kind ~task () in
      if task_kind = `RUN then begin
        page#set_close_tab_func begin fun () ->
          if page#has_process then
            Dialog.process_still_active ~name:task.Task.et_name
              ~ok:page#stop ~cancel:GtkSignal.stop_emit ()
        end;
      end;
      page#set_tab_label label;
      page#set_title task.Task.et_name;
      Gaux.may icon ~f:(fun icon -> page#set_icon (Some icon#pixbuf));
      if task.Task.et_visible then Messages.vmessages#append_page ~label_widget ~with_spinner:(task_kind <> `RUN) page#as_page;
      page#connect#working_status_changed ~callback:begin fun active ->
        (match set_active_func with None -> page#is_working#set | Some f -> f) active
      end |> ignore;
      page#misc#connect#destroy ~callback:(fun () -> views := List.remove_assoc console_id !views) |> ignore;
      views := (console_id, (page, page#vbox#coerce)) :: !views;
      page
    end

(** exec_sync *)
let exec_sync ?run_cb ?(use_thread=true) ?(at_exit=ignore) ~editor task_groups =
  let mode : [`all | `group | `single] = `single in
  let f tasks =
    try
      Project.Index.stop_watcher editor#project;
      tasks
      |> List.fold_left begin fun acc (task_kind, task) ->
        let console =
          match acc with
          | Some console when mode = `all || mode = `group ->
              console#set_task task;
              console
          | _ -> GtkThread2.sync (create ~editor task_kind) task
        in
        (*if mode = `single then (GtkThread2.async console#tab_label#set_text task.Task.et_name);*)
        begin
          match console#run ?run_cb ~use_thread:true () with
          | None -> ();
          | Some th -> Thread.join th;
        end;
        if console#has_errors || console#killed then (raise Exit);
        Some console
      end None
      |> ignore;
      Project.Index.start_watcher editor#project;
      at_exit()
    with Exit ->
      Project.Index.start_watcher editor#project;
      at_exit();
      raise Exit
  in
  let g () =
    try
      begin
        match mode with
        | `group -> List.iter (fun ts -> f ts) task_groups
        | `all | `single -> f (List.flatten task_groups)
      end;
    with Exit -> ()
  in
  if use_thread then (ignore(Thread.create g ())) else (g ())


(** exec *)
let exec ~editor ?use_thread ?(with_deps=false) task_kind target =
  if Preferences.preferences#get.editor_save_all_bef_comp && (
      [`COMPILE; `COMPILE_ONLY] |> List.mem task_kind ||
      (match task_kind with
       | `RCONF rc -> rc.Rconf.build_task <> `NONE
       | _ -> false)
    )
  then (editor#save_all());
  let project = editor#project in
  let can_compile_native = project.Prj.can_compile_native in
  let filter_tasks = Target.filter_external_tasks target in
  let tasks_clean () =
    if Oebuild.check_restrictions target.restrictions then
      (* External build tasks *)
      let et_before_clean = filter_tasks Task.Before_clean in
      let et_clean = filter_tasks Task.Clean in
      let et_clean = if et_clean = [] then [`CLEAN, begin
          let cmd, args = Target.create_cmd_line target in
          let name = sprintf "Clean \xC2\xAB%s\xC2\xBB" (Filename.basename target.Target.name) in
          Task.create ~name ~env:[] ~dir:"" ~cmd ~args:(args @ [true, "-clean"]) ()
        end] else et_clean in
      let et_after_clean = filter_tasks Task.After_clean in
      (* Execute sequence *)
      Project.clean_tmp project;
      et_before_clean @ et_clean @ et_after_clean;
    else []
  in
  let tasks_annot () =
    if Oebuild.check_restrictions target.restrictions then
      [`ANNOT, begin
          let cmd, args = Target.create_cmd_line target in
          let args = (true, "-c") (*:: (true, "-annot")*) :: args in
          let name = sprintf "Compile \xC2\xAB%s\xC2\xBB" (Filename.basename target.Target.name) in
          Task.create ~name ~env:[] ~dir:"" ~cmd ~args ()
        end]
    else []
  in
  let build_deps = if with_deps then Target.find_target_dependencies project.Prj.targets target else [] in
  let compile_name = sprintf "Compile \xC2\xAB%s\xC2\xBB" (Filename.basename target.name) in
  let build_name = sprintf "Build \xC2\xAB%s\xC2\xBB" (Filename.basename target.name) in
  let compile_buffer = fun () ->
    (* N.B. compile_buffer also updates ocaml-index *)
    GtkThread2.async editor#with_current_page (fun p -> p#compile_buffer ?join:None ());
  in
  match task_kind with
  | `CLEANALL ->
      let cmd, args = Target.create_cmd_line target in
      let args = args @ [true, "-distclean"] in
      let task = Task.create ~name:"Clean Project" ~env:[] ~dir:"" ~cmd ~args () in
      exec_sync ~editor [[`CLEANALL, task]];
      (*let console = create ~editor `CLEANALL task in
        ignore (console#button_run#connect#clicked ~callback:(fun () -> ignore (console#run())));
        ignore(console#run ?use_thread ());*)
      (* Remove executables *)
      List.iter begin fun target ->
        let compilation =
          (if target.byt then [Oebuild.Bytecode] else [])
          @ (if target.opt && project.Prj.can_compile_native then [Oebuild.Native] else [])
        in
        (*let files = Str.split (Str.regexp " +") target.files in*)
        let outname = target.outname in
        let outkind = match target.target_type with
          | Executable -> Oebuild.Executable
          | Library -> Oebuild.Library
          | Plugin -> Oebuild.Plugin
          | Pack -> Oebuild.Pack
          | External -> Oebuild.External
        in
        List.iter begin fun compilation ->
          let outname = Oebuild.get_output_name ~compilation ~outkind ~outname  ~dontaddopt:target.dontaddopt () in
          Oebuild_util.remove_file ~verbose:false outname
        end compilation
      end project.Prj.targets;
  | `CLEAN -> exec_sync ~editor [tasks_clean ()];
  | `ANNOT -> exec_sync ~editor [tasks_annot ()];
  | `COMPILE ->
      let rec f () = exec_sync ~run_cb:f ~editor ~at_exit:compile_buffer (tasks_compile ~name:build_name ~build_deps ~can_compile_native target) in
      f()
  | `COMPILE_ONLY ->
      exec_sync ~editor ~at_exit:compile_buffer (tasks_compile ~flags:["-c"] ~name:compile_name ~build_deps ~can_compile_native target);
  | `RCONF rc ->
      if Oebuild.check_restrictions target.restrictions then
        let compilation = if target.Target.opt then Oebuild.Native else Oebuild.Bytecode in
        let outkind = match target.target_type with
          | Executable -> Oebuild.Executable
          | Library -> Oebuild.Library
          | Plugin -> Oebuild.Plugin
          | Pack -> Oebuild.Pack
          | External -> Oebuild.External
        in
        let outname = Oebuild.get_output_name ~compilation ~outkind ~outname:target.outname ~dontaddopt:target.dontaddopt () in
        let name = rc.Rconf.name in
        let prior_tasks =
          match rc.Rconf.build_task with
          | `NONE -> []
          | `CLEAN -> [tasks_clean ()]
          | `COMPILE -> tasks_compile ~name:build_name ~build_deps ~can_compile_native target
          | `REBUILD -> [(tasks_clean ()); List.flatten (tasks_compile ~name:build_name ~build_deps ~can_compile_native target)]
          | `ETASK name ->
              let etask = List.find ((=) name) target.Target.external_tasks in
              [[`OTHER, etask]]
        in
        let tasks = prior_tasks @ [[
            `RUN, Task.create
              ~name
              ~env:rc.Rconf.env
              ~env_replace:rc.Rconf.env_replace
              ~dir:""
              ~cmd:(Filename.current_dir_name // outname)
              ~args:(List.filter (fun (e, _) -> e) rc.Rconf.args) ();
          ]] in
        let rec f () = ignore (exec_sync ~run_cb:f ~editor tasks) in
        f();
      else ()
  | `INSTALL_LIBRARY ->
      let oebuild, args = Target.create_cmd_line target in
      let name = Filename.basename target.Target.name in
      let tasks = (*prior_tasks @*) [
        `RUN, Task.create
          ~name
          ~env:[]
          ~dir:""
          ~cmd:oebuild
          ~args:(args @ [true, "-install " ^ target.Target.lib_install_path]) ();
      ] in
      let rec f () = ignore (exec_sync ~run_cb:f ~editor [tasks]) in
      f();
  | _ -> assert false






