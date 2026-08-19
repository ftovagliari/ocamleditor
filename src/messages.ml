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

open GUtil
open Utils
open Preferences

exception Cancel_process_termination

let hpaned = ref None
let vpaned = ref None

let table = Hashtbl.create 17

(** page *)
class virtual page ~role =
  object (self)
    val detached = new detached ()
    val mutable detached_window = None
    val is_working = new GUtil.variable true
    val is_active = new GUtil.variable false
    val mutable holder : messages option = None
    val mutable close_tab_func = None
    val icon = new GUtil.variable None
    val title = new GUtil.variable ""
    method icon = icon#get
    method set_icon = icon#set
    method title = title#get
    method set_title = title#set
    method as_page = (self :> page)
    method set_holder p = holder <- Some p
    method present () =
      match detached_window with
      | Some window -> window#present()
      | _ -> Gaux.may holder ~f:(fun messages -> messages#present self#coerce)
    method is_working = is_working
    method is_active = is_active
    (*method virtual holder_changed : messages -> unit*)
    method holder_changed : messages -> unit = self#set_holder
    method virtual coerce : GObj.widget
    method virtual misc : GObj.misc_ops
    method virtual destroy : unit -> unit
    method set_close_tab_func f = close_tab_func <- Some f
    method set_button (b : GButton.button) =
      Gaux.may close_tab_func ~f:(fun f -> b#connect#clicked ~callback:f);

    method connect_detach = new detach_signals ~detached

    method private detach button_detach =
      if Preferences.preferences#get.detach_message_panes_separately then begin
        match detached_window with
        | Some (window : GWindow.window) ->
            begin
              match holder with
              | Some messages_pane ->
                  ignore (messages_pane#reparent self#misc#get_oid);
                  window#destroy();
                  detached_window <- None;
                  detached#call false;
                  self#present ();
                  Gaux.may (GWindow.toplevel self#coerce) ~f:(fun w -> w#present());
              | _ -> assert false
            end;
        | _ ->
            button_detach#misc#set_sensitive false; (* Fixes the button state *)
            (*button_detach#misc#set_state `NORMAL;*)
            let icon = match self#icon with Some x -> x | _ -> (??? Icons.oe) in
            let rect = self#misc#allocation in
            let has_memo = Preferences.geometry_memo.Gmisclib.Window.GeometryMemo.enabled in
            let memo_table = Preferences.geometry_memo.Gmisclib.Window.GeometryMemo.table in
            let width, height =
              if has_memo &&  Hashtbl.mem memo_table role then None, None else (Some rect.Gtk.width), (Some rect.Gtk.height)
            in
            let window = GWindow.window ~title:self#title ~icon ?width ?height ~border_width:0 ~position:`CENTER ~show:false () in
            window#set_geometry_hints ~pos:true ~user_pos:true ~user_size:true self#coerce;
            Gmisclib.Window.GeometryMemo.add ~key:role ~window Preferences.geometry_memo;
            self#misc#reparent window#coerce;
            detached_window <- Some window;
            window#show();
            detached#call true;
            button_detach#misc#set_sensitive true;
      end else begin
        match holder with
        | Some messages_pane -> messages_pane#detach button_detach
        | _ -> assert false
      end

    initializer
      let callback x =
        match detached_window with
        | Some window -> window#set_title x
        | _ -> ()
      in
      ignore (title#connect#changed ~callback);
      ignore (icon#connect#changed ~callback:begin fun x ->
          match detached_window with
          | Some window -> window#set_icon x
          | _ -> ()
        end);
  end

and detach_signals ~detached =
  object (self)
    inherit ml_signals [detached#disconnect ]
    method detached = detached#connect ~after
  end

and detached () = object (self) inherit [bool] signal () end


(** messages *)
and messages ~(paned : GPack.paned) () =
  let notebook        = GPack.notebook ~scrollable:true ~border_width:0 () in
  let remove_page     = new remove_page () in
  let switch_page     = new switch_page () in
  let visible_changed = new visible_changed () in
  object (self)
    val mutable detached_window = None
    val mutable parent = None
    inherit GObj.widget notebook#as_widget
    inherit Messages_tab.drag_handler

    val mutable visible = true;
    val mutable empty_page = None
    val mutable current_page = None

    initializer
      paned#add2 notebook#coerce;
      !hpaned |> Option.iter (fun hpaned -> notebook#set_tab_pos (if paned = hpaned then `BOTTOM(*`TOP*) else `BOTTOM));
      ignore (notebook#connect#remove ~callback:
                begin fun w ->
                  try if self#empty && self#visible then (self#set_visible false);
                  with Gpointer.Null -> ()
                end);
      self#set_visible false;
      (*  *)
      notebook#drag#dest_set Messages_tab.targets ~actions:[`MOVE ];
      ignore (notebook#drag#connect#data_received ~callback:self#data_received);
      notebook#connect#switch_page ~callback:begin fun num ->
        Gaux.may current_page ~f:(fun page -> page#is_active#set false);
        let page = notebook#get_nth_page num in
        try
          let _, page = Hashtbl.find table page#get_oid in
          page#is_active#set true;
          current_page <- Some page;
          switch_page#call page;
        with Not_found -> ()
      end |> ignore;
      self#misc#connect#map ~callback:begin fun _ ->
        Gaux.may (GWindow.toplevel self#coerce) ~f:begin fun (w : GWindow.window) ->
          w#event#connect#focus_in ~callback:begin fun _ ->
            Gaux.may current_page ~f:(fun page -> page#is_active#set true);
            false
          end |> ignore;
          w#event#connect#focus_out ~callback:begin fun _ ->
            Gaux.may current_page ~f:(fun page -> page#is_active#set false);
            false
          end |> ignore;
        end;
      end |> ignore;
      self#connect#visible_changed ~callback:begin fun visible ->
        Gaux.may current_page ~f:(fun page -> page#is_active#set visible);
      end |> ignore;
      self#connect#switch_page ~callback:begin fun page ->
        match detached_window with
        | None -> ()
        | Some window -> window#set_title page#title
      end |> ignore;

    method detach (button_detach : GButton.tool_button) =
      match detached_window with
      | Some (window : GWindow.window) ->
          begin
            match parent with
            | Some (container : GObj.widget) ->
                container#misc#hide();
                button_detach#misc#set_sensitive false;
                ignore (self#misc#reparent container#coerce);
                detached_window <- None;
                window#destroy();
                (*detached#call false;*)
                Gaux.may (GWindow.toplevel self) ~f:(fun w -> w#present());
                container#misc#show_all();
                button_detach#misc#set_sensitive true;
                (* TODO: Lablgtk3 issue, set_state *)
                (*button_detach#misc#set_state `NORMAL;*)
            | _ -> assert false
          end;
      | _ ->
          button_detach#misc#set_sensitive false; (* Fixes the button state *)
          let role = "messages-pane" in
          let rect = self#misc#allocation in
          let has_memo = Preferences.geometry_memo.Gmisclib.Window.GeometryMemo.enabled in
          let memo_table = Preferences.geometry_memo.Gmisclib.Window.GeometryMemo.table in
          let width, height =
            if has_memo && Hashtbl.mem memo_table role then None, None else (Some rect.Gtk.width), (Some rect.Gtk.height)
          in
          let window = GWindow.window ~title:"Messages" ~icon:(??? Icons.oe) ?width ?height ~border_width:0 ~position:`CENTER  ~show:false () in
          window#set_geometry_hints ~pos:true ~user_pos:true ~user_size:true self#coerce;
          Gmisclib.Window.GeometryMemo.add ~key:role ~window Preferences.geometry_memo;
          ignore (window#event#connect#delete ~callback:begin fun _ ->
              self#detach button_detach;
              self#remove_all_tabs();
              true
            end);
          parent <- self#misc#parent;
          detached_window <- Some window;
          self#misc#reparent window#coerce;
          window#show();
          Gaux.may current_page ~f:(fun page -> window#set_title page#title);
          (*detached#call true;*)
          button_detach#misc#set_sensitive true;
          (* TODO: Lablgtk3 issue, set_state *)
          (*button_detach#misc#set_state `NORMAL;*)

    method private empty =
      let len = List.length notebook#children in
      len = 0 || len = 1 && empty_page <> None

    method private set_page = notebook#set_page

    method private data_received context ~x ~y data ~info ~time =
      let failed () = context#finish ~success:false ~del:false ~time in
      if data#format = 8 then  begin
        try
          let oid = int_of_string data#data in
          let _, page = self#reparent oid in
          page#set_holder (self :> messages);
          notebook#goto_page (notebook#page_num page#coerce);
          context#finish ~success:true ~del:false ~time;
          page#holder_changed (self :> messages);
        with Not_found -> failed()
      end else (failed())

    method private remove_empty_page () =
      Gaux.may empty_page ~f:begin fun p ->
        notebook#set_show_tabs true;
        empty_page <- None;
        p#destroy()
      end;

    method reparent oid =
      let label_widget, page = Hashtbl.find table oid in
      let tab_label = new Messages_tab.widget ~with_spinner:false ~page () in
      ignore (tab_label#button#connect#clicked ~callback:page#destroy);
      match page#misc#parent with
      | Some parent when parent#misc#get_oid = self#misc#get_oid ->
          (*self#detach page;*)
          label_widget, page
      | Some _ ->
          label_widget#misc#reparent tab_label#hbox#coerce;
          page#misc#reparent self#coerce;
          self#set_page ~tab_label:tab_label#coerce page#coerce;
          self#remove_empty_page();
          label_widget, page
      | _ -> assert false

    method visible = visible

    method set_position = paned#set_position
    method position = paned#position

    method remove_all_tabs () =
      try
        List.iter (fun c -> try (remove_page#call c); notebook#remove c; c#destroy() with Gpointer.Null -> ()) notebook#children;
        self#set_visible false;
      with Gpointer.Null -> ()

    method append_page ~label_widget ?with_spinner (page : page) =
      let tab_label = new Messages_tab.widget ~label_widget ?with_spinner ~page () in
      Hashtbl.add table page#misc#get_oid (label_widget, page);
      let _ = notebook#append_page ~tab_label:tab_label#coerce page#coerce in
      ignore (page#misc#connect#destroy ~callback:begin fun () ->
          remove_page#call page#coerce;
          Hashtbl.remove table page#misc#get_oid;
        end);
      page#set_holder (self :> messages);
      page#set_button tab_label#button;
      self#remove_empty_page();

    method set_visible x =
      if x <> visible then begin
        begin
          match detached_window with
          | Some window -> if x then (window#present ()) else (window#misc#hide ());
          | _ -> if x then (paned#child2#misc#show ()) else (paned#child2#misc#hide ());
        end;
        let h = paned#misc#allocation.Gtk.height in
        if paned#position > (h - 30) then paned#set_position (h * 5 / 8);
        visible <- not visible;
      end;
      if visible && List.length notebook#children = 0 then begin
        let page =
          let label = GMisc.label ~text:"No messages" () in
          new no_messages ~label
        in
        notebook#set_show_tabs false;
        ignore (self#append_page ~with_spinner:false ~label_widget:(GMisc.label())#coerce page#as_page);
        empty_page <- Some page
      end;
      visible_changed#call visible;

    method present page =
      if List.exists (fun c -> c#misc#get_oid = page#misc#get_oid) notebook#children then begin
        self#set_visible true;
        notebook#goto_page (notebook#page_num page);
      end

    method connect = new signals ~remove_page ~visible_changed ~switch_page
  end

and no_messages ~label =
  object
    inherit page ~role:"<no-messages>"
    inherit GObj.widget label#as_widget
  end


and signals ~remove_page ~visible_changed ~switch_page =
  object
    inherit ml_signals [remove_page#disconnect; visible_changed#disconnect; switch_page#disconnect ]
    method remove_page = remove_page#connect ~after
    method visible_changed = visible_changed#connect ~after
    method switch_page = switch_page#connect ~after
  end

and remove_page () = object inherit [GObj.widget] signal () end
and visible_changed () = object inherit [bool] signal () end
and switch_page () = object inherit [page] signal () end

let vmessages =
  let store = ref None in
  fun () ->
    match !store with
    | Some mw -> mw
    | _ ->
        let paned = Option.get !vpaned in
        let messages = new messages ~paned () in
        store := Some messages;
        messages

let hmessages =
  let store = ref None in
  fun () ->
    match !store with
    | Some mw -> mw
    | _ ->
        let paned = Option.get !hpaned in
        let messages = new messages ~paned () in
        store := Some messages;
        messages
























