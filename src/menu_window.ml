(** "Window" menu: switching between files and navigation through the location history.

    The entries for the open files (radio items) are added to [items.window] by the browser. *)

open GdkKeysyms

(** Commands of the Window menu. Returns the predicates for "can go back" and "can go forward". *)
let register ~browser =
  let editor = browser#editor in
  let can_go_back () = let back, _, _ = editor#location_history_is_empty () in not back in
  let can_go_forward () = let _, forward, _ = editor#location_history_is_empty () in not forward in
  let has_last_edit_location () = let _, _, last = editor#location_history_is_empty () in not last in
  let reg id label ?accels ?enabled run =
    ignore (Command.register ~id:("window." ^ id) ~label ?accels ?enabled run)
  in
  reg "switch" "Switch..." ~enabled:(fun () -> editor#get_page `ACTIVE <> None) editor#dialog_file_select;
  reg "prev_location" "Previous Location" ~accels:[[`MOD1], _Left] ~enabled:can_go_back begin fun () ->
    browser#goto_location `PREV;
    browser#set_menu_item_nav_history_sensitive()
  end;
  reg "next_location" "Next Location" ~accels:[[`MOD1], _Right] ~enabled:can_go_forward begin fun () ->
    browser#goto_location `NEXT;
    browser#set_menu_item_nav_history_sensitive()
  end;
  reg "last_edit_location" "Last Edit Location" ~accels:[[`MOD1], _End; [`MOD1], _KP_End]
    ~enabled:has_last_edit_location (fun () -> browser#goto_location `LAST);
  reg "clear_location_history" "Clear Location History"
    ~enabled:(fun () -> Location_history.length editor#location_history > 0) begin fun () ->
    Location_history.clear editor#location_history;
    browser#set_menu_item_nav_history_sensitive()
  end;
  can_go_back, can_go_forward

(** Creates the Window menu; [items.window] is the menu to fill. *)
let create ~browser ~group ~items =
  let can_go_back, can_go_forward = register ~browser in
  let editor = browser#editor in
  (* Entries of the "Back"/"Forward" submenus: the locations of the history, rebuilt when the menu opens. *)
  let locations dir submenu =
    let history =
      match dir with
      | `BACK -> Location_history.get_history_backward editor#location_history
      | `FORWARD -> Location_history.get_history_forward editor#location_history
    in
    if history = [] then [] else
      let sep = GMenu.separator_item ~packing:submenu#add () in
      sep :: List.map begin fun entry ->
        let label = Location_history.string_of_location entry in
        let label = if entry.Location_history.kind = `EDIT then label ^ "*" else label in
        let item = GMenu.menu_item ~label ~packing:submenu#add () in
        ignore (item#connect#activate ~callback:(fun () -> browser#goto_location (`LOCATION entry)));
        item
      end history
  in
  let back =
    Menu_builder.dynamic_submenu ~group ~category:"Window" ~label:"Back" ~enabled:can_go_back
      ~static:Menu_builder.[item "window.prev_location"] ~fill:(locations `BACK)
  in
  let forward =
    Menu_builder.dynamic_submenu ~group ~category:"Window" ~label:"Forward" ~enabled:can_go_forward
      ~static:Menu_builder.[item "window.next_location"] ~fill:(locations `FORWARD)
  in
  (* Radio items of the open files: check the one of the current page when the menu opens. *)
  let sync_pages menu =
    ignore (menu#misc#connect#map ~callback:begin fun () ->
        editor#with_current_page begin fun page ->
          let current_page_oid = page#misc#get_oid in
          items.Menu_types.window_signal_locked <- true;
          List.iter (fun (oid, item) -> item#set_active (oid = current_page_oid)) items.Menu_types.window_pages;
          items.Menu_types.window_signal_locked <- false
        end
      end)
  in
  (* Mouse buttons "back" and "forward". *)
  ignore (browser#window#event#connect#button_release ~callback:begin fun ev ->
      begin match GdkEvent.Button.button ev with
        | 8 -> ignore (Command.invoke (Command.find "window.prev_location"))
        | 9 -> ignore (Command.invoke (Command.find "window.next_location"))
        | _ -> ()
      end;
      false
    end);
  browser#set_menu_item_nav_history_sensitive();
  Menu_builder.create ~group ~label:"Window" ~menu:items.Menu_types.window Menu_builder.[
      item "window.switch";
      separator;
      custom back; custom forward;
      item "window.last_edit_location";
      item "window.clear_location_history";
      custom sync_pages;
    ]
