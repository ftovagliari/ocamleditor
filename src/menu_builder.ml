(** Builds menus from a declarative description.

    A menu is a list of {!node}s that refer to {!Command}s by identifier, so the structure of the
    menubar (order, grouping, nesting) lives in one readable place and reorganizing it means editing
    a list. Everything else (label, shortcuts, action, availability) comes from the command.

    {b Sensitivity.} While a menu is open its items are greyed out according to
    [Command.t.enabled]; as soon as it is closed they are made sensitive again, so that GTK never
    ignores a shortcut because of a stale state. The availability is checked again by
    {!Command.invoke} when the command runs. *)

type node =
  | Item of string                       (** Command identifier. *)
  | Separator
  | Sub of sub
  | Custom of (GMenu.menu -> unit)       (** Escape hatch for dynamic or special items. *)

and sub = { label : string; enabled : unit -> bool; nodes : node list }

let item id = Item id
let separator = Separator
let sub ?(enabled=fun () -> true) label nodes = Sub { label; enabled; nodes }
let custom f = Custom f

let accel_flags = [`VISIBLE; `LOCKED]

(** Fills [menu] with the items described by [nodes]. Shortcuts are registered in [group], which must
    be attached to the window. [category] is the menu path (e.g. ["File › Recent Files"]) recorded
    in the commands that do not have one yet. *)
let rec populate ~group ?(category="") (menu : GMenu.menu) nodes =
  let refreshers = ref [] and releasers = ref [] in
  let track ~refresh ~release =
    refreshers := refresh :: !refreshers;
    releasers := release :: !releasers
  in
  List.iter begin function
    | Separator -> ignore (GMenu.separator_item ~packing:menu#add ())
    | Custom f -> f menu
    | Item id ->
        let cmd = Command.find id in
        if cmd.Command.category = "" then cmd.Command.category <- category;
        let label = Command.current_label cmd in
        let activate () = ignore (Command.invoke cmd) in
        begin match cmd.Command.checked with
        | None ->
            let gitem = GMenu.menu_item ~label ~packing:menu#add () in
            List.iter (fun (modi, key) -> gitem#add_accelerator ~group ~modi ~flags:accel_flags key) cmd.Command.accels;
            ignore (gitem#connect#activate ~callback:activate);
            Command.bind cmd gitem#coerce
        | Some checked ->
            let gitem = GMenu.check_menu_item ~label ~active:(checked ()) ~packing:menu#add () in
            List.iter (fun (modi, key) -> gitem#add_accelerator ~group ~modi ~flags:accel_flags key) cmd.Command.accels;
            let sign = gitem#connect#activate ~callback:activate in
            Command.bind_check cmd gitem sign
        end;
        track ~refresh:(fun () -> Command.refresh cmd) ~release:(fun () -> Command.release cmd)
    | Sub { label; enabled; nodes } ->
        let gitem = GMenu.menu_item ~label ~packing:menu#add () in
        let submenu = GMenu.menu ~packing:gitem#set_submenu () in
        let category = if category = "" then label else category ^ " \xE2\x80\xBA " ^ label in
        populate ~group ~category submenu nodes;
        track
          ~refresh:(fun () -> gitem#misc#set_sensitive (enabled ()))
          ~release:(fun () -> gitem#misc#set_sensitive true)
  end nodes;
  (* "map"/"unmap", not "show"/"hide": GTK menus are created already "shown", so "show" does not fire
     the first time the menu is popped up. *)
  ignore (menu#misc#connect#map ~callback:(fun () -> List.iter (fun f -> f ()) !refreshers));
  ignore (menu#misc#connect#unmap ~callback:(fun () -> List.iter (fun f -> f ()) !releasers))

(** A submenu whose entries depend on the current state (the targets of the project, the recent
    files...), to be used as a {!Custom} node. The [static] nodes are built once. [fill] is called
    every time the {e parent} menu opens (so that the geometry of the submenu is right when it pops
    up) and returns the items it added, which are removed the next time. The header is greyed out
    while [enabled] is false. *)
let dynamic_submenu ~group ?(category="") ?(enabled=fun () -> true) ~label ?(static=[]) ~fill (menu : GMenu.menu) =
  let header = GMenu.menu_item ~label ~packing:menu#add () in
  let submenu = GMenu.menu ~packing:header#set_submenu () in
  let category = if category = "" then label else category ^ " \xE2\x80\xBA " ^ label in
  populate ~group ~category submenu static;
  let dynamic = ref [] in
  ignore (menu#misc#connect#map ~callback:begin fun () ->
      List.iter (fun (item : GMenu.menu_item) -> submenu#remove item) !dynamic;
      dynamic := fill submenu;
      header#misc#set_sensitive (enabled ())
    end);
  ignore (menu#misc#connect#unmap ~callback:(fun () -> header#misc#set_sensitive true))

(** Entries added directly to a menu (not to a submenu) that depend on the current state, to be used as
    a {!Custom} node: [fill] is called every time the menu opens and returns the items it added, which
    are removed the next time. *)
let dynamic_items ~fill (menu : GMenu.menu) =
  let dynamic = ref [] in
  ignore (menu#misc#connect#map ~callback:begin fun () ->
      List.iter (fun (item : GMenu.menu_item) -> menu#remove item) !dynamic;
      dynamic := fill menu
    end)

(** Creates a top-level menu (an entry of the menubar). Pass [menu] to fill an existing menu. *)
let create ~group ~label ?menu nodes =
  let header = GMenu.menu_item ~label () in
  let menu =
    match menu with
    | Some menu -> header#set_submenu menu; menu
    | None -> GMenu.menu ~packing:header#set_submenu ()
  in
  let cursor = Gdk.Cursor.create `ARROW in
  ignore (menu#event#connect#motion_notify ~callback:begin fun _ ->
      Gdk.Window.set_cursor menu#misc#window cursor;
      false
    end);
  populate ~group ~category:label menu nodes;
  header, menu
