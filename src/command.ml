(** Commands: the single source of truth for what the user can do from the menubar, a command
    palette, toolbars and context menus.

    A command bundles an identifier, a label, a description, the keyboard shortcuts, the action and
    the predicate that says whether the action is currently available.

    {b Why.} Menu items used to carry all this state themselves: the action was connected to the
    item, the item was made sensitive/insensitive by a callback run when the mouse entered the menu
    header, and the shortcuts were registered on the item. A shortcut on an insensitive item is
    ignored by GTK, so a stale sensitivity (computed the last time the menu was opened) made
    shortcuts like "Select to Matching Delimiter" randomly stop working.

    Here availability is a function ([enabled]) evaluated {e when the command is invoked}, so it can
    never be stale; the sensitivity of the menu items is only a visual hint, applied while a menu is
    open (see {!Menu_builder}).

    {b Command palette.} The registry already holds what a palette needs:
    - {!all} lists every registered command, in registration order (menu order, since menus register
      their commands as they are built);
    - [label] (or {!current_label} when it depends on the context), {!category} (the menu path, e.g.
      ["Edit"] or ["Search › Bookmarks"]), [description] and [keywords] are the searchable text;
    - {!shortcut_label} is the text to show next to the name;
    - [enabled] can be evaluated for every command when the palette opens, so it must stay cheap,
      synchronous and free of I/O; slow sources (Merlin...) belong in [on_menu_open], never in
      [enabled];
    - {!invoke} runs a command: close the palette and give the focus back to the editor {e first},
      because most actions act on the current page.

    A command does not need a menu entry to exist: register it and it will be found by the palette.
    Commands that only a palette can reach should set [category] explicitly. *)

type accel = Gdk.Tags.modifier list * Gdk.keysym

type t = {
  id : string;                          (** Stable identifier, e.g. ["edit.select_delimiter"]. *)
  label : string;
  label_fn : (unit -> string) option;   (** Overrides [label] when it depends on the context, e.g. the current file. *)
  description : string;                 (** One line, shown by the palette; may be empty. *)
  keywords : string list;               (** Extra words to find the command, e.g. synonyms. *)
  mutable category : string;            (** Menu path; filled in by {!Menu_builder} unless given at registration. *)
  icon : string option;                 (** Glyph of [Icon_set]. *)
  accels : accel list;
  enabled : unit -> bool;               (** Evaluated at invocation time. *)
  checked : (unit -> bool) option;      (** For toggles: the current state, shown as a check mark. [run] flips it. *)
  run : unit -> unit;
  on_menu_open : ((bool -> unit) -> unit) option;
  (** Called each time a menu containing the command opens, to refine the visual hint with a source
      that is too slow or asynchronous to be part of [enabled] (e.g. Merlin). It receives a function
      that greys the items out (or not); the function is ignored once the menu has closed. It never
      prevents the command from being invoked. *)
  mutable menu_open : bool;
  mutable items : GObj.widget list;     (** Menu items bound to this command. *)
  mutable checks : (GMenu.check_menu_item * GtkSignal.id) list;
  (** The bound items that are check items, with the id of the handler to block while syncing them. *)
}

let table : (string, t) Hashtbl.t = Hashtbl.create 127

(* Most recently registered first. *)
let registered : t list ref = ref []

let register ~id ~label ?label_fn ?(description="") ?(keywords=[]) ?(category="") ?icon
    ?(accels=[]) ?(enabled=fun () -> true) ?checked ?on_menu_open run =
  let cmd = {
    id; label; label_fn; description; keywords; category; icon; accels; enabled; checked; run; on_menu_open;
    menu_open = false; items = []; checks = [];
  } in
  if Hashtbl.mem table id then begin
    Printf.eprintf "Command.register: %S is registered more than once, the last definition wins.\n%!" id;
    registered := List.filter (fun c -> c.id <> id) !registered
  end;
  Hashtbl.replace table id cmd;
  registered := cmd :: !registered;
  cmd

let find_opt id = Hashtbl.find_opt table id

let find id =
  match find_opt id with
  | Some cmd -> cmd
  | None -> invalid_arg (Printf.sprintf "Command.find: unknown command %S" id)

(** Every registered command, in registration order. *)
let all () = List.rev !registered

(** The label to display now. *)
let current_label cmd = match cmd.label_fn with Some f -> f () | None -> cmd.label

let category cmd = cmd.category

(** "Ctrl+Shift+D", in the user's language. *)
let accel_label ((modi, key) : accel) = GtkData.AccelGroup.get_label ~key ~modi

(** Label of the first shortcut of the command, if any. *)
let shortcut_label cmd = match cmd.accels with a :: _ -> Some (accel_label a) | [] -> None

(** Runs the command if it is available; returns whether it ran. This is the only way an action
    should be triggered, whatever its origin (menu, shortcut, toolbar, palette). *)
let invoke cmd =
  if cmd.enabled () then (cmd.run (); true) else false

let bind cmd (item : GObj.widget) = cmd.items <- item :: cmd.items

(** Binds a check item; [sign] is the id of its "activate" handler. *)
let bind_check cmd (item : GMenu.check_menu_item) sign =
  bind cmd item#coerce;
  cmd.checks <- (item, sign) :: cmd.checks

(** The (first) menu item bound to the command, e.g. to ask it for its geometry. *)
let item cmd = match List.rev cmd.items with i :: _ -> Some i | [] -> None

(** Visual feedback: grey out the items of an unavailable command. *)
let refresh cmd =
  let set sensitive = if cmd.menu_open then List.iter (fun i -> i#misc#set_sensitive sensitive) cmd.items in
  cmd.menu_open <- true;
  Option.iter begin fun checked ->
    let active = checked () in
    List.iter begin fun (i, sign) ->
      i#misc#handler_block sign;
      i#set_active active;
      i#misc#handler_unblock sign
    end cmd.checks
  end cmd.checked;
  if cmd.label_fn <> None then begin
    let label = current_label cmd in
    List.iter (fun i -> i#misc#set_property "label" (`STRING (Some label))) cmd.items
  end;
  let sensitive = cmd.enabled () in
  set sensitive;
  if sensitive then Option.iter (fun hint -> hint set) cmd.on_menu_open

(** Items must be sensitive whenever their menu is closed, otherwise GTK would ignore their shortcuts. *)
let release cmd =
  cmd.menu_open <- false;
  List.iter (fun i -> i#misc#set_sensitive true) cmd.items

(** Shortcuts used by more than one command: [((modifiers, key), command ids)]. *)
let conflicts () =
  let by_accel = Hashtbl.create 127 in
  Hashtbl.iter begin fun _ cmd ->
    List.iter begin fun (modi, key) ->
      let k = (List.sort compare modi, key) in
      Hashtbl.replace by_accel k (cmd.id :: (try Hashtbl.find by_accel k with Not_found -> []))
    end cmd.accels
  end table;
  Hashtbl.fold (fun k ids acc -> if List.length ids > 1 then (k, List.sort compare ids) :: acc else acc) by_accel []
