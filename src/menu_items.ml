open Option_syntax

type t = {
  item : GMenu.menu_item option ref;
  mutable icon : string option;
  mutable label : string;
  mutable description : string;
  mutable accel : (Gdk.Tags.modifier list * Gdk.keysym) list;
}

let accel_group : Gtk.accel_group option ref = ref None
let accel_flags : Gtk.Tags.accel_flag list ref = ref [`VISIBLE; `LOCKED]
let table : t list ref = ref []

let create ?icon ~label ?(description="") ?(accel=[])
    ?(packing : (GMenu.menu_item -> unit) option)
    (item : GMenu.menu_item option ref)
  =
  let gitem = GMenu.menu_item ~label () in
  packing |> Option.iter (fun pack -> pack gitem);
  accel |> List.iter begin fun (modi, key) ->
    let@ group = !accel_group in
    gitem#add_accelerator ~group ~modi ~flags:!accel_flags key
  end;
  let res = { item; icon; label; description; accel } in
  item := Some gitem;
  table := res :: !table;
  gitem

module File = struct
  let close_project : GMenu.menu_item option ref = ref None
end

module Search = struct
  let find_definition : GMenu.menu_item option ref = ref None
  let find_references : GMenu.menu_item option ref = ref None
end
