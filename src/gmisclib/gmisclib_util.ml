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

exception Mark_deleted

let fade_window_enabled = ref false

(** fade_window *)
let fade_window ?(incr=0.10) ?(stop=1.0) window =
  if !fade_window_enabled then begin
    window#set_opacity 0.0;
    window#show();
    let callback =
      let opa = ref 0.0 in fun () ->
        window#set_opacity !opa;
        opa := !opa +. incr;
        !opa <= stop;
    in
    ignore (callback() : bool);
    ignore (GMain.Timeout.add ~ms:20 ~callback : GMain.Timeout.id)
  end else window#present()

(** esc_destroy_window *)
let esc_destroy_window window =
  ignore (window#event#connect#key_press ~callback:begin fun ev ->
      let key = GdkEvent.Key.keyval ev in
      if key = GdkKeysyms._Escape then (window#destroy(); true) else false
    end);;

(** idle_add_gen *)
let idle_add_gen ?prio f = GMain.Idle.add ?prio begin fun () ->
    try f ()
    with ex -> (eprintf "%s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace())); false
  end

(** idle_add *)
let idle_add ?prio (f : unit -> unit) = ignore (GMain.Idle.add ?prio begin fun () ->
    try f (); false
    with ex -> (eprintf "%s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace())); false
  end)

(** Schedules a sequence of operations to be executed sequentially in the idle loop.
    This function takes a list of operations and schedules them to run one after another
    when the system is idle, using GTK's idle callback mechanism. Operations are chained
    together so that completing one operation triggers the next. *)
let idleize_cascade =
  let idleize ?prio f k () = idle_add ?prio (f (); k) in
  let compose ff = List.fold_left (fun acc f -> f acc) ignore ff in
  fun ?prio operations ->
    operations |> List.map (idleize ?prio) |> compose;;

(** get_iter_at_mark_safe *)
let get_iter_at_mark_safe buffer mark =
  (*try*)
  if GtkText.Mark.get_deleted mark then (raise Mark_deleted)
  else (GtkText.Buffer.get_iter_at_mark buffer mark)
(*with ex ->
  Printf.eprintf "File \"gtk_util.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());
  raise ex*)

let get_iter_at_mark_opt buffer mark =
  (*try*)
  if GtkText.Mark.get_deleted mark then None
  else Some (GtkText.Buffer.get_iter_at_mark buffer mark)
(*with ex ->
  Printf.eprintf "File \"gtk_util.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());
  raise ex*)

(** set_tag_paragraph_background *)
let set_tag_paragraph_background (tag : GText.tag) =
  Gobject.Property.set tag#as_tag {Gobject.name="paragraph-background"; conv=Gobject.Data.string}

(** treeview_is_path_onscreen *)
let treeview_is_path_onscreen (view : GTree.view) path =
  let rect = view#get_cell_area ~path () in
  let y = float (Gdk.Rectangle.y rect) in
  0. <= y && y <= view#vadjustment#page_size;;













