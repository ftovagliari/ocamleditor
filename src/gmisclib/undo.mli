(*

  OCamlEditor
  Copyright (C) 2010 Francesco Tovagliari

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


(** Undo/Redo facilities for the {!GText.buffer} object. *)

type action

(** Undo/Redo manager for the {!GText.buffer} object.

    NOTE: Redo stack is automatically cleared at the beginning of every interactive
    action. Inserting or deleting text programmatically requires operations to
    be enclosed in a user action, i.e. between {!GText.text.begin_user_action}
    and {!GText.text.end_user_action} or {!Undo.manager.begin_block} and
    {!Undo.manager.end_block}.
*)
class manager :
  buffer:GText.buffer ->
  object

    (** Begin a block of actions. All changes between this call and the call
        of [end_block] will be undone as a single action. *)
    method begin_block : name:string -> unit

    (** @return [true] if there are redo operations available. *)
    method can_redo : bool

    (** @return [true] if there are undo operations available. *)
    method can_undo : bool

    method connect : manager_signals

    (** Stop recording changes to the buffer. Undo and redo stacks
        are not cleared. *)
    method disable : unit -> unit

    (** Start recording changes to the buffer. The [manager] is
        initially disabled. *)
    method enable : unit -> unit

    (**  *)
    method is_enabled : bool

    (** Ends a block of actions. *)
    method end_block : unit -> unit

    (** Try to perform a single redo. See {!manager.undo}. *)
    method redo : unit -> bool

    (** Try to perform a single undo.
        @return [true] if the undo has been performed; [false] if either the
        undo stack is empty or the cursor is in a position where no undo
        action was recorded: in this case the undo stack is not
        popped - the buffer is left unchanged - and the cursor is placed
        at the position where the topmost action of the stack can be reverted.
    *)
    method undo : unit -> bool

    (** Apply the given function so that changes it makes to the buffer will
        be undone by calling the given [inverse] function. If the first
        function ([f]) returns [false] its inverse function ([g]) is not
        stored in the undo stack: the undo manager only applies [f] and
        discards [g].
    *)
    method func : (unit -> bool) -> inverse:(unit -> bool) -> unit

    (** Length of then internal undo and redo stacks. Intended for testing purposes. *)
    method length : int * int

    method stats : int * int * int * int

  end

and manager_signals :
  undo:string GUtil.signal ->
  redo:string GUtil.signal ->
  can_undo_changed:bool GUtil.signal ->
  can_redo_changed:bool GUtil.signal ->
  object ('a)
    method after : 'a

    (** Emitted when the ability to undo has changed. *)
    method can_undo_changed : callback:(bool -> unit) -> GtkSignal.id

    (** Emitted when the ability to redo has changed. *)
    method can_redo_changed : callback:(bool -> unit) -> GtkSignal.id
    method disconnect : GtkSignal.id -> unit

    (** Emitted when an undo operation has been performed. If this operation
        is a block then [name] is the name of the block. *)
    method redo : callback:(name:string -> unit) -> GtkSignal.id

    (** Emitted when a redo operation has been performed. If this operation
        is a block then [name] is the name of the block. *)
    method undo : callback:(name:string -> unit) -> GtkSignal.id
  end
