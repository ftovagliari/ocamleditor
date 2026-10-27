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

(** TODO: Investigate the use of GADT for type action, to reduce the use of `assert false` *)
type action =
  | Insert      of action_insert
  | Delete      of action_delete
  | Begin_block of string (* Block name *)
  | End_block   of string (* Block name *)
  | Func        of ((unit -> bool) * (unit -> bool))
  (* If the first function (f) returns false its inverse function (g) is not stored in the
     undo stack: the undo manager only applies f and discards g. *)

and action_insert = {
  mutable pos : int; (* Where to delete *)
  mutable length : int;
}

and action_delete = {
  mutable text   : string;    (* Text deleted *)
  mutable where  : int;       (* Where to reinsert *)
  mutable bounds : int * int; (* Selection bounds *)
  mutable prev   : int;       (* Previous delete position in the same action *)
}

class manager ~(buffer : GText.buffer) =
  let undos = Stack.create () in
  let redos = Stack.create () in
  object (self)
    val undo = new signal ()
    val redo = new signal ()
    val can_undo_changed = new signal ()
    val can_redo_changed = new signal ()
    val text_buffer = buffer#as_buffer
    val mutable current_block_name = ""
    val mutable current_stack = undos
    val mutable pending_action : action option = None;
    val mutable enabled = false
    val mutable sign_insert = buffer#connect#insert_text ~callback:(fun _ _ -> ())
    val mutable sign_delete = buffer#connect#delete_range ~callback:(fun ~start:_ ~stop:_ -> ())

    initializer
      GtkSignal.disconnect text_buffer sign_insert;
      GtkSignal.disconnect text_buffer sign_delete;

      (* Clear the redo stack at the beginning of a user action *)
      ignore (buffer#connect#begin_user_action ~callback:begin fun () ->
          if enabled then begin
            current_stack <- undos;
            Stack.clear redos;
            can_redo_changed#call self#can_redo;
          end
        end);

      (* Before insert *)
      sign_insert <- buffer#connect#insert_text ~callback:begin fun start text ->
          let length = Glib.Utf8.length text in
          try
            let insert_action =
              match [@warning "-4"] pending_action with
              | Some ((Insert a) as pending) when start#equal (buffer#get_iter (`OFFSET a.pos)) ->
                  if length > 1 || text = "\n" then begin
                    self#push pending (a.pos, a.pos);
                    let pos = start#offset + length in
                    self#push (Insert {pos=pos; length=length}) (pos, pos);
                    pending_action <- None;
                    raise Exit
                  end else a
              | Some ((Insert a) as pending) ->
                  self#push pending (a.pos, a.pos);
                  {pos=start#offset; length=0}
              | Some ((Delete a) as pending) ->
                  self#push pending (a.where, a.where);
                  {pos=start#offset; length=0}
              | None -> {pos=start#offset; length=0}
              | _ -> assert false
            in
            insert_action.pos <- start#offset + length;
            insert_action.length <- insert_action.length + length;
            pending_action <- Some (Insert insert_action);
            if current_stack == undos then (can_undo_changed#call self#can_undo)
            else (can_redo_changed#call self#can_redo);
          with Exit -> ()
        end;

      (* Before delete *)
      sign_delete <- buffer#connect#delete_range ~callback:begin fun ~start ~stop ->
          let delete_action =
            match [@warning "-4"] pending_action with
            | Some ((Insert a) as pending) ->
                self#push pending (a.pos, a.pos);
                {text=""; where=start#offset; bounds=(0,0); prev=start#offset}
            | Some ((Delete action) as pending) ->
                let prev = buffer#get_iter (`OFFSET action.prev) in
                if not (start#equal prev) && not (stop#equal prev) then begin
                  self#push pending (action.where, action.where);
                  {text=""; where=start#offset; bounds=(0,0); prev=start#offset}
                end else begin
                  action.prev <- start#offset;
                  action
                end
            | None -> {text=""; where=start#offset; bounds=(0,0); prev=start#offset}
            | _ -> assert false
          in
          pending_action <- Some (Delete delete_action);
          if current_stack == undos then (can_undo_changed#call self#can_undo) else (can_redo_changed#call self#can_redo);
          let b1, b2 = buffer#selection_bounds in
          if b1#equal start && b2#equal stop then begin
            (* Selected text *)
            let text = buffer#get_text ~start ~stop () in
            let length = Glib.Utf8.length text in
            delete_action.text <- text;
            delete_action.bounds <- (length, 0);
            self#push (Delete delete_action) (start#offset, start#offset);
          end else if (start#equal (buffer#get_iter `INSERT)) then begin
            (* Del *)
            delete_action.text <- delete_action.text ^ (buffer#get_text ~start ~stop ());
            delete_action.bounds <- (let b = fst delete_action.bounds in (b, b));
          end else begin
            (* Backspace *)
            let text = buffer#get_text ~start ~stop () in
            let length = Glib.Utf8.length text in
            delete_action.text <- text ^ delete_action.text;
            let b1 = (fst delete_action.bounds) + length in
            let b2 = if length = 1 then b1 else 0 in
            delete_action.bounds <- (b1, b2);
            delete_action.where <- start#offset;
          end;
        end;

      (* The Undo and Redo signals *)
      ignore (self#connect#undo ~callback:(fun ~name:_ -> self#pop undos));
      ignore (self#connect#redo ~callback:(fun ~name:_ -> self#pop redos));

      (* Undo initially disabled *)
      self#disable();

    method private push pending (b1, b2) =
      Stack.push (pending, b1, b2, Unix.gettimeofday()) current_stack;
      pending_action <- None;
      if current_stack == undos then (can_undo_changed#call self#can_undo) else (can_redo_changed#call self#can_redo);

    method private push_pending_action () =
      match [@warning "-4"] pending_action with
      | None -> ()
      | Some ((Insert a) as pending) -> self#push pending (a.pos, a.pos)
      | Some ((Delete a) as pending) -> self#push pending (a.where, a.where)
      | _ -> assert false

    method private pop ?(in_block=false) stack =
      let (action, b1, b2, t1) (*as pending*) = Stack.pop stack in
      (* current_stack is used by the insert/delete handlers when activated by the undo/redo of popped actions. *)
      current_stack <- (if stack == undos then redos else undos);
      begin
        match action with
        | Insert action ->
            let stop = buffer#get_iter (`OFFSET action.pos) in
            buffer#delete ~start:(stop#backward_chars action.length) ~stop;
            if in_block then (self#pop ~in_block stack)
            else begin (* Pop all contiguous Inserts *)
              try
                begin
                  match [@warning "-4"] Stack.top stack with
                  | Insert _, _, _, t2 when abs_float(t1 -. t2) <= 0.2 -> self#pop stack
                  | _ -> ()
                end;
              with Stack.Empty -> ()
            end
        | Delete action ->
            let iter = buffer#get_iter (`OFFSET b1) in
            buffer#insert ~iter action.text;
            let where = buffer#get_iter (`OFFSET b1) in
            if stack == undos then begin
              let b1, b2 = action.bounds in
              buffer#select_range (where#forward_chars b1) (where#forward_chars b2);
            end;
            if in_block then (self#pop ~in_block stack);
        | End_block name ->
            self#push (Begin_block name) (b1, b2);
            self#pop ~in_block:true stack; (* Popping from End_block to Begin_block *)
        | Begin_block name ->
            self#push_pending_action();
            self#push (End_block name) (b1, b2);
            buffer#select_range (buffer#get_iter (`OFFSET b1)) (buffer#get_iter (`OFFSET b2));
            (assert in_block);
        | Func (f, g) ->
            buffer#select_range (buffer#get_iter (`OFFSET b1)) (buffer#get_iter (`OFFSET b2));
            self#disable();
            let action =
              if stack == undos then (ignore (g()); (Func (g, f))) else (ignore (f()); (Func (f, g)))
            in
            self#push action
              ((buffer#get_iter `INSERT)#offset, (buffer#get_iter `SEL_BOUND)#offset);
            self#enable();
      end;
      self#push_pending_action();
      current_stack <- stack;
      can_undo_changed#call self#can_undo;
      can_redo_changed#call self#can_redo;

    method private revert stack =
      if enabled then begin
        self#push_pending_action();
        if current_block_name <> "" then (self#end_block());
        (*let opposite = if stack == undos then redos else undos in*)
        if Stack.is_empty stack then begin
          false
        end else begin
          let ins = (buffer#get_iter `INSERT) in
          let sel = (buffer#get_iter `SEL_BOUND) in
          let block_name, b1, b2 =
            match [@warning "-4"] Stack.top stack with (* Cannot raise Stack.Empty *)
            | (Begin_block name), b1, b2, _ -> name, b1, b2
            | (End_block name), b1, b2, _ -> name, b1, b2
            | _, b1, b2, _ -> "", b1, b2
          in
          let b1, b2 = (buffer#get_iter (`OFFSET b1)), (buffer#get_iter (`OFFSET b2)) in
          if ins#equal b1 && sel#equal b2 then begin
            if stack == undos then (undo#call block_name) else (redo#call block_name); (* pop *)
            true
          end else begin
            buffer#select_range b1 b2;
            false
          end;
        end;
      end else false

    method begin_block ~name =
      if enabled && current_block_name = "" then
        self#push_pending_action();
      buffer#begin_user_action();
      current_block_name <- name;
      self#push (Begin_block name)
        ((buffer#get_iter `INSERT)#offset, (buffer#get_iter `SEL_BOUND)#offset);

    method end_block () =
      if enabled && current_block_name <> "" then
        self#push_pending_action();
      begin
        match [@warning "-4"] Stack.top current_stack with
        | (Begin_block name), _, _, _ when name = current_block_name ->
            ignore (Stack.pop current_stack) (* remove empty blocks from the stack *)
        | _ ->
            self#push (End_block current_block_name)
              ((buffer#get_iter `INSERT)#offset, (buffer#get_iter `SEL_BOUND)#offset);
      end;
      current_block_name <- "";
      buffer#end_user_action();

    method func f ~inverse =
      if enabled then
        self#push_pending_action();
      buffer#begin_user_action();
      self#disable();
      if f() then begin
        self#push (Func (f, inverse))
          ((buffer#get_iter `INSERT)#offset, (buffer#get_iter `SEL_BOUND)#offset);
      end;
      self#enable();
      buffer#end_user_action();

    method enable () =
      enabled <- true;
      GtkSignal.handler_unblock text_buffer sign_delete;
      GtkSignal.handler_unblock text_buffer sign_insert;

    method disable () =
      self#push_pending_action();
      enabled <- false;
      GtkSignal.handler_block text_buffer sign_delete;
      GtkSignal.handler_block text_buffer sign_insert;

    method is_enabled = enabled

    method can_undo = (not (Stack.is_empty undos))
                      || (match pending_action with Some _ when current_stack == undos -> true | _ -> false)

    method can_redo = (not (Stack.is_empty redos))
                      || (match pending_action with Some _ when current_stack == redos -> true | _ -> false)

    method redo () = self#revert redos
    method undo () = self#revert undos

    method length = Stack.length undos, Stack.length redos

    method stats =
      Stack.length undos, Obj.reachable_words (Obj.repr undos) * 8,
      Stack.length redos, Obj.reachable_words (Obj.repr redos) * 8

    method connect = new manager_signals ~undo ~redo ~can_undo_changed ~can_redo_changed
  end

and manager_signals ~undo ~redo ~can_undo_changed ~can_redo_changed =
  object
    inherit ml_signals [ undo#disconnect; redo#disconnect; can_undo_changed#disconnect; can_redo_changed#disconnect ]
    method can_undo_changed ~callback = can_undo_changed#connect ~after ~callback
    method can_redo_changed ~callback = can_redo_changed#connect ~after ~callback
    method undo ~callback = undo#connect ~after ~callback:(fun name -> callback ~name)
    method redo ~callback = redo#connect ~after ~callback:(fun name -> callback ~name)
  end













