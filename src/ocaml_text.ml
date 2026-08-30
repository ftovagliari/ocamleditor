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

open Parser
open Utils

let shells = ref []

let create_shell = ref ((fun () -> failwith "Ocaml_text.create_shell") : unit -> unit)
let show_messages = ref ((fun () -> failwith "Ocaml_text.show_messages") : unit -> unit)

(** Buffer *)
class buffer ?project ?file ?(lexical_enabled=false) () =
  let is_ocaml_file filename =
    filename ^^^ ".ml" || filename ^^^ ".mli" || filename ^^^ ".mll" || filename ^^^ ".mly"
  in
  object (self)
    inherit Text.buffer ?project ?file () as super
    val mutable lexical_enabled = (lexical_enabled || begin
        match file with
        | Some file when is_ocaml_file file#basename -> true
        | _ -> false
      end)
    val mutable lexical_tags = []
    val mutable shell : Shell_view.widget option = None
    val mutable select_word_state = []
    val mutable select_word_state_init = None
    val mutable last_autocomp_time = 0.0

    method filename = match file with Some fn -> fn#filename | _ -> ""

    method is_ocaml_file = is_ocaml_file
    method colorize ?start ?stop () = Lexical.tag ?start ?stop self#as_gtext_buffer

    method is_changed_after_last_autocomp = last_autocomp_time < self#as_text_buffer#last_edit_time
    method sync_autocomp_time () = last_autocomp_time <- self#as_text_buffer#last_edit_time

    method set_lexical_enabled x = lexical_enabled <- x
    method lexical_enabled = lexical_enabled

    method! indent ?decrease () =
      let old = lexical_enabled in
      self#set_lexical_enabled false;
      super#indent ?decrease ();
      self#set_lexical_enabled old;

    method trim_lines () =
      let nlines = self#line_count in
      let ins_line = (self#get_iter `INSERT)#line in
      for i = 0 to nlines - 1 do
        if i <> ins_line then begin
          let it = self#get_iter (`LINE i) in
          let line = self#get_line_at_iter it in
          try
            let _ = Str.search_forward (Utils.regexp "\\([ \t]+\\)\r?\n$") line 0 in
            let len = String.length (Str.matched_group 1 line) in
            let stop = it#forward_to_line_end in
            let start = stop#backward_chars len in
            self#delete ~start ~stop;
          with Not_found -> ();
        end
      done;

    method shell = match shell with None -> None
                                  | Some sh -> if not sh#alive then (shell <- None; shell) else shell

    method select_shell () =
      shell <- (match !shells with
          | sh :: _ -> Some sh
          | _ -> None)

    method send_to_shell () =
      let phrase = self#phrase_at_cursor () in
      match self#shell with
      | None ->
          self#select_shell ();
          if self#shell = None then begin
            !create_shell();
            self#select_shell ()
          end;
          self#send_to_shell ()
      | Some sh ->
          let phrase = String.trim phrase in
          if String.length phrase > 0 then begin
            let phrase =
              try
                Str.search_backward (Utils.regexp ";;") phrase (String.length phrase) |> ignore;
                phrase
              with Not_found -> phrase ^ ";;"
            in
            sh#send phrase;
            sh#send "\n";
          end

    method phrase_at_cursor () =
      let phrase = self#selection_text() in
      if String.length phrase > 0 then phrase else begin
        let text = self#get_text () in
        let buffer = Lexing.from_string text in
        let start = ref 0
        and block_start = ref []
        and pend = ref (-1)
        and after = ref false in
        while !pend = -1 do
          let token = Lexer.token buffer in
          let pos =
            if token = SEMISEMI then Lexing.lexeme_end buffer
            else Lexing.lexeme_start buffer
          in
          let bol = (pos = 0) || text.[pos-1] = '\n' in
          let it_pos = self#get_iter (`OFFSET pos) in
          let it_ins = self#get_iter `INSERT in
          if not !after && (it_pos#compare it_ins >= if bol then 1 else 0)
          then begin
            after := true;
            let anon, real = List.partition (fun x -> x = -1) !block_start in
            block_start := anon;
            if real <> [] then start := (match real with x :: _ -> x | _ -> assert false);
          end;
          match token with
          | CLASS | EXTERNAL | EXCEPTION | FUNCTOR
          | LET | MODULE | OPEN | TYPE | VAL | HASH when bol ->
              if !block_start = [] then
                if !after then pend := pos else start := pos
              else block_start := pos :: List.tl !block_start
          | SEMISEMI ->
              if !block_start = [] then
                if !after then pend := Lexing.lexeme_start buffer
                else start := pos
              else block_start := pos :: List.tl !block_start
          | BEGIN | OBJECT ->
              block_start := -1 :: !block_start
          | STRUCT | SIG ->
              block_start := Lexing.lexeme_end buffer :: !block_start
          | END ->
              if !block_start = [] then
                if !after then pend := pos else ()
              else block_start := List.tl !block_start
          | EOF ->
              pend := pos
          | _ -> ()
        done;
        let phrase = String.trim (String.sub text !start (!pend - !start)) in
        phrase
      end

    method select_ocaml_word ?pat () =
      match pat with
      | Some pat -> super#select_word ~pat ()
      | None ->
          if self#has_selection then begin
            let selection = self#selection_text () in
            let start, stop = self#selection_bounds in
            let start, _ = if start#compare stop > 0 then stop, start else start, stop in
            if String.contains selection '_' || String.contains selection '.' then begin
              let parts = Utils.split "[_.]" selection in
              let start = ref start in
              select_word_state <- List.map begin fun p ->
                  match !start#forward_search p with
                  | None -> assert false
                  | Some ((_, b) as bounds) ->
                      start := b;
                      bounds
                end parts;
              match select_word_state_init with
              | None -> ()
              | Some init ->
                  select_word_state_init <- None;
                  select_word_state <- List.filter (fun (_, b) -> init#compare b < 0) select_word_state;
            end;
            select_word_state_init <- None;
            match select_word_state with
            | (a, b) as hd :: tl ->
                self#select_range a b;
                select_word_state <- tl;
                hd
            | _ ->
                self#place_cursor ~where:start;
                (*let bounds = self#select_ocaml_word ~pat:Ocaml_word_bound.regexp () in*)
                let bounds = self#select_ocaml_word ~pat:Ocaml_word_bound.longid_sharp () in
                select_word_state_init <- None;
                bounds
          end else begin
            select_word_state_init <- Some (self#get_iter `INSERT);
            let bounds = super#select_word ~pat:Ocaml_word_bound.regexp () in
            select_word_state <- [];
            bounds
          end

    method get_lident_at_cursor () =
      let stop = self#get_iter `INSERT in
      let start = (stop#backward_find_char begin fun c ->
          not (Glib.Unichar.isalnum c) &&
          let s = Glib.Utf8.from_unichar c in not (List.mem s ["."; "_"; "'"])
        end)#forward_char in
      let lident = String.trim (self#get_text ~start ~stop ()) in
      let lident = Str.split_delim (Utils.regexp "\\.") lident in
      let lident =
        if match lident with x :: _ -> x = "" | _ -> false
        then List.tl lident else lident in
      lident

    method tag_table_lexical : (GText.tag option) list = lexical_tags

    initializer
      (* Lexical *)
      (*let ocamldoc_paragraph_enabled =
        match file with
          | Some file when file#name ^^ ".ml" -> Oe_config.ocamldoc_paragraph_bgcolor_enabled
          | _ -> false
        in*)
      self#init_tags ();
      let tag_table = new GText.tag_table self#tag_table in
      lexical_tags <- List.map (fun x -> Some x) (List.filter_map begin fun n ->
          match tag_table#lookup n with
          | Some t -> Some (new GText.tag t)
          | _ -> None
        end !Lexical.tags);
      (* Lexical coloring disabled for undo of indent *)
      let old_lexical = ref lexical_enabled in
      ignore (undo#connect#undo ~callback:begin fun ~name ->
          if name = "indent" then (old_lexical := self#lexical_enabled; self#set_lexical_enabled false;);
        end);
      ignore (undo#connect#after#undo ~callback:begin fun ~name ->
          if name = "indent" then (self#set_lexical_enabled !old_lexical);
        end);
      ignore (undo#connect#redo ~callback:begin fun ~name ->
          if name = "indent" then (old_lexical := self#lexical_enabled; self#set_lexical_enabled false;);
        end);
      ignore (undo#connect#after#redo ~callback:begin fun ~name ->
          if name = "indent" then (self#set_lexical_enabled !old_lexical);
        end);

    method as_text_buffer = (self :> Text.buffer)
  end

(** View *)
and view ?project ?buffer () =
  let buffer = match buffer with None -> new buffer ?project () | Some b -> b in
  (* Double-click selects OCaml identifiers; click on a selected range
     reduces the selection to part of the identifier. *)
  let is_two_button_press = ref false in
  object (self)
    inherit Text.view ?project ~buffer:buffer#as_text_buffer ()
    val mutable popup = None
    val mutable smart_click = true;
    val mutable select_enclosing_expr = None

    initializer
      select_enclosing_expr <- Some (new Enclosing_expr.manager ~ocaml_view:self);
      self#create_highlight_current_line_tag(); (* recreate current line tag after code folding highlight to draw it above *)
      self#event#connect#button_release ~callback:self#button_release_handler |> ignore;
      self#event#connect#button_press ~callback:self#button_press_handler |> ignore

    method obuffer = buffer

    method smart_click = smart_click
    method set_smart_click x = smart_click <- x

    method private button_press_handler ev =
      if GdkEvent.Button.button ev = 1 && smart_click then begin
        match GdkEvent.get_type ev with
        | `TWO_BUTTON_PRESS ->
            is_two_button_press := true;
            self#obuffer#select_word ~pat:Ocaml_word_bound.regexp () |> ignore;
            true (* true *)
        | `BUTTON_PRESS when buffer#has_selection ->
            let x = int_of_float (GdkEvent.Button.x ev) in
            let y = int_of_float (GdkEvent.Button.y ev) in
            let x, y = self#window_to_buffer_coords ~tag:`TEXT ~x ~y in
            let where = self#get_iter_at_location ~x ~y in
            let start, stop = buffer#selection_bounds in
            let start, stop = if start#compare stop > 0 then stop, start else start, stop in
            if where#in_range ~start ~stop then begin
              buffer#place_cursor ~where;
              let a, b = self#obuffer#select_word ~pat:Ocaml_word_bound.part () in
              if a#equal start && b#equal stop then begin
                buffer#place_cursor ~where;
                false
              end else true; (* true *)
            end else false;
        | _ -> false
      end else false

    method private button_release_handler ev =
      if GdkEvent.Button.button ev = 1 && smart_click then begin
        match GdkEvent.get_type ev with
        | `BUTTON_RELEASE when !is_two_button_press ->
            is_two_button_press := false;
            self#obuffer#select_word ~pat:Ocaml_word_bound.regexp () |> ignore;
            false;
        | _ -> false
      end else false

    method private comment_selection () =
      let bounds = "(\042", "\042)" in
      let tb = self#buffer in
      let mstart, mstop = self#get_selection_bounds true in
      let start = tb#get_iter_at_mark mstart in
      let stop = tb#get_iter_at_mark mstop in
      let s1 = tb#get_text ~start ~stop:(start#forward_chars 2)  () in
      let s2 = tb#get_text ~start:(stop#backward_chars 2) ~stop () in
      if s1 = (fst bounds) && s2 = (snd bounds) then begin
        tb#delete_interactive ~start:start ~stop:(start#forward_chars 2) () |> ignore;
        let stop = tb#get_iter_at_mark mstop in
        tb#delete_interactive ~start:(stop#backward_chars 2) ~stop () |> ignore;
      end else begin
        tb#insert_interactive ~iter:start (fst bounds) |> ignore;
        let stop = tb#get_iter_at_mark mstop in
        tb#insert_interactive ~iter:stop (snd bounds) |> ignore;
        tb#move_mark mstart ~where:(((tb#get_iter_at_mark mstart))#backward_chars 2);
      end;
      let start, stop = tb#get_iter_at_mark mstart, tb#get_iter_at_mark mstop in
      let start = start#backward_lines 2 in
      let stop = stop#forward_lines 2 in
      Lexical.tag ~start ~stop self#buffer

    method private get_selection_bounds trim =
      let in_indent (iter : GText.iter) =
        Text_util.is_blank iter#char &&
        let bol = iter#set_line_offset 0 in
        let it = iter#backward_find_char ~limit:bol Text_util.not_blank in
        it#equal bol
      in
      let tb = self#buffer in
      let mstart : GText.mark = `SEL_BOUND in
      let mstop : GText.mark = `INSERT in
      let start = tb#get_iter_at_mark mstart in
      let stop = tb#get_iter_at_mark mstop in
      let mstart, mstop = if start#compare stop > 0 then mstop, mstart else mstart, mstop in
      let start, stop = if start#compare stop > 0 then stop, start else start, stop in
      if trim then begin
        let start, stop =
          (if in_indent start then start#forward_find_char Text_util.not_blank else start),
          (if in_indent stop then (stop#backward_find_char Text_util.not_blank)#forward_char else stop)
        in
        tb#move_mark mstart ~where:start;
        tb#move_mark mstop ~where:stop;
      end;
      mstart, mstop

    method toggle_comment (select_nearest : bool) =
      if buffer#lexical_enabled then begin
        buffer#undo#begin_block ~name:"comment";
        let tb = self#buffer in
        let start = tb#get_iter_at_mark `SEL_BOUND in
        let stop = tb#get_iter `INSERT in
        let comp = start#compare stop in
        let pos = stop#offset in
        let selection = self#buffer#get_text ~start ~stop () in
        if selection = " " || selection = "  " then begin
          let boc, eoc =
            if selection = " " then "(\042", " \042)" else "(\042\042", "\042)"
          in
          let mstart, mstop = self#get_selection_bounds false in
          let iter = tb#get_iter_at_mark mstart in
          self#buffer#insert_interactive ~iter boc |> ignore;
          let iter = tb#get_iter_at_mark mstop in
          self#buffer#insert_interactive ~iter eoc |> ignore;
          let start = tb#get_iter_at_mark mstart in
          tb#place_cursor ~where:start#forward_char;
        end else begin
          if comp <> 0 then begin
            self#comment_selection ();
          end else if select_nearest then begin
            match Comments.nearest (Comments.scan (tb#get_text ())) pos with
            | None -> ()
            | Some (b, e) ->
                let i, s = if abs(pos - b) <= abs(pos - e) then b, e else e, b in
                tb#select_range (tb#get_iter_at_char s) (tb#get_iter_at_char i);
                self#scroll_lazy (tb#get_iter `INSERT);
          end else begin
            buffer#move_mark `SEL_BOUND ~where:((tb#get_iter `INSERT)#set_line_offset 0);
            buffer#move_mark `INSERT ~where:(tb#get_iter `SEL_BOUND)#forward_to_line_end;
            self#toggle_comment false;
          end
        end;
        buffer#undo#end_block ();
        self#misc#grab_focus();
      end

    method select_enclosing_expr ?(iter = buffer#get_iter_at_mark `INSERT) () =
      select_enclosing_expr |> Option.iter (fun sel -> sel#start ~iter)

  end
