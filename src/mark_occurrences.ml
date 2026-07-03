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


module Log = Common.Log.Make(struct let prefix = "MARK_OCCURRENCES" end)
let _ =
  Log.set_print_timestamp true;
  Log.set_verbosity `DEBUG

class manager ~view =
  let buffer = view#tbuffer in
  let [@inline] (=>) a b = not a || b in
  let match_whole_word_only = true in
  let filename =
    match buffer#file with
    | Some (file : Editor_file.file) -> file#filename
    | _ -> ""
  in
  let tag = buffer#create_tag ?name:(Some "mark_occurrences") [] in
  let clear_func (m1, m2) =
    let start = buffer#get_iter_at_mark m1 in
    let stop = buffer#get_iter_at_mark m2 in
    buffer#remove_tag tag ~start ~stop;
    buffer#delete_mark m1;
    buffer#delete_mark m2
  in
  object (self)
    val mark_set = new mark_set ()
    (*val tag = buffer#create_tag ?name:(Some "mark_occurrences") []*)
    val mutable word_marks : (GText.mark * GText.mark) list = []
    val mutable ref_marks : (GText.mark * GText.mark) list = []
    val mutable last_merlin_invoke_time = 0.0
    val mutable last_merlin_invoke_line = 0
    val mutable last_merlin_invoke_col = 0

    method words = word_marks
    method refs = ref_marks
    method tag : GText.tag = tag

    method private clear_words () =
      match word_marks with
      | [] -> false
      | _ ->
          List.iter clear_func word_marks;
          word_marks <- [];
          true

    method private clear_refs () =
      match ref_marks with
      | [] -> false
      | _ ->
          List.iter begin fun (m1, m2) ->
            buffer#delete_mark m1;
            buffer#delete_mark m2
          end ref_marks;
          ref_marks <- [];
          true

    method clear () =
      if self#clear_words() || self#clear_refs() then mark_set#call()

    method private get_word_at_iter ?iter () =
      buffer#select_word
        ?iter
        ?pat:(Some Ocaml_word_bound.regexp)
        ?select:(Some false)
        ?search:(Some true) ()

    method private get_text (start, stop) =
      buffer#get_text
        ?start:(Some start)
        ?stop:(Some stop)
        ?slice:(Some false)
        ?visible:(Some false) ()

    method mark_refs () =
      let iter = buffer#get_iter `INSERT in
      let line = iter#line + 1 in
      let col = iter#line_offset in
      if line <> last_merlin_invoke_line || col <> last_merlin_invoke_col then begin
        view#filter_outline_text (function `Ref _ -> false | _ -> true);
        GtkBase.Widget.queue_draw view#as_widget;
        last_merlin_invoke_time <- buffer#last_edit_time;
        let text = buffer#get_text ?start:None ?stop:None ?slice:None ?visible:None () in
        last_merlin_invoke_line <- line;
        last_merlin_invoke_col <- col;
        Merlin.occurrences ~identifier_at:(line, col) ~filename ~scope:`Buffer ~buffer:text ()
        |> Async.start_with_continuation begin function
        | Merlin.Ok ranges ->
            let open Merlin_j in
            if last_merlin_invoke_time = buffer#last_edit_time then
              Gmisclib.Idle.add begin fun () ->
                (*GtkThread.async begin fun () ->*)
                self#clear_refs() |> ignore;
                ranges
                |> List.fold_left begin fun acc range ->
                  (*Printf.printf "----> %s %b (%s)\n%!"
                    (match range.file with Some f -> f | _ -> "<No-file>")
                    (range.file = Some filename) filename;*)
                  let last_line = buffer#end_iter#line + 1 in
                  if 0 < range.start.line && range.start.line <= last_line &&
                     0 < range.stop.line && range.stop.line <= buffer#end_iter#line + 1
                  then begin
                    let start_line = buffer#get_iter (`LINE (range.start.line - 1)) in
                    if range.start.col < start_line#chars_in_line then
                      let stop_line = buffer#get_iter (`LINE (range.stop.line - 1)) in
                      if range.stop.col < stop_line#chars_in_line then
                        let start = buffer#get_iter (`LINECHAR (range.start.line - 1, range.start.col)) in
                        let stop = buffer#get_iter (`LINECHAR (range.stop.line - 1, range.stop.col)) in
                        let m1 = buffer#create_mark ?name:None ?left_gravity:None start in
                        let m2 = buffer#create_mark ?name:None ?left_gravity:None stop in
                        ref_marks <- (`MARK m1, `MARK m2) :: ref_marks;
                        `Ref (m1, m2) :: acc
                      else acc
                    else acc
                  end else acc
                end []
                |> view#add_outline_text;
                if ref_marks <> [] then mark_set#call();
              end
        | Merlin.Failure _ | Merlin.Error _ -> ()
        end
      end

    method mark_words () =
      self#clear_words() |> ignore;
      match view#options#mark_occurrences with
      | true, under_cursor, _ ->
          let text = buffer#selection_text () in
          let text =
            if text = "" then self#get_word_at_iter () |> self#get_text
            else text
          in
          let text = text |> String.trim in
          if String.length text > 1 && not (String.contains text '\n') && not (String.contains text '\r') then begin
            let start = buffer#start_iter in
            let stop = buffer#end_iter in
            let iter = ref start in
            while !iter#compare stop < 0 do
              match !iter#forward_search ?flags:None ?limit:(Some stop) text with
              | Some (a, b) ->
                  let found =
                    match_whole_word_only => (self#get_word_at_iter ~iter:a () |> self#get_text |> (=) text)
                  in
                  if found then begin
                    buffer#apply_tag tag ~start:a ~stop:b;
                    let m1 = buffer#create_mark ?name:None ?left_gravity:None a in
                    let m2 = buffer#create_mark ?name:None ?left_gravity:None b in
                    word_marks <- (`MARK m1, `MARK m2) :: word_marks;
                  end;
                  iter := b;
              | _ -> iter := stop
            done;
            if word_marks <> [] then mark_set#call()
          end
      | _ -> ()

    method mark () =
      self#mark_refs();
      self#mark_words()

    method connect = new signals ~mark_set
  end

and signals ~mark_set = object
  inherit GUtil.ml_signals [mark_set#disconnect]
  method mark_set = mark_set#connect ~after
end

and mark_set () = object inherit [unit] GUtil.signal () end

