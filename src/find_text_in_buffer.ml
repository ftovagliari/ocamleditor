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

(** find_forward *)
let find_forward ?start ?stop ?(all=true) ~(buffer : GText.buffer) ~regexp ~canceled () =
  let start_iter, stop_iter =
    match start, stop with
    | Some i1, Some i2 -> i1, i2
    | None, None -> buffer#start_iter, buffer#end_iter
    | Some start, None -> start, buffer#end_iter
    | None, Some stop -> buffer#start_iter, stop
  in
  let iter = ref start_iter#copy in
  let linenum = ref 0 in
  let bol = ref 0 in
  let lines_involved = ref [] in
  begin
    try
      while true do
        let line =
          if !iter#ends_line then "" else begin
            let line_end = !iter#forward_to_line_end in
            let stop_text = if line_end#compare stop_iter <= 0 then line_end else stop_iter in
            buffer#get_text ~start:!iter ~stop:stop_text ()
          end
        in
        linenum := (!iter#line + 1);
        bol := (!iter#set_line_index 0)#offset;
        let offsets = ref [] in
        let pos = ref 0 in
        begin
          try
            while true do
              if canceled() then (raise Find_text.Canceled);
              if Str.search_forward regexp line !pos >= 0 then begin
                let start, stop = Str.group_beginning 0, Str.group_end 0 in
                let start_offset = Convert.offset_from_pos line ~pos:start in
                let stop_offset = Convert.offset_from_pos line ~pos:stop in
                offsets := (start_offset, stop_offset) :: !offsets;
                if not all then (raise (Find_text.Found_step (!linenum - 1,
                                                              (!iter#line_offset + start_offset),
                                                              (!iter#line_offset + stop_offset))));
                pos := stop;
              end
            done
          with Not_found -> ()
        end;
        if List.length !offsets > 0 then begin
          lines_involved :=
            {Find_text.line = line; linenum = !linenum; bol = !bol; offsets = (List.rev !offsets); marks = []} ::
            !lines_involved;
        end;
        iter := !iter#forward_line;
        if !iter#compare stop_iter >= 0 then (raise Exit);
      done;
    with Exit -> ()
  end;
  if List.length !lines_involved > 0 then begin
    Some (List.rev !lines_involved)
  end else None

(** find_backward *)
let find_backward ~(start : GText.iter) ~(buffer : GText.buffer) ~regexp ~canceled () =
  let iter = ref start#copy in
  let linenum = ref 0 in
  let bol = ref 0 in
  let lines_involved = ref [] in
  begin
    try
      while true do
        let line =
          let start_line = !iter#set_line_offset 0 in
          buffer#get_text ~start:start_line ~stop:!iter ()
        in
        begin
          linenum := !iter#line + 1;
          bol := (!iter#set_line_index 0)#offset;
          let offsets = ref [] in
          let pos = ref (String.length line) in
          begin
            try
              while true do
                if canceled() then (raise Find_text.Canceled);
                if Str.search_backward regexp line !pos >= 0 then begin
                  let start, stop = Str.group_beginning 0, Str.group_end 0 in
                  let start_offset = Convert.offset_from_pos line ~pos:start in
                  let stop_offset = Convert.offset_from_pos line ~pos:stop in
                  offsets := (start_offset, stop_offset) :: !offsets;
                  raise (Find_text.Found_step (!linenum - 1, start_offset, stop_offset));
                  (*pos := start;*)
                end;
              done;
            with Not_found -> ()
          end;
          if List.length !offsets > 0 then begin
            lines_involved :=
              {Find_text.line = line; linenum = !linenum; bol = !bol; offsets = (List.rev !offsets); marks = []} ::
              !lines_involved;
          end;
          iter := !iter#backward_line;
          if !iter#compare buffer#start_iter = 0 then (raise Exit);
          if not !iter#ends_line || !iter#line_offset > 0 then (iter := !iter#forward_to_line_end);
        end;
      done
    with Exit -> ()
  end;
  if List.length !lines_involved > 0 then begin
    Some (List.rev !lines_involved)
  end else None

(** find *)
let find direction ~(view : Text.view) ~canceled =
  match Find_text.status.Find_text.current_regexp with
  | None -> raise Find_text.No_current_regexp
  | Some regexp -> begin
      let buffer = view#buffer in
      let start =
        let b1, b2 = buffer#selection_bounds in
        match direction with Find_text.Forward -> b2 | Find_text.Backward -> b1
      in
      try
        begin
          let res =
            match direction with
            | Find_text.Forward -> find_forward ~start ~all:false ~buffer ~regexp ~canceled ()
            | Find_text.Backward -> find_backward ~start ~buffer ~regexp ~canceled ()
          in
          match res with None -> begin
              let message = sprintf "String \xC2\xAB%s\xC2\xBB not found." Find_text.status.Find_text.text_find#get in
              Dialog.info ~title:"Search Failed" ~message view
            end | _ -> assert false
        end
      with Find_text.Found_step (line, o1, o2) ->
        begin
          begin
            match direction with
            | Find_text.Forward ->
                buffer#select_range
                  (buffer#get_iter (`LINECHAR (line, o2)))
                  (buffer#get_iter (`LINECHAR (line, o1)));
            | Find_text.Backward ->
                buffer#select_range
                  (buffer#get_iter (`LINECHAR (line, o1)))
                  (buffer#get_iter (`LINECHAR (line, o2)));
          end;
          view#scroll_aligned (view#buffer#get_iter `INSERT);
        end
    end
