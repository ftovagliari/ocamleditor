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


open Text_util

let smart_home ~view state =
  let buffer : GText.buffer = view#buffer in
  let iter = buffer#get_iter `INSERT in
  if iter#chars_in_line > 1 then begin
    let offset0 = iter#set_line_offset 0 in
    let where =
      if view#options#smart_home then begin
        let initial_pos = iter#copy in
        let where =
          if Glib.Unichar.isspace offset0#char then begin
            let first_non_blank = offset0#forward_find_char not_blank in
            if first_non_blank#line <> iter#line then begin
              if iter#line_index > 0 then offset0 else iter#forward_to_line_end
            end else first_non_blank
          end else offset0
        in
        if initial_pos#compare where = 0 then offset0 else where
      end else begin
        if iter#starts_line then iter#forward_find_char not_blank
        else offset0
      end
    in
    if state = [`SHIFT] then buffer#move_mark `INSERT ~where
    else buffer#place_cursor ~where;
    (view :> GText.view)#scroll_to_iter ~within_margin:0.1 where |> ignore;
    true
  end else false;;

let smart_end ~view state =
  let buffer : GText.buffer = view#buffer in
  let backward_non_blank iter =
    let rec f it =
      let stop = it#backward_char in
      if [" "; "\t"; "\r"] |> List.for_all ((<>) (it#get_text ~stop)) then it
      else f stop
    in
    f iter
  in
  let initial_pos = buffer#get_iter `INSERT in
  let where =
    if view#options#smart_end then begin
      let where =
        (if initial_pos#ends_line then initial_pos else initial_pos#forward_to_line_end)
        |> backward_non_blank
      in
      if initial_pos#compare where = 0 && not initial_pos#ends_line
      then where#forward_to_line_end
      else where
    end else begin
      let where =
        if initial_pos#ends_line then initial_pos |> backward_non_blank
        else initial_pos#forward_to_line_end
      in
      if initial_pos#compare where = 0 then where |> backward_non_blank else where
    end
  in
  if state = [`SHIFT] then buffer#move_mark `INSERT ~where
  else buffer#place_cursor ~where;
  view#scroll_mark_onscreen `INSERT;
  true
;;
