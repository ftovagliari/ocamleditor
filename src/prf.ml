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

type t = {
  name : string;
  mutable calls : int;
  mutable time : float;
  mutable enabled : bool;
}

let start_time = Unix.gettimeofday()

let profiles : t list ref = ref []

let create name =
  let prf = {
    name = name;
    calls = 0;
    time = 0.0;
    enabled = true;
  } in
  profiles := prf :: !profiles;
  prf

(*
let prf_draw_markers                 = create true  "prf_draw_markers_view_expose"
let prf_error_indication_view_expose = create true  "prf_error_indication_view_expose"
let prf_paint_global_gutter          = create true  "prf_paint_global_gutter"
let prf_error_indication_tooltip     = create true  "prf_error_indication_tooltip"
let prf_error_indication_appy_tag    = create true  "prf_error_indication_appy_tag"
let prf_lexical_tag_insert           = create false "prf_lexical_tag_insert"
let prf_draw_gutter          = create true  "prf_draw_gutter"
let prf_none_other_markers           = create true  "prf_none_other_markers"
let prf_draw_white_spces             = create true  "prf_draw_white_spces"
*)

let draw_margins = create __FUNCTION__
let draw_margin_ln = create __FUNCTION__
let build_margin_ln = create __FUNCTION__
let draw_margin_markers = create __FUNCTION__
let draw_margin_diff = create __FUNCTION__
let expose = create __FUNCTION__

(*let prf_line_numbers          = create true  "prf_line_numbers"
  let prf_other_markers         = create true  "prf_other_markers"
  let prf_scan_folding_points   = create true  "prf_scan_folding_points"
  let prf_outline_select        = create true  "prf_outline_select"
  let innermost_enclosing_delim = create true  "innermost_enclosing_delim"
  let prf_delimiters_scan       = create true  "prf_delimiters_scan"
  let prf_autosave              = create true  "prf_autosave"
  let prf_compile_buffer        = create true  "prf_compile_buffer"
  let prf_draw_dot_leaders      = create true  "prf_draw_dot_leaders"
  let prf_location_history_add  = create true  "prf_location_history_add"
  let prf_colorize_within_nearest_tag_bounds  = create true  "prf_colorize_within_nearest_tag_bounds"
*)

let register prf f x =
  if not prf.enabled then (f x) else
    let finally time =
      prf.calls <- prf.calls + 1;
      prf.time <- prf.time +. (Unix.gettimeofday() -. time);
    in
    let time = Unix.gettimeofday() in
    let result = try f x with e -> begin
        finally time;
        raise e
      end in
    finally time;
    result

let print_report () =
  if !profiles <> [] then
    let total = (Unix.gettimeofday()) -. start_time in
    printf " %-50s : %7s  %7s  %7s  %7s  %9s\n%!" "" "Calls" "Avg" "Tot" "" "calls/min";
    printf "\
----------------------------------------------------------------------------------------------------\n%!";
    let perc = ref 0.0 in
    !profiles
    |> List.sort (fun a b -> Float.compare b.time a.time)
    |> List.iter begin fun prf ->
      if prf.enabled then begin
        let pc = prf.time /. total *. 100. in
        perc := !perc +. pc;
        printf " %-50s : %7d  %7.3f  %7.2f  %6.2f%%  %9.2f\n%!"
          (Utils.rpad (prf.name ^ " ") '.' 50)
          prf.calls
          (prf.time /. (float prf.calls))
          prf.time
          pc
          (((float prf.calls) /. (total *. 60.)) *. 1000.)
      end
    end;
    printf "\
----------------------------------------------------------------------------------------------------\n%!";
    printf " %-50s   %7d  %7.3f  %7.2f  %6.2f%%\n%!"
      (Utils.rpad "" ' ' 50) 0 0.0 total !perc
