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

open Utils
open Printf
open Settings_t

type dcolor = GDraw.color color

(** Configuration     Section =================================================== *)

let dot_attributes                       = " -Glabelloc=t -Gfontsize=26pt -Gfontname=\"Helvetica\" -Nfontsize=16pt -Nfontname=\"Helvetica\""
let ocp_indent_tab_key_enabled           = true
let autosave_enabled                     = true
let autosave_interval                    = 5_000 (* milliseconds *)
let autosave_keep_backup                 = 3. *. 24. *. 60. *. 60.  (* 3 days, in milliseconds *)
let ocamldoc_paragraph_border_enabled    = false
let ocamldoc_paragraph_bgcolor_enabled   = true
let fade_window_enabled                  = true (* Fade effect for popup windows *)
let matching_delim_border_color : dcolor = { light = `NAME "#ff0000"; dark = `NAME "#ff0000" }
let word_border_color : dcolor           = { light = `NAME "#ffffff"; dark = `NAME "#ffffff" }
let ref_border_color : dcolor            = { light = `NAME "#0090d0"; dark = `NAME "#00d090" }
let ref_bg_color : dcolor                = { light = `NAME "#0090d0"; dark = `NAME "#00d090" }
let error_popup_bg_color : dcolor        = { light = `NAME "#ffeef2"; dark = `NAME "#ffc6d4" }
let error_popup_border_color : dcolor    = { light = `NAME "#ff6a99"; dark = `NAME "#ff6a99" }
let error_underline_color : dcolor       = { light = `NAME "#ff0000"; dark = `NAME "#ff0000" }
let error_underline_shadow : dcolor      = { light = `NAME "#FFa0a0"; dark = `NAME "#FFa0a0" }
let error_underline_mode                 = (`CUSTOM : [`GTK | `CUSTOM])
let warning_popup_bg_color : dcolor      = { light = `NAME "#fff4e8"; dark = `NAME "#fecc8f" }
let warning_popup_border_color : dcolor  = { light = `NAME "#FFB33C"; dark = `NAME "darkorange" }
let warning_underline_color : dcolor     = { light = `NAME "#fff4e8"; dark = `NAME "#6e3d00" }
let warning_underline_shadow : dcolor    = { light = `NAME "#FFE36C"; dark = `NAME "#FFE36C" }
let warning_unused_color                 = { light = "#a0a0a0"; dark = "#707070" }
let warning_unused_tag_name              = "tag_warning_unused"
let warning_tootip_enabled               = false
let current_line_width                   = 2 (* Must be >= 1. Left margin is automatically increased by current_line_width *)
let current_line_style                   = (*`ON_OFF_DASH*) `SOLID
let current_line_join                    = (*`ROUND `MITER `BEVEL *) `BEVEL
let on_off_dashes                        = [3; 3]
let use_theme_colors_when_possible       = true

(** Whether the [collapse_to_definitions] command should collapse all definitions,
    including nested ones, or only affect top-level definitions. *)
let code_folding_deep_collapse           = true
let code_folding_highlight_color         = { light = "#e1eded"; dark = "#202530" }
let code_folding_tag_invisible_name      = "fold-invisible"
let code_folding_tag_highlight_name      = "fold-highlight"
let code_folding_expander_color          = `NAME "#ff0000"

let global_gutter_size                   = 30
let global_gutter_diff_color_add         = { light = "#60b060"; dark = "#13401e" }
let global_gutter_diff_color_del         = { light = "#ff6060"; dark = "#3e181d" }
let global_gutter_diff_color_change      = { light = "#0079FF"; dark = "#0079FF" }
let global_gutter_diff_style             = (`COLOR false : [`BW | `COLOR of bool])
let find_replace_history_max_length      = 75
let find_text_output_highlight           = `DEFAULT, `DEFAULT (*`NAME "#ffff7e", `NONE*) (* Background and foreground colors to highlight occurrences where the pattern matches.
                                                                                            (`NONE=do not change color; `DEFAULT=default color; `NAME=specific color)*)
let find_text_output_linenumber_fgcolor  = { light = "#000000"; dark = "#e1e1e1" }
let file_history_filename                = App_config.ocamleditor_user_home // "file_history"
let file_history_max_length              = 300
let project_history_filename             = App_config.ocamleditor_user_home // "project_history"
let project_history_max_length           = 15
let location_history_proximity           = 20 (* characters *)
let location_history_max_length          = 30 (* hint *)
let location_history_max_edit            = 5
let module_browser_max_results           = 150 (* Max. number of search results to display in the search_entry as you type *)
let module_browser_secondary_title_color = { light = "#877033"; dark = "#707070" }
let completion_name_table_enabled        = true
let layout_find_module_browser           = `VERTICAL
(* Path relative to the project home directory where to find custom templates. *)
let template_project_filename            = ".extensions" // "templates.cma"
let targetlist_alternating_row_colors    = None
let editor_tab_color_alt_active          = `NAME "#a7a2ae"
let editor_tab_color_alt_normal          = `NAME "#310080"
let colored_types                        = true
let unify_statusbars                     = false

(** End of Configuration Section ============================================ *)

let ocaml_codeset = "UTF-8"

let _ = Printexc.record_backtrace true

let _ = App_config.ensure_ocamleditor_user_home ()
let _ = Unix.putenv "TERM" ""
let getenv_ocamllib = try Some (Sys.getenv "OCAMLLIB") with Not_found -> None

(** Commands *)
let find_command name =
  let basename = name in
  let path = (!! Sys.executable_name) // basename in
  if Sys.file_exists path && not (Sys.is_directory path) then path
  else
    let path = (!! Sys.executable_name) // name // basename in
    if Sys.file_exists path then path
    else basename

let oebuild_command = App_config.get_oebuild_command ()

let get_version ?(ok_status=0) command =
  try
    let redirect_stderr = "1>/dev/null 2>/dev/null" in
    let cmd = sprintf "%s %s" command redirect_stderr in
    let status_not_found = [127] in
    let status = Sys.command cmd in
    (*Printf.printf "%s -- %d -- %b\n%!" cmd status (status = ok_status);*)
    if status = ok_status || not (List.mem status status_not_found) then
      let redirect_stderr = " 2>&1" in
      let cmd = sprintf "%s %s" command redirect_stderr in
      (match Shell.get_command_output cmd with ver :: _ -> Some ver | _ -> None)
    else failwith cmd
  with Failure _ -> None

let dot_version = get_version "dot -V"
let plink_version = get_version "plink -V" (* exits with status = 1 *)
let xdg_open_version = get_version "xdg-open --version"
let git_version = get_version ~ok_status:1 "git --version"


(** GTK config *)
(* Adjustments according to the GTK version *)
let gtk_major, gtk_minor, _ = GMain.Main.version
let current_line_border_adjust, dash_style, dash_style_offset =
  match gtk_major, gtk_minor with
  | 2, 14 -> 0, `ON_OFF_DASH, None
  | 2, 16 -> 0, `DOUBLE_DASH, None
  | 2, 20 -> 1, `ON_OFF_DASH, (Some 2)
  | 2, 22 -> 2, `DOUBLE_DASH, None
  | 2, 24 -> 1, `ON_OFF_DASH, (Some 2)
  | _     -> 1, `DOUBLE_DASH, None

(** Clear OCAMLLIB environment variable *)
let _ = Ocaml_config.putenv_ocamllib ()

(** geometry_memo_filename *)
let geometry_memo_filename = Filename.concat App_config.ocamleditor_user_home "geometry_memo.ocaml"
let _ =
  let old = Filename.concat App_config.ocamleditor_user_home "message_window_positions" in
  if Sys.file_exists old then Sys.remove old
