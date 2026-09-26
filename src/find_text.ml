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
open Utils
open Convert

module Log = Common.Log.Make(struct let prefix = "FIND-TEXT" end)
let _ =
  Log.set_print_timestamp true;
  Log.set_verbosity `DEBUG

exception Buffer_changed of int * string * string
exception Skip_file
exception Found_step of int * int * int
exception No_current_regexp
exception Canceled

type direction = Backward | Forward

type path = Project_source | Specified of string | Only_open_files

type history_model = {
  model : GTree.list_store;
  column : string GTree.column;
}

type status = {
  mutable text_find        : string GUtil.variable;
  mutable text_repl        : string;
  mutable use_regexp       : bool;
  mutable case_sensitive   : bool;
  mutable match_whole_word : bool;
  mutable direction        : direction;
  mutable path             : path;
  mutable recursive        : bool;
  mutable pattern          : string option;
  mutable current_regexp   : Str.regexp option;
  mutable h_find           : history_model;
  mutable h_repl           : history_model;
  mutable h_path           : history_model;
  mutable h_pattern        : history_model;
  status_filename          : string;
}

type result_entry = {
  filename               : string;
  mutable lines          : result_line list
}

and result_line = {
  line                   : string;
  linenum                : int;
  bol                    : int;
  offsets                : (int * int) list;
  mutable marks          : (string * string) list
}

let atd_path_of_path p =
  match p with
  | Project_source -> `Project_source
  | Specified s -> `Specified s
  | Only_open_files -> `Only_open_files

let path_of_atd_path (p : Find_text_t.path_type) =
  match p with
  | `Project_source -> Project_source
  | `Specified s -> Specified s
  | `Only_open_files -> Only_open_files

let default_patterns = [ "*.{ml,mli,mll,mly,txt}" ]

let create_history_model () =
  let cols = new GTree.column_list in
  let column      = cols#add Gobject.Data.string in
  {model = GTree.list_store cols; column = column}

let populate_model data =
  (* Here, we rebuild the data model because clearing the pre-existing model
     using `history.model#clear()` turns out to be very slow, even
     for just a few dozen elements. *)
  let history = create_history_model () in
  List.iter begin fun x ->
    let row = history.model#append () in
    history.model#set ~row ~column:history.column x
  end data;
  history

(** status *)
let status =
  let status_filename =
    let old_name = App_config.ocamleditor_user_home // "find_in_path.xml" in
    let xml_name = App_config.ocamleditor_user_home // "find_text.xml" in
    if Sys.file_exists old_name then (try Sys.rename old_name xml_name with _ -> ());
    App_config.ocamleditor_user_home // "find_text.json"
  in {
    status_filename = status_filename;
    text_find       = new GUtil.variable "";
    text_repl       = "";
    use_regexp      = false;
    case_sensitive  = false;
    match_whole_word = false;
    direction       = Forward;
    path            = Project_source;
    recursive       = false;
    pattern         = Some "*.ml";
    current_regexp  = None;
    h_find          = create_history_model ();
    h_repl          = create_history_model ();
    h_path          = create_history_model ();
    h_pattern       = create_history_model ();
  }

let read_history history =
  let tmp = ref [] in
  history.model#foreach begin fun _ row ->
    tmp := (history.model#get ~row ~column:history.column) :: !tmp;
    false
  end;
  List.rev !tmp

let write_status () =
  let update_model prepend history =
    let hist = read_history history in
    let new_data =
      if prepend <> ""
      then prepend :: (hist |> List.filter ((<>) prepend))
      else hist
    in
    let new_data =
      if List.length new_data > Oe_config.find_replace_history_max_length then
        List.filteri (fun i _ -> i < Oe_config.find_replace_history_max_length) new_data
      else new_data
    in
    populate_model new_data
  in
  status.h_find <- update_model status.text_find#get status.h_find;
  status.h_repl <- update_model status.text_repl status.h_repl;
  let path_str = match status.path with Project_source -> "" | Specified x -> x | Only_open_files -> "" in
  status.h_path <- update_model path_str status.h_path;
  let pat_str = match status.pattern with None -> "" | Some x -> x in
  status.h_pattern <- update_model pat_str status.h_pattern;

  let atd_status = {
    Find_text_t.use_regexp = status.use_regexp;
    case_sensitive = status.case_sensitive;
    match_whole_word = status.match_whole_word;
    recursive = status.recursive;
    pattern_enabled = (status.pattern <> None);
    path = atd_path_of_path status.path;
    history_find = read_history status.h_find;
    history_repl = read_history status.h_repl;
    history_path = read_history status.h_path;
    history_pattern = read_history status.h_pattern;
  } in
  try
    let json_str = Find_text_j.string_of_find_text_status atd_status |> Yojson.Safe.prettify in
    Out_channel.with_open_bin status.status_filename (fun oc -> Out_channel.output_string oc json_str);
  with ex ->
    eprintf "Failed to write find_text status to %s: %s\n%!" status.status_filename (Printexc.to_string ex)

let read_status () =
  if Sys.file_exists status.status_filename then begin
    try
      let chan = open_in_bin status.status_filename in
      let content = really_input_string chan (in_channel_length chan) in
      close_in chan;
      let module J = Find_text_j in
      let atd_status = J.find_text_status_of_string content in
      status.use_regexp <- atd_status.J.use_regexp;
      status.case_sensitive <- atd_status.J.case_sensitive;
      status.match_whole_word <- atd_status.J.match_whole_word;
      status.recursive <- atd_status.J.recursive;
      status.pattern <- if atd_status.J.pattern_enabled then Some "" else None;
      status.path <- path_of_atd_path atd_status.J.path;

      status.h_find <- populate_model atd_status.J.history_find;
      status.h_repl <- populate_model atd_status.J.history_repl;
      status.h_path <- populate_model atd_status.J.history_path;
      status.h_pattern <- populate_model atd_status.J.history_pattern;
    with ex ->
      eprintf "Failed to read find_text status from %s: %s\n%!" status.status_filename (Printexc.to_string ex);
      if Sys.file_exists status.status_filename then (try Sys.remove status.status_filename with _ -> ())
  end

(** create_regexp *)
let create_regexp ~project
    ?(use_regexp=status.use_regexp)
    ?(case_sensitive=status.case_sensitive)
    ?(match_whole_word=status.match_whole_word)
    ~text () =
  match match_whole_word, use_regexp, case_sensitive with
  | false, true, true -> Str.regexp text
  | false, true, false -> Str.regexp_case_fold text
  | false, false, true -> Str.regexp_string text
  | false, false, false -> Str.regexp_string_case_fold text
  | true, true, true -> Str.regexp (sprintf "\\b%s\\b" text)
  | true, true, false -> Str.regexp_case_fold (sprintf "\\b%s\\b" text)
  | true, false, true -> Str.regexp (sprintf "\\b%s\\b" (Str.quote text))
  | true, false, false -> Str.regexp_case_fold (sprintf "\\b%s\\b" (Str.quote text))

(** update_status *)
let update_status
    ~project
    ~text_find
    ?(text_repl=status.text_repl)
    ?(use_regexp=status.use_regexp)
    ?(case_sensitive=status.case_sensitive)
    ?(match_whole_word=status.match_whole_word)
    ?(direction=status.direction)
    ?(path=status.path)
    ?(recursive=status.recursive)
    ?(pattern=status.pattern) () =
  status.text_find#set text_find;
  status.text_repl <- text_repl;
  status.use_regexp <- use_regexp;
  status.case_sensitive <- case_sensitive;
  status.match_whole_word <- match_whole_word;
  status.recursive <- recursive;
  status.direction <- direction;
  status.pattern <- pattern;
  status.path <- path;
  let regexp = create_regexp
      ~project
      ~use_regexp:status.use_regexp
      ~case_sensitive:status.case_sensitive
      ~text:status.text_find#get ()
  in
  status.current_regexp <- Some regexp;
  write_status()

(** clear_history *)
let clear_history () =
  status.h_find.model#clear();
  status.h_repl.model#clear();
  status.h_pattern.model#clear();
  write_status()

let _ = begin
  Incremental_search.set_last_incremental := begin fun text regexp ->
    status.current_regexp <- Some regexp;
    status.text_find#set text;
  end;
  read_status ()
end
