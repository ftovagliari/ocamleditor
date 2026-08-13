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

exception Buffer_changed of int * string * string
exception Skip_file
exception Found_step of int * int * int
exception No_current_regexp
exception Canceled

type direction = Backward | Forward

type path = Project_source | Specified of string | Only_open_files

type history_model = {
  model                  : GTree.list_store;
  column                 : string GTree.column;
}

type status = {
  mutable text_find      : string GUtil.variable;
  mutable text_repl      : string;
  mutable use_regexp     : bool;
  mutable case_sensitive : bool;
  mutable match_whole_word : bool;
  mutable direction      : direction;
  mutable path           : path;
  mutable recursive      : bool;
  mutable pattern        : string option;
  mutable current_regexp : Str.regexp option;
  mutable hist_find_list : string list;
  mutable hist_repl_list : string list;
  mutable hist_path_list : string list;
  mutable hist_pattern_list : string list;
  h_find                 : history_model;
  h_repl                 : history_model;
  h_path                 : history_model;
  h_pattern              : history_model;
  status_filename        : string;
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
    hist_find_list    = [];
    hist_repl_list    = [];
    hist_path_list    = [];
    hist_pattern_list = [];
    h_find          =
      (let cols = new GTree.column_list in
       let column      = cols#add Gobject.Data.string in
       {model = GTree.list_store cols; column = column});
    h_repl          =
      (let cols = new GTree.column_list in
       let column      = cols#add Gobject.Data.string in
       {model = GTree.list_store cols; column = column});
    h_path          =
      (let cols = new GTree.column_list in
       let column      = cols#add Gobject.Data.string in
       {model = GTree.list_store cols; column = column});
    h_pattern       =
      (let cols = new GTree.column_list in
       let column      = cols#add Gobject.Data.string in
       {model = GTree.list_store cols; column = column});
  }

let write_status () =
  let update_list prepend current_list (model : GTree.list_store) column =
    (* 1. Aggiorna la lista OCaml pura *)
    let filtered = if prepend <> "" then List.filter ((<>) prepend) current_list else current_list in
    let updated = if prepend <> "" then prepend :: filtered else filtered in
    let final_list =
      if List.length updated > Oe_config.find_replace_history_max_length then
        List.filteri (fun i _ -> i < Oe_config.find_replace_history_max_length) updated
      else
        updated
    in
    (* 2. Sincronizza il modello GTK per la GUI *)
    model#clear ();
    List.iter begin fun h ->
      let row = model#append () in
      model#set ~row ~column h
    end final_list;

    final_list
  in

  (* Aggiorna e salva usando le liste OCaml *)
  status.hist_find_list <- update_list status.text_find#get status.hist_find_list status.h_find.model status.h_find.column;
  status.hist_repl_list <- update_list status.text_repl status.hist_repl_list status.h_repl.model status.h_repl.column;

  let path_str = match status.path with Project_source -> "" | Specified x -> x | Only_open_files -> "" in
  status.hist_path_list <- update_list path_str status.hist_path_list status.h_path.model status.h_path.column;

  let pat_str = match status.pattern with None -> "" | Some x -> x in
  status.hist_pattern_list <- update_list pat_str status.hist_pattern_list status.h_pattern.model status.h_pattern.column;

  let atd_status = {
    Find_text_t.use_regexp = status.use_regexp;
    case_sensitive = status.case_sensitive;
    match_whole_word = status.match_whole_word;
    recursive = status.recursive;
    pattern_enabled = (status.pattern <> None);
    path = atd_path_of_path status.path;
    history_find = List.map to_utf8 status.hist_find_list;
    history_repl = List.map to_utf8 status.hist_repl_list;
    history_path = List.map to_utf8 status.hist_path_list;
    history_pattern = List.map to_utf8 status.hist_pattern_list;
  } in
  try
    let json_str = Find_text_j.string_of_find_text_status atd_status |> Yojson.Safe.prettify in
    let ochan = open_out status.status_filename in
    Fun.protect
      ~finally:(fun () -> close_out ochan)
      (fun () -> output_string ochan json_str)
  with ex ->
    eprintf "Failed to write find_text status to %s: %s\n%!" status.status_filename (Printexc.to_string ex)

let read_status () =
  if Sys.file_exists status.status_filename then begin
    try
      let chan = open_in_bin status.status_filename in
      let content = really_input_string chan (in_channel_length chan) in
      close_in chan;
      let atd_status = Find_text_j.find_text_status_of_string content in
      status.use_regexp <- atd_status.use_regexp;
      status.case_sensitive <- atd_status.case_sensitive;
      status.match_whole_word <- atd_status.match_whole_word;
      status.recursive <- atd_status.recursive;
      status.pattern <- if atd_status.pattern_enabled then Some "" else None;
      status.path <- path_of_atd_path atd_status.path;

      (* Salva nelle liste OCaml e popola GTK *)
      status.hist_find_list <- atd_status.history_find;
      status.hist_repl_list <- atd_status.history_repl;
      status.hist_path_list <- atd_status.history_path;
      status.hist_pattern_list <- atd_status.history_pattern;

      List.iter begin fun x ->
        let row = status.h_find.model#append () in
        status.h_find.model#set ~row ~column:status.h_find.column x
      end status.hist_find_list;
      List.iter begin fun x ->
        let row = status.h_repl.model#append () in
        status.h_repl.model#set ~row ~column:status.h_repl.column x
      end status.hist_repl_list;
      List.iter begin fun x ->
        let row = status.h_path.model#append () in
        status.h_path.model#set ~row ~column:status.h_path.column x
      end status.hist_path_list;
      List.iter begin fun x ->
        let row = status.h_pattern.model#append () in
        status.h_pattern.model#set ~row ~column:status.h_pattern.column x
      end status.hist_pattern_list;
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
  status.hist_find_list <- [];
  status.hist_repl_list <- [];
  write_status()

let _ = begin
  Incremental_search.set_last_incremental := begin fun text regexp ->
    status.current_regexp <- Some regexp;
    status.text_find#set text;
  end;
  read_status ()
end
























