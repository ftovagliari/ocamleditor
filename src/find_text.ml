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

(** status *)
let status =
  let status_filename =
    let old_name = App_config.ocamleditor_user_home // "find_in_path.xml" in
    let new_name = App_config.ocamleditor_user_home // "find_text.xml" in
    if Sys.file_exists old_name then (Sys.rename old_name new_name);
    new_name
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

(** write_status *)
let write_status () =
  let get_history prepend (model : GTree.list_store) column =
    let hist = ref [] in
    if prepend <> "" then (model#set ~row:(model#prepend ()) ~column prepend);
    model#foreach begin fun _ row ->
      let txt = model#get ~row ~column in
      if (List.length !hist <= Oe_config.find_replace_history_max_length)
      && not (List.mem txt !hist) then (hist := txt :: !hist);
      false
    end;
    let hist = List.rev !hist in
    model#clear();
    List.iter (fun h -> model#set ~row:(model#append()) ~column h) hist;
    hist
  in
  let xml =
    Xml.Element ("find_text", [
        "check_regexp", (string_of_bool status.use_regexp);
        "check_case", (string_of_bool status.case_sensitive);
        "check_match_whole_word", (string_of_bool status.match_whole_word);
        "check_rec", (string_of_bool status.recursive);
        "check_pat", (string_of_bool (status.pattern <> None));
        "radio_path", (string_of_bool (match status.path with Specified _ -> true | _ -> false));
        "radio_src", (string_of_bool (status.path = Project_source));
        "radio_only_open_files", (string_of_bool (status.path = Only_open_files));
      ], [
                   Xml.Element ("history_find", [], List.map (fun x -> Xml.Element ("element", [], [Xml.PCData x]))
                                  (get_history status.text_find#get status.h_find.model status.h_find.column));
                   Xml.Element ("history_repl", [], List.map (fun x -> Xml.Element ("element", [], [Xml.PCData x]))
                                  (get_history status.text_repl status.h_repl.model status.h_repl.column));
                   Xml.Element ("history_path", [], List.map (fun x -> Xml.Element ("element", [], [Xml.PCData x]))
                                  (get_history
                                     (match status.path with Project_source -> "" | Specified x -> x | Only_open_files -> "")
                                     status.h_path.model status.h_path.column));
                   Xml.Element ("history_pattern", [], List.map (fun x -> Xml.Element ("element", [], [Xml.PCData x]))
                                  (get_history
                                     (match status.pattern with None -> "" | Some x -> x)
                                     status.h_pattern.model status.h_pattern.column));
                 ])
  in
  let ochan = open_out status.status_filename in
  lazy (output_string ochan ("<!-- OCamlEditor XML Find-Text History -->\n" ^ (Xml.to_string_fmt xml)))
  /*finally*/ lazy (close_out ochan)

(** read_status *)
let read_status () =
  try
    let xml = Xml.parse_file status.status_filename in
    let attribs = Xml.attribs xml in
    status.use_regexp <- (bool_of_string (List.assoc "check_regexp" attribs));
    status.case_sensitive <- (bool_of_string (List.assoc "check_case" attribs));
    status.match_whole_word <-
      (match List.assoc_opt "check_match_whole_word" attribs with Some x -> bool_of_string x | _ -> false);
    status.recursive <- (bool_of_string (List.assoc "check_rec" attribs));
    status.pattern <- if bool_of_string (List.assoc "check_pat" attribs) then Some "" else None;
    status.path <-
      if bool_of_string (List.assoc "radio_path" attribs) then (Specified "")
      else if bool_of_string (List.assoc "radio_src" attribs) then Project_source
      else if bool_of_string (List.assoc "radio_only_open_files" attribs) then Only_open_files
      else assert false;
    let value xml =
      match Xml.children xml with
      | [] -> ""
      | x :: [] -> Xml.pcdata x
      | _ -> assert false
    in
    Xml.iter begin fun node ->
      match Xml.tag node with
      | "history_find" when (List.length (Xml.children node) > 0) -> List.iter begin fun x ->
          let row = status.h_find.model#append () in
          status.h_find.model#set ~row ~column:status.h_find.column (value x)
        end (Xml.children node);
      | "history_repl" when (List.length (Xml.children node) > 0) ->
          List.iter begin fun x ->
            let row = status.h_repl.model#append () in
            status.h_repl.model#set ~row ~column:status.h_repl.column (value x)
          end (Xml.children node);
      | "history_path" when (List.length (Xml.children node) > 0) -> List.iter begin fun x ->
          let row = status.h_path.model#append () in
          status.h_path.model#set ~row ~column:status.h_path.column (value x)
        end (Xml.children node);
      | "history_pattern" when (List.length (Xml.children node) > 0) -> List.iter begin fun x ->
          let row = status.h_pattern.model#append () in
          status.h_pattern.model#set ~row ~column:status.h_pattern.column (value x)
        end (Xml.children node);
      | _ -> ()
    end xml
  with
  | Xml_light_errors.File_not_found _ -> ()
  | Xml.Error _ -> begin
      if Sys.file_exists status.status_filename then (Sys.remove status.status_filename)
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
  write_status()

let _ = begin
  Incremental_search.set_last_incremental := begin fun text regexp ->
    status.current_regexp <- Some regexp;
    status.text_find#set text;
  end;
  read_status ()
end
























