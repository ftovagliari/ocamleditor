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

let get_switch_view_sensitive project page =
  (project.Prj.in_source_path page#get_filename) <> None

let update_labels
    ~code_folding
    ~select_in_outline
    ~enable_code_folding
    ~collapse_enclosing
    ~unfold_all
    ~show_whitespace_chars
    ~signal_show_whitespace_chars
    ~toggle_word_wrap
    ~signal_toggle_wrod_wrap
    ~switch_viewer
    ~rev_history
    editor =
  let page = editor#get_page `ACTIVE in
  let has_current_page = page <> None in
  let is_ml =
    match page with
    | None -> false
    | Some page ->
        let name = page#get_filename in
        name ^^^ ".ml" || name ^^^ ".mli"
  in
  code_folding#misc#set_sensitive is_ml;
  select_in_outline#misc#set_sensitive is_ml;
  enable_code_folding#set_active Preferences.preferences#get.editor_code_folding_enabled;
  List.iter (fun x -> x#misc#set_sensitive enable_code_folding#active) [collapse_enclosing; unfold_all];
  show_whitespace_chars#misc#handler_block signal_show_whitespace_chars;
  show_whitespace_chars#set_active (editor#show_whitespace_chars);
  show_whitespace_chars#misc#handler_unblock signal_show_whitespace_chars;
  show_whitespace_chars#misc#set_sensitive has_current_page;
  toggle_word_wrap#misc#handler_block signal_toggle_wrod_wrap;
  toggle_word_wrap#set_active editor#word_wrap;
  toggle_word_wrap#misc#handler_unblock signal_toggle_wrod_wrap;
  toggle_word_wrap#misc#set_sensitive has_current_page;
  switch_viewer#misc#set_sensitive has_current_page;
  rev_history#misc#set_sensitive has_current_page;
  Option.iter (fun page ->
      switch_viewer#misc#set_sensitive (get_switch_view_sensitive editor#project page)) page;;

let toggle_code_folding ~enable_code_folding editor =
  Preferences.preferences#get.editor_code_folding_enabled <- enable_code_folding#active;
  Preferences.save()

