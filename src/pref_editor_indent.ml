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


open Pref_page

(** An excerpt from the [ocp-indent --help] output, relevant for setting the
    configuration string.

    I may revert to using the output of [ocp-indent --help] to stay up to date,
    but I need to figure how to remove the parts which are not relevant.
*)
let ocp_indent_help = {|A configuration definition is a list of bindings in the form
NAME=VALUE or of PRESET, separated by commas or newlines

Syntax: [PRESET,]VAR=VALUE[,VAR=VALUE...]
base=INT (default=2)
    Indentation used when none of the following options applies.
        let foo =
        ..bar
type=INT (default=2)
    Indentation for type definitions.
        type t =
        ..int
in=INT (default=0)
    Indentation after `let ... in', unless followed by another `let'.
        let foo = () in
        ..bar
with=INT (default=0)
    Indentation after `match ... with', `try ... with' or `function'.
        match foo with
        ..| _ -> bar
match_clause=INT (default=2)
    Indentation for clauses inside a pattern-match (after arrows).
        match foo with
        | _ ->
        ..bar
ppx_stritem_ext=INT (default=2)
    Indentation for items inside a [%%id ... ] extension node).
        [%% id.id
        ..let x = 3 ]
max_indent=<INT|none> (default=4)
    When nesting expressions on the same line, their indentations are
    stacked in some cases so that they remain correct if you close
    them one per line. However, this can lead to large indentations in
    complex code, so this parameter sets a maximum indentation. Note
    that it only affects indentation after function arrows and opening
    parens at the ends of lines.
        let f = g (h (i (fun x ->
        ....x)
          )
        )
strict_with=<always|never|auto> (default=never)
    If `never', match bars are indented, superseding `with', whenever
    `match with' doesn't start its line. If `auto', there are
    exceptions for constructs like `begin match with'. If `always',
    `with' is always strictly respected, and additionally applies to
    variant types definition, for consistency.
    Example with `strict_with=never,with=0':
        begin match foo with
        ..| _ -> bar
        end
strict_else=<always|never|auto> (default=always)
    If `always', indent after the `else' keyword normally, like after
    `then'. If `auto', indent after `else' unless in a few
    "unclosable" cases (`let .... in', `match', etc.). If `never', the
    `else' keyword won't indent when followed by a newline.
    Example with `strict_else=auto':
        if cond then
          foo
        else
        let x = bar in
        baz
strict_comments=BOOL (default=false)
    In-comment indentation is normally preserved, as long as it
    respects the left margin or the comments starts with a newline.
    Setting this to `true' forces alignment within comments. Lines
    starting with `*' are always aligned
align_ops=BOOL (default=true)
    Toggles preference of column-alignment over line indentation for
    most of the common operators and after mid-line opening
    parentheses.
    Example with `align_ops=true':
        let f x = x
                  + y

    Example with `align_ops=false':
        let f x = x
          + y
align_params=<always|never|auto> (default=auto)
    If `never', function parameters are indented one level from the
    line of the function. If `always', they are aligned from the
    column of the function. if `auto', alignment is chosen over
    indentation in a few cases, e.g. after match arrows
    Example with `align_params=never':
        match foo with
        | _ -> some_fun
          ..parameter

    Example with `align_params=always' or `auto':
        match foo with
        | _ -> some_fun
               ..parameter
Available presets are `normal', the default, `apprentice' which may
make some aspects of the syntax more obvious for beginners, and
`JaneStreet'.
|}

let ocp_indent_options_markup = "<b>ocp-indent --config</b> <i>(*Will be ignored if the project contains an .ocp-indent file*)</i>"

(** pref_editor_indent *)
class pref_editor_indent title ?packing () =
  let vbox        = GPack.vbox ~spacing ?packing () in
  let box         = GPack.hbox ~spacing:5 ~packing:vbox#pack () in
  let _           = GMisc.label ~text:"Tab key on empty lines:" ~xalign:0.0 ~packing:box#pack () in
  let combo_empty, _ = GEdit.combo_box_text ~active:0 ~strings:[
      "Indent according to formatting options"; "Indent to match preceding line";
    ] ~packing:box#add () in
  let box         = GPack.vbox ~spacing:2 ~packing:(vbox#pack ~expand:true) () in
  let _           = GMisc.label ~markup: ocp_indent_options_markup ~xalign:0.0 ~packing:box#pack () in
  let box         = GPack.vbox ~spacing:5 ~packing:(box#pack ~expand:true) () in
  let buffer      = GText.buffer () in
  let sw          = GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~shadow_type:`IN ~packing:(box#pack ~expand:true) () in
  let view        = GText.view ~buffer ~packing:sw#add () in
  let buffer_help = GText.buffer () in
  let sw_help     = GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~shadow_type:`IN ~packing:(box#pack ~expand:true) () in
  let view_help   = GText.view ~buffer:buffer_help ~packing:sw_help#add () in
  object
    inherit page title vbox

    initializer
      view#set_left_margin 2;
      view#set_right_margin 2;
      view_help#set_left_margin 2;
      view_help#set_right_margin 2;
      view_help#set_editable false;
      view#set_wrap_mode `WORD;
      view_help#set_wrap_mode `WORD;
      let pref = Preferences.preferences#get in
      view#misc#modify_font_by_name pref.editor_base_font;
      view_help#misc#modify_font_by_name pref.editor_base_font;
      view_help#set_cursor_visible false;
      try
        buffer_help#set_text ocp_indent_help;
      with Glib.Convert.Error _ -> ()

    method write pref =
      pref.editor_indent_empty_line <- (match combo_empty#active with
          | 0 -> true
          | _ -> false);
      pref.editor_indent_config <- buffer#get_text();

    method read pref =
      combo_empty#set_active (if pref.editor_indent_empty_line then 0 else 1);
      buffer#set_text pref.editor_indent_config;
  end
