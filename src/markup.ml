open Printf
module ColorOps = Color
open Preferences

let color_of_kind = function
  | "Value" -> Preferences.editor_tag_color "uident"
  | "Type" -> Preferences.editor_tag_color "uident"
  | "Module" -> Preferences.editor_tag_color "uident"
  | "Constructor" -> Preferences.editor_tag_color "uident"
  | "Variant" -> Preferences.editor_tag_color "uident"
  | "Label" -> Preferences.editor_tag_color "uident"
  | "Class" -> Preferences.editor_tag_color "structure"
  | "Method" -> Preferences.editor_tag_color "structure"
  | "ClassType" -> Preferences.editor_tag_color "lident"
  | "Signature" -> Preferences.editor_tag_color "lident"
  | "Exn" -> `NAME "red" |> GDraw.color
  | "#" -> Preferences.editor_tag_color "lident"
  | x -> Preferences.editor_tag_color "lident"

let icon_of_kind kind =
  let color = kind |> color_of_kind |> ColorOps.name_of_gdk in
  match kind with
  | "Value" -> sprintf "<span color='%s'></span>" color
  | "Type" -> sprintf "<span color='%s'>󰬛</span>" color
  | "Module" -> sprintf "<span color='%s'> </span>" color
  | "Constructor" -> sprintf "<span color='%s'>󰘵</span>" color
  | "Variant" -> sprintf "<span color='%s'>󰓼</span>" color
  | "Label" -> sprintf "<span color='%s'>󰌕</span>" color
  | "Class" -> sprintf "<span size='larger' color='%s'></span>" color
  | "ClassType" -> sprintf "<span size='larger' style='italic' color='%s'></span>" color
  | "Method" -> sprintf "<span color='%s'></span>" color
  | "Signature" -> sprintf "<span size='larger' color='%s'> </span>" color
  | "Exn" -> sprintf "<span style='italic' color='%s'>󱈸</span>" color
  | "#" -> sprintf "<span color='%s'></span>" color
  | x -> sprintf "<span color='%s'>%s</span>" color x

let type_info ?(color=Oe_config.colored_types) text =
  if color then
    Lexical_markup.parse ~use_bold:false Preferences.preferences#get ?highlights:None text
    |> Print_type.replace_simbols_in_markup
  else Print_type.markup2 text

class odoc () =
  let code_color = ?? (preferences#get.Settings_j.editor_fg_color_popup) in
  let code_font_size =
    let font = preferences#get.Settings_j.editor_completion_font in
    Str.string_after font (String.rindex font ' ' + 1)
  in
  let code_font_name = Preferences.preferences#get.Settings_j.editor_base_font in
  let code_font_family =
    String.sub code_font_name 0 (Option.value (String.rindex_opt code_font_name ' ') ~default:(String.length code_font_name)) in
  let code_span text =
    text
    |> Glib.Markup.escape_text
    |> sprintf "<span color='%s' font='%s %s'>%s</span>" code_color code_font_family code_font_size
  in
  object (self)
    method code_font_family = code_font_family
    method code_font_size = code_font_size

    (** Render an ocamldoc comment body as Pango markup.

        [parse_comment] recovers from invalid syntax instead of raising, which is
        what we want for a completion popup: a malformed comment degrades rather
        than losing the whole tooltip. Its warnings are therefore ignored, and
        the location we pass is a placeholder, since the text reaches us already
        extracted and there is no file to point back to. *)
    method convert info =
      let location = { Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 } in
      Odoc_parser.parse_comment ~location ~text:info
      |> Odoc_parser.ast
      |> List.filter_map begin fun element ->
        match Odoc_parser.Loc.value element with
        (* @param, @return and friends were kept apart from the description by
           Odoc_info and never rendered here; keep leaving them out. *)
        | `Tag _ -> None
        | `Heading (_, _, text) ->
            Some (sprintf "<span weight='bold'>%s</span>" (self#inlines text))
        | #Odoc_parser.Ast.nestable_block_element as block ->
            Some (self#block (Odoc_parser.Loc.same element block))
      end
      |> String.concat "\n"

    method private blocks blocks =
      blocks |> List.map self#block |> String.concat "\n"

    method private block block =
      let span = Odoc_parser.Loc.location block in
      match Odoc_parser.Loc.value block with
      | `Paragraph text -> self#inlines text
      | `Code_block { Odoc_parser.Ast.content; _ } ->
          (* The AST holds the raw text between the delimiters; codeblock_content
             applies odoc's de-indentation rules. *)
          let code, _ = Odoc_parser.codeblock_content span (Odoc_parser.Loc.value content) in
          code
          |> Glib.Markup.escape_text
          |> sprintf "\n<span color='%s' font='%s %s'>%s</span>\n" code_color code_font_family code_font_size
      | `Verbatim text ->
          let text, _ = Odoc_parser.verbatim_content span text in
          text
          |> Glib.Markup.escape_text
          |> sprintf "<tt>%s</tt>"
      | `List (`Unordered, _, items) ->
          items
          |> List.map self#blocks
          |> String.concat "\n\u{2022}  "
          |> sprintf "\n\u{2022}  %s\n"
      | `List (`Ordered, _, items) ->
          "\n" ^
          (items
           |> List.map self#blocks
           |> List.mapi (fun i -> sprintf "%3d)  %s" (i + 1))
           |> String.concat "\n")
      | `Math_block text -> sprintf "<tt>%s</tt>" (Glib.Markup.escape_text text)
      (* Nothing sensible to show in a one-paragraph popup. *)
      | `Modules _ | `Table _ | `Media _ -> ""

    method private inlines text =
      text
      |> List.map (fun element -> self#inline (Odoc_parser.Loc.value element))
      |> String.concat ""

    method private inline = function
      | `Word word -> Glib.Markup.escape_text word
      | `Space _ -> " "
      | `Code_span code -> code_span code
      (* Markup aimed at another backend (HTML, LaTeX): show it as plain text. *)
      | `Raw_markup (_, text) -> Glib.Markup.escape_text text
      | `Math_span text -> sprintf "<tt>%s</tt>" (Glib.Markup.escape_text text)
      | `Styled (style, text) ->
          let text = self#inlines text in
          begin
            match style with
            | `Bold -> sprintf "<b>%s</b>" text
            | `Italic | `Emphasis -> sprintf "<i>%s</i>" text
            | `Superscript -> sprintf "<sup>%s</sup>" text
            | `Subscript -> sprintf "<sub>%s</sub>" text
          end
      | `Reference (_, target, text) ->
          sprintf "%s%s" (code_span (Odoc_parser.Loc.value target)) (self#inlines text)
      | `Link (url, text) ->
          sprintf "%s (<tt>%s</tt>)" (self#inlines text) (Glib.Markup.escape_text url)
  end
