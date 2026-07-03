open Printf
module ColorOps = Color
open Preferences

let re_indent = Str.regexp "^[ ]+"
let re_multi_space = Str.regexp " [ ]+"
let re_newlines = Str.regexp "[\n\r]+"

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
  let pending_newline = ref false in
  object (self)
    method code_font_family = code_font_family
    method code_font_size = code_font_size

    method convert info =
      let open Odoc_info in
      let odoc = info_of_string info in
      match odoc.i_desc with
      | None -> ""
      | Some idesc ->
          idesc
          |> List.map self#convert_element
          |> String.concat ""

    method private convert_element element =
      let open Odoc_info in
      let markup_of_elements text = text |> List.map self#convert_element |> String.concat "" in
      begin
        match element with
        | Odoc_info.Raw text ->
            let spc = if !pending_newline then "" else " " in
            pending_newline := false;
            text
            |> Str.global_replace re_newlines " "
            |> Str.global_replace re_multi_space " "
            |> Str.global_replace re_indent spc
            |> Glib.Markup.escape_text
        | Code code ->
            code
            |> Glib.Markup.escape_text
            |> sprintf "<span color='%s' font='%s %s'>%s</span>" code_color code_font_family code_font_size
        | CodePre code ->
            code
            |> Glib.Markup.escape_text
            |> sprintf "\n<span color='%s' font='%s %s'>%s</span>\n" code_color code_font_family code_font_size
        | Verbatim text ->
            text
            |> Glib.Markup.escape_text
            |> sprintf "<tt>%s</tt>"
        | Bold text ->
            sprintf "<b>%s</b>" (markup_of_elements text)
        | Italic text ->
            sprintf "<i>%s</i>" (markup_of_elements text)
        | Emphasize text ->
            sprintf "<i>%s</i>" (markup_of_elements text)
        | List text ->
            text
            |> List.map (fun t -> t |> List.map self#convert_element |> String.concat "")
            |> String.concat "\n\u{2022}  "
            |> sprintf "\n\u{2022}  %s\n"
        | Enum text ->
            "\n" ^
            (text
             |> List.map markup_of_elements
             |> List.mapi (fun i -> Printf.sprintf "%3d)  %s" (i+1))
             |> String.concat "\n")
        | Newline -> pending_newline := true; "\n" (*"<span color='red'>\u{ebea}</span>"*)
        | Block text ->
            sprintf "<span color='red'>Block %s</span>" (markup_of_elements text)
        | Link (link, text) ->
            sprintf "%s (<tt>%s</tt>)" (markup_of_elements text) link
        | Ref (name, _kind, text) ->
            sprintf "<span color='%s' font='%s %s'>%s</span>%s" code_color code_font_family code_font_size name
              (match text with None -> "" | Some text -> (markup_of_elements text))
        | Module_list _ -> ""
        | Index_list -> ""
        | Target (a, b) -> sprintf "<span color='red'>Target %S %S</span>" a b
        | Center text -> sprintf "<span color='red'>Center %S</span>" (markup_of_elements text)
        | Left text -> sprintf "<span color='red'>Left %S</span>" (markup_of_elements text)
        | Right text -> sprintf "<span color='red'>Right %S</span>" (markup_of_elements text)
        | Title (level, _, text) -> sprintf "<span weight='bold'>%s</span>" (markup_of_elements text)
        | Latex text -> sprintf "<tt>%s</tt>" text
        | Superscript text -> sprintf "<sup>%s</sup>" (markup_of_elements text)
        | Subscript text -> sprintf "<sub>%s</sub>" (markup_of_elements text)
        | Custom (a, text) ->
            sprintf "<span color='red'>Custom %S %s</span>" a (markup_of_elements text)
      end
  end
