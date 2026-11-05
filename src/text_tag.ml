module ColorOps = Color
open Preferences
open Settings_j

let table = GText.tag_table ()

module Name = struct
  let current_line_highlight = "current_line_highlight"
  let old_current_line_highlight = "highlight_current_line"
  let ocamldoc_paragraph = "ocamldoc_paragraph"
  let ocamldoc = "ocamldoc"
end

let create name =
  let tag = GText.tag ~name () in
  table#add tag#as_tag;
  tag

let current_line_highlight = create Name.current_line_highlight
let matching_delim = create "matching_delim"
let warning_unused = create Oe_config.warning_unused_tag_name
let warning = create "tag_warning"
let error = create "tag_error"
let mark_occurrences = create "mark_occurrences"
let quick_info = create "quick_info"
let fold_highlight = create "fold_highlight"
let fold_invisible = create "fold_invisible"
let diff_add = create "diff_add"
let diff_delete = create "diff_delete"
let ocamldoc = create Name.ocamldoc
let ocamldoc_paragraph = create Name.ocamldoc_paragraph

let lexical : GText.tag list ref = ref []

let to_gdk_color x = GDraw.color (`NAME ?? x)

let update_properties pref =
  let editor_tags = pref.editor_tags in
  let bg_color =
    let color =
      try Preferences.editor_tag_bg_color Name.current_line_highlight
      with Not_found -> Preferences.editor_tag_bg_color Name.old_current_line_highlight
    in
    ColorOps.name_of_gdk color
  in
  current_line_highlight#set_properties [ `PARAGRAPH_BACKGROUND bg_color; `PARAGRAPH_BACKGROUND_SET true ];

  matching_delim#set_properties [
    `BACKGROUND_GDK (Preferences.editor_tag_color "highlight");
    `BACKGROUND_FULL_HEIGHT_SET true];

  warning_unused#set_properties
    [`FOREGROUND (?? Oe_config.warning_unused_color); `STYLE `ITALIC];

  warning#set_properties [ `LEFT_MARGIN 0 (* TODO view#left_margin *) ];

  error#set_properties [ `LEFT_MARGIN 0 (* TODO view#left_margin *) ];

  mark_occurrences#set_properties [
    `BACKGROUND (?? (pref.editor_mark_occurrences_bg_color));
    `BACKGROUND_FULL_HEIGHT_SET true];

  let bg_color = ?? (Preferences.preferences#get.Settings_t.editor_bg_color_popup) in
  quick_info#set_properties [ `BACKGROUND bg_color ];

  fold_highlight#set_properties
    [ `PARAGRAPH_BACKGROUND (?? Oe_config.code_folding_highlight_color) ];

  fold_invisible#set_properties [ `INVISIBLE true ];

  let color_add = (?? Oe_config.global_gutter_diff_color_add) in
  diff_add#set_properties [ `PARAGRAPH_BACKGROUND color_add ];

  let color_del = (?? Oe_config.global_gutter_diff_color_del) in
  diff_delete#set_properties [ `PARAGRAPH_BACKGROUND color_del ];

  let odoc = pref.editor_tags |> List.find (fun t -> t.name = Name.ocamldoc) in
  let odocprops = [
    Some (`FOREGROUND_GDK (to_gdk_color odoc.color));
    (if odoc.weight > 0 then Some (`WEIGHT (`CUSTOM odoc.weight)) else None);
    (match odoc.scale with 1.0 -> None | _ -> Some (`SCALE (`CUSTOM odoc.scale)));
    (match odoc.style with `NORMAL -> None | _ -> Some (`STYLE `ITALIC));
    (match odoc.underline with `NONE -> None | _ -> Some (`UNDERLINE `SINGLE));
    Some (`PIXELS_BELOW_LINES 1);
    Some (`PIXELS_ABOVE_LINES 1) ]
  in
  begin (* `PARAGRAPH_BACKGROUND color for regular ocamldoc comments. *)
    let bg1 = ?? (pref.editor_ocamldoc_paragraph_bgcolor_1) |> Option.map (fun c -> `PARAGRAPH_BACKGROUND c) in
    bg1 :: odocprops |> List.filter_map Fun.id |> ocamldoc_paragraph#set_properties;
  end;

  begin
    let bg2 = ?? (pref.editor_ocamldoc_paragraph_bgcolor_2) |> Option.map (fun c -> `PARAGRAPH_BACKGROUND c) in
    bg2 :: odocprops |> List.filter_map Fun.id |> ocamldoc#set_properties;
  end;

  pref.editor_tags
  |> List.filter (fun tp -> tp.name <> Name.ocamldoc)
  |> List.iter begin fun tagprop ->
    let properties = [
      Some (`FOREGROUND_GDK (to_gdk_color tagprop.color));
      (if tagprop.weight > 0 then Some (`WEIGHT (`CUSTOM tagprop.weight)) else None);
      (match tagprop.scale with 1.0 -> None | _ -> Some (`SCALE (`CUSTOM tagprop.scale)));
      (match tagprop.style with `NORMAL -> None | _ -> Some (`STYLE `ITALIC));
      (match tagprop.underline with `NONE -> None | _ -> Some (`UNDERLINE `SINGLE));
    ] |> List.filter_map Fun.id in
    let properties = if tagprop.bg_default then properties else (`BACKGROUND_GDK (to_gdk_color tagprop.bg_color)) :: properties in
    table#lookup tagprop.name |> Option.iter table#remove;
    let tag = create tagprop.name in
    tag#set_properties properties;
    lexical := tag :: !lexical;
  end;

  lexical := ocamldoc :: ocamldoc_paragraph :: !lexical;
;;

let init () =
  (* Give the unused warning tag the highest priority *)
  GtkText.Tag.set_priority warning_unused#as_tag (table#size - 1);
  update_properties Preferences.preferences#get;
  preferences#connect#changed ~callback:update_properties |> ignore;;

init ()
