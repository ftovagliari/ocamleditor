module ColorOps = Color
open Preferences
open Settings_j

type group = {
  group : string;
  table : GText.tag_table;
  current_line_highlight : GText.tag;
  matching_delim : GText.tag;
  warning_unused : GText.tag;
  warning : GText.tag;
  error : GText.tag;
  mark_occurrences : GText.tag;
  quick_info : GText.tag;
  fold_highlight : GText.tag;
  fold_invisible : GText.tag;
  diff_add : GText.tag;
  diff_delete : GText.tag;
  ocamldoc : GText.tag;
  ocamldoc_paragraph : GText.tag;
  find_text_result_line : GText.tag;
  find_text_result_linenum : GText.tag;
  find_text_insensitive : GText.tag;
  find_text_result_hit : GText.tag;
  mutable lexical : (string * GText.tag) list;
}

module Name = struct
  let current_line_highlight = "current_line_highlight"
  let matching_delim = "matching_delim"
  let comment = "comment"
  let ocamldoc_paragraph = "ocamldoc_paragraph"
  let ocamldoc = "ocamldoc"
end

let create_tag table name =
  let tag = GText.tag ~name () in
  table#add tag#as_tag;
  tag

let create_group name =
  let table = GText.tag_table () in
  let create = create_tag table in
  {
    group = name;
    table;
    current_line_highlight = create Name.current_line_highlight;
    matching_delim = create Name.matching_delim;
    warning_unused = create Oe_config.warning_unused_tag_name;
    warning = create "tag_warning";
    error = create "tag_error";
    mark_occurrences = create "mark_occurrences";
    quick_info = create "quick_info";
    fold_highlight = create "fold_highlight";
    fold_invisible = create "fold_invisible";
    diff_add = create "diff_add";
    diff_delete = create "diff_delete";
    ocamldoc = create Name.ocamldoc;
    ocamldoc_paragraph = create Name.ocamldoc_paragraph;
    find_text_result_line = create "find_text_result_line";
    find_text_result_linenum = create "find_text_result_linenum";
    find_text_result_hit = create "find_text_result_hit";
    find_text_insensitive = create "find_text_insensitive";
    lexical = [];
  }

let to_gdk_color x = GDraw.color (`NAME ?? x)

let main : group = create_group "main"

let update_properties (g : group) pref =
  g.warning_unused#set_properties
    [`FOREGROUND (?? Oe_config.warning_unused_color); `STYLE `ITALIC];

  g.warning#set_properties [ `LEFT_MARGIN 0 (* TODO view#left_margin *) ];

  g.error#set_properties [ `LEFT_MARGIN 0 (* TODO view#left_margin *) ];

  g.mark_occurrences#set_properties [
    `BACKGROUND (?? (pref.editor_mark_occurrences_bg_color));
    `BACKGROUND_FULL_HEIGHT true];

  let bg_color = ?? (pref.Settings_t.editor_bg_color_popup) in
  g.quick_info#set_properties [ `BACKGROUND bg_color ];

  g.fold_highlight#set_properties
    [ `PARAGRAPH_BACKGROUND (?? Oe_config.code_folding_highlight_color) ];

  g.fold_invisible#set_properties [ `INVISIBLE true ];

  let color_add = (?? Oe_config.diff_color_add) in
  g.diff_add#set_properties [ `PARAGRAPH_BACKGROUND color_add ];

  let color_del = (?? Oe_config.diff_color_delete) in
  g.diff_delete#set_properties [ `PARAGRAPH_BACKGROUND color_del ];

  g.find_text_result_linenum#set_properties
    [`FOREGROUND (?? Oe_config.find_text_output_linenumber_fgcolor); `WEIGHT `NORMAL;
     `BACKGROUND_FULL_HEIGHT_SET true; `SCALE `SMALL ];

  g.find_text_insensitive#set_properties [`FOREGROUND "#b0b0b0"; `STYLE `ITALIC];

  g.find_text_result_hit#set_properties [`BACKGROUND (?? (pref.editor_mark_occurrences_bg_color))];

  let odoc = pref.editor_tags |> List.find (fun t -> t.name = Name.ocamldoc) in
  let odocprops = [
    `FOREGROUND_GDK (to_gdk_color odoc.color);
    `WEIGHT (if odoc.weight > 0 then `CUSTOM odoc.weight else `NORMAL);
    (match odoc.scale with 1.0 -> `SCALE (`MEDIUM) | _ -> `SCALE (`CUSTOM odoc.scale));
    (match odoc.style with `NORMAL -> `STYLE `NORMAL | _ -> `STYLE `ITALIC);
    (match odoc.underline with `NONE -> `UNDERLINE `NONE | _ -> `UNDERLINE `SINGLE);
    `PIXELS_BELOW_LINES 1; `PIXELS_ABOVE_LINES 1 ]
  in
  g.ocamldoc_paragraph#set_properties begin (* `PARAGRAPH_BACKGROUND color for regular ocamldoc comments. *)
    match ?? (pref.editor_ocamldoc_paragraph_bgcolor_1) with
    | Some bg1 -> (`PARAGRAPH_BACKGROUND bg1) :: odocprops
    | _ -> odocprops
  end;

  g.ocamldoc#set_properties begin
    match ?? (pref.editor_ocamldoc_paragraph_bgcolor_2) with
    | Some bg2 -> (`PARAGRAPH_BACKGROUND bg2) :: odocprops
    | _ -> odocprops
  end;

  pref.editor_tags
  |> List.filter (fun tp -> tp.name <> Name.ocamldoc) (* ocamldoc handled separately *)
  |> List.iter begin fun setting ->
    let tag, properties =
      if setting.name = Name.current_line_highlight then
        g.current_line_highlight,
        [ `PARAGRAPH_BACKGROUND_GDK (to_gdk_color setting.bg_color) ]
      else if setting.name = Name.matching_delim then
        g.matching_delim,
        if setting.bg_default then [ `BACKGROUND_SET false ] else
          [ `BACKGROUND_GDK (to_gdk_color setting.bg_color) ]
      else begin
        Option.iter g.table#remove (g.table#lookup setting.name);
        let tag = create_tag g.table setting.name in
        let properties = [
          `FOREGROUND_GDK (to_gdk_color setting.color);
          `WEIGHT (if setting.weight > 0 then `CUSTOM setting.weight else `NORMAL);
          (match setting.scale with 1.0 -> `SCALE `MEDIUM | _ -> `SCALE (`CUSTOM setting.scale));
          (match setting.style with `NORMAL -> `STYLE `NORMAL | _ -> `STYLE `ITALIC);
          (match setting.underline with `NONE -> `UNDERLINE `NONE | _ -> `UNDERLINE `SINGLE);
        ] in
        tag, if setting.bg_default then properties else (`BACKGROUND_GDK (to_gdk_color setting.bg_color)) :: properties
      end
    in
    tag#set_properties properties;
    (*let name = Gobject.Property.get tag#as_tag {Gobject.name="name"; conv=Gobject.Data.string} in*)
    g.lexical <- (setting.name, tag) :: g.lexical;
  end;

  g.lexical <-
    (Name.ocamldoc, g.ocamldoc) ::
    (Name.ocamldoc_paragraph, g.ocamldoc_paragraph) ::
    g.lexical;

  GtkText.Tag.set_priority g.find_text_result_hit#as_tag (g.table#size - 1);
  GtkText.Tag.set_priority g.matching_delim#as_tag (g.table#size - 1);
  GtkText.Tag.set_priority g.warning_unused#as_tag (g.table#size - 1);
;;

let init () =
  update_properties main Preferences.preferences#get;
  preferences#connect#changed ~callback:(update_properties main) |> ignore;;

init ()
