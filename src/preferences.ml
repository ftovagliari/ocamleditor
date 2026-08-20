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

open Settings_t

let default_values =
  let settings = Settings_j.settings_of_string "{}" in
  let default_editor_tags =
    [
      { name = "control";
        color = { light = "blue"; dark = "#87CEFA" };
        bg_color = { light = "#ffffff"; dark = "#000000" };
        weight = 700; style = `NORMAL; underline = `NONE; scale = 1.0; bg_default = true };
      { name = "define";
        color = { light = "#008600"; dark = "#2BA52B" };
        bg_color = { light = "#ffffff"; dark = "#000000" };
        weight = 700; style = `NORMAL; underline = `NONE; scale = 1.0; bg_default = true };
      { name = "structure";
        color = { light = "purple"; dark = "#C889C8" };
        bg_color = { light = "#ffffff"; dark = "#000000" };
        weight = 700; style = `NORMAL; underline = `NONE; scale = 1.0; bg_default = true };
      { name = "char";
        color = { light = "firebrick3"; dark = "#D2691E" };
        bg_color = { light = "#ffffff"; dark = "#000000" };
        weight = 0; style = `NORMAL; underline = `NONE; scale = 1.0; bg_default = true };
      { name = "infix";
        color = { light = "#B34B4B"; dark = "#ff6a6a" };
        bg_color = { light = "#ffffff"; dark = "#000000" };
        weight = 500; style = `NORMAL; underline = `NONE; scale = 1.0; bg_default = true };
      { name = "label";
        color = { light = "saddlebrown"; dark = "#B77871" };
        bg_color = { light = "#ffffff"; dark = "#000000" };
        weight = 700; style = `NORMAL; underline = `NONE; scale = 1.0; bg_default = true };
      { name = "uident";
        color = { light = "#2626FF"; dark = "#6299FF" };
        bg_color = { light = "#ffffff"; dark = "#000000" };
        weight = 700; style = `NORMAL; underline = `NONE; scale = 1.0; bg_default = true };
      { name = "number";
        color = { light = "blue"; dark = "#add8e6" };
        bg_color = { light = "#ffffff"; dark = "#000000" };
        weight = 0; style = `NORMAL; underline = `NONE; scale = 1.0; bg_default = true };
      { name = "custom";
        color = { light = "#FF5959"; dark = "#FF5959" };
        bg_color = { light = "#ffffff"; dark = "#000000" };
        weight = 700; style = `ITALIC; underline = `NONE; scale = 1.0; bg_default = true };
      { name = "lident";
        color = { light = "black"; dark = "white" };
        bg_color = { light = "#ffffff"; dark = "#000000" };
        weight = 0; style = `NORMAL; underline = `NONE; scale = 1.0; bg_default = true };
      { name = "symbol";
        color = { light = "black"; dark = "white" };
        bg_color = { light = "#ffffff"; dark = "#000000" };
        weight = 500; style = `NORMAL; underline = `NONE; scale = 1.0; bg_default = true };
      { name = "name_def";
        color = { light = "black"; dark = "white" };
        bg_color = { light = "#ffffff"; dark = "#000000" };
        weight = 600; style = `NORMAL; underline = `NONE; scale = 1.0; bg_default = true };
      { name = "method_name_def";
        color = { light = "black"; dark = "white" };
        bg_color = { light = "#ffffff"; dark = "#000000" };
        weight = 600; style = `NORMAL; underline = `NONE; scale = 1.0; bg_default = true };
      { name = "comment";
        color = { light = "#CD1076"; dark = "#D85991" };
        bg_color = { light = "#ffffff"; dark = "#000000" };
        weight = 0; style = `ITALIC; underline = `NONE; scale = 0.8; bg_default = true };
      { name = "ocamldoc";
        color = { light = "deeppink3"; dark = "#D85991" };
        bg_color = { light = "#ffffff"; dark = "#000000" };
        weight = 400; style = `ITALIC; underline = `NONE; scale = 0.8; bg_default = true };
      { name = "highlight";
        color = { light = "#ffff00"; dark = "#1e1e1e" };
        bg_color = { light = "#ffffff"; dark = "#000000" };
        weight = 0; style = `NORMAL; underline = `NONE; scale = 1.0; bg_default = true };
      { name = "highlight_current_line";
        color = { light = "#90e090"; dark = "#2E7100" };
        bg_color = { light = "#e3ffe3"; dark = "#223316" };
        weight = 0; style = `NORMAL; underline = `NONE; scale = 1.0; bg_default = true };
      { name = "record_label";
        color = { light = "#474747"; dark = "#d0d0d0" };
        bg_color = { light = "#ffffff"; dark = "#000000" };
        weight = 0; style = `ITALIC; underline = `NONE; scale = 1.0; bg_default = true };
      { name = "selection";
        color = { light = "#ffffff"; dark = "#000000" };
        bg_color = { light = "#3584e4"; dark = "#4B81AD" };
        weight = 0; style = `NORMAL; underline = `NONE; scale = 1.0; bg_default = false };
      { name = "annotation";
        color = { light = "#444488"; dark = "#C68BFF" };
        bg_color = { light = "#ffffff"; dark = "#000000" };
        weight = 0; style = `ITALIC; underline = `NONE; scale = 1.0; bg_default = true };
    ]
  in
  settings.editor_tags <- default_editor_tags;
  settings

let preferences = new GUtil.variable default_values

let geometry_memo = Gmisclib.Window.GeometryMemo.create ~filename:Oe_config.geometry_memo_filename ()

module Themes = struct
  let (//) = Filename.concat
  let (!!) = Filename.dirname

  let directory =
    [
      Some ((!! (!! Sys.executable_name)) // "share" // "themes");
      (try Some ((Sys.getenv "HOME") // ".themes") with Not_found -> None);
      Some "/usr/share/themes";
    ]
    |> List.filter_map Fun.id
    |> List.find_opt Sys.file_exists

  let avail_themes =
    match directory with
    | Some dir when Sys.file_exists dir -> List.sort compare (Array.to_list (Sys.readdir dir))
    | _ -> []

  let is_dark_theme (widget : GObj.widget) =
    let fg_normal = widget#misc#style#fg `NORMAL |> Color.rgb_of_gdk |> Color.avg in
    let bg_normal = widget#misc#style#bg `NORMAL |> Color.rgb_of_gdk |> Color.avg in
    fg_normal > bg_normal
end

module Icon = struct
  let update_otherwidgets_icon pref =
    Otherwidgets_config.app_icon :=
      (fun () -> if pref.theme_is_dark then Icons.Dark.oe else Icons.Light.oe);
    Otherwidgets_config.icon_path :=
      (fun () -> if pref.theme_is_dark then Icons.Dark.path else Icons.Light.path)

  let _ =
    update_otherwidgets_icon preferences#get;
    preferences#connect#changed ~callback:update_otherwidgets_icon

  let get_themed_icon (icon_light, icon_dark) =
    if preferences#get.theme_is_dark then icon_dark else icon_light

  let get_themed_filename basename =
    if preferences#get.theme_is_dark
    then Filename.concat Icons.Dark.path basename
    else Filename.concat Icons.Light.path basename
end

module Color = struct
  let [@ inline] get_themed_color color =
    if preferences#get.theme_is_dark then color.dark else color.light

  let set_themed_color color x =
    if preferences#get.theme_is_dark then color.dark <- x else color.light <- x

  let new_themed_color x alt =
    if preferences#get.theme_is_dark
    then { light = alt.light; dark = x }
    else { light = x; dark = alt.dark }
end

(** Alias for [Color.get_themed_color] *)
let (??) = Color.get_themed_color

(** Alias for [Color.get_themed_icon] *)
let (???) = Icon.get_themed_icon

let editor_tag_bg_color_name tagname =
  let color = (List.find (fun t -> t.name = tagname) preferences#get.editor_tags).bg_color in
  let color_name = Color.get_themed_color color in
  (`NAME color_name)

let [@ inline] editor_tag_bg_color tagname = editor_tag_bg_color_name tagname |> GDraw.color

let editor_tag_color_name tagname =
  let color = (List.find (fun t -> t.name = tagname) preferences#get.editor_tags).color in
  let color_name = Color.get_themed_color color in
  `NAME color_name

let [@ inline] editor_tag_color tagname = editor_tag_color_name tagname |> GDraw.color

let editor_tag_scale tagname =
  (List.find (fun t -> t.name = tagname) preferences#get.editor_tags).scale

let editor_tag_label = function
  | "control"                -> "Control"
  | "define"                 -> "Definition"
  | "structure"              -> "Structure"
  | "char"                   -> "String"
  | "infix"                  -> "Infix operator"
  | "label"                  -> "Label"
  | "uident"                 -> "Capitalized identifier"
  | "number"                 -> "Number"
  | "custom"                 -> "Exception occurrence"
  | "lident"                 -> "Lowercase identifier"
  | "symbol"                 -> "Symbol"
  | "name_def"               -> "Name definition"
  | "method_name_def"        -> "Method name definition"
  | "comment"                -> "Comment"
  | "ocamldoc"               -> "ocamldoc"
  | "highlight"              -> "Delimiter match highlight"
  | "highlight_current_line" -> "Line highlight"
  | "record_label"           -> "Record label"
  | "selection"              -> "Selection"
  | "annotation"             -> "Annotation"
  | x -> x

let filename = Filename.concat App_config.ocamleditor_user_home "settings.json"

let save () =
  let chan = open_out_bin filename in
  begin
    try
      let json = preferences#get |> Settings_j.string_of_settings |> Yojson.Safe.prettify in
      output_string chan json;
      close_out chan;
      preferences#set { preferences#get with timestamp = Unix.gettimeofday() }
    with ex ->
      begin
        Printf.eprintf "Failed to save settings to file \"%s\".\n%s%!" filename (Printexc.to_string ex);
        close_out_noerr chan
      end;
  end

let load () =
  if Sys.file_exists filename then begin
    let chan = open_in_bin filename in
    begin
      try
        let json = really_input_string chan (in_channel_length  chan) in
        let settings = Settings_j.settings_of_string json in
        preferences#set settings;
        close_in chan
      with ex ->
        begin
          close_in_noerr chan;
          Printf.printf "Failed to load settings from file \"%s\", using defaults.\n%!" filename;
        end;
    end;
  end else begin
    Printf.printf "File \"%s\" not found, using defaults.\n%!" filename;
  end;
  begin
    match Themes.avail_themes with
    | [] ->
        preferences#get.theme <- None;
        preferences#get.theme_is_dark <- false;
    |  _-> ()
  end;
  Gmisclib.Window.GeometryMemo.set_enabled geometry_memo preferences#get.remember_window_geometry;
  Gmisclib.Window.GeometryMemo.set_delayed geometry_memo preferences#get.geometry_delayed;
  Otherwidgets_config.geometry_memo := (fun () -> geometry_memo)

let reset_defaults () =
  if Sys.file_exists filename then Sys.remove filename;
  preferences#set default_values;
  save()

let _ =
  let wrong_filename = Filename.concat App_config.ocamleditor_user_home "settings.new.json" in
  if Sys.file_exists wrong_filename then Sys.rename wrong_filename filename;
  load()
