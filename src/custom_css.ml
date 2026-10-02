open Printf
open Preferences
open Settings_j

let css_provider_from_data data =
  let provider = GObj.css_provider () in
  provider#load_from_data data;
  provider

let get_editor_selection_colors () =
  let pref = Preferences.preferences#get in
  let editor_tags = pref.editor_tags in
  let default_bg_color =
    if pref.editor_bg_color_theme then ?? (Preferences.default_values.editor_bg_color_user)
    else ?? (pref.editor_bg_color_user);
  in
  match List.find_opt (fun t -> t.name = "selection") editor_tags with
  | Some t ->
      let bg_color = if t.bg_default then "initial" else ?? (t.bg_color) in
      let fg_color = if t.bg_default then default_bg_color else ?? (t.color) in
      bg_color, fg_color
  | _ -> failwith ""

let apply () =
  let bg_sel_color, _ = get_editor_selection_colors() in
  GtkData.StyleContext.add_provider_for_screen
    (Gdk.Screen.default ())
    begin
      css_provider_from_data (sprintf {|
        .editor-scrollbar scrollbar slider { min-width: 21px; border-radius: 2px;}
        .editor-scrollbar scrollbar:hover,
        .editor-scrollbar scrollbar.hovering {
          opacity: 0.5;
        }
        .outline-button button {
          padding: 3px;
          margin: 0px;
          min-width: 0px;
          min-height: 0px;
        }
        .statusbar-button {
          padding: 1px 2px 1px 2px;
          margin: 0px;
          min-width: 0px;
          min-height: 0px;
        }
        window.dialog-window { border: 1px solid @theme_selected_bg_color; }
        box.incremental-search { background-color: @theme_bg_color; border: 1px solid @theme_selected_bg_color; padding: 5px}
        box.incremental-search-hidden { opacity: 0.0}
        box.incremental-search-visible { opacity: 1.0}

        popover.no-arrow > treeview.view:selected,
        treeview.view:selected:focus,
        treeview.view:selected:backdrop {
            background-color: @theme_selected_bg_color;
            color: @theme_selected_fg_color;
        }
        popover.no-arrow {
          background-color: transparent;
          border: none;
          margin: 0px;
        }
        popover.no-arrow > box {
          border: 1px solid @borders;
          background-color: @theme_base_color;
        }
        popover.quick-info {
          border: 1px solid @borders;
          background-color: @theme_base_color;
        }
        popover.quick-info > * { background-color: @theme_base_color; }

      |} (*bg_sel_color*))
    end#as_css_provider
    GtkData.StyleContext.ProviderPriority.application;
