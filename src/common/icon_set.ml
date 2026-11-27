open Printf

let title_bar = "<span font='18' color='#909090'>\u{e10e}</span>" (**  *)

let bookmark = [|
  "\u{f03a1}"; (* 󰎡 *)
  "\u{f03a4}"; (* 󰎤 *)
  "\u{f03a7}"; (* 󰎧 *)
  "\u{f03aa}"; (* 󰎪 *)
  "\u{f03ad}"; (* 󰎭 *)
  "\u{f03b1}"; (* 󰎱 *)
  "\u{f03b3}"; (* 󰎳 *)
  "\u{f03b6}"; (* 󰎶 *)
  "\u{f03b9}"; (* 󰎹 *)
  "\u{f03bc}" (* 󰎼 *)
|]

let window_minimize = "\u{f05b0}" (** 󰖰 *)

let window_restore = "\u{f05b2}" (** 󰖲 *)

let window_close = "\u{f05ad}" (** 󰖭 *)

let file_status_modified = "\u{f0c7}" ^ "\u{2005}" (**  *)

let file_status_readonly = "\u{ea75}" (**  *)

let tab_close = "\u{f0156}" (** 󰅖 *)

let tab_close_modified = "\u{f0765}" (** 󰝥 *)

let warning = "\u{f071}" (**  *)

let error = "\u{f0159}" (** 󰅙 *)

let search_stop = "\u{f04d}" (**  *)

let search_repeat = "\u{eb37}" (**  *)

let search_new = "\u{f422}" (**  *)

let search_prev_result = "\u{f47d}" (**  *)

let search_next_result = "\u{f460}" (**  *)

let search_prev_page = "\u{ea9b}" (**  *)

let search_next_page = "\u{ea9c}" (**  *)

let search_prev_line = "\u{eab5}" (**  *)

let search_next_line = "\u{eab6}" (**  *)

let search_remove = "\u{eb3b}" (**  *)

let search_again = "\u{f0349}" (** 󰍉 *)

let search_find_all = "\u{f13b8}" (** 󱎸 *)

let detach_window = "\u{eb23}" (**  *)

let button_menu_down = "\u{f035d}" (** 󰍝 *)

let isearch_case_sensitive = "\u{eab1}" (**  *)

let expander_open = "\u{f47c}" (**  *)

let expander_closed = "\u{f460}" (**  *)

let find_references = "\u{e113}" (**  *)

let find_definition = "\u{e112}" (**  *)

let outline_refresh = "\u{f0453}" (** 󰑓 *)

let outline_collapse = "\u{f102}" (**  *)

let outline_nested = "\u{e681}" (**  *)

let outline_goto_cursor = "\u{ea9b}" (**  *)

let outline_follow_cursor = "\u{ebcb}" (**  *)

let outline_sort_by_name = "\u{f05bd}" (** 󰖽 *)

let outline_sort_by_kind = "\u{f1385}" (** 󱎅 *)

let editor_font_increase = "\u{f09f4}" (** 󰧴 *)

let editor_font_decrease = "\u{f09f3}" (** 󰧳 *)

let editor_rowspacing_incr = "\u{f084f}" (** 󰡏 *)

let editor_rowspacing_decr = "\u{f084d}" (** 󰡍 *)

let editor_whitespace = "\u{eb7d}" (**  *)

let editor_word_wrap = "\u{eb80}" (**  *)

let editor_dep_graph = "\u{f104a}" (** 󱁊 *)

let editor_line_number = "\u{e0a1}\u{2009}" (**  *)

let editor_column_number = "<small>\u{e0a3}</small>\u{2009}" (**  *)

let git_unpushed = "\u{eaa1}" (**  *)

let git_pending = "\u{ea73}" (**  *)

let git_branch = "\u{f062c}" (** 󰘬 *)

let git_path = "\u{e65d}" ^ " " (**  *)

let opam_switch = sprintf "<span size='large'>%s </span>" "\u{e67a}" (**  *)

let console_clear = "\u{eabf}" (**  *)

let console_compile = "\u{eba2}" (**  *)

let toolbar_back = "\u{f09c0}" (** 󰧀 *)

let toolbar_forward = "\u{f09c2}" (** 󰧂 *)

let toolbar_last_edit_loc = "\u{f09b7}" (** 󰦷 *)

let toolbar_new_file = "\u{ea7f}" (**  *)

let toolbar_open_file = "\u{f115}" (**  *)

let toolbar_save = "\u{f0c7}" (**  *)

let toolbar_save_as = "\u{f0cfb}" (** 󰳻 *)

let toolbar_save_all = sprintf "<span size='x-large'>%s</span>" "\u{eb49}" (**  *)

let toolbar_close = sprintf "<span size='medium'>%s</span>" "\u{e20d}" (**  *)

let toolbar_undo = "\u{f054c}" (** 󰕌 *)

let toolbar_redo = "\u{f044e}" (** 󰑎 *)

let toolbar_find_in_buffer = "\u{f0c7d}" (** 󰱽 *)

let toolbar_find_in_path = "\u{f0969}" (** 󰥩 *)

let toolbar_view_messages = "\u{f10a9}" (** 󱂩 *)

let toolbar_view_hmessages = "\u{f10ab}" (** 󱂫 *)

let toolbar_eval_toplevel = sprintf "<span size='x-large'>%s</span>" "\u{e00a}" (**  *)

let toolbar_compile_file = "\u{f0966}" (** 󰥦 *)

let toolbar_targets = sprintf "<span size='x-large'>%s</span>" "\u{eab3}" (**  *)

let toolbar_clean = "\u{f00e2}" (** 󰃢 *)

let toolbar_compile = "\u{e110}" (**  *)

let toolbar_build = "\u{e111}" (**  *)

let toolbar_run = "\u{f04b}" (**  *)

let refresh = "\u{eb37}" (**  *)

let history = "\u{f02da}" (** 󰋚 *)

let edit = "\u{f03eb}" (** 󰏫 *)

let delete = "\u{f01b4}" (** 󰆴 *)

let revert_to_saved = "\u{f099b}" (** 󰦛 *)

let diff = "\u{f018a}" (** 󰆊 *)

let arrow_up = "\u{f005d}" (** 󰁝 *)

let arrow_down = "\u{f0045}" (** 󰁅 *)

let target_executable = "<span color='green'>\u{f04b}</span>" (**  *)

let target_library = "\u{f0331}\u{2005}" (** 󰌱 *)

let target_plugin = "\u{f0431}" (** 󰐱 *)

let target_external = "\u{f0493}" (** 󰒓 *)

let target_findlib = "\u{f03d6}" (** 󰏖 *)

let shell_use_file = "\u{f022e}" (** 󰈮 *)

let shell_load_cm = "\u{f107b}" (** 󱁻 *)

let shell_directory = "\u{f024b}" (** 󰉋 *)

let shell_load_path = "\u{f107f}" (** 󱁿 *)

let file_tree_folder = "\u{f024b}" (** 󰉋 *)

let file_tree_folder_open = "\u{f0770}" (** 󰝰 *)

let file_tree_source = "\u{f022e}" (** 󰈮 *)

let file_tree_config = "\u{f0493}" (** 󰒓 *)

let file_tree_other = "\u{f0219}" (** 󰈙 *)
