(** "Tools" menu. *)

(** Commands of the Tools menu. *)
let register ~browser =
  let editor = browser#editor in
  let reg id label run = ignore (Command.register ~id:("tools." ^ id) ~label run) in
  reg "preferences" "Preferences" (fun () -> Preferences_tool.create ~editor ());
  reg "toplevel" "OCaml Toplevel" browser#shell;
  reg "external_tools" "External Tools" browser#dialog_external_tools

(** Creates the Tools menu. The external tools configured by the user follow the fixed entries; they
    need a project (for the macros) and are rebuilt every time the menu opens. *)
let create ~browser ~group =
  register ~browser;
  let editor = browser#editor in
  let external_tools =
    Menu_builder.dynamic_items ~fill:begin fun menu ->
      match browser#current_project#get with
      | None -> []
      | Some project ->
          let _, _, translate_macro =
            External_tools.get_macros
              ~get_editor:(fun () -> editor)
              ~get_current_project:(fun () -> project) ()
          in
          External_tools.inject (External_tools.read()) menu translate_macro
    end
  in
  Menu_builder.create ~group ~label:"Tools" Menu_builder.[
      item "tools.preferences";
      separator;
      item "tools.toplevel"; item "tools.external_tools";
      separator;
      custom external_tools;
    ]
