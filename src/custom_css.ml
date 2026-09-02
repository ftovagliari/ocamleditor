let css_provider_from_data data =
  let provider = GObj.css_provider () in
  provider#load_from_data data;
  provider

let apply () =
  GtkData.StyleContext.add_provider_for_screen
    (Gdk.Screen.default ())
    (css_provider_from_data {|
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
    |})#as_css_provider
    GtkData.StyleContext.ProviderPriority.application;
