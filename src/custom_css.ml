let css_provider_from_data data =
  let provider = GObj.css_provider () in
  provider#load_from_data data;
  provider

let apply () =
  GtkData.StyleContext.add_provider_for_screen
    (Gdk.Screen.default ())
    (css_provider_from_data {|
      .editor-scrollbar scrollbar:hover,
      .editor-scrollbar scrollbar.hovering {
        opacity: 0.5;
      }
    |})#as_css_provider
    GtkData.StyleContext.ProviderPriority.application;
