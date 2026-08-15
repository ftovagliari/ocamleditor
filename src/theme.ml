open Settings_t

class monitor (window : GWindow.window) =
  object (self)
    val is_current_theme_dark = new GUtil.variable false
    val mutable is_restart_required = false
    val mutable is_initial_theme_dark = false

    initializer
      let is_visible = window#visible in
      window#show();
      is_initial_theme_dark <- Preferences.Themes.is_dark_theme window#coerce;
      if not is_visible then window#misc#hide();
      is_current_theme_dark#set is_initial_theme_dark;
      (* Get notified if GTK theme changes from outside *)
      is_current_theme_dark#connect#changed ~callback:begin fun _ ->
        self#update_preferences();
        is_restart_required <- (is_current_theme_dark#get <> is_initial_theme_dark);
      end |> ignore;
      window#misc#connect#style_set ~callback:begin fun () ->
        (* style_set is signaled multiple times on single theme change *)
        is_current_theme_dark#set (Preferences.Themes.is_dark_theme window#coerce)
      end |> ignore;
      window#event#connect#focus_in ~callback:begin fun ev ->
        if is_restart_required then begin
          let message = "The system theme settings have changed. A restart maybe required for the theme to take full effect." in
          let dialog =
            GWindow.message_dialog ~message_type:`WARNING ~buttons:GWindow.Buttons.ok
              ~message ~position:`CENTER_ON_PARENT ~parent:window ~show:true ()
          in dialog#run() |> begin function _ ->
            is_restart_required <- false;
            dialog#destroy()
          end
        end;
        false
      end |> ignore;
      self#update_preferences()

    method private update_preferences () =
      Preferences.preferences#get.theme_is_dark <- is_current_theme_dark#get;
      Preferences.save();
  end
