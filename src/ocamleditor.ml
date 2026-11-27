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


let install_fonts () =
  let (/) = Filename.concat in
  let font_names =
    App_config.application_fonts
    |> File_util.get_files_in_directory
    |> List.map Filename.basename
  in
  let font_dir = (Sys.getenv "HOME") / ".local" / "share" / "fonts" in
  Utils.mkdir_p font_dir;
  let copy_font name =
    Printf.printf "Installing font %s\n%!" (font_dir / name);
    let src = App_config.application_fonts / name in
    let dest = font_dir / name in
    File_util.cp src dest
  in
  font_names
  |> List.iter begin fun name ->
    let filename = font_dir / name in
    let should_copy =
      not (Sys.file_exists filename) ||
      let stat_old = Unix.stat filename in
      let stat_new = Unix.stat (App_config.application_fonts / name) in
      stat_new.Unix.st_mtime > stat_old.Unix.st_mtime
    in
    if should_copy then copy_font name  end

(** main *)
let main () = begin
  Unix.putenv "LC_MESSAGES" "C";
  let open Preferences in
  let _locale = GtkMain.Main.init ~setlocale:false () in
  let start splashscreen =
    Messages.vpaned := Some (GPack.paned `VERTICAL ());
    Messages.hpaned := Some (GPack.paned `HORIZONTAL ());
    let window = GWindow.window
        ~title:About.program_name
        ~icon:(??? Logo.oe)
        ~position:`CENTER
        ~decorated:true
        ~focus_on_map:true
        ~resizable:true
        ~type_hint:`NORMAL
        ~kind:`TOPLEVEL
        ~show:true ()
    in
    (* The splash screen owns the focus: when it is destroyed some window managers (e.g. WSLg) give the
       focus to another application and leave the main window behind, so raise it explicitly. *)
    let close_splashscreen w =
      (* Activate the main window while the splash screen still exists, so that destroying the latter
         does not hand the activation over to another application; then repeat it, because the window
         manager may ignore the first request. *)
      window#present();
      w#destroy();
      window#present();
      Gmisclib.Timeout.add "raise-main-window" ~ms:250 ~callback:begin fun () ->
        window#present();
        false
      end |> ignore
    in
    Gaux.may splashscreen ~f:(fun w ->
        w#event#connect#button_press ~callback:(fun _ -> close_splashscreen w; false) |> ignore;
        w#set_transient_for window#as_window);
    Gtk_theme.set_theme ~context:window#misc#pango_context ();
    let _ = new Theme.monitor window in
    let browser = Browser.create window in
    (* Before browser initialization *)
    browser#connect#startup ~callback:begin fun () ->
      install_fonts();
      Custom_css.apply();
      Sys.chdir (Filename.dirname Sys.executable_name);
      Project_json.init();
    end |> ignore;
    browser#connect#after#startup ~callback:begin fun () ->
      Gaux.may (browser#editor#get_page `ACTIVE) ~f:(fun page -> page#view#misc#grab_focus());
      Gaux.may splashscreen ~f:begin fun w ->
        Gmisclib.Timeout.add "splashscreen" ~ms:2000 ~callback:begin fun () ->
          close_splashscreen w;
          false
        end |> ignore;
      end;
    end |> ignore;
    (*  *)
    browser#startup();
  in
  begin
    match Browser.splashscreen() with
    | None -> start None
    | Some splashscreen ->
        splashscreen#misc#connect#after#show ~callback:begin fun () ->
          (* [start] runs inside a GLib source, so an exception escaping it is
             caught by the C wrapper and reduced to "GSourceFunc: callback raised
             an exception", with no indication of what failed and the splash
             screen left on screen. Report it ourselves. *)
          Gmisclib.Timeout.add __FUNCTION__ ~ms:100 ~callback:begin fun () ->
            begin
              try start (Some splashscreen)
              with ex ->
                Printf.eprintf "Startup failed: %s\n%s\n%!"
                  (Printexc.to_string ex) (Printexc.get_backtrace ())
            end;
            false
          end |> ignore;
        end |> ignore;
        splashscreen#show();
  end;
  GtkThread.main ();
end

let _ = Printexc.print main ()
