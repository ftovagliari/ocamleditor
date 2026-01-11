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


(** fade_out *)
let fade_out window =
  if Ocaml_config.is_mingw then begin
    GMain.Timeout.add ~ms:5 ~callback:begin fun () ->
      let opa = max 0. (window#opacity -. 0.1) in
      if opa > 0. then begin
        window#set_opacity opa;
        true
      end else begin
        window#destroy();
        false
      end
    end |> ignore;
  end else begin
    while window#opacity > 0. do
      Thread.delay 0.02;
      let opa = max 0. (window#opacity -. 0.1) in
      if opa >= 0. then begin
        window#set_opacity opa;
      end
    done;
  end;
  window#destroy()

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
  install_fonts();
  let open Preferences in
  (* let _ = About.build_id := Build_id.timestamp in
     let _ = About.git_hash := Build_id.git_hash in *)
  let _locale = GtkMain.Main.init ~setlocale:false () in

  let start splashscreen =
    let window = GWindow.window
        ~title:About.program_name
        ~icon:(??? Icons.oe)
        ~position:`CENTER
        ~width:1
        ~height:1
        ~decorated:false
        ~focus_on_map:true
        ~allow_shrink:true
        ~allow_grow:true
        ~resizable:true
        ~type_hint:`NORMAL
        ~kind:`TOPLEVEL
        ~show:false ()
    in
    Gtk_theme.set_theme ~context:window#misc#pango_context ();
    window#iconify(); (* doesn't work on WSL *)
    window#move ~x:0 ~y:0;
    let _ = new Theme.monitor window in
    let browser = Browser.create window in
    (* Before browser initialization *)
    browser#connect#startup ~callback:begin fun () ->
      Gaux.may splashscreen ~f:(fun w -> w#set_transient_for browser#window#as_window);
      Sys.chdir (Filename.dirname Sys.executable_name);
      Printf.printf "%s\n%!" (System_properties.to_string());
      Project_xml.init();
    end |> ignore;
    browser#connect#after#startup ~callback:begin fun () ->
      Gmisclib.Idle.add ~prio:300 begin fun () ->
        (*window#set_position `CENTER_ALWAYS;
          window#move ~x:0 ~y:0;
          window#set_position `CENTER;*)
        window#set_decorated true;
        window#deiconify();
        window#present();
        Gaux.may (browser#editor#get_page `ACTIVE) ~f:(fun page -> page#view#misc#grab_focus());
        Gaux.may splashscreen ~f:fade_out;
      end
    end |> ignore;
    (*  *)
    browser#startup();
  in
  begin
    match Browser.splashscreen() with
    | None -> start None
    | Some splashscreen ->
        splashscreen#misc#connect#after#show ~callback:begin fun () ->
          GMain.Timeout.add ~ms:100 ~callback:(fun () -> start (Some splashscreen); false) |> ignore;
          (*Gmisclib.Idle.add ~prio:300 (fun () -> start (Some splashscreen));*)
        end |> ignore;
        splashscreen#present();
  end;
  GtkThread2.main ();
end

let _ = Printexc.print main ()
