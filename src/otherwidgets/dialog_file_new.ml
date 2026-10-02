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

(*let rec create_filename project n =
  if n = 1000 then (failwith "Browser#dialog_file_new (mkname)");
  let name = Filename.concat (project.Project.root // Project.src)
    (sprintf "untitled%s.ml" (if n = 0 then "" else string_of_int n)) in
  if not (Sys.file_exists name) then name else (create_filename project (n + 1));;*)

let show ~editor () =
  let window = GWindow.file_chooser_dialog
      ~action:`SAVE ~icon:(!Otherwidgets_config.app_icon())
      ~title:"New File..."
      ~position:`CENTER ~modal:true ~show:false ()
  in
  window#set_select_multiple false;
  window#add_select_button_stock `OK `OK;
  window#add_button_stock `CANCEL `CANCEL;
  window#set_default_response `OK;
  let default_filename = "untitled.ml" in
  ignore (window#set_current_folder (Filename.dirname default_filename));
  ignore (window#set_current_name (Filename.basename default_filename));
  let rec run () =
    match window#run () with
    | `OK ->
        Gaux.may window#filename ~f:begin fun filename ->
          if Sys.file_exists filename then begin
            Dialog.info ~title:"Error" ~message_type:`ERROR ~message:"File already exists." window;
            run()
          end else begin
            let chan = open_out_gen [Open_creat; Open_excl; Open_text] 0o664 filename in
            close_out chan;
            match editor#open_file ~active:false ~offset:0 ?remote:None filename with
            | Some page ->
                editor#load_page ?scroll:None page;
                editor#goto_view page#view;
                window#destroy()
            | _ ->
                Dialog.info ~title:"Error" ~message_type:`ERROR ~message:"" window;
          end
        end;
    | _ -> window#destroy()
  in run()





