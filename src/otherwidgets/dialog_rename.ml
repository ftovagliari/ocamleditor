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


open Printf

(** colorize *)
let colorize page filename =
  let old = page#buffer#lexical_enabled in
  let is_lexical_enabled = page#buffer#is_ocaml_file filename in
  page#buffer#set_lexical_enabled is_lexical_enabled;
  if is_lexical_enabled && not old then (page#buffer#colorize ?start:None ?stop:None ())
  else if not is_lexical_enabled then begin
    let start = page#buffer#start_iter in
    let stop = page#buffer#end_iter in
    List.iter (function Some t -> page#buffer#remove_tag t ~start ~stop | _ -> ()) page#buffer#tag_table_lexical;
    page#error_indication#remove_tag();
  end

(** sync_page *)
let sync_page ~editor ~page ~filename ?remote () =
  let file = editor#create_file ?remote filename in
  page#set_file (Some file);
  colorize page filename

(** rename *)
let rename ~editor ~page ~filename () =
  match page#file with
  | Some file ->
      file#rename filename;
      sync_page ~editor ~page ~filename ?remote:file#remote ();
  | _ -> assert false

(** ask_overwrite *)
let ask_overwrite ~run ~overwrite ~filename window =
  match
    Dialog.confirm ~message:(sprintf
                               "Are you sure you want to overwrite file \xC2\xAB%s\xC2\xBB?" (Filename.basename filename))
      ~yes:("Overwrite", overwrite)
      ~no:("Do Not Overwrite", ignore)
      ~title:"Overwrite File" window;
  with `CANCEL | `NO -> run() | `YES -> window#destroy()

(** window *)
let window ~editor ~page () =
  let window = GWindow.file_chooser_dialog
      ~action:`SAVE ~icon:(!Otherwidgets_config.app_icon())
      ~title:(sprintf "Rename \xC2\xAB%s\xC2\xBB" (Filename.basename page#get_filename))
      ~position:`CENTER ~modal:true ~show:false ()
  in
  window#set_select_multiple false;
  window#add_select_button_stock `OK `OK;
  window#add_button_stock `CANCEL `CANCEL;
  window#set_default_response `OK;
  ignore (window#set_filename page#get_filename);
  let rec run () =
    match window#run () with
    | `OK ->
        Gaux.may window#filename ~f:begin fun filename ->
          let buffer : GText.buffer = page#buffer#as_text_buffer#as_gtext_buffer in
          if Sys.file_exists filename then begin
            let overwrite () =
              editor#pages
              |> List.find_map (fun p -> if p#get_filename = filename then Some (editor#close p) else None)
              |> ignore;
              rename ~editor ~page ~filename ();
            in
            ask_overwrite ~run ~overwrite ~filename window
          end else (rename ~editor ~page ~filename (); window#destroy());
        end;
    | _ -> window#destroy()
  in run()



