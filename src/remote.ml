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
open Utils

(* user@host, (password, key, pubkey, passphrase, last_filename) *)
type history_entry = (string * (string * string option * string option * string * string))

let _ = Curl.global_init Curl.CURLINIT_GLOBALALL

let re_space = Str.regexp " "
let re_spaces = Str.regexp " +"
let re_cap = Str.regexp "^"
let re_colon = Str.regexp ":"

let month_of_string = function
  | "Jan" -> 0
  | "Feb" -> 1
  | "Mar" -> 2
  | "Apr" -> 3
  | "May" -> 4
  | "Jun" -> 5
  | "Jul" -> 6
  | "Aug" -> 7
  | "Sep" -> 8
  | "Oct" -> 9
  | "Nov" -> 10
  | "Dec" -> 11
  | _ -> assert false

let check_new_filename_exists page filename =
  let remote =
    match page#file with
    | Some file -> (match file#remote with Some remote -> remote | _ -> assert false)
    | _ -> assert false
  in
  let newfile = Editor_file.create ~remote filename in
  let newfile_exists = newfile#exists in
  newfile#cleanup();
  remote, newfile_exists


exception Error of int * string * string

(** file *)
class file ~host ~user ~pwd ~sslkey ~sshpublickeyfile ~sslkeypasswd ~filename : Editor_file_type.abstract_file =
  object (self)

    val mutable filename = filename

    val curl = new Curl.handle

    val mutable mtime = 0.0

    initializer
      match self#stat () with
      | Some perm -> mtime <- perm.Editor_file_type.mtime;
      | _ -> ()

    method cleanup () = curl#cleanup
    method filename = filename
    method dirname = Filename.dirname filename
    method basename = Filename.basename filename

    method set_filename fn = filename <- fn

    method private get_url () = sprintf "sftp://%s%s@%s%s"
        user (if pwd = "" then "" else ":" ^ pwd) host filename

    method private set_url url =
      curl#set_url url;
      if sslkey <> "" then begin
        curl#set_sshprivatekeyfile sslkey;
        curl#set_sslkeypasswd sslkeypasswd;
      end;
      if sshpublickeyfile <> "" then curl#set_sshpublickeyfile sshpublickeyfile;

    method private protect : 'a 'b.('a -> 'b) -> 'a -> 'b = fun f x ->
      try f x
      with Curl.CurlException (code, num, msg) -> raise (Error (num, (Curl.strerror code), msg))

    method private perm_of_string str =
      assert (String.length str = 10);
      0

    method private time_of_string str =
      let open Unix in
      match Str.split re_spaces str with
      | [mon; day; year_time] ->
          let ltm = Unix.localtime (Unix.gettimeofday ()) in
          let year, hour, min =
            if String.contains year_time ':' then
              let time = Str.split re_colon year_time in
              let year = ltm.tm_year in
              begin
                match time with
                | [h; m] ->
                    year,
                    int_of_string h,
                    int_of_string m
                | _ -> assert false
              end
            else (int_of_string year_time) - 1900, 0, 0
          in
          mktime {
            tm_sec = 0;
            tm_min = min;
            tm_hour = hour;
            tm_mday = int_of_string day;
            tm_mon = month_of_string mon;
            tm_year = year;
            tm_wday = 0;
            tm_yday = 0;
            tm_isdst = ltm.tm_isdst;
          }
      | _ -> assert false

    method stat () =
      self#protect begin fun () ->
        let path = if filename = "" then "" else self#dirname in
        ksprintf self#set_url "sftp://%s%s@%s%s/" user (if pwd = "" then "" else ":" ^ pwd) host path;
        curl#set_upload false;
        let buf = Buffer.create 1000 in
        curl#set_writefunction begin fun str ->
          Buffer.add_string buf str;
          String.length str;
        end;
        curl#perform;
        let listing = Buffer.contents buf in
        let re_filename = ksprintf Str.regexp " %s$" self#basename in
        try
          let stop = Str.search_forward re_filename listing 0 in
          let start = Str.search_backward re_cap listing stop in
          let line = String.sub listing start (stop - start) in
          let perm = String.sub line 0 10 in (* -rwxr--r-x *)
          let time = String.sub line (String.length line - 12) 12 in (* 1554 Jan  7 18:41 *)
          let stop = String.length line - 12 - 2 in
          let start = Str.search_backward re_space line stop in
          let size = int_of_string (String.sub line (start + 1) (stop - start)) in
          let ftime, time = self#time_of_string time in
          (*Printf.printf "%S -- %S -- (%d/%d/%d %d:%d) -- %d\n%!" line perm
             (time.Unix.tm_year + 1900)
             (time.Unix.tm_mon + 1)
             time.Unix.tm_mday
             time.Unix.tm_hour
             time.Unix.tm_min
             size;*)
          Some {Editor_file_type.perm; size; mtime=ftime};
        with Not_found -> None
      end ()

    method read =
      self#protect begin fun () ->
        let buf = Buffer.create 100 in
        self#set_url (self#get_url ());
        curl#set_writefunction begin fun str ->
          Buffer.add_string buf str;
          String.length str;
        end;
        curl#perform;
        Buffer.contents buf;
      end ()

    method write str =
      self#protect begin fun () ->
        self#set_url (self#get_url ());
        curl#set_upload true;
        let buf = ref str in
        curl#set_readfunction begin fun max_bytes ->
          if !buf = "" then "" else
            let blen = String.length !buf in
            let len = min max_bytes blen in
            let bytes = String.sub !buf 0 len in
            buf := String.sub !buf len (blen - len);
            bytes
        end;
        curl#perform;
        mtime <- Unix.gettimeofday();
        ignore (self#stat());
      end ()

    method list () =
      self#protect begin fun () ->
        let path = if Filename.is_implicit filename then "/" ^ filename else filename in
        let path = if path = "/" then "" else path in
        ksprintf self#set_url "sftp://%s%s@%s%s/" user (if pwd = "" then "" else ":" ^ pwd) host path;
        curl#set_upload false;
        curl#set_ftplistonly true;
        let buf = Buffer.create 1000 in
        curl#set_writefunction begin fun str ->
          Buffer.add_string buf str;
          String.length str;
        end;
        curl#perform;
        Str.split (Str.regexp "[\r\n]") (Buffer.contents buf)
      end ()

    method is_readonly =
      match self#stat() with
      | Some stat ->
          let perm = stat.Editor_file_type.perm in
          perm.[1] = 'r' && perm.[2] = '-'
      | _ -> true

    method is_writeable =
      match self#stat() with
      | Some stat ->
          let perm = stat.Editor_file_type.perm in
          perm.[1] = 'r' && perm.[2] = 'w'
      | _ -> false

    method last_modified () =
      match self#stat() with
      | Some stat -> stat.Editor_file_type.mtime
      | _ -> 0.0

    method changed = mtime < self#last_modified()

    method exists = self#stat () <> None

    method backup ?move_to () =
      Printf.eprintf "[Remote] backup not implemented\n%!" ;
      ""

    method rename newpath =
      self#protect begin fun () ->
        ksprintf self#set_url "sftp://%s%s@%s" user (if pwd = "" then "" else ":" ^ pwd) host;
        curl#set_postquote [sprintf "rename '%s' '%s'" filename newpath];
        curl#set_upload false;
        curl#perform;
        filename <- newpath;
      end ()

    method remove = if self#exists then
        self#protect begin fun () ->
          ksprintf self#set_url "sftp://%s%s@%s" user (if pwd = "" then "" else ":" ^ pwd) host;
          curl#set_postquote [sprintf "rm '%s'" filename];
          curl#set_upload false;
          curl#perform;
        end ()

    method remote = Some {Editor_file_type.host; user; pwd; sslkey; sshpublickeyfile; sslkeypasswd}

  end

let _ = Editor_file.create_remote := new file

(** widget *)
class widget ?packing () =
  let open_file = new open_file () in
  let vbox = GPack.vbox ~spacing:8 ?packing () in
  (* Read history *)
  let history_filename = App_config.ocamleditor_user_home // "remote_history" in
  let read_history () =
    if Sys.file_exists history_filename then begin
      let chan = open_in_gen [Open_binary; Open_rdonly] 0o600 history_filename in
      try
        let history : history_entry list = input_value chan in
        close_in chan;
        history
      with
      | End_of_file -> (close_in chan; [])
      | ex -> (close_in chan; raise ex)
    end else []
  in
  (* Write history *)
  let write_history (history : history_entry list) =
    let chan = open_out_gen [Open_creat; Open_binary; Open_trunc; Open_wronly] 0o600 history_filename in
    try
      output_value chan history;
      close_out chan
    with ex -> (close_out chan; raise ex);
  in
  (* Delete history *)
  let clear_history () =
    if Sys.file_exists history_filename then Sys.remove history_filename
  in
  (*  *)
  let _ = GMisc.label ~markup:"<b><span font_size='larger'>Connection for SFTP access to remote system</span></b>" ~ypad:0 ~xalign:0.0 ~packing:vbox#pack () in
  let _ = GMisc.label ~height:2  ~ypad:0 ~packing:vbox#pack () in

  let hbox = GPack.vbox ~packing:vbox#pack () in
  let _ = GMisc.label ~text:"Username@Hostname" ~xalign:0.0 ~packing:hbox#pack () in
  let entry_user_host = GEdit.entry ~width_chars:50 ~packing:hbox#add () in
  let cols = new GTree.column_list in
  let col_user_host = cols#add Gobject.Data.string in
  let model_user_host = GTree.list_store cols in
  let entry_user_host_compl = GEdit.entry_completion ~model:model_user_host ~entry:entry_user_host () in
  let _ = entry_user_host_compl#set_text_column col_user_host in
  let _  = entry_user_host#misc#modify_font_by_name "bold" in

  let hbox = GPack.vbox ~packing:vbox#pack () in
  let _ = GMisc.label ~text:"Password" ~xalign:0.0 ~packing:hbox#pack () in
  let box = GPack.hbox ~spacing:1 ~packing:hbox#pack () in
  let entry_pwd = GEdit.entry ~visibility:false ~packing:box#add () in

  let hbox = GPack.vbox ~packing:vbox#pack () in
  let _ = GMisc.label ~text:"Private key file name" ~xalign:0.0 ~packing:hbox#pack () in
  let box = GPack.hbox ~spacing:1 ~packing:hbox#pack () in
  let entry_key = GFile.chooser_button ~action:`OPEN ~packing:box#add () in
  let button_clear_key = GButton.button ~packing:box#pack () in
  let _ = button_clear_key#set_image (GMisc.image ~stock:`CLEAR ~icon_size:`MENU ())#coerce in
  let _ = button_clear_key#connect#clicked ~callback:(fun () -> entry_key#unselect_all) in
  let _ = entry_key#set_current_folder App_config.user_home in

  let hbox = GPack.vbox ~packing:vbox#pack () in
  let _ = GMisc.label ~text:"Public key file name" ~xalign:0.0 ~packing:hbox#pack () in
  let box = GPack.hbox ~spacing:1 ~packing:hbox#pack () in
  let entry_pubkey = GFile.chooser_button ~action:`OPEN ~packing:box#add () in
  let button_clear_pubkey = GButton.button ~packing:box#pack () in
  let _ = button_clear_pubkey#set_image (GMisc.image ~stock:`CLEAR ~icon_size:`MENU ())#coerce in
  let _ = button_clear_pubkey#connect#clicked ~callback:(fun () -> entry_pubkey#unselect_all) in
  let _ = entry_pubkey#set_current_folder App_config.user_home in

  let hbox = GPack.vbox ~packing:vbox#pack () in
  let _ = GMisc.label ~text:"Passphrase for the private key" ~xalign:0.0 ~packing:hbox#pack () in
  let box = GPack.hbox ~spacing:1 ~packing:hbox#pack () in
  let entry_pass = GEdit.entry ~visibility:false ~packing:box#add () in

  let hbox = GPack.hbox ~spacing:3 ~packing:vbox#pack () in
  let check_save_password = GButton.check_button ~label:"Save password" ~packing:hbox#add () in
  let button_clear_history = GButton.button ~label:"Clear history" ~packing:hbox#pack () in

  let button_connect = GButton.button ~stock:`CONNECT ~packing:vbox#pack () in

  let hbox = GPack.vbox ~spacing:3 ~packing:vbox#pack () in
  let _ = GMisc.label ~text:"Remote filename:" ~xalign:0.0 ~packing:hbox#pack () in
  let entry_filename = GEdit.entry ~packing:hbox#pack () in
  let cols = new GTree.column_list in
  let col_filename = cols#add Gobject.Data.string in
  let model_filename = GTree.list_store cols in
  let entry_filename_compl = GEdit.entry_completion ~minimum_key_length:1 ~model:model_filename ~entry:entry_filename () in
  let _ = entry_filename_compl#set_text_column col_filename in
  let _  = entry_filename#misc#set_sensitive false in
  let _  = entry_filename#misc#modify_font_by_name "sans 10" in
  object (self)
    inherit GObj.widget vbox#as_widget

    val mutable history = read_history ()
    val mutable remote_file = None

    initializer
      ignore (button_clear_history#connect#clicked ~callback:begin fun () ->
          clear_history();
          history <- [];
          model_user_host#clear();
        end);
      ignore (self#misc#connect#destroy ~callback:begin fun _ ->
          Option.iter (fun f -> f#cleanup()) remote_file
        end);
      List.iter begin fun text ->
        let row = model_user_host#append () in
        model_user_host#set ~row ~column:col_user_host text
      end (List.map (fun (k, _) -> k) history);
      ignore (entry_user_host_compl#connect#match_selected ~callback:begin fun model row ->
          try
            let row = model#convert_iter_to_child_iter row in
            let key = model_user_host#get ~row ~column:col_user_host in
            let password, private_key, public_key, passphrase, last_filename = List.assoc key history in
            entry_user_host#set_text key;
            entry_pwd#set_text password;
            entry_pass#set_text passphrase;
            entry_filename#set_text last_filename;
            if password <> "" then check_save_password#set_active true;
            if passphrase <> "" then check_save_password#set_active true;
            (match private_key with Some x -> ignore (entry_key#select_filename x) | _ -> entry_key#unselect_all);
            (match public_key with Some x -> ignore (entry_pubkey#select_filename x) | _ -> entry_pubkey#unselect_all);
            button_connect#misc#grab_focus();
            false
          with Not_found -> true
        end);
      ignore (button_connect#connect#clicked ~callback:begin fun () ->
          self#remote_connect();
          entry_filename#misc#set_sensitive true;
          entry_filename#misc#grab_focus();
        end);
      ignore (entry_user_host#connect#changed ~callback:begin fun () ->
          entry_filename#misc#set_sensitive false;
          entry_pwd#set_text "";
          entry_filename#set_text ""
        end);
      ignore (entry_filename#connect#changed ~callback:begin fun () ->
          if entry_filename#text = "" || entry_filename#text.[String.length entry_filename#text - 1] = '/' then self#file_completion()
        end);
      ignore (entry_filename_compl#connect#match_selected ~callback:begin fun model row ->
          let is_directory filename =
            let filename = if filename.[String.length filename - 1] = '/' then String.sub filename 0 (String.length filename - 1) else filename in
            match remote_file with
            | Some file ->
                file#set_filename filename;
                begin
                  try ignore (file#list ()); true
                  with
                  | Error _ -> false
                  | ex ->
                      Printf.eprintf "File \"remote.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());
                      false
                end;
            | _ -> false
          in
          Gdk.Window.set_cursor self#misc#window (Gdk.Cursor.create `WATCH);
          entry_filename#set_editable false;
          let row = model#convert_iter_to_child_iter row in
          let filename = model_filename#get ~row ~column:col_filename in
          let is_dir = is_directory filename in
          if is_dir then begin
            Gmisclib.Idle.add ~prio:200 begin fun () ->
              let pos = entry_filename#insert_text ~pos:(Glib.Utf8.length entry_filename#text) "/" in
              entry_filename#select_region ~start:pos ~stop:pos;
              self#file_completion();
              entry_filename#set_editable true;
              Gdk.Window.set_cursor self#misc#window (Gdk.Cursor.create `ARROW);
            end
          end else (Gdk.Window.set_cursor self#misc#window (Gdk.Cursor.create `ARROW));
          false
        end)

    method private file_completion () =
      match remote_file with
      | Some file ->
          Gdk.Window.set_cursor self#misc#window (Gdk.Cursor.create `WATCH);
          begin
            try
              let filename = entry_filename#text in
              let len = String.length filename in
              let filename = if len > 0 && filename.[len - 1] = '/' then String.sub filename 0 (len - 1) else filename in
              file#set_filename filename;
              model_filename#clear();
              let re = Str.regexp ".*\\(\\(\\.cm.+\\)\\|\\(\\.o.?.?\\)\\|\\(\\.lib\\)\\|\\(\\.a\\)\\|\\(\\.exe\\)\\)$" in
              List.iter begin fun text ->
                if not (Str.string_match re text 0) then
                  let row = model_filename#append () in
                  let text = entry_filename#text ^ text in
                  model_filename#set ~row ~column:col_filename text
              end (file#list ());
              entry_filename_compl#complete();
              Gdk.Window.set_cursor self#misc#window (Gdk.Cursor.create `ARROW);
            with ex ->
              Gdk.Window.set_cursor self#misc#window (Gdk.Cursor.create `ARROW);
              raise ex
          end;
      | _ -> ()

    method private split_user_host text =
      let pos = String.index text '@' in
      let user = Str.string_before text pos in
      let host = Str.string_after text (pos + 1) in
      user, host

    method private remote_connect () =
      let finally () =
        Gmisclib.Idle.add begin fun () ->
          entry_user_host#misc#grab_focus ();
          entry_filename#misc#set_sensitive false;
        end
      in
      let title = "Error while connecting to remote host" in
      let message = sprintf "Error while connecting to\n%s\n\n" entry_user_host#text in
      try
        let user, host = self#split_user_host entry_user_host#text in
        let sslkey = match entry_key#filename with Some x -> x | _ -> "" in
        let sshpublickeyfile = match entry_pubkey#filename with Some x -> x | _ -> "" in
        let file = new file ~host ~user ~pwd:entry_pwd#text ~sslkey ~sshpublickeyfile ~sslkeypasswd:entry_pass#text ~filename:"" in
        remote_file <- Some file;
        self#write_history();
        if entry_filename#text = "" then (self#file_completion());
        entry_filename#misc#grab_focus();
      with
      | Error (_, reason, _) ->
          Dialog.message ~title ~message:(message ^ reason) `ERROR;
          finally()
      | ex ->
          Dialog.display_exn ~parent:self ~title ~message ex;
          finally()

    method private write_history () =
      let user_host = entry_user_host#text in
      let pwd = if check_save_password#active then entry_pwd#text else "" in
      let pass = if check_save_password#active then entry_pass#text else "" in
      history <- (user_host, (pwd, entry_key#filename, entry_pubkey#filename, pass, entry_filename#text)) ::
                 (List.remove_assoc user_host history);
      write_history history;

    method apply () =
      try
        let user, host = self#split_user_host entry_user_host#text in
        let sslkey = match entry_key#filename with Some x -> x | _ -> "" in
        let sshpublickeyfile = match entry_pubkey#filename with Some x -> x | _ -> "" in
        let remote = {Editor_file_type.host; user; pwd = entry_pwd#text; sslkey; sshpublickeyfile; sslkeypasswd = entry_pass#text } in
        self#write_history();
        open_file#call (remote, entry_filename#text);
      with Not_found ->
        Dialog.message ~title:"Invalid address" ~message:"Invalid address" `ERROR;
        entry_user_host#misc#grab_focus();

    method connect = new signals ~open_file

  end
(** Signals *)
and open_file () = object inherit [Editor_file_type.remote_login * string] GUtil.signal () end

and signals ~open_file =
  object
    inherit GUtil.ml_signals [open_file#disconnect]
    method open_file = open_file#connect ~after
  end

open Preferences
(** dialog_rename *)
let dialog_rename ~editor ~page () =
  match page#file with
  | None -> ()
  | Some _ ->
      let window = GWindow.dialog
          ~icon:(??? Icons.oe) ~title:(sprintf "Rename file?")
          ~position:`CENTER ~modal:true ~show:false ()
      in
      let _ = GMisc.label ~text:(sprintf "Rename remote file\n\n%s" page#get_title) ~xalign:0.0 ~packing:window#vbox#pack () in
      let entry = GEdit.entry ~text:page#get_filename ~packing:window#vbox#pack () in
      window#add_button_stock `OK `OK;
      window#add_button_stock `CANCEL `CANCEL;
      window#set_border_width 8;
      window#vbox#set_spacing 5;
      ignore (window#event#connect#key_release ~callback:begin fun ev ->
          let key = GdkEvent.Key.keyval ev in
          if key = GdkKeysyms._Escape then (window#response `CANCEL; true)
          else if key = GdkKeysyms._Return then begin
            window#response `OK;
            true
          end else false
        end);
      let rec run () =
        match window#run () with
        | `OK ->
            let filename = entry#text in
            if filename <> page#get_filename then begin
              let _, new_filename_exists = check_new_filename_exists page filename in
              if new_filename_exists then begin
                let overwrite () =
                  editor#pages
                  |> List.find_map (fun p -> if p#get_filename = filename then Some (editor#close p) else None)
                  |> ignore;
                  Dialog_rename.rename ~editor ~page ~filename ();
                in
                Dialog_rename.ask_overwrite ~run ~overwrite ~filename window
              end else begin
                Dialog_rename.rename ~editor ~page ~filename ();
                window#destroy()
              end
            end else window#destroy()
        | _ -> window#destroy()
      in run()

(** dialog_save_as *)
let dialog_save_as ~editor ~page () =
  match page#file with
  | None -> ()
  | Some _ ->
      let window = GWindow.dialog
          ~icon:(??? Icons.oe) ~title:"Save as..."
          ~position:`CENTER ~modal:true ~show:false ()
      in
      let _ = GMisc.label ~text:(sprintf "Save remote file\n\n%s\n\nas:" page#get_title) ~xalign:0.0 ~packing:window#vbox#pack () in
      let entry = GEdit.entry ~text:page#get_filename ~packing:window#vbox#pack () in
      window#add_button_stock `OK `OK;
      window#add_button_stock `CANCEL `CANCEL;
      window#set_border_width 8;
      window#vbox#set_spacing 5;
      ignore (window#event#connect#key_release ~callback:begin fun ev ->
          let key = GdkEvent.Key.keyval ev in
          if key = GdkKeysyms._Escape then (window#response `CANCEL; true)
          else if key = GdkKeysyms._Return then begin
            window#response `OK;
            true
          end else false
        end);
      let rec run () =
        match window#run () with
        | `OK ->
            let filename = entry#text in
            if filename <> page#get_filename then begin
              let remote, new_filename_exists = check_new_filename_exists page filename in
              let buffer : GText.buffer = page#buffer#as_text_buffer#as_gtext_buffer in
              let text = buffer#get_text () in
              let create_file () =
                let open Editor_file_type in
                let newfile = new file ~host:remote.host ~user:remote.user ~pwd:remote.pwd
                  ~sslkey:remote.sslkey ~sshpublickeyfile:remote.sshpublickeyfile
                  ~sslkeypasswd:remote.sslkeypasswd
                  ~filename
                in
                newfile#write text;
                Dialog_save_as.sync_editor ~editor ~page ~filename window;
                newfile#cleanup();
              in
              if new_filename_exists then begin
                Dialog_rename.ask_overwrite ~run ~overwrite:create_file ~filename window
              end else begin
                create_file();
                window#destroy();
              end
            end else window#destroy()
        | _ -> window#destroy()
      in run()
