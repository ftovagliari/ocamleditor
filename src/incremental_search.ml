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

open GdkKeysyms
open GUtil
open Printf
module ColorOps = Color
open Preferences

let set_last_incremental = ref (fun text regexp -> failwith "set_last_incremental")

(* Options *)
class status () =
  object (self)
    val mutable project : Prj.t option = None
    val mutable text_find = ""
    val mutable text_replace = ""
    val mutable backward = false;
    val mutable incremental = false;
    val mutable i_search = false;
    val mutable use_regexp = false;
    val mutable case_sensitive = false;
    val mutable match_whole_word = false;
    val mutable history_find : string list = []
    val mutable history_replace : string list = []
    val mutable view = None
    method backward = backward
    method incremental = incremental
    method i_search = i_search
    method use_regexp = use_regexp
    method case_sensitive = case_sensitive
    method match_whole_word = match_whole_word
    method set_backward v = backward <- v
    method set_incremental v = incremental <- v
    method set_i_search v = i_search <- v
    method set_use_regexp v = use_regexp <- v
    method set_case_sensitive v = case_sensitive <- v
    method set_match_whole_word v = match_whole_word <- v
    method history_find = history_find
    method history_replace = history_replace
    method text_find = text_find
    method text_replace = text_replace
    method set_text_find t = text_find <- t
    method set_text_replace t = text_replace <- t
    method update_history () =
      if not incremental then begin
        let txt = text_find in
        if not (List.mem txt history_find) then history_find <- txt :: history_find;
        let txt = text_replace in
        if not (List.mem txt history_replace) then history_replace <- txt :: history_replace;
      end else begin
        !set_last_incremental text_find (Str.regexp text_find);
      end
    method set_project x = project <- Some x
  end

type not_found_control = NOT_EXISTS | SEARCH_TO_BOTTOM | SEARCH_FROM_TOP | STOP | STOP_AFTER of int
type search_result = {pos : int; text : string}

(* dialog *)
class incremental () =
  let status = new status () in
  let signal_found = new GUtil.signal () in
  object (self)
    val mutable view : Text.view option = None
    val mutable window = None
    val mutable replace_window = None

    method private view = view
    method private set_view v = view <- v

    method private find ?view ?(control=SEARCH_TO_BOTTOM) () =
      Gaux.may view ~f:(fun _ -> self#set_view view);
      match self#view with
      | None -> false
      | Some v ->
          if status#text_find <> "" then begin
            status#update_history();
            let buf = v#buffer in
            let ins = buf#get_iter_at_mark `INSERT in
            let sel = buf#get_iter_at_mark `SEL_BOUND in
            let start =
              if ((ins#compare sel) < 0 && not status#backward)
              || ((sel#compare ins) < 0 && status#backward) then sel else ins in
            let start =
              if status#incremental && start = sel then ins
              else if status#incremental && start = ins then sel
              else start
            in
            let found =
              if status#use_regexp then self#find_regexp ~control v start
              else self#find_string ~control v start;
            in
            if found then (signal_found#call v);
            found
          end else begin
            if not status#i_search then (assert false) (*(self#show ~view:v ())*);
            false;
          end

    method private find_string ~control (view : Text.view) start =
      let buffer = view#buffer in
      let search = if status#backward then start#backward_search else start#forward_search in
      let text = status#text_find in
      match search text with
      | None -> self#not_found text control view
      | Some (i1, i2) ->
          begin
            match control with
            | STOP_AFTER bound when i1#offset >= bound ->
                false
            | _ ->
                view#scroll_lazy i1;
                (*Gmisclib.Idle.add (fun () -> ignore (view#scroll_to_iter ~use_align:true ~xalign:1.0 ~yalign:0.5 i1));*)
                if status#backward then buffer#select_range i2 i1
                else buffer#select_range i1 i2;
                true
          end

    method private find_regexp ~control (view : Text.view) start =
      let text_to_find = status#text_find in
      let buffer = view#buffer in
      try
        let ins = if status#backward then start#backward_char else start in
        let pos = ins#offset in
        let text = buffer#get_text () in
        let pat = if status#case_sensitive then Str.regexp text_to_find
          else Str.regexp_case_fold text_to_find in
        let pos = if status#backward then Str.search_backward pat text pos
          else Str.search_forward pat text pos in
        let start = buffer#get_iter_at_char pos in
        let stop = buffer#get_iter_at_char (Str.match_end()) in
        view#scroll_lazy start;
        (*Gmisclib.Idle.add (fun () -> ignore (view#scroll_to_iter ~use_align:true ~xalign:1.0 ~yalign:0.5 start));*)
        if status#backward then buffer#select_range stop start
        else buffer#select_range start stop;
        true
      with
      | Not_found -> self#not_found status#text_find control view
      | _ -> false

    method i_search =
      let inc = ref status#incremental in
      let old_back = ref status#backward in
      let old_case = ref status#case_sensitive in
      let old_match_whole_word = ref status#match_whole_word in
      let old_regexp = ref status#use_regexp in
      fun ?(full_find : (Gdk.Tags.modifier list * Gdk.keysym * (unit -> unit)) option) ~view ~project () ->
        status#set_case_sensitive false;
        status#set_match_whole_word false;
        status#set_use_regexp true;
        status#set_i_search true;
        status#set_text_find "";
        status#set_project project;
        self#set_view (Some view);
        let dialog =
          match Sys.os_type with
          | _ -> GWindow.window ~allow_grow:true
                   ?type_hint:(Some `DIALOG)
                   ~decorated:false ~modal:false ~border_width:1 ()
        in
        dialog#set_skip_taskbar_hint true;
        dialog#set_skip_pager_hint true;
        let move () =
          (* Coordinate del puntatore relative al desktop *)
          let pX, pY = Gdk.Window.get_pointer_location (Gdk.Window.root_parent ()) in
          (* Coordinate del puntatore relative alla vista *)
          let win = (match view#get_window `WIDGET
                     with None -> failwith "Incremental_search.i_search: view#get_window `WIDGET = None" | Some w -> w) in
          let px, py = Gdk.Window.get_pointer_location win in
          dialog#move ~x:(pX - px + view#misc#allocation.Gtk.width - dialog#misc#allocation.Gtk.width - 5) ~y:(pY - py + 5);
        in
        let search ?(inc=false) (dir : [`BACKWARD | `FORWARD]) =
          begin
            match dir with
            | `BACKWARD -> status#set_backward true
            | `FORWARD -> status#set_backward false
          end;
          status#set_incremental inc;
          self#find ~control:STOP ~view ()
        in
        let ebox = GBin.event_box ~border_width:0 ~packing:dialog#add () in
        let box = GPack.hbox ~spacing:0 ~border_width:5 ~packing:ebox#add () in
        dialog#misc#modify_bg [`NORMAL, `COLOR (dialog#misc#style#bg `SELECTED)];
        let _ =
          if not Oe_config.use_theme_colors_when_possible then
            ebox#misc#modify_bg [`NORMAL, `NAME ?? (Preferences.preferences#get.editor_bg_color_popup)] in
        let lab = GMisc.label ~markup:"<b><big>Search for: </big></b>\n<span size='xx-small'>Ctrl+F for Find/Replace</span>"
            ~xalign:0.0 ~xpad:0 ~packing:(box#pack ~expand:true ~fill:true) () in
        let e = GEdit.entry ~packing:(box#pack ~expand:false ~fill:false) () in
        e#connect#changed ~callback:begin
          let prev = ref e#text in
          let locked = ref false in fun () ->
            if not !locked then begin
              status#set_text_find (e#text);
              if search ~inc:true `FORWARD then prev := e#text
              else begin
                let t = if String.length e#text > String.length !prev
                  then !prev else e#text in
                locked := true;
                locked := false;
                status#set_text_find t;
              end;
            end
        end |> ignore;
        dialog#event#connect#focus_in ~callback:begin fun _ ->
          move();
          false
        end |> ignore;
        dialog#event#connect#focus_out ~callback:begin fun _ ->
          dialog#destroy();
          true
        end |> ignore;
        dialog#event#connect#key_press ~callback:
          begin fun ev ->
            let state = GdkEvent.Key.state ev |> List.sort compare in
            let keyval = GdkEvent.Key.keyval ev in
            match full_find with
            | Some (modi, key, find_func)
              when state = (modi |> List.sort compare) && keyval = key ->
                dialog#destroy();
                find_func ();
                true
            | _ ->
                if keyval = _Left || keyval = _Right || keyval = _Escape then (dialog#destroy(); true)
                else if keyval = _Up then (move(); search `BACKWARD |> ignore; true)
                else if keyval = _Down then (move(); search `FORWARD |> ignore; true)
                else false;
          end |> ignore;
        dialog#connect#destroy ~callback:
          begin fun () ->
            status#set_incremental !inc;
            status#set_backward !old_back;
            status#set_i_search false;
            status#set_case_sensitive !old_case;
            status#set_match_whole_word !old_match_whole_word;
            status#set_use_regexp !old_regexp;
          end |> ignore;
        dialog#show();
        e#misc#grab_focus()

    method private not_found text choice view =
      match choice with
      | NOT_EXISTS ->
          let s = if status#use_regexp then "Regexp" else "String" in
          Dialog.info view ~message:(s ^ " \"" ^ text ^ "\" not found.");
          false
      | SEARCH_TO_BOTTOM ->
          Gmisclib.Idle.add (fun () -> ignore (view#scroll_to_iter (view#buffer#get_iter (if status#backward then `START else `END))));
          let message = GWindow.message_dialog
              ~message:(if status#backward then "Top reached searching \""^text^"\" backward.\nSearch from the end?"
                        else "Bottom reached searching \""^text^"\" forward.\nSearch from the beginning?")
              ~modal:true ~position:`CENTER
              ~message_type:`QUESTION ~buttons:(GWindow.Buttons.yes_no) () in
          begin match window with
          | None -> Gaux.may (GWindow.toplevel view)
                      ~f:(fun x -> message#set_transient_for x#as_window)
          | Some w -> message#set_transient_for w#window#as_window
          end;
          let response = message#run() in
          message#destroy();
          begin match response with
          | `YES -> self#not_found text SEARCH_FROM_TOP view
          | _ -> false
          end;
      | SEARCH_FROM_TOP ->
          let top = if status#backward then view#buffer#get_iter `END
            else view#buffer#get_iter `START in
          view#buffer#select_range top top;
          self#find ~control:NOT_EXISTS ();
      | STOP -> false
      | _ -> false

    method connect = new signals ~found:signal_found
  end

and signals ~found = object
  inherit GUtil.ml_signals [found#disconnect]
  method found ~callback = found#connect ~after ~callback
end




















