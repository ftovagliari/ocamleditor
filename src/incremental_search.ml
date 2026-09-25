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
open Printf
module ColorOps = Color

let set_last_incremental = ref (fun text regexp -> failwith "set_last_incremental")

let create_glyph_pixbuf height color ~slashed text =
  let font_size = height * 62 / 100 in
  let font_desc = Pango.Font.from_string (sprintf "FiraCode OCamlEditor Bold %d" font_size) in

  let surface = Cairo.Image.create Cairo.Image.ARGB32 ~w:height ~h:height in
  let cr = Cairo.create surface in
  let layout = Cairo_pango.create_layout cr in
  Pango.Layout.set_font_description layout font_desc;
  Pango.Layout.set_text layout text;
  let w, h = Pango.Layout.get_pixel_size layout in

  let r = (Gdk.Color.red color |> float) /. 65535.0 in
  let g = (Gdk.Color.green color |> float) /. 65535.0 in
  let b = (Gdk.Color.blue color |> float) /. 65535.0 in

  Cairo.set_source_rgba cr r g b 1.0;
  if slashed then begin
    Cairo.move_to cr 0. (float h);
    Cairo.line_to cr (float w) 0.;
    Cairo.set_line_width cr 2.0;
    Cairo.stroke cr;
  end;
  Cairo_pango.show_layout cr layout;

  let data = Cairo.Image.get_data8 surface in
  let region = Gpointer.region_of_bigarray data in
  GdkPixbuf.from_data
    region
    ~has_alpha:true
    ~bits:8
    ~width:height
    ~height
    ~rowstride:(Cairo.Image.get_stride surface)

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
                (*view#scroll_aligned i1;*)
                view#scroll_to_iter  i1 |> ignore;
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
        (*view#scroll_aligned start;*)
        view#scroll_to_iter ~within_margin:0.1 start |> ignore;

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
      fun ~view ~project () ->
        status#set_case_sensitive false;
        status#set_match_whole_word false;
        status#set_use_regexp true;
        status#set_i_search true;
        status#set_text_find "";
        status#set_project project;
        self#set_view (Some view);

        let ebox = GBin.event_box ~border_width:0 ~show:true () in
        ebox#misc#set_property "visible-window" (`BOOL true);
        let box = GPack.hbox ~spacing:0 ~border_width:0 ~packing:ebox#add ~show:true () in
        box#misc#style_context#add_class "incremental-search";
        box#misc#style_context#add_class "incremental-search-hidden";
        let child = ebox#coerce in
        let search ?(inc=false) (dir : [`BACKWARD | `FORWARD]) =
          begin
            match dir with
            | `BACKWARD -> status#set_backward true
            | `FORWARD -> status#set_backward false
          end;
          status#set_incremental inc;
          self#find ~control:STOP ~view ()
        in
        let _ = GMisc.label ~markup:"<b>Search for: </b>\n<span size='48%'>Ctrl+F for Find/Replace</span>"
            ~xalign:0.0 ~xpad:0 ~packing:(box#pack ~expand:true ~fill:true) () in
        let entry = GEdit.entry ~width_chars:20 ~packing:(box#pack ~expand:false ~fill:false) () in
        entry#connect#changed ~callback:begin
          let prev = ref entry#text in
          let locked = ref false in fun () ->
            if not !locked then begin
              status#set_text_find (entry#text);
              if search ~inc:true `FORWARD then prev := entry#text
              else begin
                let t = if String.length entry#text > String.length !prev
                  then !prev else entry#text in
                locked := true;
                locked := false;
                status#set_text_find t;
              end;
            end
        end |> ignore;
        entry#event#connect#key_press ~callback:
          begin fun ev ->
            let keyval = GdkEvent.Key.keyval ev in
            if keyval = _Left || keyval = _Right || keyval = _Escape then (child#destroy(); true)
            else if keyval = _Up then (search `BACKWARD |> ignore; true)
            else if keyval = _Down then (search `FORWARD |> ignore; true)
            else false;
          end |> ignore;
        child#misc#connect#destroy ~callback:
          begin fun () ->
            Gmisclib.Idle.add ~prio:200 view#misc#grab_focus;
            status#set_incremental !inc;
            status#set_backward !old_back;
            status#set_i_search false;
            status#set_case_sensitive !old_case;
            status#set_match_whole_word !old_match_whole_word;
            status#set_use_regexp !old_regexp;
          end |> ignore;
        let sign_id = ref None in
        sign_id := Some (view#event#connect#focus_in ~callback:begin fun _ ->
            child#destroy();
            Option.iter (GtkSignal.disconnect view#as_widget) !sign_id;
            sign_id := None;
            false
          end);
        let y = 0 in
        view#add_child_in_window ~child ~which_window:`WIDGET ~x:0 ~y;
        child#misc#connect#size_allocate ~callback:begin fun alloc ->
          let r = view#visible_rect in
          let scrollbar_width = 25 in
          let x =
            view#get_border_window_size `LEFT +
            Gdk.Rectangle.width r -
            child#misc#allocation.Gtk.width -
            scrollbar_width
          in
          Gmisclib.Idle.add begin fun () ->
            view#move_child ~child ~x ~y;
            box#misc#style_context#add_class "incremental-search-visible";
          end;
        end |> ignore;
        entry#set_secondary_icon_activatable true;
        let set_secondary_icon ?(alloc=entry#misc#allocation) () =
          let style = entry#misc#style in
          let color = style#fg `NORMAL in
          let icon = create_glyph_pixbuf alloc.Gtk.height color ~slashed:(not status#case_sensitive) "\u{eab1}" in
          entry#set_secondary_icon_pixbuf icon;
        in
        let sign_id = ref None in
        sign_id := Some (entry#misc#connect#size_allocate ~callback:begin fun alloc ->
            Gmisclib.Idle.add begin fun () ->
              set_secondary_icon ~alloc ();
              Option.iter (GtkSignal.disconnect entry#as_widget) !sign_id;
              sign_id := None
            end;
          end);
        entry#connect#icon_press ~callback:begin fun pos _ ->
          match pos with
          | `PRIMARY -> ()
          | `SECONDARY ->
              status#set_case_sensitive (not status#case_sensitive);
              set_secondary_icon ();
        end |> ignore;
        Gmisclib.Idle.add ~prio:300 entry#misc#grab_focus;
        child

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
      | STOP_AFTER _ -> false

    method connect = new signals ~found:signal_found
  end

and signals ~found = object
  inherit GUtil.ml_signals [found#disconnect]
  method found ~callback = found#connect ~after ~callback
end




















