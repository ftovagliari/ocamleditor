open Margin
open Merlin_j
open GUtil
module ColorOps = Color
open Preferences
open Printf
open Cairo_drawable

let is_debug = false
let suppress_invisible = is_debug && false
let enable_transition = true

module Log = Common.Log.Make(struct let prefix = "FOLD" end)
let _ =
  Log.set_print_timestamp true;
  Log.set_verbosity (if is_debug then `DEBUG else `ERROR)

module Icons = struct
  let expander_open = "\u{f107}"
  let expander_closed = "\u{f105}"
end

exception Invalid_linechar of int * int * int * int

let counter = ref 0

let mk_polygon n r =
  let pi = 3.14189 in
  let m = float n in
  Array.make n r
  |> Array.mapi begin fun i r ->
    let i = float i in
    r *. cos (2. *. pi *. i /. m) |> int_of_float,
    r *. sin (2. *. pi *. i /. m) |> int_of_float
  end

let dot = mk_polygon 4 2.3 |> Array.to_list

(** Module for working with text regions defined by [GText.iter] pairs.

    A region represents a contiguous span of text bounded by two [GText.iter]
    positions. This module provides operations for containment checking,
    overlap detection, and region arithmetic (subtraction). *)
module Region = struct

  type t = GText.iter * GText.iter

  let to_string ((a, b) : t) = sprintf "(%d, %d)" a#offset b#offset

  (** [region |< container] checks if [region] is completely contained within [container]. *)
  let (|<) ((a, b) : t) ((start, stop) : t) = a#in_range ~start ~stop && b#in_range ~start ~stop

  (** [container <|| region] checks if [region] starts before
      [container] but overlaps with it (i.e., extends into [container]). *)
  let (<||) ((a, b) : t) ((start, stop) : t) = a#offset < start#offset && b#in_range ~start ~stop

  (** [container ||> region] checks if [region] starts within [container] but extends beyond it. *)
  let (||>) ((start, stop) : t) ((a, b) : t) = a#in_range ~start ~stop && stop#offset < b#offset

  let (!=) ((a, b) : t) ((c, d) : t) = not (a#equal c && b#equal d)

  (** [(--) minuend subtrahend] computes the difference between two regions,
        returning the parts of [minuend] not covered by [subtrahend].  *)
  let (--) (minuend : t) (subtrahend : t) =
    let mstart, mstop = minuend in
    let sstart, sstop = subtrahend in
    if subtrahend |< minuend then [ mstart, sstart; sstop, mstop ]
    else if minuend |< subtrahend then []
    else if subtrahend <|| minuend then [ sstop, mstop ]
    else if minuend ||> subtrahend then [ mstart, sstart ]
    else [ minuend ]

  (** [filter_maximal_regions regions] filters a list of regions to keep only the
      maximal ones - those that are not contained within any other region in the list.

      A region is considered maximal if no other region in the list contains it.
      This effectively removes redundant regions that are fully covered by larger ones. *)
  let filter_maximal_regions (regions: t list) : t list =
    List.filter begin fun reg ->
      not (List.exists (fun other -> reg != other && (reg |< other)) regions)
    end regions

  (** [difference container regions] computes the difference between a
      container region and a list of regions, returning the parts of the container
      that are not covered by any of the regions.

      This function takes a large region [container] and subtracts all the regions
      in the [regions] list from it, effectively finding the "gaps" or uncovered
      areas within the container.

      @note The function handles overlapping regions correctly by applying
          subtractions sequentially. Each subtraction may split existing
          regions into multiple parts. *)
  let difference (region: t) (regions : t list) : t list =
    regions
    |> List.fold_left begin fun remaining_regions region_to_subtract ->
      remaining_regions
      |> List.fold_left (fun acc region -> (region -- region_to_subtract) @ acc) []
    end [region]
end

type toggle_type = Expand | Collapse

class expander ~(view : Ocaml_text.view) ~tag_highlight ~tag_invisible ?packing () =
  let ebox = GBin.event_box ?packing () in
  let markup = if is_debug then sprintf "<span size='x-small'>%d</span>%s" !counter Icons.expander_open else Icons.expander_open in
  let label = Gtk_util.label_icon markup ~packing:ebox#add in
  let buffer = view#buffer in
  let id = !counter in
  let mark_folding_point =
    let m = buffer#create_mark ~name:(sprintf "fold-%d" id) buffer#start_iter in (* mark placed in the initializer *)
    GtkText.Mark.set_visible m is_debug;
    `MARK m
  in
  let mark_foot =
    let m = buffer#create_mark ~name:(sprintf "fold-foot-%d" id) buffer#start_iter in (* mark placed in the initializer *)
    GtkText.Mark.set_visible m is_debug;
    `MARK m
  in
  object (self)
    inherit GObj.widget ebox#as_widget

    (** The expander is invalid if the text it contains has changed and, as
        such, has already been destroyed *)
    val mutable is_valid = true
    val mutable is_expanded = true
    val mutable is_definition = false
    val mutable hash = 0
    val mutable y = 0

    val begin_expand = new begin_expand()
    val toggled = new toggled()
    val refresh_needed = new refresh_needed()
    val mutable contains_mark_occurrence = false

    initializer
      incr counter;
      view#add_child_in_window ~child:self#coerce ~which_window:`LEFT ~x:0 ~y:0;
      ebox#misc#set_property "visible-window" (`BOOL is_debug);
      ebox#event#connect#button_press ~callback:begin fun ev ->
        Log.println `DEBUG "BUTTON_PRESS %d %b" self#id is_expanded;
        Gmisclib.Idle.add ~prio:300 (fun () ->
            if is_expanded then self#collapse() else self#expand_node());
        false
      end |> ignore;
      (* Preview of the fold region when the mouse is over the expander. *)
      ebox#event#connect#enter_notify ~callback:begin fun ev ->
        Gdk.Window.set_cursor ebox#misc#window (Gdk.Cursor.create `HAND2);
        if is_expanded then buffer#apply_tag tag_highlight ~start:self#head ~stop:self#foot;
        false
      end |> ignore;
      ebox#event#connect#leave_notify ~callback:begin fun ev ->
        Gdk.Window.set_cursor ebox#misc#window (Gdk.Cursor.create `ARROW);
        if is_expanded then
          (* FIX add tag_highlight again to the nested expanders *)
          buffer#remove_tag tag_highlight ~start:self#head ~stop:self#foot;
        false
      end |> ignore;
      self#misc#connect#destroy ~callback:begin fun () ->
        Log.println `DEBUG "DESTROY %d" self#id;
        buffer#delete_mark mark_folding_point;
        buffer#delete_mark mark_foot
      end |> ignore

    method hash = hash

    method calculate_hash =
      Hashtbl.hash (buffer#get_text ~start:self#head ~stop:self#body ())

    method set_is_definition x = is_definition <- x
    method is_definition = is_definition

    method set_position () =
      let y_line, _ = view#get_line_yrange self#head in
      y <- y_line;

    method place_marks ~folding_point ~(foot : GText.iter) =
      buffer#move_mark mark_folding_point ~where:folding_point;
      let limit = if foot#ends_line then foot else foot#forward_to_line_end in
      let where =
        (* Handles definitions like "type...{...\n} and ..." or ";;" *)
        let foot =
          match foot#forward_search ~limit ";;" with
          | Some (_, it) -> it
          | _ -> foot
        in
        let it = foot#forward_find_char ~limit Text_util.not_blank in
        if it#ends_line then foot#forward_line#set_line_offset 0 else it
      in
      (* let ... in ... *)
      let where =
        let stop = where#forward_chars 2 in
        if where#get_text ~stop = "in" then begin
          let it = stop#forward_find_char ~limit Text_util.not_blank in
          if it#ends_line then it#forward_line#set_line_offset 0 else (where#set_line_offset 0)
        end else where
      in
      buffer#move_mark mark_foot ~where;
      let new_hash = self#calculate_hash in
      if hash <> 0 && new_hash <> hash then begin
        Gmisclib.Idle.add begin fun () ->
          self#invalidate();
          Log.println `DEBUG ">>>> refresh_needed ";
          refresh_needed#call()
        end
      end else hash <- new_hash

    (** Check if this expander is still valid (text hasn't changed invalidating it). *)
    method is_valid = is_valid

    method invalidate () =
      self#misc#hide();
      self#expand_node ~transition:false ();
      is_valid <- false;
      self#destroy();

      (** Get the unique identifier of this expander. *)
    method id = id

    (** The iter where the {i folding point} starts. It is between head and body. *)
    method folding_point = buffer#get_iter mark_folding_point

    (** The start of the region to highlight when the mouse pointer is over the expander. *)
    method private head = self#body#set_line_offset 0

    (** The start of the region to be made invisible when the expander is collapsed. *)
    method body = self#folding_point#forward_to_line_end

    (** The end of the foldable region. *)
    method foot = buffer#get_iter mark_foot

    method is_expanded = is_expanded
    method is_collapsed = not is_expanded

    (** Checks if the given iterator is within the body of this expander. *)
    method body_contains iter = self#body#compare iter <= 0 && iter#compare self#foot < 0

    (** An expander is visible if its start position does not have a tag with
        the invisible property, that is, if it is not contained in the body of
        another collapsed expander. *)
    method is_visible = not self#is_hidden
    method is_hidden = (self#body#set_line_offset 0)#has_tag tag_invisible

    method expand_node ?transition () =
      if self#is_collapsed then begin
        Log.println `DEBUG "EXPAND %d" self#id;
        let nested = ref [] in
        begin_expand#call nested;
        let segments =
          !nested
          |> List.filter_map (fun e -> if e#is_collapsed then Some (e#body#forward_line, e#foot) else None)
          |> Region.filter_maximal_regions
          |> List.map (fun seg ->
              Log.println `DEBUG "    nested_collapsed: %s" (Region.to_string seg);
              seg)
          |> Region.difference (self#body, self#foot)
        in
        label#set_label
          (if is_debug then sprintf "<span size='x-small'>%d</span>%s" id Icons.expander_open
           else sprintf "<big>%s</big>" Icons.expander_open);
        is_expanded <- true;
        self#show_region ?transition ~segments ~cont:begin fun () ->
          toggled#call Expand
        end ();
      end

    method collapse () =
      if self#is_expanded then begin
        Log.println `DEBUG "COLLAPSE %d" self#id;
        let iter = buffer#get_iter `INSERT in
        if iter#compare self#body > 0 && iter#compare self#foot <= 0 then
          buffer#place_cursor ~where:self#body;
        buffer#remove_tag tag_highlight ~start:self#head ~stop:self#foot;
        self#hide_region ~cont:begin fun () ->
          label#set_label
            (if is_debug then sprintf "<span size='x-small'>%d</span>%s" id Icons.expander_closed
             else sprintf "<big>%s</big>" Icons.expander_closed);
          is_expanded <- false;
          toggled#call Collapse
        end ();
      end

    method private transition_lines f start stop k =
      let start_line = start#line in
      let current_line = ref stop#line in
      let step () =
        if !current_line <= start_line then begin
          k();
          false
        end else begin
          let rem = !current_line - start_line in
          let chunk = max 1 (rem * 2 / 3) in
          let stop_l = !current_line in
          current_line := !current_line - chunk;
          let start_l = !current_line in
          let start =
            let next = buffer#get_iter (`LINE start_l) in
            if next#is_start then buffer#start_iter else next
          in
          let stop = buffer#get_iter (`LINE stop_l) in
          f ~start ~stop;
          true
        end
      in
      GMain.Timeout.add ~ms:25 ~callback:step |> ignore;

    method hide_region ?(cont=ignore) () =
      let hide = buffer#apply_tag tag_invisible in
      if enable_transition then
        self#transition_lines hide self#body#forward_line self#foot cont
      else begin
        hide ~start:self#body ~stop:self#foot;
        cont()
      end;
      buffer#apply_tag tag_highlight ~start:self#head ~stop:self#head#forward_to_line_end;

    method show_region ?(segments=[self#body#forward_line, self#foot]) ?(transition=true) ?(cont=ignore) () =
      Log.println `DEBUG "    show_region %d -- %s" self#id (Region.to_string (self#body, self#foot));
      let segments = segments |> List.sort (fun (a, _) (b, _) -> compare a#offset b#offset) in
      let show = buffer#remove_tag tag_invisible in
      if enable_transition && transition then begin
        segments
        |> List.iter (fun (start, stop) ->
            self#transition_lines show start stop cont)
      end else begin
        segments |> List.iter (fun (start, stop) -> show ~start ~stop);
        cont()
      end;
      buffer#remove_tag tag_highlight ~start:self#head ~stop:self#head#forward_to_line_end;

    method show (top : int) (left : int) (height : int) =
      Prf.register Prf.show_fold_expander begin fun () ->
        let y_view = y - top in
        if y_view < 0 || y_view > height || self#is_hidden then begin
          self#misc#hide()
        end else begin
          view#move_child ~child:self#coerce ~x:left ~y;
          self#misc#show()
        end
      end ()

    (** Sets whether this expander contains a marked occurrence.
        Used for highlighting expanders that contain search results. *)
    method set_contains_mark_occurrence value = contains_mark_occurrence <- value

    (** Returns true if the folded region contains a marked occurrence. *)
    method contains_mark_occurrence = contains_mark_occurrence

    method connect = new signals ~begin_expand ~toggled ~refresh_needed
  end

and toggled () = object inherit [toggle_type] signal () end
and begin_expand () = object inherit [expander list ref] signal () end
and refresh_needed () = object inherit [unit] signal () end
and signals ~begin_expand ~toggled ~refresh_needed =
  object
    inherit ml_signals [begin_expand#disconnect; toggled#disconnect; refresh_needed#disconnect]
    method begin_expand = begin_expand#connect ~after
    method toggled = toggled#connect ~after
    method refresh_needed = refresh_needed#connect ~after
  end

(** A class representing the folding margin for an editor view. This margin manages all the
    expander widgets and synchronizes them with the code structure using Merlin. *)
class margin_fold (outline : Oe.outline) (view : Ocaml_text.view) =
  let size = if is_debug then 30 else 13 in
  let spacing = 5 in
  let buffer = view#obuffer in
  let add_tag name properties =
    match GtkText.TagTable.lookup buffer#tag_table name with
    | Some tag -> new GText.tag tag
    | _ -> buffer#create_tag ~name properties
  in
  let color_expander = Preferences.editor_tag_color_name "comment" in
  let color_occurrences = `NAME ?? (Preferences.preferences#get.Settings_j.editor_mark_occurrences_bg_color) in
  let tag_highlight = add_tag Oe_config.code_folding_tag_highlight_name
      [ `PARAGRAPH_BACKGROUND (?? Oe_config.code_folding_highlight_color) ] in
  let tag_invisible = add_tag Oe_config.code_folding_tag_invisible_name
      (if suppress_invisible then [ `STRIKETHROUGH true ] else [ `INVISIBLE true ])
  in
  (** Traverses an outline structure in depth-first order, applying function [f] to each node.
      [f] receives the parent node (if any) and the current node as arguments. *)
  let rec iter_depth_first f parent (ol : Merlin_j.outline list) =
    match ol with
    | [] -> ()
    | hd :: tl ->
        f parent hd;
        iter_depth_first f (Some hd) hd.ol_children;
        iter_depth_first f parent tl
  in
  let char_width =
    let desc =
      (* TODO Update on preferences change *)
      Preferences.preferences#get.Settings_t.editor_base_font
      |> GPango.font_description_from_string
    in
    GPango.to_pixels (view#misc#pango_context#get_metrics ~desc ())#approx_digit_width
  in
  let (!!) pos = sprintf "(%d, %d)" pos.line (pos.col + 1) in
  object (self)
    inherit [unit] Margin.widget ()

    (** The margin is refresh pending when it is requested to be drawn but cannot be
        drawn because the buffer has changed and folding points currently
        cached have become invalid. The drawing of the margin is therefore
        postponed until the folding points are back in sync. *)
    val is_refresh_pending = new GUtil.variable false

    val mutable expanders : expander list = []
    val begin_expander_toggled = new begin_expander_toggled()
    val expander_toggled = new expander_toggled()
    val mutable signals = []

    method scope = Local
    method kind = FOLDING
    method color = "#505050"
    method index = 30
    method size = size

    method is_refresh_pending = is_refresh_pending

    method build ~start ~stop = ()

    method draw_margin ~view ~drawable ~top ~left ~height ~start ~stop =
      Prf.register Prf.draw_margin_fold begin fun () ->
        if outline#is_valid then begin
          try
            (*Log.println `DEBUG "%s: is_refresh_pending = %b, expanders = %d"
              __FUNCTION__ is_refresh_pending#get (List.length expanders);*)
            (* Separate the case in which markers and expanders need to be
               reconstructed from the case in which only the positions within the visible
               area need to be updated (for example in the case of scrolling). *)
            if is_refresh_pending#get || expanders = [] then begin
              Log.println `DEBUG "%s: is_refresh_pending = %b, expanders = %d"
                __FUNCTION__ is_refresh_pending#get (List.length expanders);
              let methods = ref [] in
              expanders <-
                List.filter_map begin fun ex ->
                  ex#misc#hide();
                  if ex#is_valid then Some ex else None
                end expanders;
              outline#get
              |> iter_depth_first begin fun parent ol ->
                (*Log.println `DEBUG "%s %s %s - %s" ol.ol_kind ol.ol_name !!(ol.ol_start) !!(ol.ol_stop);*)
                ol.ol_parent <- parent;
                if ol.ol_kind = "Method" then methods := ol :: !methods
                else self#get_or_create_expander ol top left height;
              end None;
              !methods |> List.iter (fun ol -> self#get_or_create_expander ol top left height);
              is_refresh_pending#set false;
              expanders |> List.iter begin fun ex ->
                let new_hash = ex#calculate_hash in
                (*Log.println `DEBUG "EXPANDER %d: reg=%s hash=%d %d"
                  ex#id (Region.to_string (ex#body, ex#foot))
                  ex#hash new_hash;*)
                if ex#hash <> new_hash then begin
                  ex#invalidate();
                  is_refresh_pending#set true;
                end
              end;
            end else
              expanders |> List.iter (fun ex -> ex#show top left height);
          with Invalid_linechar (ln, el, cn, cl) ->
            Log.println `ERROR "===>> Invalid_linechar (%d/%d, %d/%d) <<===" ln el cn cl;
            is_refresh_pending#set true
        end else begin
          Log.println `DEBUG "outline is invalid (buffer is changed after last update or the outline timer is off)";
          is_refresh_pending#set true
        end
      end ()

    method private get_iter_at_pos (pos : Merlin_j.pos) =
      let ln = pos.line - 1 in
      if ln < 0 || ln > buffer#end_iter#line then
        raise (Invalid_linechar (pos.line, buffer#end_iter#line, pos.col, -99));
      let iter_line = buffer#get_iter (`LINE ln) in
      if pos.col >= iter_line#chars_in_line then
        raise (Invalid_linechar (pos.line, -99, pos.col, iter_line#chars_in_line));
      buffer#get_iter (`LINECHAR (ln, pos.col))

    method private get_or_create_expander ol top left height =
      let start = self#get_iter_at_pos ol.ol_start in
      let stop = self#get_iter_at_pos ol.ol_stop in
      if stop#line > start#line then begin
        let expander =
          match expanders |> List.find_opt (fun exp -> exp#folding_point#equal start) with
          | None ->
              let expander = new expander ~tag_highlight ~tag_invisible ~view () in
              expander#place_marks ~folding_point:start ~foot:stop;
              expanders <- expander :: expanders;
              expander#show_region();
              expander#connect#after#begin_expand ~callback:(fun nested ->
                  begin_expander_toggled#call (expander, nested)) |> ignore;
              expander#connect#after#toggled ~callback:(fun _ -> expander_toggled#call expander) |> ignore;
              expander#connect#refresh_needed ~callback:(fun () -> is_refresh_pending#set true) |> ignore;
              expander
          | Some expander ->
              expander
        in
        expander#set_is_definition
          (ol.ol_kind = "Value" && (Oe_config.code_folding_deep_collapse || ol.ol_parent <> None)
           || ol.ol_kind = "Method");
        expander#set_position ();
        (* Hide expanders inside invisible regions *)
        if expander#is_hidden
        then expander#misc#hide()
        else expander#show top left height
      end

    method find_nested_expanders (expander : expander) =
      let all_nested =
        expanders
        |> List.filter (fun exp -> exp#is_valid && expander#body_contains exp#body)
        |> List.filter (fun exp -> exp#id <> expander#id)
      in
      all_nested
    (*|> List.filter (fun exp ->
        not (all_nested
             |> List.filter (fun x -> x#id <> exp#id)
             |> List.exists (fun x -> x#body_contains exp#body)))*)

    method draw_ellipsis _ =
      match view#get_window `TEXT with
      | Some window ->
          let drawable = Gdk.Cairo.create window in
          set_line_attributes drawable ~width:1 ~style:`SOLID ();
          set_foreground drawable color_expander;
          let vrect = view#visible_rect in
          let y0 = Gdk.Rectangle.y vrect in
          expanders
          |> List.iter begin fun expander ->
            if expander#is_collapsed && expander#is_visible then
              let start = expander#body in
              let y, height = view#get_line_yrange start in
              let x_chars = start#line_index in
              let x = (x_chars + 1) * char_width in
              let y = y - y0 + 1 in
              let height = height - 4 in (* do not overlap current line border *)
              let width = height * 8 / 5 in
              if expander#contains_mark_occurrence then begin
                set_foreground drawable color_occurrences;
                rectangle drawable ~x ~y ~filled:true ~width ~height ();
                set_foreground drawable color_expander;
              end;
              rectangle drawable ~x ~y ~filled:false ~width ~height ();
              let h3 = height / 3 in
              let x = x + width / 2 in
              let y = y + h3 + h3 in
              dot |> List.map (fun (xd, yd) -> x + xd - h3, y + yd) |> polygon drawable ~filled:true;
              dot |> List.map (fun (xd, yd) -> x + xd,      y + yd) |> polygon drawable ~filled:true;
              dot |> List.map (fun (xd, yd) -> x + xd + h3, y + yd) |> polygon drawable ~filled:true;
          end;
          false
      | _ -> false

    method iter_expanders func = List.iter func expanders

    method private connect_signals () =
      signals <- [
        `VIEW (view#misc#connect#after#draw ~callback:self#draw_ellipsis);
        `BUFFER (buffer#connect#mark_set ~callback:begin fun _ mark ->
            match GtkText.Mark.get_name mark with
            | Some "insert" ->
                expanders
                |> List.iter begin fun exp ->
                  if exp#is_collapsed then begin
                    let iter = buffer#get_iter `INSERT in
                    if iter#line > exp#body#line && exp#body_contains iter
                    then exp#expand_node ?transition:(Some true) ()
                  end
                end
            | _ -> ()
          end);
      ]

    method private disconnect_signals () =
      signals |> List.iter (function
          | `VIEW sign -> GtkSignal.disconnect view#as_view sign
          | `BUFFER sign -> GtkSignal.disconnect buffer#as_buffer sign);
      signals <- [];

    method enable () =
      self#set_is_visible true;
      self#connect_signals();

    method disable () =
      self#set_is_visible false;
      expanders |> List.iter (fun exp -> exp#expand_node ?transition:(Some false) ());
      expanders |> List.iter (fun exp -> exp#invalidate());
      expanders <- [];
      is_refresh_pending#set false;
      self#disconnect_signals();

    method private configure is_enabled =
      if is_enabled then self#enable() else self#disable()

    method connect = new margin_signals ~begin_expander_toggled ~expander_toggled

    initializer
      self#set_is_visible Preferences.preferences#get.Settings_j.editor_code_folding_enabled;
      Preferences.preferences#connect#changed ~callback:begin fun pref ->
        self#configure pref.Settings_j.editor_code_folding_enabled
      end |> ignore;
      self#connect#expander_toggled ~callback:(fun _ -> is_refresh_pending#set true) |> ignore;
      outline#connect#changed ~callback:begin fun _ ->
        is_refresh_pending#set true;
        Log.println `DEBUG "CHANGED %b" is_refresh_pending#get;
        Gmisclib.Idle.add ~prio:100 begin fun () ->
          view#build_gutter(); (* triggers draw *)
        end;
      end |> ignore;
      if is_debug then
        is_refresh_pending#connect#changed ~callback:begin fun value ->
          Log.println `DEBUG "is_refresh_pending <- %b" value;
        end |> ignore;
      self#configure self#is_visible;
  end

and begin_expander_toggled () = object inherit [expander * expander list ref] signal () end
and expander_toggled () = object inherit [expander] signal () end
and margin_signals ~begin_expander_toggled ~expander_toggled =
  object
    inherit ml_signals [begin_expander_toggled#disconnect; expander_toggled#disconnect]
    method begin_expander_toggled = begin_expander_toggled#connect ~after
    method expander_toggled = expander_toggled#connect ~after
  end

let pages : (int * margin_fold) list ref = ref []

let init_page (page : Editor_page.page) =
  try
    page#margin_manager#get FOLDING
    |> begin function
    | None ->
        let outline = page#outline in
        let margin = new margin_fold outline page#ocaml_view in
        page#margin_manager#add (margin :> Margin.margin);
        margin#connect#begin_expander_toggled ~callback:(fun (expander, nested) ->
            nested := margin#find_nested_expanders expander) |> ignore;
        margin#connect#expander_toggled ~callback:begin fun _ ->
          page#margin_manager#build();
          Gmisclib.Idle.add page#margin_manager#draw;
        end |> ignore;
        page#misc#connect#destroy ~callback:begin fun () ->
          pages := List.filter begin fun (oid, margin) ->
              page#margin_manager#remove (margin :> Margin.margin);
              oid <> page#misc#get_oid
            end !pages
        end |> ignore;
        pages := (page#misc#get_oid, margin) :: !pages;
        (* Highlight expanders that contain marked occurrences *)
        page#mark_occurrences_manager#connect#mark_set ~callback:begin fun () ->
          margin#iter_expanders begin fun exp ->
            exp#set_contains_mark_occurrence false;
            if not exp#is_expanded then begin
              if page#mark_occurrences_manager#words
                |> List.exists (fun (m1, _) ->
                    exp#body_contains (page#buffer#get_iter_at_mark m1))
              then exp#set_contains_mark_occurrence true;
            end
          end
        end |> ignore;
        (*margin#connect#after#expander_toggled ~callback:begin fun _ ->
          outline#refresh();
          Gmisclib.Idle.add ~prio:300 begin fun () ->
            Printf.printf "*** 1\n%!" ;
            page#margin_manager#draw();
            Printf.printf "*** 2\n%!" ;
          end
          end |> ignore;*)
        (* Remove highlights from all expanders when there are no marked occurrences *)
        (*page#view#mark_occurrences_manager#connect#mark_set ~callback:begin fun () ->
          match page#view#mark_occurrences_manager#words with
          | [] -> GtkBase.Widget.queue_draw page#view#as_widget
          | _ -> ()
          end |> ignore;*)
        (* Disable while page is loading. *)
        page#connect#load ~callback:begin
          let old_is_visible = ref margin#is_visible in
          function
          | `Begin ->
              old_is_visible := margin#is_visible;
              margin#disable()
          | `End when !old_is_visible ->
              Gmisclib.Idle.add ~prio:300 begin fun () ->
                margin#enable();
                page#view#build_gutter();
                (*GtkBase.Widget.queue_draw page#view#as_widget;*)
              end
          | _ -> ()
        end |> ignore;
    | _ -> ()
    end
  with ex ->
    Printf.eprintf "File \"margin_fold.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace())

let iter_page_expanders (page : Editor_page.page) callback =
  match !pages |> List.assoc_opt page#misc#get_oid with
  | Some margin -> margin#iter_expanders callback
  | _ -> ()

let collapse_to_definitions (page : Editor_page.page) =
  iter_page_expanders page (fun exp -> if exp#is_definition then exp#collapse ())

let expand_all (page : Editor_page.page) =
  iter_page_expanders page (fun exp -> exp#expand_node())

let init_editor editor =
  editor#connect#add_page ~callback:init_page |> ignore;
  (*  editor#connect#remove_page ~callback:begin fun page ->
      pages := List.filter begin fun (oid, margin) ->
          page#view#margin#remove (margin :> Margin.margin);
          oid <> page#misc#get_oid
        end !pages
      end |> ignore*)

