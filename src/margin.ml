module ColorOps = Color

type scope = Local | Global [@@deriving show]

type kind = FOLDING | LINE_NUMBERS | MARKERS | DIFF | GLOBAL_DIFF | ERRORS | OCCURRENCES | CURRENT_LINE [@@deriving show]

type overview_ruler_mode = Integrated | Separated

let text_pixel_size ?layout font_desc text =
  let layout =
    match layout with
    | Some layout -> layout
    | None ->
        let surface = Cairo.Image.create Cairo.Image.ARGB32 ~w:1 ~h:1 in
        let dummy_cr = Cairo.create surface in
        Cairo_pango.create_layout dummy_cr
  in
  Pango.Layout.set_font_description layout font_desc;
  Pango.Layout.set_text layout text;
  let rect = Pango.Layout.get_pixel_extent layout in
  let width = rect.Pango.x + rect.Pango.width in
  let height = rect.Pango.y + rect.Pango.height in
  (width, height)

type 'a click_signal = ?after:bool -> (GdkEvent.Button.t -> unit) -> GtkSignal.id

class virtual margin () =
  object (self)
    val click = new click
    val mouseover = new mouseover
    val mouseout = new mouseout
    val mutable is_visible = true
    method virtual index : int
    method virtual scope : scope
    method virtual kind : kind
    method virtual color : string
    method virtual build : start:GText.iter -> stop:GText.iter -> unit
    method is_visible = is_visible
    method set_is_visible x = is_visible <- x
    method name = show_kind self#kind
    method virtual size : int (* width of the margin in pixels *)

    (** Called whenever the margin needs to be redrawn.
        @param top The top edge of the margin area to be drawn, in buffer coordinates.
    *)
    method virtual draw_margin :
      view:GText.view ->
      drawable:Gdk.cairo ->
      top:int -> left:int -> height:int ->
      start:GText.iter -> stop:GText.iter -> unit

    method find_nearest_iter_at_y (y : float) = (None : GText.iter option)

    method trigger = object
      method click = click#call
      method mouseover = mouseover#call
      method mouseout = mouseout#call
    end

    method event = new signals ~click ~mouseover ~mouseout
  end

and signals ~click ~mouseover ~mouseout = object
  inherit GUtil.ml_signals [ click#disconnect; mouseover#disconnect; mouseout#disconnect ]
  method click = click#connect ~after
  method mouseover = mouseover#connect ~after
  method mouseout = mouseout#connect ~after
end

and click = object inherit [GText.iter] GUtil.signal () end
and mouseover = object inherit [GdkEvent.Crossing.t] GUtil.signal () end
and mouseout = object inherit [GdkEvent.Crossing.t] GUtil.signal () end

class virtual ['a] widget () =
  object
    inherit margin ()
    val mutable model : (int * ' a) list = []
  end
