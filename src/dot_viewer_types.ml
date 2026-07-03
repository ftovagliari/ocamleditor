class virtual viewer =
  object
    method virtual display : filename:string -> unit
    method virtual coerce : GObj.widget
    method virtual destroy : unit -> unit
  end

module type DEVICE = sig
  val lang : string
  val draw : filename:string -> unit
end
