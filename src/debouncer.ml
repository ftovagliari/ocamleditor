type debouncer = {
  delay : int;
  enabled : bool;
  mutable timer_id : Glib.Timeout.id option;
}

let create ?(enabled=true) ~ms () = { delay = ms; timer_id = None; enabled }

let schedule debouncer action =
  (*if debouncer.enabled then begin*)
  Option.iter Glib.Timeout.remove debouncer.timer_id;
  let id =
    Glib.Timeout.add ~ms:debouncer.delay ~callback:begin fun () ->
      action ();
      debouncer.timer_id <- None;
      false
    end
  in
  debouncer.timer_id <- Some id
(*end*)
