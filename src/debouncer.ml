type debouncer = {
  delay : int;
  mutable timer_id : Glib.Timeout.id option;
}

let create ~ms = { delay = ms; timer_id = None }

let schedule debouncer action =
  Option.iter Glib.Timeout.remove debouncer.timer_id;
  let id =
    Glib.Timeout.add ~ms:debouncer.delay ~callback:begin fun () ->
      action ();
      debouncer.timer_id <- None;
      false
    end
  in
  debouncer.timer_id <- Some id
