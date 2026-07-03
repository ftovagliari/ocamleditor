let locate ~filename ~text ~(iter : GText.iter) =
  Merlin.locate ~position:(iter#line + 1, iter#line_offset)
    ~filename
    ~buffer:text
    ~look_for:`Implementation ()
  |> Async.run_synchronously
  |> begin function
  | Merlin.Ok result ->
      begin
        match result with
        | `String msg ->
            Merlin.Error msg
        | `Assoc assoc ->
            let file, ln, col = ref None, ref 0, ref 0 in
            assoc |> List.iter (function
                | "file", `String x -> file := Some x
                | "pos", `Assoc ["line", `Int l; "col", `Int c]
                | "pos", `Assoc ["col", `Int c; "line", `Int l] -> ln := l - 1; col := c
                | _ -> ());
            let stop_col = !col(* iter#forward_word_end #line_offset - 1*) in
            let start = { Merlin_t.line = !ln; col = !col } in
            let stop = { Merlin_t.line = !ln; col = stop_col } in
            Merlin.Ok (Some { Merlin_t.file = !file; start; stop } )
        | _ ->
            Merlin.Ok None
      end
  | Merlin.Failure msg
  | Merlin.Error msg -> Merlin.Error msg
  end

let references ~filename ~text ~(iter : GText.iter) =
  let line = iter#line + 1 in
  let col = iter#line_offset in
  Merlin.occurrences ~identifier_at:(line, col) ~scope:`Project ~filename ~buffer:text ()
  |> Async.map ~name:"references"
    (function Merlin.Ok ranges -> ranges | Merlin.Failure _ | Merlin.Error _ -> [])

