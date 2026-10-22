open Merlin_t

module Log = Common.Log.Make(struct let prefix = "ENCL_EXPR" end)
let _ = Log.set_verbosity `ERROR

let iters_of_range buffer range =
  let start = buffer#get_iter (`LINECHAR (range.start.line - 1, range.start.col)) in
  let stop = buffer#get_iter (`LINECHAR (range.stop.line - 1, range.stop.col)) in
  let length = abs (stop#offset - start#offset) in
  start, stop, length

class manager ~ocaml_view =
  let (<<) (lower_bound, upper_bound) value = min (max lower_bound value) upper_bound in
  let filename = match ocaml_view#obuffer#file with Some file -> file#filename | _ -> "" in
  object (self)
    val mutable ranges = [||]
    val mutable index = 0
    val mutable signal_ids : (GtkSignal.id * (GtkSignal.id -> unit)) list = []
    val mutable is_rev_order = false

    method private select buffer =
      match ranges with
      | ranges when 0 <= index && index < Array.length ranges ->
          index <- (0, (Array.length ranges)) << (index + (if is_rev_order then -1 else 1));
          let start, stop = ranges.(index) in
          buffer#select_range start stop;
      | _ -> ()

    method private add_signal disconnect sid =
      signal_ids <- (sid, disconnect) :: signal_ids

    method private remove_signal sid =
      match signal_ids |> List.find_opt (fun (id, _) -> id = sid) with
      | Some (sid, disconnect) ->
          disconnect sid;
          signal_ids <- signal_ids |> List.filter (fun (id, _) -> id <> sid);
      | _ -> ()

    method private reset () =
      Log.println `DEBUG "RESET %s" filename;
      ranges <- [||];
      index <- 0;
      is_rev_order <- false;
      signal_ids |> List.iter (fun (sid, _) -> self#remove_signal sid);

    method start ~(iter : GText.iter) =
      let buffer : GText.buffer = ocaml_view#obuffer#as_gtext_buffer in
      match ranges with
      | [||] ->
          let extra =
            let ins, sel = buffer#selection_bounds in
            let length = abs (sel#offset - ins#offset) in
            match ocaml_view#current_matching_tag_bounds with
            | [rstart, _; _, lstop] -> (* current_matching_tag_bounds are in reverse order *)
                let start = buffer#get_iter_at_mark (`MARK lstop) in
                let stop = buffer#get_iter_at_mark (`MARK rstart) in
                [ start, stop, abs (stop#offset - start#offset); ins, sel, length ]
            | _ -> [ ins, sel, length ]
          in
          Merlin.enclosing
            ~position:(iter#line + 1, iter#line_offset)
            ~filename:filename
            ~buffer:(buffer#get_text ()) ()
          |> Async.start_with_continuation begin function
          | Merlin.Ok rr ->
              ranges <-
                (rr |> List.map (iters_of_range buffer)) @ extra
                |> List.sort (fun (_, _, l1) (_, _, l2) -> compare l1 l2)
                |> List.map (fun (a, b, _) -> a, b)
                |> Array.of_list;
              GtkThread.sync self#select buffer;
              buffer#connect#mark_set ~callback:begin fun iter mark ->
                (* WARNING mark_set is emitted only when the cursor is moved by the user,
                   not when programmatically inserting/deleting text. *)
                (* The mark_set signal behaves quite strangely here.
                   Changing the view from a buffer that has a selection to
                   another view, if a selection is made on this second view,
                   something happens in the first view: "selection_bound"
                   automatically moves to where "insert" is, mark_set is
                   signaled but buffer#has_selection remains true. *)
                let mark_name = GtkText.Mark.get_name mark in
                mark_name |> Option.value ~default:""
                |> Log.println `DEBUG "mark_set %S %s %b %S"
                  filename
                  (Printf.sprintf "%d:%d" (iter#line + 1) iter#line_offset)
                  buffer#has_selection;
                if mark_name = Some "selection_bound" && not buffer#has_selection
                then self#reset ()
              end |> self#add_signal (GtkSignal.disconnect buffer#as_buffer);
              buffer#connect#changed ~callback:(fun _ -> self#reset ())
              |> self#add_signal (GtkSignal.disconnect buffer#as_buffer);
              let gtext_view = ocaml_view#as_gtext_view in
              gtext_view#event#connect#key_press ~callback:begin fun ev ->
                if GdkEvent.Key.keyval ev = GdkKeysyms._Control_R || GdkEvent.Key.keyval ev = GdkKeysyms._Control_L then
                  is_rev_order <- not is_rev_order
                else if GdkEvent.Key.keyval ev = GdkKeysyms._Escape then self#reset ();
                false
              end |> self#add_signal (GtkSignal.disconnect gtext_view#as_view);
              gtext_view#event#connect#focus_out ~callback:begin fun _ ->
                self#reset ();
                false
              end |> self#add_signal (GtkSignal.disconnect gtext_view#as_view);
          | Merlin.Failure msg -> Printf.printf "%s\n%!" msg
          | Merlin.Error msg -> Printf.eprintf "File %s: %s\n%!" __FILE__ msg
          end
      | _ -> self#select buffer
  end
