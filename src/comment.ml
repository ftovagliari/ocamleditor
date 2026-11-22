(** Scans [text] and returns the raw list of comments with their locations. *)
let scan text =
  Lexer.init();
  let lexbuf = Lexing.from_string text in
  begin
    try
      while true do
        if Lexer.token lexbuf = EOF then raise End_of_file
      done
    with
    | End_of_file -> ()
    | Lexer.Error (_err, _loc) -> ()
  end;
  Lexer.comments ()

(** Scans [text] and returns comment byte offset intervals [(start, stop, is_odoc)]. *)
let scan_offsets text =
  scan text
  |> List.map begin fun (c, l) ->
    let is_odoc = String.starts_with ~prefix:"*" c in
    let open Location in
    let open Lexing in
    let bc = l.loc_start.pos_cnum in
    let ec = l.loc_end.pos_cnum in
    (bc, ec, is_odoc)
  end

type position = {
  start_line : int;
  start_col : int;
  stop_line : int;
  stop_col : int;
  is_odoc : bool
}

(** Scans [text] and returns the list of comment positions with line and column bounds. *)
let scan_positions text =
  scan text
  |> List.map begin fun (c, l) ->
    let open Location in
    let is_odoc = String.starts_with ~prefix:"*" c in
    let _, start_line, start_col = Location.get_pos_info l.loc_start in
    let _, stop_line, stop_col = Location.get_pos_info l.loc_end in
    { start_line; start_col; stop_line; stop_col; is_odoc }
  end

(** Checks whether position [(ln, cn)] lies strictly inside the comment bounds. *)
let is_inside ln cn { start_line; start_col; stop_line; stop_col; _ } =
  let after_start = (ln > start_line) || (ln = start_line && cn > start_col) in
  let before_end  = (ln < stop_line) || (ln = stop_line && cn < stop_col) in
  after_start && before_end

(** Returns [true] if [(ln, cn)] falls inside any comment in [comments]. *)
let in_comment (comments : position list) ln cn =
  List.exists (is_inside ln cn) comments

(** Scans [text] and returns [true] if [(ln, cn)] falls inside a comment. *)
let in_comment_from_text text = in_comment (scan_positions text)

(** Finds the first comment in [comments] containing [(ln, cn)], if any. *)
let find_at (comments : position list) ln cn =
  List.find_opt (is_inside ln cn) comments

(** Returns [true] if position [(l1, c1)] precedes or equals [(l2, c2)]. *)
let pos_leq (l1, c1) (l2, c2) = (l1 < l2) || (l1 = l2 && c1 <= c2)

(** Filters [comments] to those fully enclosed between [(ln1, cn1)] and [(ln2, cn2)]. *)
let filter_between (comments : position list) (ln1, cn1) (ln2, cn2) =
  comments |> List.filter (fun { start_line; start_col; stop_line; stop_col; _ } ->
      pos_leq (ln1, cn1) (start_line, start_col)
      && pos_leq (stop_line, stop_col) (ln2, cn2))

(** Computes the [(line_diff, col_diff)] distance between two positions. *)
let pos_dist (l1, c1) (l2, c2) =
  (abs (l1 - l2), abs (c1 - c2))

(** Computes the [(line_diff, col_diff)] distance from [(ln, cn)] to a comment. *)
let dist_to_comment ~ln ~cn { start_line; start_col; stop_line; stop_col; _ } =
  if (ln > start_line || (ln = start_line && cn >= start_col))
  && (ln < stop_line || (ln = stop_line && cn <= stop_col)) then
    (0, 0)
  else if ln < start_line || (ln = start_line && cn < start_col) then
    pos_dist (ln, cn) (start_line, start_col)
  else
    pos_dist (ln, cn) (stop_line, stop_col)

(** Returns the comment in [comments] closest to [(ln, cn)], or [None] if empty. *)
let nearest (comments : position list) ln cn =
  match comments with
  | [] -> None
  | first :: rest ->
      let best, _ =
        List.fold_left (fun (best_c, best_d) item ->
            let d = dist_to_comment ~ln ~cn item in
            if d < best_d then (item, d) else (best_c, best_d)
          ) (first, dist_to_comment ~ln ~cn first) rest
      in
      Some best
