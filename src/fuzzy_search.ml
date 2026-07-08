type algorithm = BruteForce | Greedy | GreedyInterleaved | GreedyFlat of int

let [@inline] ( @|> ) g f = if g then f else Fun.id

module type ELEMENT =
sig
  type t
  val to_string : t -> string
  val separator : string
  val of_string : string -> t
  val deserialize : string -> t array
end

(*module Word = struct
  type t = string
  let to_string = Fun.id
  let of_string = Fun.id
  let deserialize = Fuzzy_utils.prepare_words
  let separator = " "
  end*)

module Letter = struct
  type t = char
  let to_string c = if c = ' ' then "\u{2423}" else Stdlib.String.make 1 c
  let of_string s = s.[0]
  let deserialize s = s |> String.lowercase_ascii |> String.to_seq |> Array.of_seq
  let separator = ""
end

module type FUZZY = sig
  type path
  type segment = U of string | M of string
  type result = {
    number_of_paths : int;
    total_paths_length : float;
    raw_coverage : float;
    compactness : float;
    safe_match_length : float;
    sorensen_dice : float;
    jaccard : float;
    p_relevance : float;
    s_relevance : float;
    score : float;
    score' : float;
    score'' : float;
    lp : int;
    ls : int;
    paths : path list;
    marked_string : segment list;
    time : float;
    time_reduce : float;
    iterations : float;
  }

  val string_of_result : result -> string

  val set_debug : bool -> unit

  val compare :
    ?simplify:bool ->
    ?min_score:float ->
    ?min_path_len:int -> algorithm -> string -> string -> result
end

module Make = functor (Elt : ELEMENT) ->
struct

  open Printf

  type matrix_elem =
    | Match_forward of Elt.t
    | Match_backward of Elt.t
    | Mismatch_backward
    | Mismatch_forward
    | Uncovered of Elt.t
    | Mark of Elt.t

  type path = Elt.t list

  type segment = U of string | M of string

  type result = {
    number_of_paths : int;
    total_paths_length : float;
    raw_coverage : float;
    compactness : float;
    safe_match_length : float;
    sorensen_dice : float;
    jaccard : float;
    p_relevance : float;
    s_relevance : float;
    score : float;
    score' : float;
    score'' : float;
    lp : int;
    ls : int;
    paths : path list;
    marked_string : segment list;
    time : float;
    time_reduce : float;
    iterations : float;
  }

  let print_path (path : path) = path |> List.map Elt.to_string |> String.concat Elt.separator

  let print_matrix p s m =
    Printf.printf "\n    %s\n%!" (String.lowercase_ascii s);
    for i = 0 to Array.length m - 1 do
      Printf.printf "%2d %c" i p.[i];
      for j = 0 to Array.length (m.(i)) - 1 do
        match m.(i).(j) with
        | Match_backward value -> Printf.printf "\027[7;33m%s\027[0m" (Elt.to_string value)
        | Match_forward value -> Printf.printf "\027[7;32m%s\027[0m" (Elt.to_string value)
        | Mismatch_backward -> printf "\027[2;35m\u{2593}\027[0m" (*"\u{2592}"*) (*"\u{2e2c}"*)
        | Mismatch_forward -> printf "\027[2;31m\u{2593}\027[0m" (*"\u{2592}"*) (*"\u{2e2c}"*)
        | Uncovered value -> Printf.printf "\027[2m%s\027[0m" (Elt.to_string value)
        | Mark value -> Printf.printf "\027[5;2;31m%s\027[0m" (*"\u{2593}"*) (Elt.to_string value)
      done;
      Printf.printf "\n%!"
    done;;

  let debug = ref false
  let set_debug b = debug := b

  let init_matrix a b =
    Array.init (Array.length a) begin fun i ->
      Array.init (Array.length b) begin fun j ->
        if a.(i) = b.(j) then Uncovered a.(i) else Uncovered (Elt.of_string ".")
      end
    end

  let find_paths algorithm ?(min_path_len=0) a b =
    let can_go_back =
      match algorithm with
      | BruteForce -> false
      | Greedy -> true
      | GreedyInterleaved -> true
      | GreedyFlat _ -> true
    in
    let length_a = Array.length a in
    let length_b = Array.length b in
    let last_a = length_a - 1 in
    let last_b = length_b - 1 in
    let path = ref (Deque.create length_a)  in
    let paths = ref [] in
    let iterations = ref 0 in
    let end_path () =
      let path_added = Deque.size !path > min_path_len in
      paths := if path_added then !path :: !paths else !paths;
      path := Deque.create length_a;
      path_added
    in
    let matrix = if !debug || algorithm = BruteForce then Some (init_matrix a b) else None in
    let optional_matrix_func =
      Option.map begin fun m v i j ->
        match [@warning "-4"] m.(i).(j) with
        | Uncovered _ -> m.(i).(j) <- v
        | _ when algorithm = BruteForce -> () (* BruteForce algorithm may scan already covered cells *)
        | _ -> () (*ksprintf failwith "Already covered (%d, %d)" i j*)
      end matrix
    in
    let rec search_backward_gen on_cell i j =
      if i >= 0 && j >= 0 then begin
        incr iterations;
        if a.(i) <> b.(j) then
          Option.iter (fun f -> f Mismatch_backward i j) on_cell
        else begin
          Option.iter (fun f -> f (Match_backward a.(i)) i j) on_cell;
          Deque.push_front !path ((i, j), ref a.(i));
          search_backward_gen on_cell (i - 1) (j - 1)
        end
      end
    in
    let search_backward = search_backward_gen optional_matrix_func in
    let rec search_forward_gen on_cell i j match_found =
      incr iterations;
      if a.(i) <> b.(j) then begin
        Option.iter (fun f -> f Mismatch_forward i j) on_cell;
        i, j, match_found
      end else begin
        Option.iter (fun f -> f (Match_forward a.(i)) i j) on_cell;
        Deque.push_back !path ((i, j), ref a.(i));
        if i >= last_a || j >= last_b then begin
          i, j, true
        end else
          search_forward_gen on_cell (i + 1) (j + 1) true
      end
    in
    let search_forward = search_forward_gen optional_matrix_func in
    let inner_loop i start stop =
      let (<=), next = if start < stop then (<=), incr else (>), decr in
      let j = ref start in
      while !j <= stop do
        let fi, fj, is_match = search_forward !i !j false in
        if can_go_back then begin
          if is_match then search_backward (!i - 1) (!j - 1);
        end;
        let path_collected = end_path () in
        if can_go_back && path_collected then begin
          i := fi;
          j := fj;
        end;
        next j;
      done;
    in
    let i = ref 0 in
    while !i <= last_a do
      let i' = !i in
      begin
        match algorithm with
        | GreedyInterleaved ->
            inner_loop i 0 last_b;
            i := i';
            inner_loop i last_b 0;
            incr i;
        | GreedyFlat span ->
            inner_loop i 0 last_b;
            i := min length_a (i' + span);
        | BruteForce | Greedy ->
            inner_loop i 0 last_b;
            incr i;
      end;
    done;
    let paths =
      !paths
      |> List.rev
      |> List.map begin fun path ->
        path
        |> Deque.to_array
        |> Array.map (fun (pos, { contents = v }) -> pos, v)
        |> Array.to_list
      end
    in
    paths, !iterations, if !debug then matrix else None;;

  let greedy_cleaned_up ?(min_path_len=0) a b =
    let length_a = Array.length a in
    let length_b = Array.length b in
    let last_a = length_a - 1 in
    let last_b = length_b - 1 in
    let path = ref (Deque.create length_a)  in
    let paths = ref [] in
    let end_path () =
      let path_added = Deque.size !path > min_path_len in
      paths := if path_added then !path :: !paths else !paths;
      path := Deque.create length_a;
      path_added
    in
    let rec search_backward i j =
      if i >= 0 && j >= 0 then begin
        if a.(i) = b.(j) then begin
          Deque.push_front !path ((i, j), ref a.(i));
          search_backward (i - 1) (j - 1)
        end
      end
    in
    let rec search_forward i j match_found =
      if a.(i) <> b.(j) then begin
        i, j, match_found
      end else begin
        Deque.push_back !path ((i, j), ref a.(i));
        if i >= last_a || j >= last_b then begin
          i, j, true
        end else
          search_forward (i + 1) (j + 1) true
      end
    in
    let i = ref 0 in
    while !i <= last_a do
      let j = ref 0 in
      while !j <= last_b do
        let fi, fj, is_match = search_forward !i !j false in
        if is_match then search_backward (!i - 1) (!j - 1);
        let path_collected = end_path () in
        if path_collected then begin
          i := fi;
          j := fj;
        end;
        incr j;
      done;
      incr i;
    done;
    !paths
    |> List.rev
    |> List.map begin fun path ->
      path
      |> Deque.to_array
      |> Array.map (fun (pos, { contents = v }) -> pos, v)
      |> Array.to_list
    end;;

  (** Removes shortest overlapping paths *)
  let reduce_aggressive paths =
    let paths = Array.of_list paths in
    let n_paths = Array.length paths in
    for i = 0 to n_paths - 1 do
      match paths.(i) with
      | [] -> ()
      | (((xi, yi), _) :: _) as path_i ->
          let len_i = List.length path_i in
          let end_xi = xi + len_i - 1 in (* Ultima riga occupata *)
          let end_yi = yi + len_i - 1 in (* Ultima colonna occupata *)
          let j = ref (i + 1) in
          while !j < n_paths do
            if i = !j then incr j
            else
              match paths.(!j) with
              | [] -> incr j
              | (((xj, yj), _) :: _) as path_j ->
                  let len_j = List.length path_j in
                  let end_xj = xj + len_j - 1 in
                  let end_yj = yj + len_j - 1 in
                  if xi <= end_xj && xj <= end_xi || yi <= end_yj && yj <= end_yi then begin
                    if len_j > len_i then begin
                      paths.(i) <- [];
                      j := n_paths
                    end else begin
                      paths.(!j) <- [];
                      incr j
                    end
                  end else incr j
          done
    done;
    paths |> Array.to_list |> List.filter ((<>) [])
  ;;

  (* Mark matched segments in the reference string based on paths *)
  let mark_string (reference : Elt.t array) (paths : ((int * int) * Elt.t) list list) : segment list =
    let len = Array.length reference in
    if len = 0 then []
    else
      let matched = Array.make len false in
      (* Mark all positions that appear in any path *)
      paths
      |> List.iter (fun path ->
          List.iter (fun ((_, j), _) ->
              if j >= 0 && j < len then matched.(j) <- true) path);

      (* Convert matched array to segment list *)
      let rec collect i current_status current_acc segments_acc =
        if i = len then
          let seg_str = String.concat Elt.separator (List.rev current_acc) in
          let seg = if current_status then M seg_str else U seg_str in
          List.rev (seg :: segments_acc)
        else
          let is_matched = matched.(i) in
          let elem_str = Elt.to_string reference.(i) in
          if is_matched = current_status then
            collect (i + 1) current_status (elem_str :: current_acc) segments_acc
          else
            let seg_str = String.concat Elt.separator (List.rev current_acc) in
            let seg = if current_status then M seg_str else U seg_str in
            collect (i + 1) is_matched [elem_str] (seg :: segments_acc)
      in
      let first_elem_str = Elt.to_string reference.(0) in
      collect 1 matched.(0) [first_elem_str] []
  ;;

  let compare ?(simplify=true) ?(min_score=0.62) ?(min_path_len=0) algorithm pat str =
    let pattern = Elt.deserialize pat in
    let reference = Elt.deserialize str in
    let len_pat, len_str = Array.length pattern, Array.length reference in
    let find = find_paths algorithm ~min_path_len in
    let time0 = Unix.gettimeofday () in
    let paths_raw, iterations, debug_matrix = find pattern reference in
    let time = Unix.gettimeofday () in
    let paths_raw = paths_raw |> simplify @|> reduce_aggressive (*reduce_to_supersets*) in
    let time_reduce = Unix.gettimeofday () in
    let paths = paths_raw |> List.map (fun p -> p |> List.map (fun (_, v) -> v)) in
    Option.iter (print_matrix pat str) debug_matrix;
    (*if !debug then
      paths
      |> List.map (fun p -> p |> List.map Elt.to_string |> String.concat "")
      |> String.concat ", " |> printf "%s\n%!";*)
    let max_len = float (max len_pat len_str) in
    let lp = float len_pat in
    let ls = float len_str in

    let total_paths_length = List.fold_left (fun sum l -> List.length l + sum) 0 paths |> float in
    let raw_coverage = min total_paths_length max_len in
    let number_of_paths = List.length paths in
    let compactness = if number_of_paths > 0 then 1. /. (float number_of_paths) else 0. in
    let safe_match_length = min total_paths_length (min lp ls) in
    let sorensen_dice = (2.0 *. safe_match_length) /. (lp +. ls) in
    let p_relevance = raw_coverage /. lp in
    let s_relevance = raw_coverage /. ls in
    let _score = (p_relevance *. 0.4) +. (s_relevance *. 0.4) +. (compactness *. 0.2) in
    let score' = sorensen_dice *. compactness in
    let score'' = (sorensen_dice *. 0.8) +. (compactness *. 0.2) in
    let jaccard = safe_match_length /. (lp +. ls -. safe_match_length) in

    let top = lp +. (*lp +.*) 1. +. 2. +. 1. in
    let score = total_paths_length +. 2.*.compactness +. s_relevance +. p_relevance in
    let score = score /. top in
    if score >= min_score then
      let marked_string = mark_string reference paths_raw in
      Some {
        number_of_paths;
        total_paths_length;
        raw_coverage;
        compactness;
        safe_match_length;
        sorensen_dice;
        jaccard;
        p_relevance;
        s_relevance;
        score;
        score';
        score'';
        lp = len_pat;
        ls = len_str;
        paths;
        marked_string;
        time = time -. time0;
        time_reduce = time_reduce -. time;
        iterations = float iterations /. float (len_pat * len_str) *. 100.
      }
    else None
  ;;

  let string_of_result (r : result) : string =
    let paths_str =
      (* Mappa ogni path in stringa e li unisce con una virgola *)
      let path_strings = List.map print_path r.paths in
      "[" ^ (String.concat "; " path_strings) ^ "]"
    in
    (*let marked_str =
      let segment_strings = List.map (function
          | U s -> "U \"" ^ String.escaped s ^ "\""
          | M s -> "M \"" ^ String.escaped s ^ "\"") r.marked_string
      in
      "[" ^ (String.concat "; " segment_strings) ^ "]"
      in*)
    Printf.sprintf
      "{\n\
       \tscore = %.2f;\n\
       \tscore' = %.2f;\n\
       \tscore'' = %.2f;\n\
       \tnumber_of_paths = %d;\n\
       \ttotal_paths_length = %.2f;\n\
       \traw_coverage = %.2f;\n\
       \tcompactness = %.2f;\n\
       \tsafe_match_length = %.2f;\n\
       \tsorensen_dice = %.2f;\n\
       \tjaccard = %.2f;\n\
       \tp_relevance = %.2f;\n\
       \ts_relevance = %.2f;\n\
       \tlp = %d;\n\
       \tls = %d;\n\
       \tpaths = %s;\n\
       \ttime = %f; time_reduce = %f\n\
       \titerations = %.1f%% of %d\n\
       }"
      r.score
      r.score'
      r.score''
      r.number_of_paths
      r.total_paths_length
      r.raw_coverage
      r.compactness
      r.safe_match_length
      r.sorensen_dice
      r.jaccard
      r.p_relevance
      r.s_relevance
      r.lp
      r.ls
      paths_str
      r.time r.time_reduce
      r.iterations (r.lp * r.ls)

end
