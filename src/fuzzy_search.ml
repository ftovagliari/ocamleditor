let [@inline] ( @|> ) g f = if g then f else Fun.id

module type ELEMENT =
sig
  type t
  val to_string : t -> string
  val separator : string
  val of_string : string -> t
  val deserialize : string -> t array
end

module Word = struct
  type t = string
  let to_string = Fun.id
  let of_string = Fun.id
  let deserialize text =
    text
    |> String.trim
    |> Str.split (Str.regexp "[ \t\r\n,.;:?!]+")
    |> List.filter (fun w -> String.length w > 3 (*&& Dictionary.words |> List.mem w*))
    |> List.map String.lowercase_ascii
    |> Array.of_list
  let separator = "-"
end

module Letter = struct
  type t = char
  let to_string = Stdlib.String.make 1
  let of_string s = s.[0]
  let deserialize s = s |> String.lowercase_ascii |> String.to_seq |> Array.of_seq
  let separator = ""
end

module Make = functor (Elt : ELEMENT) ->
struct

  open Printf

  type path = Elt.t list

  let print_path (path : path) = path |> List.map Elt.to_string |> String.concat Elt.separator

  let print_matrix m =
    Printf.printf "\n%!";
    for i = 0 to Array.length m - 1 do
      for j = 0 to Array.length (m.(i)) - 1 do
        match m.(i).(j) with
        | Some value ->printf "%s " (Elt.to_string value)
        | _ -> printf ". "
      done;
      Printf.printf "\n%!"
    done;;

  let debug = ref true

  let find_common_paths_brute a b =
    let length_a = Array.length a in
    let length_b = Array.length b in
    let last_a = length_a - 1 in
    let last_b = length_b - 1 in
    let matrix = Array.make_matrix length_a length_b None in
    let path = ref [] in
    let paths = ref [] in
    let count = ref 0 in
    let end_path () =
      paths := !path :: !paths;
      path := [];
    in
    let rec search i j has_prev =
      incr count;
      if a.(i) <> b.(j) then begin
        if has_prev then end_path()
      end else begin
        matrix.(i).(j) <- Some a.(i);
        path := ((i, j), ref a.(i)) :: !path;
        if i >= last_a || j >= last_b then end_path()
        else (search [@tailcall]) (i + 1) (j + 1) true
      end
    in
    for i = 0 to last_a do
      for j = 0 to last_b do
        match matrix.(i).(j) with
        | None -> search i j false
        | _ -> () (* Skip the matrix cells already processed *)
      done;
    done;
    let paths =
      !paths
      |> List.rev
      |> List.map begin fun path ->
        path |> List.rev |> List.map (fun (pos, { contents = v }) -> pos, v)
      end
    in
    if !Sys.interactive then
      Printf.printf "brute: %d/%d (%.1f%%) -- paths: %d\n%!"
        !count (length_a * length_b) ((float !count /. float (length_a * length_b)) *. 100.)
        (List.length paths);
    paths, if !debug && !Sys.interactive then Some matrix else None;;

  let find_common_paths_greedy2 a b =
    let length_a = Array.length a in
    let length_b = Array.length b in
    let half_lengthb = length_b / 2 in
    let last_a = length_a - 1 in
    let last_b = length_b - 1 in
    let path = ref [] in
    let paths = ref [] in
    let count = ref 0 in
    let end_path () =
      paths := !path :: !paths;
      path := [];
    in
    let rec search i j =
      incr count;
      if a.(i) = b.(j) then begin
        path := ((i, j), ref a.(i)) :: !path;
        if i = last_a || j = last_b
        then (end_path(); Some (i, j))
        else (search [@tailcall]) (i + 1) (j + 1)
      end else if List.length !path > 1
      then (end_path(); Some (i - 1, j - 1))
      else (path := []; None)
    in
    let i = ref 0 in
    while !i < length_a do
      let j = ref 0 in
      while !j < length_b do
        begin
          match search !i !j with
          | Some (i', j') ->
              if j' <= half_lengthb then j := j'
              else begin
                i := i';
                j := length_b;
              end
          | _ -> ()
        end;
        incr j;
      done;
      incr i;
    done;
    let paths =
      !paths
      |> List.rev
      |> List.map begin fun path ->
        path |> List.rev |> List.map (fun (pos, { contents = v }) -> pos, v)
      end
    in
    if !Sys.interactive then
      Printf.printf "greedy2: %d/%d (%.1f%%) -- paths: %d\n%!"
        !count (length_a * length_b) ((float !count /. float (length_a * length_b)) *. 100.)
        (List.length paths);
    paths, None;;

  let find_common_paths_greedy a b =
    let length_a = Array.length a in
    let length_b = Array.length b in
    let last_a = length_a - 1 in
    let last_b = length_b - 1 in
    let path = ref [] in
    let paths = ref [] in
    let count = ref 0 in
    let end_path () =
      paths := !path :: !paths;
      path := [];
    in
    let rec search i j =
      incr count;
      if a.(i) <> b.(j) then begin
        if List.length !path > 1 then (end_path(); i - 1)
        else (path := []; -1)
      end else begin
        path := ((i, j), ref a.(i)) :: !path;
        if i = last_a || j = last_b
        then (end_path(); i)
        else (search [@tailcall]) (i + 1) (j + 1)
      end;
    in
    let i = ref 0 in
    while !i < length_a do
      let j = ref 0 in
      while !j < length_b do
        let i' = search !i !j in
        if i' >= 0 then begin
          i := i'; (* see incr i below *)
          j := length_b
        end;
        incr j;
      done;
      incr i;
    done;
    let paths =
      !paths
      |> List.rev
      |> List.map begin fun path ->
        path |> List.rev |> List.map (fun (pos, { contents = v }) -> pos, v)
      end
    in
    if !Sys.interactive then
      Printf.printf "greedy: %d/%d (%.1f%%) -- paths: %d\n%!"
        !count (length_a * length_b) ((float !count /. float (length_a * length_b)) *. 100.)
        (List.length paths);
    paths, None;;

  (** Removes shortest overlapping paths *)
  let reduce paths =
    let paths = Array.of_list paths in
    let n_paths = Array.length paths in
    for i = 0 to n_paths - 1 do
      match paths.(i) with
      | [] -> ()
      | (((xi, yi), _) :: _) as path_i ->
          let len_i = List.length path_i in
          for j = 0 to n_paths - 1 do
            if i <> j then
              match paths.(j) with
              | [] -> ()
              | path_j ->
                  let ij_overlap = path_j |> List.exists (fun ((x, y), _) -> x = xi || y = yi) in
                  if ij_overlap then
                    let k = if len_i <= List.length path_j then i else j in
                    paths.(k) <- []
          done
    done;
    paths |> Array.to_list |> List.filter ((<>) [])
  ;;

  let compare ?(simplify=true) ?(min_score=0.62) algorithm pat str =
    let pat = Elt.deserialize pat in
    let str = Elt.deserialize str in
    let len_pat, len_str = Array.length pat, Array.length str in
    let find =
      match algorithm with
      | `Brute -> find_common_paths_brute
      | `Greedy -> find_common_paths_greedy
      | `Greedy2 -> find_common_paths_greedy2
    in
    let paths, debug_matrix = find pat str in
    let paths = paths |> simplify @|> reduce in
    let paths = paths |> List.map (fun p -> p |> List.map (fun (_, v) -> v)) in
    Option.iter print_matrix debug_matrix;
    if !debug && !Sys.interactive then
      paths
      |> List.map (fun p -> p |> List.map Elt.to_string |> String.concat "")
      |> String.concat ", " |> printf "%s\n%!";
    let lp = float len_pat in
    let ls = float len_str in
    let amount = List.fold_left (fun sum l -> List.length l + sum) 0 paths |> float in
    let number_of_paths = List.length paths in
    let compactness = if number_of_paths > 0 then 1. /. float number_of_paths else 0. in
    let s_relevance = amount /. ls in
    let p_relevance = amount /. lp in
    let top = lp +. (*lp +.*) 1. +. 2. in
    let score = amount +. compactness +. s_relevance +. p_relevance in
    let score_perc = score /. top in
    if !debug && !Sys.interactive then
      Printf.printf "score:%.2f am:%d cp:%.2f sr:%.2f pr:%.2f lp:%d ls:%d paths:%d\n%!"
        score_perc (int_of_float amount) compactness s_relevance p_relevance len_pat len_str (List.length paths);
    let score =
      if !Sys.interactive then score_perc
      else if score_perc >= min_score then score_perc
      else 0.
    in
    score, (paths : path list)
  ;;

end

module [@local] FuzzyLetters = Make(Letter);;
