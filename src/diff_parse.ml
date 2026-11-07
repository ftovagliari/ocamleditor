type index =
  | One of int (* one line *)
  | Many of int * int (* many lines, we have the first and the last *)

type diff =
  | Add of index * index * string
  | Delete of index * index * string
  | Change of index * string * index * string

type diffs = diff list

let text ~final_nl rev_lines =
  String.concat "\n" (List.rev (if final_nl then "" :: rev_lines else rev_lines))

let index s =
  match String.index_opt s ',' with
  | None -> One (int_of_string s)
  | Some i ->
      Many (int_of_string (String.sub s 0 i),
            int_of_string (String.sub s (i + 1) (String.length s - i - 1)))

let header l =
  let n = String.length l in
  let rec find i =
    if i >= n then failwith ("invalid diff header: " ^ l)
    else match l.[i] with 'a' | 'd' | 'c' as c -> (c, i) | _ -> find (i + 1)
  in
  let c, i = find 0 in
  (index (String.sub l 0 i), c, index (String.sub l (i + 1) (n - i - 1)))

let block p lines =
  let rec go acc lines =
    match Seq.uncons lines with
    | Some (l, rest) when String.length l >= 2 && l.[0] = p && l.[1] = ' ' ->
        go (String.sub l 2 (String.length l - 2) :: acc) rest
    | Some (l, rest) when String.starts_with ~prefix:"\\ " l ->
        (text ~final_nl:false acc, rest)
    | Some (l, rest) -> (text ~final_nl:true acc, Seq.cons l rest)
    | None -> (text ~final_nl:true acc, Seq.empty)
  in
  go [] lines

let rec parse acc lines =
  match Seq.uncons lines with
  | None -> List.rev acc
  | Some ("", rest) -> parse acc rest
  | Some (l, rest) ->
      let i1, c, i2 = header l in
      (match c with
       | 'a' ->
           let s, rest = block '>' rest in
           parse (Add (i1, i2, s) :: acc) rest
       | 'd' ->
           let s, rest = block '<' rest in
           parse (Delete (i1, i2, s) :: acc) rest
       | _ ->
           let s1, rest = block '<' rest in
           (match Seq.uncons rest with
            | Some ("---", rest) ->
                let s2, rest = block '>' rest in
                parse (Change (i1, s1, i2, s2) :: acc) rest
            | _ -> failwith "separator '---' expected"))

let of_seq lines = parse [] lines

let of_string s = of_seq (List.to_seq (String.split_on_char '\n' s))

let from_channel ic =
  of_seq (Seq.of_dispenser (fun () -> In_channel.input_line ic))
