type 'a t = {
  data : 'a option Array.t;
  max_size : int;
  mutable head : int;
  mutable tail : int;
  mutable size : int;
}

let create max_size = {
  data = Array.make max_size None;
  max_size;
  head = 0;
  tail = 0;
  size = 0;
}

let is_full q = q.size = q.max_size

let is_empty q = q.size = 0

let size q = q.size

let push_front q x =
  if is_full q then failwith "Deque overflow";
  if not (is_empty q) then
    q.head <- (q.head - 1 + q.max_size) mod q.max_size;
  q.data.(q.head) <- Some x;
  q.size <- q.size + 1

let push_back q x =
  if is_full q then failwith "Deque overflow";
  if not (is_empty q) then
    q.tail <- (q.tail + 1) mod q.max_size;
  q.data.(q.tail) <- Some x;
  q.size <- q.size + 1

let to_array q =
  if is_empty q then [||]
  else
    Array.init q.size (fun i ->
        let idx = (q.head + i) mod q.max_size in
        match q.data.(idx) with
        | Some x -> x
        | None -> failwith "missing element"
      )
