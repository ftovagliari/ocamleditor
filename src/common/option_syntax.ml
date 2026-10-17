let ( let+ ) = Option.bind

let (let@) o f =
  match o with
  | None -> ()
  | Some v -> f v
