let (let+) = Option.bind
let (let@) o f = match o with Some v -> f v | None -> ()
