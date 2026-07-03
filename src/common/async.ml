type 'a expr = {
  name : string;
  computation : unit -> 'a
}

type 'a state = Created | Running of Thread.t | Ran_to_completion of 'a | Faulted of exn

type 'a task = {
  id : int;
  mutex : Mutex.t;           (* A mutex to synchronize access to the task state. *)
  mutable state : 'a state;  (* The current state of the task. *)
}

let [@inline] (>>) f g x = f (g x)

let [@inline] create ?(name="unnamed") f = { name; computation = f }
let return = create

let [@inline] bind (f : 'a -> 'b expr) (a : 'a expr) = (f >> a.computation) ()
let [@inline] (>>=) a f = bind f a

let [@inline] map ?(name="unnamed") f (a : 'a expr) = create ~name (f >> a.computation)
let continue_with = map

let ( let* ) a f = (>>=) a f
let ( let+ ) a f = map f a
let tids = Atomic.make 0

let start_as_task (a : 'a expr) =
  let task = {
    id = Atomic.fetch_and_add tids 1;
    mutex = Mutex.create();
    state = Created;
  } in
  Mutex.lock task.mutex;
  task.state <-
    Running begin
      Thread.create begin fun () ->
        try
          let result = a.computation () in
          Mutex.lock task.mutex;
          task.state <- Ran_to_completion result;
          Mutex.unlock task.mutex;
        with ex ->
          let thid = Thread.id (Thread.self()) in
          Printf.printf "%s, task %S (%d-%d) failed: %s\n%!" __MODULE__ a.name thid task.id (Printexc.to_string ex);
          Printf.eprintf "File \"async.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());
          Mutex.lock task.mutex;
          task.state <- Faulted ex;
          Mutex.unlock task.mutex;
      end ()
    end;
  Mutex.unlock task.mutex;
  task

let [@inline] start (a : 'a expr) = start_as_task a |> ignore

let [@inline] start_with_continuation ?(name="unnamed") cont (a : 'a expr) =
  map ~name cont a |> start

let rec await (task : 'a task) =
  Mutex.lock task.mutex;
  let state = task.state in
  Mutex.unlock task.mutex;
  match state with
  | Ran_to_completion result -> result
  | Running thread ->
      Thread.join thread;
      await task
  | Created -> assert false
  | Faulted ex -> raise ex

let [@inline] run_synchronously (a : 'a expr) = await (start_as_task a);;

let concurrent ?name (computations : 'a expr array) =
  create ?name (fun () -> computations |> Array.map (fun c -> start_as_task c |> await))

module Infix = struct
  let (>=>) a f = start_with_continuation f a
end


(*module Test = struct

  let a1 = return begin fun () ->
      Printf.printf "Begin a1 %!" ;
      Thread.delay 5.;
      Printf.printf "End a1\n%!" ;
      3
    end;;

  let test = return begin fun () ->
      let* b1 = a1 in
      return (fun () -> b1 + 2)
    end;;

  let test1 =
    let* b1 = a1 in
    return (fun () -> b1 + 2);;

  let test2 =
    let+ b1 = a1 in
    b1 + 2;;

  let _ = run_synchronously test2;;

  (* Monad Laws *)
  let x = 3;;
  let m = return (fun () -> x);;
  let f x = return (fun () -> string_of_int x);;
  let g x = return (fun () -> "("^x^")" );;
  (* I Law *)
  assert (run_synchronously (m >>= f) = run_synchronously (f x));;
  (* II Law *)
  let left = m >>= (fun z -> return (fun () -> z));;
  assert (run_synchronously left = run_synchronously m);;
  (* III Law *)
  let left = (m >>= f) >>= g;;
  let right = m >>= (fun x -> f x >>= g);;
  assert (run_synchronously left = run_synchronously right);;

  end
*)
