open Parsetree

module Log = Common.Log.Make(struct let prefix = "UNIFY" end)
let _ =
  Log.set_print_timestamp true;
  Log.set_verbosity `ERROR

let blanks = ["[\r\n\t ]+", " "]

let tee f x = f x; x

let print_type te =
  (*Format.pp_set_geometry ~margin:80 ~max_indent:2 Format.std_formatter;*)
  Format.asprintf "%a" Pprintast.core_type te
  |> Str.split (Str.regexp "\n") |> List.map String.trim |> String.concat "\n"
(*let buf = Buffer.create 100 in
  let formatter = Format.formatter_of_buffer buf in
  Format.pp_set_geometry ~margin:80 ~max_indent:2 formatter;
  Pprintast.core_type formatter te;
  Format.pp_print_flush formatter ();
  Buffer.contents buf |> Str.split (Str.regexp "\n") |> List.map String.trim |> String.concat "\n"*)

exception Cannot_be_combined
exception Cannot_be_unified

let [@inline] core_type desc =
  { ptyp_desc = desc; ptyp_loc = Location.none; ptyp_loc_stack = []; ptyp_attributes = [] }

let [@inline] object_field desc =
  { pof_desc = desc; pof_loc = Location.none; pof_attributes = [] }

let [@inline] row_field desc =
  { prf_desc = desc; prf_loc = Location.none; prf_attributes = [] }

let try_combine l1 l2 = try List.combine l1 l2 with Invalid_argument _ -> raise Cannot_be_combined

(** A sort of unification algorithm to determine the set of substitutions to
    apply to the first type to obtain the second, assuming that the two types
    are already unifiable. *)
let find_substitutions te1 te2 =
  let pte1 = Parse.core_type (Lexing.from_string te1) in
  let pte2 = Parse.core_type (Lexing.from_string te2) in
  (*Log.println `DEBUG "%s" (print_type pte1);
    Log.println `DEBUG "%s" (print_type pte2);*)
  let vars = ref [] in
  let rec unify ctyl ctyr =
    match [@warning "-4"] ctyl.ptyp_desc, ctyr.ptyp_desc with

    | Ptyp_var l, Ptyp_var r ->
        if l <> r then vars := (print_type ctyl, print_type ctyr) :: !vars;
        Ptyp_var l |> core_type

    | Ptyp_var name, _ ->
        vars := (print_type ctyl, print_type ctyr) :: !vars;
        Ptyp_var name |> core_type

    | Ptyp_arrow (x, l1, l2), Ptyp_arrow (_, r1, r2) ->
        Ptyp_arrow (x, unify l1 r1, unify l2 r2) |> core_type

    | Ptyp_tuple l, Ptyp_tuple r ->
        let cc = try List.combine l r with Invalid_argument _ -> raise Cannot_be_combined in
        let res =
          cc |> List.map (fun ((ll, l), (lr, r)) ->
              if ll = lr then ll, unify l r else raise Cannot_be_combined)
        in
        Ptyp_tuple res |> core_type

    | Ptyp_constr (x, l), Ptyp_constr (_, r) ->
        Ptyp_constr (x, try_combine l r |> List.map (fun (l, r) -> unify l r)) |> core_type

    | Ptyp_object (l, x), Ptyp_object (r, _) ->
        let res =
          try_combine l r
          |> List.map begin fun (l, r) ->
            match l.pof_desc, r.pof_desc with
            | Otag (x, l1), Otag (_, r1) -> Otag (x, unify l1 r1) |> object_field
            | Oinherit l2, Oinherit r2 -> Oinherit (unify l2 r2) |> object_field
            | _ -> raise Cannot_be_unified
          end
        in
        Ptyp_object (res, x) |> core_type

    | Ptyp_class (x, l), Ptyp_class (_, r) ->
        Ptyp_class(x, try_combine l r |> List.map (fun (l, r) -> unify l r)) |> core_type

    | Ptyp_alias (l, x), Ptyp_alias (r, _) ->
        Ptyp_alias(unify l r, x) |> core_type

    | Ptyp_variant (l, x, y), Ptyp_variant (r, _, _) ->
        let res =
          try_combine l r
          |> List.map begin fun (l, r) ->
            match l.prf_desc, r.prf_desc with
            | Rtag (x, y, l1), Rtag (_, _, r1) ->
                Rtag (x, y, try_combine l1 r1 |> List.map (fun (l, r) -> unify l r)) |> row_field
            | Rinherit l2, Rinherit r2 ->
                Rinherit (unify l2 r2) |> row_field
            | _ ->
                raise Cannot_be_unified
          end
        in
        Ptyp_variant(res, x, y) |> core_type

    | Ptyp_poly (x, l), Ptyp_poly (_, r) ->
        Ptyp_poly(x, unify l r) |> core_type

    | (* *) Ptyp_package l, Ptyp_package _ -> Ptyp_package l |> core_type
    | (* *) Ptyp_extension l, Ptyp_extension _ -> Ptyp_extension l |> core_type

    | Ptyp_object (l, _), Ptyp_class (x, r) ->
        Ptyp_class (x, r) |> core_type

    | _ -> raise Cannot_be_unified
  in
  try
    let result = print_type (unify pte1 pte2) in
    result,
    !vars
    |> Utils.ListExt.remove_dupl
    |> List.sort Stdlib.compare
    |> List.map begin fun (name, value) ->
      name,
      if String.starts_with ~prefix:"(" value && String.ends_with ~suffix:")" value
      then String.sub value 1 (String.length value - 2) else value
    end
  with
  | Cannot_be_combined ->
      (*Log.println `ERROR "Cannot_be_combined:\n  %s\n  %s" (print_type pte1) (print_type pte2);*)
      print_type pte1, []
  | Cannot_be_unified ->
      (*Log.println `ERROR "Cannot_be_unified:\n  %s\n  %s" (print_type pte1) (print_type pte2);*)
      print_type pte1, []

