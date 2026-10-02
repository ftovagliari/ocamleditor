open Merlin_j
open Printf

type node_id = {
  name: string;
  kind: string;
  occurrence: int;
}

type node_path = node_id list

type t = {
  added: (node_path * outline * int) list;
  removed: (node_path * outline * int) list;
  changed: (node_path * outline * outline) list; (* (path, old_node, new_node) *)
}

let empty = { added = []; removed = []; changed = [] }

let string_of_path p =
  "`" ^ (p |> List.map (fun id -> sprintf "%s" id.name (*id.occurrence*)) |> String.concat ":") ^ "`"

let index_by_identity lst =
  let module KeyMap = Map.Make(struct
      type t = string * string
      let compare = compare
    end) in

  let _, indexed_list =
    List.fold_left (fun (counts, acc) o ->
        let key = (o.ol_name, o.ol_kind) in
        let count = Option.value (KeyMap.find_opt key counts) ~default:0 in
        let id = { name = o.ol_name; kind = o.ol_kind; occurrence = count } in
        let new_counts = KeyMap.add key (count + 1) counts in
        (new_counts, (id, o) :: acc)
      ) (KeyMap.empty, []) lst
  in
  List.rev indexed_list

let combine d1 d2 = {
  added = d1.added @ d2.added;
  removed = d1.removed @ d2.removed;
  changed = d1.changed @ d2.changed;
}

let equal_node a b =
  a.ol_start = b.ol_start &&
  a.ol_stop = b.ol_stop &&
  a.ol_name = b.ol_name &&
  a.ol_kind = b.ol_kind &&
  a.ol_type = b.ol_type &&
  a.ol_deprecated = b.ol_deprecated &&
  a.ol_selection = b.ol_selection &&
  a.ol_level = b.ol_level

let rec diff_outlines_with_path
    (parent_path : node_path)
    (raw_o1 : outline list)
    (raw_o2 : outline list) : t =

  (* Ripristina l'ordine naturale (da cima a fondo del file) per questo livello *)
  let o1 = List.rev raw_o1 in
  let o2 = List.rev raw_o2 in

  let module IDMap = Map.Make(struct
      type t = node_id
      let compare = compare
    end) in

  let indexed_o1 = index_by_identity o1 in
  let indexed_o2 = index_by_identity o2 in

  let m1 = List.fold_left (fun m (id, o) -> IDMap.add id o m) IDMap.empty indexed_o1 in
  let m2 = List.fold_left (fun m (id, o) -> IDMap.add id o m) IDMap.empty indexed_o2 in

  (* Elementi rimossi con posizione reale pos in o1 *)
  let removed_curr =
    indexed_o2 (* Nota: per la scansione mantieni l'iterazione ordinata *)
    |> List.filter_map (fun _ -> None) (* dummy per la struttura *)
    (* In alternativa usa la mappatura diretta su indexed_o1: *)
  in
  let removed_curr =
    indexed_o1
    |> List.mapi (fun pos (id, o) ->
        if IDMap.mem id m2 then None
        else Some (parent_path @ [id], o, pos)
      )
    |> List.filter_map (fun x -> x)
  in

  (* Elementi aggiunti con posizione reale pos in o2 *)
  let added_curr =
    indexed_o2
    |> List.mapi (fun pos (id, o) ->
        if IDMap.mem id m1 then None
        else Some (parent_path @ [id], o, pos)
      )
    |> List.filter_map (fun x -> x)
  in

  (* Confronto e ricorsione sui figli *)
  let common_diff =
    List.fold_left (fun acc (id, o2_node) ->
        let path = parent_path @ [id] in
        match IDMap.find_opt id m1 with
        | None -> acc
        | Some o1_node ->
            let node_changed =
              if not (equal_node o1_node o2_node) then [(path, o1_node, o2_node)] else []
            in
            (* o1_node.ol_children e o2_node.ol_children verranno invertiti
               automaticamente al prossimo passo ricorsivo *)
            let children_diff =
              diff_outlines_with_path path o1_node.ol_children o2_node.ol_children
            in
            combine acc (combine { empty with changed = node_changed } children_diff)
      ) empty indexed_o2
  in

  combine { added = added_curr; removed = removed_curr; changed = [] } common_diff


(* Estrae il parent_path e l'index da un node_path *)
let split_path path =
  match List.rev path with
  | [] -> []
  | _ :: rest -> List.rev rest

let detect_renames (diff : t) : t =
  let newly_changed, remaining_added, remaining_removed =
    List.fold_left (fun (changed_acc, current_added, current_removed) (rem_path, rem_node, rem_pos) ->
        let rem_parent = split_path rem_path in

        (* Cerca un candidato in added con stesso parent_path e stesso pos *)
        match List.find_opt (fun (add_path, _, add_pos) ->
            let add_parent = split_path add_path in
            add_parent = rem_parent && add_pos = rem_pos
          ) current_added with
        | Some (add_path, add_node, _) ->
            (* Trovato rename: crea la voce per changed usando rem_path (quello presente in GTK) *)
            let new_changed = (rem_path, rem_node, add_node) in
            let updated_added = List.filter (fun (p, _, _) -> p <> add_path) current_added in
            (new_changed :: changed_acc, updated_added, current_removed)
        | None ->
            (changed_acc, current_added, (rem_path, rem_node, rem_pos) :: current_removed)
      ) ([], diff.added, []) diff.removed
  in

  {
    added = remaining_added;
    removed = remaining_removed;
    changed = diff.changed @ newly_changed;
  }

let compare_outlines o1 o2 = diff_outlines_with_path [] o1 o2 |> detect_renames


module PathKey = struct
  type t = node_path
  let equal = (=)
  let hash = Hashtbl.hash
end

module PathHashtbl = Hashtbl.Make(PathKey)
