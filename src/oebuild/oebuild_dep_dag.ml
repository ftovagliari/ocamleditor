(*

  OCamlEditor
  Copyright (C) 2010-2014 Francesco Tovagliari

  This file is part of OCamlEditor.

  OCamlEditor is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  OCamlEditor is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.

*)

open Oebuild_util
open Printf

type t = (string, string list) Hashtbl.t

type dag_option = Dag of t * Oebuild_dep.ocamldeps | Cycle of string list

exception Cycle_exception of string list

(** array_exists *)
let array_exists from p a =
  try for i = from to Array.length a - 1 do
      if p a.(i) then raise Exit
    done; false with Exit -> true

(** reduce *)
let reduce : t -> unit = function table ->
  let rec (<-?-) x y =
    let deps = try Hashtbl.find table y with Not_found -> [] in
    (List.mem x deps) || (List.exists ((<-?-) x) deps)
  in
  let is_descendant = (*Utils.Memo.create2*) (<-?-) in
  let reduce ll =
    let stop = ref "" in
    let rec reduce' ll =
      let len = Array.length ll in
      if len <= 1 then ll
      else
        let fst = ll.(0) in
        if fst = !stop then ll
        else begin
          let len = len - 1 in
          if array_exists 1 (is_descendant fst) ll
          then begin
            let tail = Array.make len "" in
            Array.blit ll 1 tail 0 len;
            reduce' tail
          end else begin
            if !stop = "" then (stop := fst);
            Array.blit ll 1 ll 0 len;
            ll.(len) <- fst;
            reduce' ll
          end
        end
    in
    Array.to_list (reduce' (Array.of_list ll))
  in
  Hashtbl.iter (fun key deps -> Hashtbl.replace table key (reduce deps)) table
;;

(** dot_of_dag *)
let dot_of_dag (dag : t) =
  let buf = Buffer.create 1000 in
  Buffer.add_string buf "digraph {\n";
  Hashtbl.iter begin fun key ->
    List.iter (ksprintf (Buffer.add_string buf) "%S -> %S;\n" key)
  end dag;
  Buffer.add_string buf "}\n";
  Buffer.contents buf;;

(** find_toplevels *)
let find_toplevels ocamldeps =
  let all_deps =
    Hashtbl.fold begin fun key (_, deps) acc ->
      Printf.printf "OCAMLDEPS: %-30s -> [%s]\n%!" key (String.concat ", " deps);
      List.rev_append deps acc
    end ocamldeps []
  in
  let toplevels =
    Hashtbl.fold begin fun key _ acc ->
      if List.mem key all_deps then acc else key :: acc
    end ocamldeps [];
  in
  Printf.printf "\nTOPLEVELS: %s\n\n%!" (String.concat ", " toplevels);
  toplevels
;;

(** create_dag *)
let create_dag ?times ?pp ~toplevel_modules ~verbose () =
  let crono = if verbose >= 4 then crono else fun ?label:_ f x -> f x in

  let dirs = List.map Filename.dirname toplevel_modules in
  let dirs = List.filter ((<>) ".") dirs in
  let dirs = remove_dupl dirs in

  let search_path = List.map Ocaml_config.expand_includes dirs in
  let search_path = String.concat " " search_path in

  let ocamldeps = Oebuild_dep.ocamldep_recursive ?times ~search_path ?pp ~verbose:false toplevel_modules in
  if verbose >= 4 then Printf.printf "OCAMLDEPS LENGTH: %d\n%!" (Hashtbl.length ocamldeps);

  (* "ocamldeps" only contains depependencies that need to be recompiled
     (there is no need to create an expensive dag containing up-to-date
     dependencies). *)
  try
    let table = Hashtbl.create 17 in
    let rec add path node =
      if List.mem node path then raise (Cycle_exception (node :: path))
      else begin
        let changed, children = try Hashtbl.find ocamldeps node with Not_found -> false, [] in
        if not (Hashtbl.mem table node) then begin
          if changed then Hashtbl.add table node children;
          List.iter (add (node :: path)) children;
        end
      end
    in
    let toplevel_modules_cmx =
      List.map (fun filename -> (Filename.chop_extension filename) ^ ".cmx") toplevel_modules
    in
    let need_find_tl = List.exists (fun tl -> not (Hashtbl.mem ocamldeps tl)) toplevel_modules_cmx in
    (* If one of the toplevel modules (of the project target) is not present
       in the ocamldep-dag, incremental compilation can be performed but we need
       to find which are the toplevel modules inside the set of dependencies. *)
    let toplevel_modules_cmx =
      if need_find_tl
      then crono ~label:"Oebuild_dep_dag.create_dag, find_toplevels" find_toplevels ocamldeps
      else toplevel_modules_cmx
    in
    crono ~label:"Oebuild_dep_dag.create_dag, add" (List.iter (add [])) toplevel_modules_cmx;
    crono ~label:"Oebuild_dep_dag.create_dag, reduce" reduce table;
    if verbose >= 4 then begin
      Printf.printf "DAG: number of nodes = %d\n%!" (Hashtbl.length table);
      Hashtbl.iter begin fun node children ->
        Printf.printf "DAG: %-40s : [%s]\n%!" node (String.concat "; " children);
      end table;
    end;
    Dag ((table : t), ocamldeps)
  with Cycle_exception cycle ->
  match cycle with
  | hd :: _ ->
      let cycle =
        let found = ref false in
        List.filter begin fun x ->
          found := !found || x = hd;
          !found
        end (List.rev cycle)
      in
      Cycle ((*List.rev*) cycle)
  | [] -> assert false
;;

