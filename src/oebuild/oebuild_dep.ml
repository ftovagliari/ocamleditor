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


open Printf

type ocamldeps = (string, bool * string list) Hashtbl.t
exception Loop_found of string

let re1 = Str.regexp " ?:\\( \\|$\\)"
let re3 = Str.regexp " "
let split_nl = Str.split (Str.regexp "\n")

(** ocamldep_command *)
let ocamldep_command ?pp ?(slash=true) ?(search_path="") () =
  sprintf "%s%s %s -native -one-line %s"
    (Ocaml_config.ocamldep())
    (match pp with Some pp when pp <> "" -> " -pp " ^ pp | _ -> "" )
    search_path
    (if slash then "-slash" else "");;

(** Run ocamldep on one or more filenames given as single string separated by space.
    Return a list of strings, one for each filename in the following format

    "<target> : <dep1> <dep2> .."
*)
let run_ocamldep_on ~ignore_stderr ~verbose ?pp ?slash ?search_path filenames =
  let redirect_stderr = if ignore_stderr then Oebuild_util.redirect_stderr_to_null else "" in
  let cmd = ocamldep_command ?pp ?slash ?search_path () in
  let cmd = sprintf "%s %s %s" cmd filenames redirect_stderr in

  if verbose then (printf "%s\n%!" cmd);
  Shell.get_command_output cmd

(** ocamldep *)
let ocamldep ?times ?pp ?(ignore_stderr=false) ?(verbose=false) ?slash ?search_path filenames =
  let table : ocamldeps = Hashtbl.create 7 in
  if String.trim filenames <> "" then begin
    let entries = run_ocamldep_on ~ignore_stderr ~verbose ?pp ?slash ?search_path filenames in

    let replace =
      match times with
      | Some (times, opt) ->
          (* The resulting ocamldep-dag only contains files that need to be recompiled. *)
          fun table target dependencies ->
            let ml = Oebuild_util.replace_extension_to_ml target in
            let changed = Oebuild_table.update ~opt times ml in
            (* changed is true is source file is newer than object file *)
            Hashtbl.replace table target (changed, dependencies)
      | _ -> fun table target dependencies ->
        Hashtbl.replace table target (true, dependencies)
    in
    let open! Oebuild_util in
    List.iter begin fun entry ->
      match Str.split re1 entry with
      | key :: _ when key ^^^ ".cmo" -> ()
      | key :: [] -> replace table key []
      | [key; deps] ->
          let deps = Str.split re3 deps in
          replace table key deps;
      | _ -> eprintf "%s\n%s\n%!" filenames entry; assert false
    end entries;
  end;
  table;;

(** ocamldep_toplevels *)
let ocamldep_toplevels ?times ?pp ?ignore_stderr ?verbose ?slash ?(search_path="") toplevel_modules =
  let search_path, filenames = List.fold_left begin fun (sp, fn) x ->
      let dir = Filename.dirname x in
      if dir = "." then sp, ("*.ml *.mli" :: fn)
      else (" -I " ^ dir) :: sp, (sprintf "%s/*.ml %s/*.mli" dir dir) :: fn
    end ([search_path], []) toplevel_modules in
  let search_path = String.concat "" (Oebuild_util.remove_dupl search_path) in
  let filenames = String.concat " " (Oebuild_util.remove_dupl filenames) in
  ocamldep ?times ?pp ?ignore_stderr ~search_path ?verbose ?slash filenames

(** if a target dependency is marked as changed mark the target as changed as well
    and return the number of updates

    Also remove the target from the build cache so that it get compiled anew.
*)
let update_dependants dag =
  let dependency_changed target dag = match Hashtbl.find dag target with
    | _, deps -> List.fold_left (fun acc dep -> acc || Hashtbl.find dag dep |> fst) false deps
    | exception Not_found -> false
  in
  let counter = ref 0 in
  Hashtbl.filter_map_inplace (
    (fun target (changed, deps) ->
       if changed then
         Some (changed, deps)
       else
       if dependency_changed target dag then begin
         incr counter;
         Some (true, deps)
       end
       else
         Some (false, deps)))
    dag;
  !counter

(** ocamldep_recursive *)
let ocamldep_recursive ?times ?pp ?(ignore_stderr=false) ?(verbose=false) ?slash ?search_path toplevel_modules =
  let dag : ocamldeps = Hashtbl.create 17 in
  let rec loop ~toplevel_modules =
    let filenames = String.concat " " toplevel_modules in
    let ocamldeps = ocamldep ?times ?pp ~ignore_stderr ~verbose ?slash ?search_path filenames in
    let new_tops =
      Hashtbl.fold begin fun key (changed, deps) acc ->
        Hashtbl.add dag key (changed, deps);
        List.rev_append deps acc
      end ocamldeps []
    in
    let new_tops = List.filter (fun tl -> not (Hashtbl.mem dag tl)) new_tops in
    let new_tops = Oebuild_util.remove_dupl new_tops in
    let new_tops = List.map Oebuild_util.replace_extension_to_ml new_tops in
    if new_tops <> [] then loop ~toplevel_modules:new_tops
  in
  loop ~toplevel_modules;
  while update_dependants dag > 0 do () done;
  dag

(** sort_dependencies *)
let sort_dependencies (dag : ocamldeps) =
  let dag = Hashtbl.copy dag in
  let get_leaves dag =
    Hashtbl.fold begin fun key (_ (*changed*), deps) acc ->
      let deps = List.filter (Hashtbl.mem dag) deps in
      if deps = [] then key :: acc else acc
    end dag []
  in
  let rec loop res =
    match get_leaves dag with
    | [] -> res
    | leaves ->
        List.iter (Hashtbl.remove dag) leaves;
        loop (List.rev_append leaves res);
  in
  List.rev (Oebuild_util.remove_dupl (loop []))
;;

(** find_dependants *)
let find_dependants =
  let re = Str.regexp "\\(.+\\.mli?\\) ?: ?\\(.*\\)" in
  let re1 = Str.regexp "\r?\n" in
  let re2 = Str.regexp " " in
  fun ~dirname ~modname ->
    (*let dir = Filename.dirname target in*)
    let dir = if dirname = Filename.current_dir_name then "" else (dirname ^ "/") in
    let cmd = sprintf "%s -modules -native %s*.ml %s*.mli%s"
        (Ocaml_config.ocamldep()) dir dir Oebuild_util.redirect_stderr_to_null in
    printf "%s (%s)\n%!" cmd modname;
    let ocamldep = String.concat "\n" (Shell.get_command_output cmd) in
    let entries = Str.split re1 ocamldep in
    let entries =
      List.map begin fun entry ->
        if Str.string_match re entry 0 then begin
          let filename = Str.matched_group 1 entry in
          let modules = Str.matched_group 2 entry in
          (filename, (Str.split re2 modules))
        end else (assert false)
      end entries
    in
    let dependants = ref [] in
    let rec loop modname =
      List.iter begin fun (filename, modules) ->
        if List.mem modname modules then begin
          if not (List.mem filename !dependants) then begin
            dependants := filename :: !dependants;
            let prefix = Filename.chop_extension filename in
            let prefix_mli = prefix ^ ".mli" in
            if List.mem_assoc prefix_mli entries then (dependants := prefix_mli :: !dependants;);
            let mdep = String.capitalize_ascii (Filename.basename prefix) in
            ignore (loop mdep);
          end
        end
      end entries;
      !dependants
    in
    loop modname

(** find_dependants *)
let find_dependants ~path ~modname =
  let dependants = List.map (fun dirname -> find_dependants ~dirname ~modname) path in
  List.flatten dependants
;;



(** ========================================================================== *)



(** find_dep (deprecated use Oebuild.ocamldep) *)
let find_dep ?pp ?(ignore_stderr=false) ?(echo=true) target =
  let dir = Filename.dirname target in
  let filenames =
    (match dir with "." -> "*.mli" | _ -> dir ^ "/" ^ "*.mli *.mli") ^ " " ^
    (match dir with "." -> "*.ml" | _ -> dir ^ "/" ^ "*.ml *.ml")
  in
  let search_path = Ocaml_config.expand_includes dir in
  let table = ocamldep ?pp ~ignore_stderr ~verbose:echo ~search_path filenames in
  let target = (Filename.chop_extension target) ^ ".cmx" in
  let anti_loop = ref [] in
  let result = ref [] in
  let rec find_chain target =
    if (List.mem target !anti_loop) && (not (List.mem target !result))
    then (raise (Loop_found (String.concat " " (List.map Oebuild_util.replace_extension_to_ml (target :: !anti_loop)))));
    anti_loop := target :: (List.filter ((<>) target) !anti_loop);
    try
      if not (List.mem target !result) then begin
        match Hashtbl.find table target with
        | (_, []) -> result := target :: !result;
        | (_, deps) ->
            List.iter find_chain deps;
            result := target :: !result;
      end
    with Not_found ->
      (* This exception can be caused by syntax errors in the source files. *)
      (ksprintf failwith "Dep: %s" target)
  in
  find_chain target;
  List.rev ((*List.map replace_extension_to_ml*) !result)
;;

(** find (deprecated use Oebuild.ocamldep) *)
let find ?pp ?ignore_stderr ?(echo=true) targets =
  let deps = List.map (find_dep ?pp ?ignore_stderr ~echo) targets in
  let deps = List.flatten deps in
  List.rev (List.fold_left begin fun acc x ->
      if not (List.mem x acc) then x :: acc else acc
    end [] deps)
;;
