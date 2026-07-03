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

open Str
open Printf

let (//) = Filename.concat
let (!!) = Filename.dirname
let (!$) = Filename.quote
let (^^^) = Filename.check_suffix

(** Function application operator with different precedence/associativity than [@@].

    [f |=> x] is equivalent to [f x].

    Unlike [@@] (very low precedence, right-associative), [|=>] has default
    precedence, useful for applying handlers after [@@]:

    [f @@ g |=> function Ok x -> ... | Error e -> ...]
*)
let (|=>) f x = f x

(** try ... finally ... *)
let finally = fun f1 f2 ->
  try
    let result = Lazy.force f1 in
    Lazy.force f2;
    result
  with e -> (Lazy.force f2; raise e)

let (@$) = finally

let ( /* ) x f = f x and ( */ ) f x = f x;;


(** crono *)
let crono ?(label="Time") f x =
  let finally time =
    Printf.fprintf stdout "%s: %f sec." label (Unix.gettimeofday() -. time);
    print_newline();
  in
  let time = Unix.gettimeofday() in
  let result = try f x with e -> begin
      finally time;
      raise e
    end in
  finally time;
  result

(** {6 List operations} *)

module ListExt =
struct
  let pos x l =
    let rec f l n =
      match l with
      | [] -> raise Not_found
      | a :: b -> if x = a then n else f b (n + 1)
    in f l 0

  let remove_dupl l = (* slow *)
    List.rev (List.fold_left (fun acc y -> if List.mem y acc then acc else y :: acc) [] l)

  (** Returns the lowest of all elements of the list, compared via operator
      [Stdlib.(<)] on the function result. *)
  let min_by f = function
    | [] -> invalid_arg "min_by"
    | hd :: tl ->
        fst (List.fold_left (fun ((_, cand) as b) x ->
            let r = f x in
            if r < cand then x, r else b) (hd, f hd) tl)

  (** Returns the greatest of all elements of the list, compared via operator
      [Stdlib.(>)] on the function result. *)
  let max_by f = function
    | [] -> invalid_arg "max_by"
    | hd :: tl ->
        fst (List.fold_left (fun ((_, cand) as b) x ->
            let r = f x in
            if r > cand then x, r else b) (hd, f hd) tl)

  let min l = min_by Fun.id l
  let max l = max_by Fun.id l

  let rev_tl ll =
    let rec f acc = function
      | [] -> invalid_arg "Empty List"
      | _ :: [] -> []
      | h :: t -> h :: acc @ (f acc t)
    in f [] ll;;

  let group_assoc ll =
    let groups =
      List.fold_left begin fun groups (k, v) ->
        match List.assoc_opt k groups with
        | Some group ->
            group := v :: !group;
            groups
        | _ -> (k, ref [v]) :: groups
      end [] ll
    in
    List.map (fun (k, group) -> (k, List.rev !group)) groups;;

  (** Applies a key-generating function to each element of a list and yields a
      list of unique keys. Each unique key contains a list of all elements that
      match to this key. *)
  let group_by f ll = group_assoc (List.map (fun x -> f x, x) ll);;

  (** Returns a list of each element in the input list and its predecessor,
      with the exception of the first element which is only returned as the
      predecessor of the second element. [pairwise [1; 2; 3] = [(1, 2); (2, 3)]] *)
  let pairwise = function
    | [] | [_] -> []
    | hd :: tl -> List.fold_left_map (fun p x -> x, (p, x)) hd tl |> snd;;

  (** Returns elements from a list as long as a specified condition is true, and then skips
      the remaining elements. *)
  let rec take_while f = function
    | hd :: _ when not (f hd) -> []
    | hd :: tl -> hd :: (take_while f tl)
    | [] -> [];;

  let rec count_while f = function
    | hd :: _ when not (f hd) -> 0
    | _ :: tl -> 1 + (count_while f tl)
    | [] -> 0;;
end

(** {6 Memoization} *)

module Memo =
struct
  let fast ~f =
    let memo = Hashtbl.create 7 in
    fun ?(force=fun _ -> false) key ->
      try
        let data = Hashtbl.find memo key in
        if force data then begin
          Hashtbl.remove memo key;
          raise Not_found;
        end;
        data
      with Not_found ->
        let data = f key in
        Hashtbl.add memo key data;
        data;;

  let create f =
    let memo = Hashtbl.create 7 in
    fun key ->
      try Hashtbl.find memo key
      with Not_found ->
        let data = f key in
        Hashtbl.add memo key data;
        data;;

  let create2 f = create (fun x -> create (f x))

  let sfast ~f =
    let memo = Hashtbl.create 1 in
    let count = ref 0 in
    let found = ref 0 in
    (fun ?(force=fun _ -> false) key ->
       incr count;
       let data = try
           let r = Hashtbl.find memo key in
           incr found;
           if force r then begin
             Hashtbl.remove memo key;
             raise Not_found;
           end;
           r
         with Not_found ->
           let data = f key in
           Hashtbl.add memo key data;
           data
       in
       data),
    (fun () ->
       Printf.fprintf stdout "%i/%i = %f" !found !count (float(!found)/.float(!count));
       print_newline()),
    (fun () ->
       Hashtbl.clear memo;
       count := 0;
       found := 0
    )
end;;


(** [Str.regexp] with memoization *)
let (!~) = Memo.fast ~f:Str.regexp
let (!~~) = Memo.fast ~f:Str.regexp_string
let regexp = (!~)
let regexp_string = (!~~)
let regexp_case_fold = Memo.fast ~f:Str.regexp_case_fold
let regexp_string_case_fold = Memo.fast ~f:Str.regexp_string_case_fold

(** Miscellaneus string functions *)

let split re = Str.split (!~ re)
let ltrim = replace_first (!~ "^\\([ \t\r\n]+\\)") ""
let rtrim = replace_first (!~ "\\([ \t\r\n]+\\)$") ""
let trim_line = global_replace (!~ "[ \t]+\\($\\)") "\\1"

let lpad txt c width =
  let result = (String.make width c) ^ txt in
  String.sub result (String.length result - width) width;;

let rpad txt c width =
  let result = txt ^ (String.make width c) in
  String.sub result 0 width

(** Replaces all occurrences of substrings with the specified replacement strings. TThe last
    substring is replaced in the text obtained after replacing the second to last substring, the
    second to last after replacing the third to last and so on.

    @param memo Specifies whether to use the memo for compiling regular expressions (default) or
    recompiling them every time.

    @param regexp Specifies whether the substring to search for is a regular expression (default)
    or an exact string.
*)
let replace_all ?(memo=true) =
  let mk_regexp = (!~) ~force:(fun _ -> not memo) in
  fun ?(regexp=true) re_te s ->
    List.fold_left begin fun s' (re, te) ->
      Str.global_replace (if not regexp then (!~~) ~force:(fun _ -> not memo) re else mk_regexp re) te s'
    end s re_te

(** Like [replace_all] but only replaces the first occurrence. *)
let replace_first ?(memo=true) =
  let mk_regexp = (!~) ~force:(fun _ -> not memo) in
  fun ?(regexp=true) re_te s ->
    List.fold_left begin fun s' (re, te) ->
      Str.replace_first (mk_regexp (if not regexp then Str.quote re else re)) te s'
    end s re_te

(** [starts_with prefix s] returns [true] if and only if [s] starts with [prefix]. *)
let starts_with prefix s = string_partial_match (regexp_string prefix) s 0

(** [strip_prefix prefix str]

    If [str] starts with [prefix] strip the prefix from [str], otherwise return
    [str].

    Example:
     [strip_prefix "foo__" "foo__bar" = "bar"]
     [strip_prefix "foo__" "baz" = "baz"]
*)
let strip_prefix prefix s =
  let s_len = String.length s in
  let prefix_len = String.length prefix in
  if s_len <= prefix_len then
    s
  else if prefix = String.sub s 0 prefix_len then
    String.sub s prefix_len (s_len - prefix_len)
  else
    s


(** {6 Regular expression search} *)

module Search =
struct
  (** Represents control statements for searches. *)
  type 'a t =
      Skip (** Ignore the substring found. *)
    | Append of 'a (** Adds the found substring to the results list. *)
    | Found of 'a (** Returns the found substring as the only result and stops the search. *)

  (** Searches for all substrings in a text. The passed function accepts the found
      string and its position as parameters and returns a control statement of
      type [Search.t]. The final result of [all] is the list of all results
      found and processed. *)
  let all pat f ?(pos=0) ?(lookahead=false) text  =
    let rec search pos acc =
      try
        let pos = search_forward pat text pos in
        let matched_string = matched_string text in
        let lookahead = if lookahead then String.length matched_string else 1 in
        let g = search (pos + lookahead) in
        (match f ~pos ~matched_string with
         | Skip -> g acc
         | Append x -> g (x :: acc)
         | Found x -> [x])
      with Not_found -> acc in
    List.rev (search pos [])
end

(** get_lines_from_file *)
let get_lines_from_file ~filename lnums =
  let chan = open_in filename in
  let result = ref [] in
  let i = ref 1 in
  let lnums = ref (List.sort compare (ListExt.remove_dupl lnums)) in
  try
    begin
      try
        while !lnums <> [] do
          match !lnums with
          | lnum :: tl ->
              let line = input_line chan in
              if !i = lnum then begin
                result := (lnum, line) :: !result;
                lnums := tl;
              end;
              incr i;
          | _ -> assert false
        done;
      with End_of_file -> ()
    end;
    close_in chan;
    List.rev !result
  with ex -> (close_in chan; raise ex);;

(** map_file_lines *)
let map_file_lines filename f =
  let ichan = open_in filename in
  let tempfile, tchan = Filename.open_temp_file (Filename.basename filename) ".tmp" in
  let finally () =
    close_in_noerr ichan;
    close_out_noerr tchan;
  in
  try
    begin
      try
        let lnum = ref 0 in
        while true do
          let line = (input_line ichan) ^ "\n" in
          let new_line = f ~lnum:!lnum ~line in
          output_string tchan new_line;
          incr lnum
        done;
      with End_of_file -> ()
    end;
    finally();
    if Sys.file_exists filename then Sys.remove filename;
    if Sys.file_exists tempfile then Sys.rename tempfile filename;
  with ex -> begin
      finally();
      raise ex
    end;;

(** pushd and popd *)
let pushd, popd =
  let stack = Stack.create () in
  begin fun dir ->
    let cwd = Sys.getcwd () in
    Stack.push cwd stack;
    Sys.chdir dir
  end, (fun () -> Sys.chdir (Stack.pop stack))

(** mkdir_p *)
let mkdir_p =
  let mkdir d = Unix.mkdir d 0o755 in
  fun d ->
    let rec loop d =
      if not (Sys.file_exists d) then begin
        loop (Filename.dirname d);
        mkdir d
      end
    in
    loop d

(** filename_split *)
let filename_split filename =
  let rec loop filename =
    let dirname = Filename.dirname filename in
    if dirname = "." || filename = dirname then [filename] else
      let basename = Filename.basename filename in
      basename :: (loop dirname)
  in List.rev (loop filename);;

(*
filename_split "a/b";;
filename_split "/a/b";;
filename_split "C:\\a\\b";;
filename_split "C:\\a/b";;
filename_split "C:/a\\b";;
Filename.dirname "C:\\a";;
Filename.dirname "C:\\";;
Filename.concat "C:\\" "a";;
*)

(** [filename_relative path dirname] returns the part of [path] relative to [dirname].
  * E.g. [filename_relative "/a/b" "/a/b/c/d" = "c/d"]
*)
let filename_relative dirname =
  let dirname = filename_split dirname in
  fun path ->
    let path = filename_split path in
    let rec loop dirname path =
      match dirname, path with
      | [], path -> path
      | a :: b, x :: y when a = x -> loop b y
      | _ -> raise Not_found
    in try Some (List.fold_left Filename.concat "" (loop dirname path)) with Not_found -> None;;

(** filename_unix_implicit *)
let filename_unix_implicit filename =
  let parts = filename_split filename in
  let parts = if Filename.is_implicit filename then parts else (List.tl parts) in
  String.concat "/" parts;;

(** modname_of_path *)
let modname_of_path path =
  String.capitalize_ascii (Filename.chop_extension (Filename.basename path))

(** open_url *)
let open_url url =
  let cmd = "xdg-open " ^ url in
  let exit_code = Sys.command cmd in
  if exit_code > 0 then ksprintf failwith "Cannot execute %s" cmd





















