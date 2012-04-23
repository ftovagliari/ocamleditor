(*

  OCamlEditor
  Copyright (C) 2010-2012 Francesco Tovagliari

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


let code = "#directory \"+threads\" #load \"str.cma\" #load \"unix.cma\" #load \"threads.cma\"\nlet split re = Str.split (Str.regexp re)\nmodule Quote = struct let path = if Sys.os_type = \"Win32\" then (fun x -> Filename.quote (Filename.quote x)) else (fun x -> x) let arg = if Sys.os_type = \"Win32\" then (fun x -> Filename.quote x) else (fun x -> x) end\nmodule Cmd = struct open Printf let expand = let trimfunc = let replace = Str.global_replace (Str.regexp \"\\\\(^[ \\t\\r\\n]+\\\\)\\\\|\\\\([ \\t\\r\\n]+$\\\\)\") in fun str -> replace \"\" str in fun ?(trim=true) ?(first_line=false) ?filter command -> let ichan = Unix.open_process_in command in let finally () = ignore (Unix.close_process_in ichan) in let data = Buffer.create 100 in begin try let get_line ichan = if trim then trimfunc (input_line ichan) else input_line ichan in while true do let line = get_line ichan in if first_line && String.length line = 0 then begin end else if first_line then begin Buffer.add_string data line; raise End_of_file end else begin match filter with | None -> Buffer.add_string data line; Buffer.add_char data '\\n' | Some f when (f line) -> Buffer.add_string data line; Buffer.add_char data '\\n' | _ -> () end done with | End_of_file -> () | ex -> (finally(); raise ex) end; finally(); if Buffer.length data = 0 then (kprintf failwith \"Cmd.expand: %s\" command); Buffer.contents data;; end\nmodule Ocaml_config = struct open Printf let rec putenv_ocamllib value = match Sys.os_type with | \"Win32\" -> let value = match value with None -> \"\" | Some x -> x in Unix.putenv \"OCAMLLIB\" value | _ -> ignore (Sys.command \"unset OCAMLLIB\") let redirect_stderr = if Sys.os_type = \"Win32\" then \" 2>NUL\" else \" 2>/dev/null\" let find_best_compiler compilers = try List.find begin fun comp -> try ignore (kprintf Cmd.expand \"%s -version%s\" comp redirect_stderr); true with _ -> false end compilers with Not_found -> kprintf failwith \"Cannot find compilers: %s\" (String.concat \", \" compilers) let find_tool which path = let commands = match which with | `BEST_OCAMLC -> [\"ocamlc.opt\"; \"ocamlc\"] | `BEST_OCAMLOPT -> [\"ocamlopt.opt\"; \"ocamlopt\"] | `BEST_OCAMLDEP -> [\"ocamldep.opt\"; \"ocamldep\"] | `BEST_OCAMLDOC -> [\"ocamldoc.opt\"; \"ocamldoc\"] | `OCAMLC -> [\"ocamlc\"] | `OCAML -> [\"ocaml\"] in let quote    = if path <> \"\" && Sys.os_type = \"Win32\" && String.contains path ' ' then Filename.quote else (fun x -> x) in let path     = if path <> \"\" then Filename.concat path \"bin\" else \"\" in find_best_compiler (List.map quote (List.map (Filename.concat path) commands)) let get_home () = try Sys.getenv \"OCAML_HOME\" with Not_found -> \"\" let expand_includes = let split = Str.split (Str.regexp \" +\") in fun compact -> if String.length compact > 0 then (\"-I \" ^ (String.concat \" -I \" (split compact))) else \"\"  let ocamlc ()   = find_tool `BEST_OCAMLC (get_home ()) let ocamlopt () = find_tool `BEST_OCAMLOPT (get_home ()) let ocamldep () = find_tool `BEST_OCAMLDEP (get_home ()) let ocamldoc () = find_tool `BEST_OCAMLDOC (get_home ()) let ocaml ()    = find_tool `OCAML (get_home ()) let ocamllib () = Cmd.expand ~first_line:true ((ocamlc()) ^ \" -where\")  let ocaml_version ?(compiler=ocamlc()) () = Cmd.expand (compiler ^ \" -v \" ^ redirect_stderr)  let can_compile_native ?ocaml_home () = let result = ref false in let filename = Filename.temp_file \"test_native\" \".ml\" in let ochan = open_out filename in begin try output_string ochan (\"0\"); close_out ochan with _ -> (close_out ochan) end; let outname = Filename.chop_extension filename in let compiler = match ocaml_home with | Some home -> find_tool `BEST_OCAMLOPT home | _ -> \"ocamlopt\" in let cmd = sprintf \"%s -o %s %s%s\" compiler outname filename redirect_stderr in result := (Sys.command cmd) = 0; if Sys.file_exists filename then (Sys.remove filename); if Sys.file_exists outname then (Sys.remove outname); let cmi = outname ^ \".cmi\" in if Sys.file_exists cmi then (Sys.remove cmi); let cmx = outname ^ \".cmx\" in if Sys.file_exists cmx then (Sys.remove cmx); let obj = outname ^ \".o\" in if Sys.file_exists obj then (Sys.remove obj); let obj = outname ^ \".obj\" in if Sys.file_exists obj then (Sys.remove obj); if !result then begin let conf = kprintf Cmd.expand \"%s -config\" compiler in let re = Str.regexp \"ccomp_type: \\\\(.*\\\\)\\n\" in if Str.search_forward re conf 0 >= 0 then begin Some (Str.matched_group 1 conf) end else Some \"<unknown ccomp_type>\" end else None; ;; end\nmodule Dep = struct open Printf exception Loop_found of string let trim = let re = Str.regexp \"[ \\n\\r\\n\\t]+$\" in Str.replace_first re \"\" let (^^) = Filename.check_suffix let (!$) = Filename.chop_extension let (//) = Filename.concat let redirect_stderr = if Sys.os_type = \"Win32\" then \" 2>NUL\" else \" 2>/dev/null\" let re1 = Str.regexp \":\\\\( \\\\|$\\\\)\" let re2 = Str.regexp \" \\\\\\\\[\\r\\n]+\" let re3 = Str.regexp \" \" let re_ss = Str.regexp \"\\\\\\\\ \" let re_00 = Str.regexp \"\\x00\\x00\" let split_nl = Str.split (Str.regexp \"\\n\")  let replace_extension x = sprintf \"%s.%s\" (Filename.chop_extension x) (if x ^^ \"cmi\" then \"mli\" else if x ^^ \"cmx\" then \"ml\" else assert false);;  let find_dep ?pp ?includes ?(with_errors=true) ?(echo=true) target = let dir = Filename.dirname target in let anti_loop = ref [] in let table = Hashtbl.create 7 in let redirect_stderr = if with_errors then \"\" else (if Sys.os_type = \"Win32\" then \" 2>NUL\" else \" 2>/dev/null\") in let command = sprintf \"%s%s %s -native -slash %s %s %s\" (Ocaml_config.ocamldep()) (match pp with Some pp when pp <> \"\" -> \" -pp \" ^ pp | _ -> \"\" ) (Ocaml_config.expand_includes dir) (match dir with \".\" -> \"*.mli\" | _ -> dir // \"*.mli *.mli\") (match dir with \".\" -> \"*.ml\" | _ -> dir // \"*.ml *.ml\") redirect_stderr in if echo then (printf \"%s\\n%!\" command); let ocamldep = Cmd.expand command in let ocamldep = Str.global_replace re2 \" \" ocamldep in let entries = split_nl ocamldep in List.iter begin fun entry -> match Str.split re1 entry with | key :: [] -> Hashtbl.add table key None | [key; deps] -> let deps = Str.global_replace re_ss \"\\x00\\x00\" deps in let deps = Str.split re3 deps in let deps = List.map (Str.global_replace re_00 \"\\\\ \") deps in Hashtbl.add table key (Some deps) | _ -> eprintf \"%s\\n%s\\n%!\" command entry; assert false end entries; let target = (Filename.chop_extension target) ^ \".cmx\" in let result = ref [] in let rec find_chain target = if (List.mem target !anti_loop) && (not (List.mem target !result)) then (raise (Loop_found (String.concat \" \" (List.map replace_extension (target :: !anti_loop))))); anti_loop := target :: (List.filter (fun x -> x <> target) !anti_loop); try if not (List.mem target !result) then begin match Hashtbl.find table target with | None -> result := target :: !result | Some deps -> List.iter find_chain deps; result := target :: !result; end with Not_found ->  (kprintf failwith \"Dep: %s\" target) in find_chain target; List.rev (List.map replace_extension !result)  let find ?pp ?includes ?with_errors ?(echo=true) targets = let deps = List.map (find_dep ?pp ?includes ?with_errors ~echo) targets in let deps = List.flatten deps in List.rev (List.fold_left begin fun acc x -> if not (List.mem x acc) then x :: acc else acc end [] deps)  let find_dependants = let re = Str.regexp \"\\\\(.+\\\\.mli?\\\\): ?\\\\(.*\\\\)\" in let re1 = Str.regexp \"\\r?\\n\" in let re2 = Str.regexp \" \" in fun ~target ~modname -> let dir = Filename.dirname target in let dir = if dir = Filename.current_dir_name then \"\" else (dir ^ \"/\") in let cmd = sprintf \"%s -modules -native %s*.ml %s*.mli%s\" (Ocaml_config.ocamldep()) dir dir redirect_stderr in printf \"%s (%s)\\n%!\" cmd modname; let ocamldep = Cmd.expand cmd in let entries = Str.split re1 ocamldep in let entries = List.map begin fun entry -> if Str.string_match re entry 0 then begin let filename = Str.matched_group 1 entry in let modules = Str.matched_group 2 entry in (filename, (Str.split re2 modules)) end else (assert false) end entries in let dependants = ref [] in let rec loop modname = List.iter begin fun (filename, modules) -> if List.mem modname modules then begin if not (List.mem filename !dependants) then begin dependants := filename :: !dependants; let prefix = Filename.chop_extension filename in let prefix_mli = prefix ^ \".mli\" in if List.mem_assoc prefix_mli entries then (dependants := prefix_mli :: !dependants;); let mdep = String.capitalize prefix in ignore (loop mdep); end end end entries; !dependants in loop modname let find_dependants ~targets ~modname = let dependants = List.map (fun target -> find_dependants ~target ~modname) targets in List.flatten dependants ;; end\nmodule Cmd_line_args = struct type state = StartArg | InUnquotedArg | InQuotedArg | InQuotedArgAfterQuote;; let format = String.concat \" \";; let parse line = let args = ref [] in let buf = Buffer.create 10 in let state = ref StartArg in let start_arg () = state := StartArg; args := (Buffer.contents buf) :: !args; Buffer.clear buf; in String.iter begin function | (' ' as ch) when !state = InQuotedArg -> Buffer.add_char buf ch | ' ' when !state = StartArg -> () | ' ' when !state = InUnquotedArg -> start_arg (); | ' ' -> start_arg () | ('\"' as ch) when !state = StartArg -> state := InQuotedArg; Buffer.add_char buf ch | ('\"' as ch) when !state = InQuotedArg -> Buffer.add_char buf ch; start_arg (); | ('\"' as ch) when !state = InQuotedArgAfterQuote -> Buffer.add_char buf ch; state := InQuotedArg; | ('\"' as ch) when !state = InUnquotedArg -> start_arg (); Buffer.add_char buf ch; state := InQuotedArg; | ('\\\\' as ch) when !state = InQuotedArg -> state := InQuotedArgAfterQuote; Buffer.add_char buf ch | ch when !state = InQuotedArgAfterQuote -> state := InQuotedArg; Buffer.add_char buf ch; | ch when !state = StartArg -> state := InUnquotedArg; Buffer.add_char buf ch; | ch -> Buffer.add_char buf ch; end line; if Buffer.length buf > 0 then (start_arg ()); List.rev !args;;  end\nmodule Task = struct open Printf type kind = [ `CLEAN | `CLEANALL | `ANNOT | `COMPILE | `RUN | `OTHER] type phase = Before_clean | Clean | After_clean | Before_compile | Compile | After_compile type t = { mutable et_name                  : string; mutable et_env                   : (bool * string) list; mutable et_env_replace           : bool;                                mutable et_dir                   : string;                                                                                            mutable et_cmd                   : string; mutable et_args                  : (bool * string) list; mutable et_phase                 : phase option; mutable et_always_run_in_project : bool; mutable et_always_run_in_script  : bool; } let string_of_phase = function | Before_clean -> \"Before_clean\" | Clean -> \"Clean\" | After_clean -> \"After_clean\" | Before_compile -> \"Before_compile\" | Compile -> \"Compile\" | After_compile -> \"After_compile\" let descr_of_phase = function | Before_clean -> \"Pre-clean\" | Clean -> \"Clean\" | After_clean -> \"Post-clean\" | Before_compile -> \"Pre-build\" | Compile -> \"Build\" | After_compile -> \"Post-build\" let phase_of_string = function | \"Before_clean\" -> Before_clean | \"Clean\" -> Clean | \"After_clean\" -> After_clean | \"Before_compile\" -> Before_compile | \"Compile\" -> Compile | \"After_compile\" -> After_compile | _ -> failwith \"phase_of_string\" let create ~name ~env ?(env_replace=false) ~dir ~cmd ~args ?phase () = { et_name                  = name; et_env                   = env; et_env_replace           = env_replace; et_dir                   = dir; et_cmd                   = cmd; et_args                  = args; et_phase                 = phase; et_always_run_in_project = false; et_always_run_in_script  = true; }  let handle f task = let tenv = Array.of_list task.et_env in let env = if task.et_env_replace then Array.concat [                        tenv] else (Array.concat [tenv                       ; (Array.map (fun e -> true, e) (Unix.environment()))]) in let env = List.filter (fun (e, _) -> e) (Array.to_list env) in let env = Array.of_list (List.map (fun (_, v) -> v) env) in let prog = task.et_cmd in let dir = if task.et_dir <> \"\" then task.et_dir else (Sys.getcwd ()) in let args = List.filter (fun (e, _) -> e) task.et_args in let args = List.flatten (List.map (fun (_, v) -> Cmd_line_args.parse v) args) in f ~env ~dir ~prog ~args;; end\nmodule Oebuild_util = struct open Printf let (!$) = Filename.chop_extension let (//) = Filename.concat let (^^) = Filename.check_suffix let is_win32, win32 = (Sys.os_type = \"Win32\"), (fun a b -> match Sys.os_type with \"Win32\" -> a | _ -> b) let may opt f = match opt with Some x -> f x | _ -> () let re_spaces = Str.regexp \" +\"  let crono ?(label=\"Time\") f x = let finally time = Printf.fprintf stdout \"%s: %f sec.\" label (Unix.gettimeofday() -. time); print_newline(); in let time = Unix.gettimeofday() in let result = try f x with e -> begin finally time; raise e end in finally time; result  let remove_dupl l = List.rev (List.fold_left (fun acc y -> if List.mem y acc then acc else y :: acc) [] l)  let remove_file ?(verbose=false) filename = try if Sys.file_exists filename then (Sys.remove filename; if verbose then print_endline filename) with Sys_error ex -> eprintf \"%s\\n%!\" ex  let command ?(echo=true) cmd = let cmd = Str.global_replace re_spaces \" \" cmd in if echo then (printf \"%s\\n%!\" cmd); let exit_code = Sys.command cmd in Pervasives.flush stderr; Pervasives.flush stdout; exit_code  let iter_chan chan f = try while true do f chan done with End_of_file -> ()  let exec ?(env=Unix.environment()) ?(echo=true) ?(join=true) ?at_exit ?(process_err=(fun ~stderr -> prerr_endline (input_line stderr))) cmd = let cmd = Str.global_replace re_spaces \" \" cmd in if echo then (print_endline cmd); let (inchan, _, errchan) as channels = Unix.open_process_full cmd env in let close () = match Unix.close_process_full channels with | Unix.WEXITED code -> code | _ -> (-1) in let thi = Thread.create begin fun () -> iter_chan inchan (fun chan -> print_endline (input_line chan)) end () in let the = Thread.create begin fun () -> iter_chan errchan (fun chan -> process_err ~stderr:chan); match at_exit with None -> () | Some f -> ignore (close()); f() end () in if join then begin Thread.join the; Thread.join thi; end; if at_exit = None then (close()) else 0  let rm = win32 \"DEL /F /Q\" \"rm -f\"  let copy_file ic oc = let buff = String.create 0x1000 in let rec copy () = let n = input ic buff 0 0x1000 in if n = 0 then () else (output oc buff 0 n; copy()) in copy() let cp ?(echo=true) src dst = let ic = open_in_bin src in let oc = open_out_bin dst in if echo then (printf \"%s -> %s\\n%!\" src dst); let finally () = close_out oc; close_in ic in try copy_file ic oc; finally() with ex -> (finally(); raise ex)  let rec mkdir_p ?(echo=true) d = if not (Sys.file_exists d) then begin mkdir_p (Filename.dirname d); printf \"mkdir -p %s\\n%!\" d; (Unix.mkdir d 0o755) end end\nmodule Oebuild_table = struct open Printf  type t = (string, float) Hashtbl.t let oebuild_times_filename = \".oebuild\" let (^^) filename opt = filename ^ (if opt then \".opt\" else \".byt\") let find (table : t) filename opt = Hashtbl.find table (filename ^^ opt) let add (table : t) filename opt = Hashtbl.add table (filename ^^ opt) let remove (table : t) filename opt = Hashtbl.remove table (filename ^^ opt)  let read () = if not (Sys.file_exists oebuild_times_filename) then begin let ochan = open_out_bin oebuild_times_filename in Marshal.to_channel ochan (Hashtbl.create 7) []; close_out ochan end; let ichan = open_in_bin oebuild_times_filename in let times = Marshal.from_channel ichan in close_in ichan; (times : t)  let write (times : t) = if Hashtbl.length times > 0 then begin let ochan = open_out_bin oebuild_times_filename in Marshal.to_channel ochan times []; close_out ochan end  let update = let get_last_compiled_time ~opt cache filename = try let time = find cache filename opt in let ext = if opt then \"cmx\" else \"cmo\" in let cm = sprintf \"%s.%s\" (Filename.chop_extension filename) ext in if Sys.file_exists cm then time else begin remove cache filename opt; raise Not_found end with Not_found -> 0.0 in fun ~deps ~opt (cache : t) filename -> let ctime = get_last_compiled_time ~opt cache filename in if ctime > 0.0 && ((Unix.stat filename).Unix.st_mtime) >= ctime then begin remove cache filename opt; end ;; end\nmodule Oebuild = struct open Printf open Oebuild_util module Table = Oebuild_table type compilation_type = Bytecode | Native | Unspecified type output_kind = Executable | Library | Plugin | Pack type build_exit = Built_successfully | Build_failed of int type process_err_func = (stderr:in_channel -> unit) let string_of_compilation_type = function | Bytecode -> \"Bytecode\" | Native -> \"Native\" | Unspecified -> \"Unspecified\" let ocamlc = Ocaml_config.ocamlc() let ocamlopt = Ocaml_config.ocamlopt() let ocamllib = Ocaml_config.ocamllib()  let compile ?(times : Table.t option) ~opt ~compiler ~cflags ~includes ~filename ?(process_err : process_err_func option) () = if Sys.file_exists filename then begin try begin match times with | Some times -> ignore (Table.find times filename opt); 0                 | _ -> raise Not_found end with Not_found -> begin let cmd = (sprintf \"%s -c %s %s %s\" compiler cflags includes filename) in let exit_code = match process_err with | None -> command cmd | Some process_err -> exec ~process_err cmd in may times (fun times -> Table.add times filename opt (Unix.gettimeofday())); exit_code end end else 0  let link ~compiler ~outkind ~lflags ~includes ~libs ~outname ~deps ?(process_err : process_err_func option) () = let opt = compiler = ocamlopt in let libs = if opt && outkind = Library then \"\" else let ext = if opt then \"cmxa\" else \"cma\" in let libs = List.map begin fun x -> if Filename.check_suffix x \".o\" then begin let x = Filename.chop_extension x in let ext = if opt then \"cmx\" else \"cmo\" in sprintf \"%s.%s\" x ext end else if Filename.check_suffix x \".obj\" then begin sprintf \"%s\" x end else (sprintf \"%s.%s\" x ext) end libs in String.concat \" \" libs in let deps = String.concat \" \" deps in kprintf (exec             ?process_err) \"%s %s %s -o %s %s %s %s\" compiler (match outkind with Library -> \"-a\" | Plugin -> \"-shared\" | Pack -> \"-pack\" | Executable -> \"\") lflags outname includes libs deps ;;  let get_output_name ~compilation ~outkind ~outname ~targets = let o_ext = match outkind with | Library when compilation = Native -> \".cmxa\" | Library -> \".cma\" | Executable when compilation = Native -> \".opt\" ^ (win32 \".exe\" \"\") | Executable -> win32 \".exe\" \"\" | Plugin -> \".cmxs\" | Pack -> \".cmx\" in let name = if outname = \"\" then begin match (List.rev targets) with | last :: _ -> Filename.chop_extension last | _ -> assert false end else outname in name ^ o_ext ;;  let install_output ~compilation ~outkind ~outname ~deps ~path ~ccomp_type = let dest_outname = Filename.basename outname in match outkind with | Library -> let path = let path = ocamllib // path in mkdir_p path; path in cp outname (path // dest_outname); let deps_mod = List.map Filename.chop_extension deps in let deps_mod = remove_dupl deps_mod in let cmis = List.map (fun d -> sprintf \"%s.cmi\" d) deps_mod in let mlis = List.map (fun cmi -> sprintf \"%s.mli\" (Filename.chop_extension cmi)) cmis in let mlis = List.filter Sys.file_exists mlis in List.iter (fun x -> ignore (cp x (path // (Filename.basename x)))) cmis; List.iter (fun x -> ignore (cp x (path // (Filename.basename x)))) mlis; if compilation = Native then begin let ext = match ccomp_type with Some \"msvc\" -> \".lib\" | Some _ ->  \".a\" | None -> assert false in let basename = sprintf \"%s%s\" (Filename.chop_extension outname) ext in cp basename (path // (Filename.basename basename)); end; | Executable -> mkdir_p path; cp outname (path // dest_outname) | Plugin | Pack -> eprintf \"\\\"install_output\\\" not implemented for Plugin or Pack.\" ;;  let run_output ~outname ~args = let args = List.rev args in if is_win32 then begin let cmd = Str.global_replace (Str.regexp \"/\") \"\\\\\\\\\" outname in let args = String.concat \" \" args in ignore (kprintf command \"%s %s\" cmd args) end else begin let cmd = Filename.current_dir_name // outname in  let args = cmd :: args in let args = Array.of_list args in Unix.execv cmd args end ;;  let sort_dependencies ~deps subset = let result = ref [] in List.iter begin fun x -> if List.mem x subset then (result := x :: !result) end deps; List.rev !result ;;  let filter_inconsistent_assumptions_error ~compiler_output ~recompile ~targets ~deps ~(cache : Table.t) ~opt = let re_inconsistent_assumptions = Str.regexp \".*make[ \\t\\r\\n]+inconsistent[ \\t\\r\\n]+assumptions[ \\t\\r\\n]+over[ \\t\\r\\n]+\\\\(interface\\\\|implementation\\\\)[ \\t\\r\\n]+\\\\([^ \\t\\r\\n]+\\\\)[ \\t\\r\\n]+\" in let re_error = Str.regexp \"Error: \" in ((fun ~stderr -> let line = input_line stderr in Buffer.add_string compiler_output (line ^ \"\\n\"); let messages = Buffer.contents compiler_output in let len = String.length messages in try let pos = Str.search_backward re_error messages len in let last_error = String.sub messages pos (len - pos) in begin try let _ = Str.search_backward re_inconsistent_assumptions last_error (String.length last_error) in let modname = Str.matched_group 2 last_error in let dependants = Dep.find_dependants ~targets ~modname in let dependants = sort_dependencies ~deps dependants in let _                    = Buffer.contents compiler_output in eprintf \"Warning (oebuild): the following files make inconsistent assumptions over interface/implementation %s: %s\\n%!\" modname (String.concat \", \" dependants); List.iter begin fun filename -> Table.remove cache filename opt; let basename = Filename.chop_extension filename in let cmi = basename ^ \".cmi\" in if Sys.file_exists cmi then (Sys.remove cmi); let cmo = basename ^ \".cmo\" in if Sys.file_exists cmo then (Sys.remove cmo); let cmx = basename ^ \".cmx\" in if Sys.file_exists cmx then (Sys.remove cmx); let obj = basename ^ (win32 \".obj\" \".o\") in if Sys.file_exists obj then (Sys.remove obj); end dependants; recompile := dependants; with Not_found -> () end with Not_found -> ()) : process_err_func) ;;  let build ~compilation ~includes ~libs ~other_mods ~outkind ~compile_only ~thread ~vmthread ~annot ~pp ~cflags ~lflags ~outname ~deps ~ms_paths ~targets ?(prof=false) () = let split_space = Str.split (Str.regexp \" +\") in  let includes = ref includes in includes := Ocaml_config.expand_includes !includes;  let libs = split_space libs in  let cflags = ref cflags in let lflags = ref lflags in  if thread then (cflags := !cflags ^ \" -thread\"; lflags := !lflags ^ \" -thread\"); if vmthread then (cflags := !cflags ^ \" -vmthread\"; lflags := !lflags ^ \" -vmthread\"); if annot then (cflags := !cflags ^ \" -annot\"); if pp <> \"\" then (cflags := !cflags ^ \" -pp \" ^ pp);  let compiler = if prof then \"ocamlcp -p a\" else if compilation = Native then ocamlopt else ocamlc in  let mods = split_space other_mods in let mods = if compilation = Native then List.map (sprintf \"%s.cmx\") mods else List.map (sprintf \"%s.cmo\") mods in if compilation = Native && !ms_paths then begin lflags := !lflags ^ \" -ccopt \\\"-LC:\\\\Programmi\\\\MIC977~1\\\\Lib -LC:\\\\Programmi\\\\MID05A~1\\\\VC\\\\lib -LC:\\\\GTK\\\\lib\\\"\" end; let times = Table.read () in let build_exit = let compilation_exit = ref 0 in  begin try let opt = compilation = Native in let compiler_output = Buffer.create 100 in let rec try_compile filename = let recompile = ref [] in let compile_exit = Table.update ~deps ~opt times filename; let exit_code = compile ~process_err:(filter_inconsistent_assumptions_error ~compiler_output ~recompile ~targets ~deps ~cache:times ~opt) ~times ~opt ~compiler ~cflags:!cflags ~includes:!includes ~filename () in if exit_code <> 0 then (Table.remove times filename opt); exit_code in if List.length !recompile > 0 then begin List.iter begin fun filename -> compilation_exit := compile ~times ~opt ~compiler ~cflags:!cflags ~includes:!includes ~filename (); if !compilation_exit <> 0 then (raise Exit) end !recompile; print_newline(); Buffer.clear compiler_output; try_compile filename; end else begin if Buffer.length compiler_output > 0 then (eprintf \"%s\\n%!\" (Buffer.contents compiler_output)); compile_exit end in List.iter begin fun filename -> compilation_exit := try_compile filename; if !compilation_exit <> 0 then (raise Exit) end deps; with Exit -> () end;  if !compilation_exit = 0 then begin let opt = compilation = Native in let obj_deps = let ext = if compilation = Native then \"cmx\" else \"cmo\" in let deps = List.filter (fun x -> x ^^ \"ml\") deps in List.map (fun x -> sprintf \"%s.%s\" (Filename.chop_extension x) ext) deps in if compile_only then !compilation_exit else begin let obj_deps = mods @ obj_deps in let compiler_output = Buffer.create 100 in let rec try_link () = let recompile = ref [] in let link_exit = link ~compiler ~outkind ~lflags:!lflags ~includes:!includes ~libs ~deps:obj_deps ~outname ~process_err:(filter_inconsistent_assumptions_error ~compiler_output ~recompile ~targets ~deps ~cache:times ~opt) () in if List.length !recompile > 0 then begin List.iter begin fun filename -> ignore (compile ~times ~opt ~compiler ~cflags:!cflags ~includes:!includes ~filename ()) end !recompile; Buffer.clear compiler_output; try_link() end else begin eprintf \"%s\\n%!\" (Buffer.contents compiler_output); link_exit end in try_link() end end else !compilation_exit in Table.write times; if build_exit = 0 then Built_successfully else (Build_failed build_exit) ;;  let clean ?(all=false) ~compilation ~outkind ~outname ~targets ~deps () = let outname = get_output_name ~compilation ~outkind ~outname ~targets in let files = List.map begin fun name -> let name = Filename.chop_extension name in [name ^ \".cmi\"] @ (if true || outkind <> Library || all then [name ^ \".cmi\"] else []) @ [ (name ^ \".cmo\"); (name ^ \".cmx\"); (name ^ \".obj\"); (name ^ \".o\"); (name ^ \".annot\"); ] end deps in let files = List.flatten files in let files = if outkind = Executable || all then outname :: files else files in let files = remove_dupl files in List.iter (fun file -> remove_file ~verbose:true file) files ;;  let suffixes sufs name = List.exists (fun suf -> name ^^ suf) sufs let clean_all () = let cwd = Sys.getcwd() in let rec clean_dir dir = if not ((Unix.lstat dir).Unix.st_kind = Unix.S_LNK) then begin let files = Sys.readdir dir in let files = Array.to_list files in let files = List.map (fun x -> dir // x) files in let directories, files = List.partition Sys.is_directory files in let files = List.filter (suffixes [\".cmi\"; \".cmo\"; \".cmx\"; \".obj\"; \".cma\"; \".cmxa\"; \".lib\"; \".a\"; \".o\"; \".annot\"]) files in List.iter (remove_file ~verbose:false) files; let oebuild_times_filename = dir // Table.oebuild_times_filename in remove_file ~verbose:false oebuild_times_filename; List.iter clean_dir directories; end in clean_dir cwd; exit 0 ;;  let check_restrictions restr = List.for_all begin function | \"IS_UNIX\" -> Sys.os_type = \"Unix\" | \"IS_WIN32\" -> Sys.os_type = \"Win32\" | \"IS_CYGWIN\" -> Sys.os_type = \"Cygwin\" | \"HAS_NATIVE\" -> Ocaml_config.can_compile_native () <> None                        | _ -> false end restr;; end\nmodule Oebuild_script_util = struct open Arg open Printf open Oebuild open Oebuild_util open Task type target = { output_name : string; output_kind : output_kind; compilation_bytecode : bool; compilation_native : bool; toplevel_modules : string; search_path : string; required_libraries : string; compiler_flags : string; linker_flags : string; thread : bool; vmthread : bool; pp : string; library_install_dir : string; other_objects : string; external_tasks : int list; restrictions : string list; } exception Error let pushd, popd = let stack = Stack.create () in begin fun dir -> let cwd = Sys.getcwd () in Stack.push cwd stack; Sys.chdir dir end, (fun () -> Sys.chdir (Stack.pop stack));; let rpad txt c width = let result = txt ^ (String.make width c) in String.sub result 0 width let get_compilation_types native t = (if t.compilation_bytecode then [Bytecode] else []) @ (if t.compilation_native && native then [Native] else []) let string_of_compilation_type native t = let compilation = get_compilation_types native t in String.concat \"/\" (List.map string_of_compilation_type compilation) let create_target ?dir f x = (match dir with Some dir -> pushd dir | _ -> ()); f x; (match dir with Some _ -> popd() | _ -> ());; let create_target_func ?tg targets = match targets with | default_target :: _ -> (match tg with Some f -> create_target f | _ -> create_target default_target) | [] -> fun _ -> ();; let string_of_outkind = function | Executable -> \"Executable\" | Library -> \"Library\" | Plugin -> \"Plugin\" | Pack -> \"Pack\";;  module Option = struct let prefix = ref \"\" let change_dir = ref \"src\" end  let ccomp_type = Ocaml_config.can_compile_native () let system_config () = let ocaml_version = Cmd.expand ~first_line:true \"ocamlc -v\" in let std_lib = Cmd.expand ~first_line:true \"ocamlc -where\" in let properties = [ \"OCaml\", ocaml_version; \"Standard library directory\", std_lib; \"OCAMLLIB\", (try Sys.getenv \"OCAMLLIB\" with Not_found -> \"<Not_found>\"); \"Native compilation supported\", (match ccomp_type with Some ccomp_type -> ccomp_type | _ -> \"No\"); ] in let buf = Buffer.create 100 in Buffer.add_string buf \"\\nSystem configuration\\n\"; let maxlength = List.fold_left (fun cand (x, _) -> let len = String.length x in max cand len) 0 properties in List.iter (fun (n, v) -> bprintf buf \"  %s : %s\\n\" (rpad (n ^ \" \") '.' maxlength) v) properties; Buffer.contents buf;;  let show = function num, (name, t) -> let files = Str.split (Str.regexp \" +\") t.toplevel_modules in let deps = Dep.find ~pp:t.pp ~with_errors:true ~echo:false files in let compilation = (if t.compilation_bytecode then [Bytecode] else []) @ (if t.compilation_native && ccomp_type <> None then [Native] else []) in let outname = List.map (fun compilation -> get_output_name ~compilation ~outkind:t.output_kind ~outname:t.output_name ~targets:files) compilation in let outkind = string_of_outkind t.output_kind in let compilation = string_of_compilation_type (ccomp_type <> None) t in let prop_1 = [ \"Restrictions\", (String.concat \",\" t.restrictions); \"Output name\", (String.concat \", \" outname); ] in let prop_2 = [ \"Search path (-I)\", t.search_path; \"Required libraries\", t.required_libraries; \"Compiler flags\", t.compiler_flags; \"Linker flags\", t.linker_flags; \"Toplevel modules\", t.toplevel_modules; \"Dependencies\", (String.concat \" \" deps); ] in let properties = if t.output_kind = Library then prop_1 @ [ \"Install directory\", (Oebuild.ocamllib // t.library_install_dir) ] @ prop_2 else prop_1 @ prop_2 in printf \"%d) %s (%s, %s)\\n%!\" num name outkind compilation; let maxlength = List.fold_left (fun cand (x, _) -> let len = String.length x in max cand len) 0 properties in List.iter (fun (n, v) -> printf \"  %s : %s\\n\" (rpad (n ^ \" \") '.' maxlength) v) properties ;; module ETask = struct let filter tasks phase = List.filter begin fun task -> if task.et_always_run_in_script then match task.et_phase with | Some ph -> ph = phase | _ -> false else false end tasks;; let execute = Task.handle begin fun ~env ~dir ~prog ~args -> let cmd = sprintf \"%s %s\" prog (String.concat \" \" args) in let old_dir = Sys.getcwd () in Sys.chdir dir; let exit_code = Oebuild_util.exec ~env cmd in Sys.chdir old_dir; if exit_code > 0 then raise Error end end  module Command = struct type t = Show | Build | Install | Clean | Distclean let command : t option ref = ref None let commands = [ \"show\",      (Show,      \" <target>... Show the build options of a target\"); \"build\",     (Build,     \" <target>... Build a target (default)\"); \"install\",   (Install,   \" <target>... Install a library\"); \"clean\",     (Clean,     \" <target>... Remove output files for the selected target\"); \"distclean\", (Distclean, \"Remove all build output\"); ] let set name = match !command with | Some _ -> false | None -> begin try let c, _ = List.assoc name commands in command := Some c; true with Not_found -> false end;; let execute_target ~(external_tasks : (int * Task.t) list) ~command  (_, (name, t)) = if Oebuild.check_restrictions t.restrictions then let compilation = (if t.compilation_bytecode then [Bytecode] else []) @ (if t.compilation_native && (ccomp_type <> None) then [Native] else []) in let files = Str.split (Str.regexp \" +\") t.toplevel_modules in let deps () = Dep.find ~pp:t.pp ~with_errors:true ~echo:false files in let etasks = List.map (fun x -> snd (List.nth external_tasks x)) t.external_tasks in List.iter begin fun compilation -> let outname = get_output_name ~compilation ~outkind:t.output_kind ~outname:t.output_name ~targets:files in match command with | Build -> List.iter ETask.execute (ETask.filter etasks Before_compile); let deps = deps() in  begin match build ~compilation ~includes:t.search_path ~libs:t.required_libraries ~other_mods:t.other_objects ~outkind:t.output_kind ~compile_only:false ~thread:t.thread ~vmthread:t.vmthread ~annot:false ~pp:t.pp ~cflags:t.compiler_flags ~lflags:t.linker_flags ~outname ~deps ~ms_paths:(ref false) ~targets:files () with | Built_successfully -> List.iter ETask.execute (ETask.filter etasks After_compile); | Build_failed n -> popd(); exit n end | Install -> let deps = deps() in install_output ~compilation ~outkind:t.output_kind ~outname ~deps ~path:t.library_install_dir ~ccomp_type | Clean -> List.iter ETask.execute (ETask.filter etasks Before_clean); let deps = deps() in clean ~compilation ~outkind:t.output_kind ~outname ~targets:files ~deps (); List.iter ETask.execute (ETask.filter etasks After_clean); | Distclean -> let deps = deps() in List.iter ETask.execute (ETask.filter etasks Before_clean); clean ~compilation ~outkind:t.output_kind ~outname ~targets:files ~deps ~all:true (); List.iter ETask.execute (ETask.filter etasks After_clean); | Show -> assert false end compilation;; let execute ~external_tasks ~target targets = pushd !Option.change_dir; try let command = match !command with Some c -> c | _ -> assert false in begin match command with | Distclean -> List.iter (fun t -> execute_target external_tasks command (0, t)) targets; clean_all() | Show -> printf \"%s\\n%!\" (system_config ()); Printf.printf \"\\n%!\" ; List.iter begin fun t -> show t; print_newline(); print_newline(); end target; | _ -> List.iter (execute_target ~external_tasks ~command) target end; popd(); with ex -> popd(); end  let target : (int * (string * target)) list ref = ref [] let add_target targets name = try begin try let n = int_of_string name in target := (n, List.nth targets (n - 1)) :: !target with _ -> target := (0, (name, (List.assoc name targets))) :: !target end with Not_found -> ();;  let main ~external_tasks ~targets = let parse_anon targets x = if not (Command.set x) then (add_target targets x) in let speclist = [ (\"-C\",      Set_string Option.change_dir, \"<dir> Change directory before running (default is \\\"src\\\")\");  ] in let speclist = Arg.align speclist in let command_name = Filename.basename Sys.argv.(0) in  let i = ref 0 in let maxlength = List.fold_left (fun cand (x, _) -> let len = String.length x in max cand len) 0 targets in let descr = String.concat \"\\n\" (List.map begin fun (name, tg) -> incr i; let name = rpad name ' ' maxlength in sprintf \"  %2d) %s %s, %s\" !i name (string_of_outkind tg.output_kind) (string_of_compilation_type (ccomp_type <> None) tg) end targets) in  let cmds = List.map begin fun (c, (_, d)) -> if d.[0] = ' ' then begin let pos = try String.index_from d 1 ' ' with Not_found -> String.length d in let arg = Str.string_before d pos in (c ^ arg), (try Str.string_after d (pos + 1) with _ -> \"\") end else c, d end Command.commands in let maxlength = List.fold_left (fun cand (x, _) -> let len = String.length x in max cand len) 0 cmds in let cmds = String.concat \"\\n\" (List.map begin fun (c, d) -> let c = rpad c ' ' maxlength in sprintf \"  %s %s\" c d end cmds) in  let help_message = sprintf \"\\nUsage\\n  Please first edit the \\\"Build Configurations\\\" section at the end of\\n  file \\\"%s\\\" to set the right options for your system, then do:\\n\\n    ocaml %s <command> [options]\\n\\nCommands\\n%s\\n\\nTargets\\n%s\\n\\nOptions\" command_name command_name cmds descr in Arg.parse speclist (parse_anon targets) help_message; if !Arg.current = 1 then (Arg.usage speclist help_message) else begin (match !Command.command with None -> Command.command := Some Command.Build | _ -> ()); Command.execute ~external_tasks ~target:(List.rev !target) targets end;; end\n\nopen Oebuild\nopen Oebuild_script_util\n"