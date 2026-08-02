open Project
open Printf
open Utils
open Oe
open Target

(** from_local_json - Load .project.local file with JSON->XML fallback for backward compatibility *)
let from_local_json proj =
  let open Prj in
  let filename = Project.fullpath_local proj in
  let filename =
    if Sys.file_exists filename
    then filename
    else Project.mk_old_filename_local proj
  in
  if Sys.file_exists filename then begin
    try
      let ic = open_in filename in
      let len = in_channel_length ic in
      let content = Bytes.create len in
      really_input ic content 0 len;
      close_in_noerr ic;
      let content = Bytes.to_string content in
      let project_local = Project_j.project_local_of_string content in
      proj.open_files <- project_local.Project_t.open_files;
      project_local.Project_t.bookmarks
      |> List.iter begin fun (bm_filename, bm_num, offset) ->
        let bm = {
          bm_filename = bm_filename;
          bm_loc = Offset offset;
          bm_num = bm_num;
          bm_marker = None;
        } in
        Project.set_bookmark bm proj
      end
    with _ ->
      (* Fallback to XML parsing if JSON parsing fails *)
      Project_xml_backcompat.from_local_xml proj
  end;;

(* JSON persistence using ATD-generated serializers. Convert between Prj.t and Project_t.project. *)

let rec atd_of_task (t : Task.t) : Project_t.external_task =
  Project_t.{
    name = t.Task.et_name;
    always_run_in_project = t.Task.et_always_run_in_project;
    always_run_in_script = t.Task.et_always_run_in_script;
    readonly = t.Task.et_readonly;
    visible = t.Task.et_visible;
    env = t.Task.et_env;
    env_replace = t.Task.et_env_replace;
    dir = t.Task.et_dir;
    cmd = t.Task.et_cmd;
    args = t.Task.et_args;
    phase = (match t.Task.et_phase with
        | Some p -> Some (Task.string_of_phase p)
        | None -> None);
  }

and atd_of_target (tg : Target.t) =
  Project_t.{
    id = tg.Target.id;
    name = tg.Target.name;
    descr = tg.Target.descr;
    default = tg.default;
    byt = tg.byt;
    opt = tg.opt;
    libs = tg.libs;
    other_objects = tg.other_objects;
    files = tg.files;
    package = tg.package;
    includes = tg.includes;
    thread = tg.thread;
    vmthread = tg.vmthread;
    pp = tg.pp;
    inline = tg.inline;
    nodep = tg.nodep;
    dontlinkdep = tg.dontlinkdep;
    dontaddopt = tg.dontaddopt;
    cflags = tg.cflags;
    lflags = tg.lflags;
    target_type = (
      match tg.target_type with
      | Target.Executable -> `Executable
      | Target.Library -> `Library
      | Target.Plugin -> `Plugin
      | Target.Pack -> `Pack
      | Target.External -> `External);
    outname = tg.outname;
    lib_install_path = tg.lib_install_path;
    external_tasks = List.map atd_of_task tg.external_tasks;
    restrictions = tg.restrictions;
    dependencies = tg.dependencies;
    is_fl_package = tg.is_fl_package;
    readonly = tg.readonly;
    visible = tg.visible;
    subsystem = (
      match tg.subsystem with
      | Some s -> Some (Target.string_of_subsystem s)
      | None -> None);
    node_collapsed = tg.node_collapsed;
  }

and atd_of_build_script (bs : Build_script.t) =
  let targets =
    bs.Build_script.bs_targets
    |> List.map begin fun t ->
      Project_t.{ target_id = t.Build_script.bst_target.Target.id;
                  show = t.Build_script.bst_show }
    end
  in
  let args =
    bs.Build_script.bs_args
    |> List.map begin fun a ->
      Project_t.{
        id = a.Build_script_args.bsa_id;
        type_ = Build_script_args.string_of_type a.Build_script_args.bsa_type;
        key = a.Build_script_args.bsa_key;
        pass = Build_script_args.string_of_pass a.Build_script_args.bsa_pass;
        command = Build_script_command.string_of_command a.Build_script_args.bsa_cmd;
        task = (
          match a.Build_script_args.bsa_task with
          | Some (bc, et) -> Some (bc.Target.id, et.Task.et_name)
          | None -> None);
        mode = (
          match a.Build_script_args.bsa_mode with
          | `add -> Build_script_args.string_of_add
          | `replace s -> s);
        default = (
          match a.Build_script_args.bsa_default with
          | `flag b -> `Flag b
          | `bool b -> `Bool b
          | `string s -> `String s);
        doc = a.Build_script_args.bsa_doc;
      }
    end
  in
  let commands =
    bs.Build_script.bs_commands
    |> List.map begin fun c ->
      Project_t.{
        name = Build_script.string_of_command c.Build_script.bsc_name;
        descr = c.Build_script.bsc_descr;
        target_id = c.Build_script.bsc_target.Target.id;
        task_name = c.Build_script.bsc_task.Task.et_name;
      } end
  in
  Project_t.{ filename = bs.Build_script.bs_filename;
              targets; args; commands }

let atd_of_project (p : Prj.t) =
  Project_t.{
    ocaml_home = p.ocaml_home;
    ocamllib = (if p.ocamllib_from_env then p.ocamllib else "");
    encoding = p.encoding;
    name = p.name;
    author = p.author;
    description = p.description;
    version = p.version;
    autocomp =
      Project_t.{ enabled = p.autocomp_enabled;
                  delay = p.autocomp_delay;
                  cflags = p.autocomp_cflags };
    targets = List.map atd_of_target p.targets;
    executables =
      p.executables
      |> List.map begin fun r ->
        Project_t.{ id = r.Rconf.id;
                    target_id = r.Rconf.target_id;
                    name = r.Rconf.name;
                    default = r.Rconf.default;
                    build_task = Target.string_of_task r.Rconf.build_task;
                    env = r.Rconf.env;
                    env_replace = r.Rconf.env_replace;
                    args = r.Rconf.args }
      end;
    build_script = atd_of_build_script p.build_script;
  }

let write_json proj =
  let atd = atd_of_project proj in
  Project_j.string_of_project atd |> Yojson.Safe.prettify

let read_json filename =
  try
    let chan = open_in_bin filename in
    let content = really_input_string chan (in_channel_length chan) in
    close_in_noerr chan;
    let atd = Project_j.project_of_string content in
    (* convert atd -> Prj.t *)
    let proj = Project.create ~filename () in
    proj.ocaml_home <- atd.Project_t.ocaml_home;
    proj.ocamllib <- atd.Project_t.ocamllib;
    proj.encoding <- atd.Project_t.encoding;
    proj.name <- atd.Project_t.name;
    proj.author <- atd.Project_t.author;
    proj.description <- atd.Project_t.description;
    proj.version <- atd.Project_t.version;
    proj.autocomp_enabled <- atd.Project_t.autocomp.Project_t.enabled;
    proj.autocomp_delay <- atd.Project_t.autocomp.Project_t.delay;
    proj.autocomp_cflags <- atd.Project_t.autocomp.Project_t.cflags;
    (* targets: create without sub_targets then later resolve links if needed *)
    let targets =
      atd.targets
      |> List.map begin fun (t_atd : Project_t.target) ->
        let tg = Target.create ~id:t_atd.Project_t.id ~name:t_atd.Project_t.name in
        tg.descr <- t_atd.Project_t.descr;
        tg.default <- t_atd.Project_t.default;
        tg.byt <- t_atd.Project_t.byt;
        tg.opt <- t_atd.Project_t.opt;
        tg.libs <- t_atd.Project_t.libs;
        tg.other_objects <- t_atd.Project_t.other_objects;
        tg.files <- t_atd.Project_t.files;
        tg.package <- t_atd.Project_t.package;
        tg.includes <- t_atd.Project_t.includes;
        tg.thread <- t_atd.Project_t.thread;
        tg.vmthread <- t_atd.Project_t.vmthread;
        tg.pp <- t_atd.Project_t.pp;
        tg.inline <- t_atd.Project_t.inline;
        tg.nodep <- t_atd.Project_t.nodep;
        tg.dontlinkdep <- t_atd.Project_t.dontlinkdep;
        tg.dontaddopt <- t_atd.Project_t.dontaddopt;
        tg.cflags <- t_atd.Project_t.cflags;
        tg.lflags <- t_atd.Project_t.lflags;
        tg.target_type <-
          (match t_atd.Project_t.target_type with
           | `Executable -> Target.Executable
           | `Library -> Target.Library
           | `Plugin -> Target.Plugin
           | `Pack -> Target.Pack
           | `External -> Target.External);
        tg.outname <- t_atd.Project_t.outname;
        tg.lib_install_path <- t_atd.Project_t.lib_install_path;
        tg.external_tasks <-
          t_atd.Project_t.external_tasks
          |> List.map begin fun (et : Project_t.external_task) ->
            Task.create ~name:et.Project_t.name ~env:et.Project_t.env
              ~dir:et.Project_t.dir
              ~cmd:et.Project_t.cmd
              ~args:et.Project_t.args
              ~run_in_project:et.Project_t.always_run_in_project
              ~run_in_script:et.Project_t.always_run_in_script
              ?phase:(match et.Project_t.phase with Some s -> Some (Task.phase_of_string s) | None -> None) ()
          end;
        tg.restrictions <- t_atd.Project_t.restrictions;
        tg.dependencies <- t_atd.Project_t.dependencies;
        tg.is_fl_package <- t_atd.Project_t.is_fl_package;
        tg.readonly <- t_atd.Project_t.readonly;
        tg.visible <- t_atd.Project_t.visible;
        tg.subsystem <-
          (match t_atd.Project_t.subsystem with Some s -> Some (Target.subsystem_of_string s) | None -> None);
        tg.node_collapsed <- t_atd.Project_t.node_collapsed;
        tg
      end in
    proj.targets <- targets;
    proj.executables <-
      atd.executables
      |> List.map begin fun (r_atd : Project_t.rconf) ->
        { Rconf.
          id = r_atd.Project_t.id;
          target_id = r_atd.Project_t.target_id;
          name = r_atd.Project_t.name;
          default = r_atd.Project_t.default;
          build_task =
            Target.task_of_string
              (proj.targets
               |> List.find (fun t -> t.Target.id = r_atd.Project_t.target_id)) r_atd.Project_t.build_task;
          env = r_atd.Project_t.env;
          env_replace = r_atd.Project_t.env_replace;
          args = r_atd.Project_t.args;
        } end;
    proj.build_script <-
      Build_script.{
        bs_filename = atd.Project_t.build_script.Project_t.filename;
        bs_targets =
          atd.Project_t.build_script.Project_t.targets
          |> List.map begin fun (bt : Project_t.build_script_target) ->
            { Build_script.bst_target =
                begin match proj.targets |> List.find_opt (fun t -> t.Target.id = bt.Project_t.target_id) with
                | Some x -> x
                | None -> (Target.create ~id:bt.Project_t.target_id ~name:(string_of_int bt.Project_t.target_id))
                end;
              Build_script.bst_show = bt.Project_t.show
            }
          end;
        bs_args =
          atd.Project_t.build_script.Project_t.args
          |> List.map begin fun (ba : Project_t.build_script_arg) ->
            let bsa_task =
              match ba.Project_t.task with
              | Some (target_id, task_name) ->
                let target = proj.targets |> List.find_opt (fun t -> t.Target.id = target_id) in
                let task = Prj.find_task proj task_name in
                begin match target, task with
                | Some target, Some task -> Some (target, task)
                | _ -> None
                end
              | None -> None
            in
            { Build_script_args.
              bsa_id = ba.Project_t.id;
              bsa_type = Build_script_args.type_of_string ba.Project_t.type_;
              bsa_key = ba.Project_t.key;
              bsa_doc = ba.Project_t.doc;
              bsa_default_override = true;
              bsa_default =
                begin match ba.Project_t.default with
                | `Flag b -> `flag b
                | `Bool b -> `bool b
                | `String s -> `string s
                end;
              bsa_task;
              bsa_mode =
                (if ba.Project_t.mode = Build_script_args.string_of_add then `add
                 else `replace ba.Project_t.mode);
              bsa_cmd = Build_script_command.command_of_string ba.Project_t.command;
              bsa_pass = Build_script_args.pass_of_string ba.Project_t.pass;
            }
          end;
        bs_commands =
          atd.Project_t.build_script.Project_t.commands
          |> List.filter_map begin fun (bc : Project_t.build_script_command) ->
            try
              let target = proj.targets |> List.find_opt (fun t -> t.Target.id = bc.Project_t.target_id) in
              let task = Prj.find_task proj bc.Project_t.task_name in
              match target, task with
              | Some target, Some task ->
                Some { Build_script.
                       bsc_name = Build_script.command_of_string bc.Project_t.name;
                       bsc_descr = bc.Project_t.descr;
                       bsc_target = target;
                       bsc_task = task;
                     }
              | _ -> None
            with _ -> None
          end;
      };
    proj
  with _ -> Project_xml_backcompat.read filename

let init () =
  Project.read_xml := Project_xml_backcompat.read;
  Project.from_local_xml := Project_xml_backcompat.from_local_xml;
  Project.write_json := write_json;
  Project.read_json := read_json;
  Project.from_local_json := from_local_json;
