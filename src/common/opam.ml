open Printf

(** Name of the currently active opam switch, or [""] if it cannot be determined. *)
let switch_show () =
  match Shell.get_command_output "opam switch show" with
  | [] -> ""
  | hd :: _ -> hd

(** Names of all the opam switches installed on the system. *)
let switch_list () = Shell.get_command_output "opam switch list --short"

(** Parses a ["major.minor.build"] version string into an integer triple.
    Raises [Failure] if the string does not have exactly three numeric components. *)
let parse_version version =
  match String.split_all ~sep:"." version with
  | [ major; minor; build ] -> int_of_string major, int_of_string minor, int_of_string build
  | _ -> failwith __FUNCTION__

(** Version of the [ocaml] package of the given opam switch, or of the current
    one if [switch] is omitted. Raises [Failure] if opam returns no output. *)
let show_ocaml_version ?switch () =
  let switch_arg = switch |> Option.map (sprintf "--switch=%s") |> Option.value ~default:"" in
  ksprintf Shell.get_command_output "opam show ocaml %s --field=version --color=never" switch_arg
  |> function [] -> failwith __FUNCTION__ | x :: _ -> x

(** Environment of the given opam switch as a list of ["NAME=value"] entries,
    obtained from [opam env] with the shell quoting removed. *)
let env ~switch =
  ksprintf Shell.get_command_output "opam env --switch=%s --set-switch --shell=sh" switch
  |> List.map begin fun line ->
    String.split_all ~sep:";" ~drop:(fun x -> String.trim x = "") line
    |> List.map begin fun nv ->
      match String.split_first ~sep:"=" nv with
      | Some (name, value) -> sprintf "%s=%s" name (String.sub value 1 (String.length value - 2))
      | _ -> ""
    end
  end
  |> List.concat
  |> List.filter (fun s -> s <> "")
