(* dune_converter.ml *)

open Printf
open Sys (* Necessario per Sys.file_exists *)
open Unix (* Necessario per Unix.mkdir *)
open Str (* Necessario per Str.regexp in Utils.split_regexp *)

(* ====================================================================== *)
(** MOCK dei Moduli Xml e XmlParser (Necessari per la compilazione)
    Questi mock simulano le interfacce delle librerie esterne. *)
module Xml = struct
  type t = Element of string * (string * string) list * t list | PCData of string
  exception Not_element
  exception No_attribute of string
  exception Error of string

  let tag = function Element (t, _, _) -> t | PCData _ -> raise Not_element
  let children = function Element (_, _, c) -> c | PCData _ -> []
  let pcdata = function PCData d -> d | Element _ -> ""
  let attrib xml name =
    match xml with
    | Element (_, attrs, _) ->
        (try List.assoc name attrs with Not_found -> raise (No_attribute name))
    | PCData _ -> raise Not_element
  let iter f xml = List.iter f (children xml)
  let error err = err
end

module XmlParser = struct
  exception Error of string
  type parser = unit
  (* Simulazione minima per il parser *)
  let make () = ()
  let parse _ _ = Xml.Element ("project", [], []) (* Restituisce un XML vuoto di mock *)
  let error_msg err = err
  (* Definizione di SFile per risolvere l'errore su XmlParser.SFile *)
  type source = SFile of string
end
(* ====================================================================== *)


(** Modulo di logging per debug e messaggi informativi [cite: 677] *)
module Log = struct
  let enabled = ref true

  let info fmt =
    if !enabled then Printf.printf ("[INFO] " ^^ fmt ^^ "\n%!")
    else Printf.ifprintf stdout fmt

  let warn fmt = Printf.eprintf ("[WARN] " ^^ fmt ^^ "\n%!")
  let error fmt = Printf.eprintf ("[ERROR] " ^^ fmt ^^ "\n%!")
end

(** Utilità per la manipolazione di stringhe e path [cite: 678] *)
module Utils = struct
  (** Normalizza un percorso di file rimuovendo "./" iniziale [cite: 678] *)
  let normalize_path path =
    if String.length path > 2 && String.sub path 0 2 = "./"
    then String.sub path 2 (String.length path - 2)
    else path

  (** Converte un nome di target in un nome valido per Dune (lowercase, no spaces) [cite: 678, 679] *)
  let sanitize_name name =
    name
    |> String.lowercase_ascii
    |> String.map (function ' ' | '-' -> '_' | c -> c)

  (** Estrae il nome base da un percorso di output [cite: 679] *)
  let output_basename outname =
    Filename.basename outname

  (** Divide una stringa usando un regexp [cite: 679] *)
  let split_regexp re str =
    Str.split (Str.regexp re) str
    |> List.filter (fun s -> String.trim s <> "")

  (** Converte stringhe vuote in None [cite: 679] *)
  let string_opt s =
    if String.trim s = "" then None else Some (String.trim s)
end

(** Rappresentazione interna di un target Dune [cite: 680] *)
type dune_target = {
  name: string;                    (** Nome del target [cite: 680] *)
  kind: [`Library | `Executable | `Plugin]; (** Tipo di artefatto [cite: 680, 681] *)
  public_name: string option;      (** Nome pubblico (per librerie) [cite: 681] *)
  modules: string list;            (** Lista moduli OCaml [cite: 681, 682] *)
  libraries: string list;          (** Dipendenze da librerie [cite: 682] *)
  flags: string list;              (** Flag di compilazione [cite: 682, 683] *)
  preprocess: string list;         (** Direttive preprocessor [cite: 683] *)
  wrapped: bool;                   (** Se wrappare i moduli [cite: 683, 684] *)
  modes: string list;              (** Mode di compilazione (byte, native) [cite: 684, 685] *)
}

(** Rappresentazione di un progetto Dune [cite: 685] *)
type dune_project = {
  name: string;                    (** Nome del progetto [cite: 686] *)
  version: string;                 (** Versione [cite: 686] *)
  lang_version: string;            (** Versione del linguaggio Dune [cite: 686, 687] *)
  targets: dune_target list;       (** Lista di target [cite: 687] *)
  packages: string list;           (** Package findlib richiesti [cite: 688] *)
}

(** Parser per file XML di progetto OCamlEditor [cite: 688] *)
module OEProject = struct
  (** Estrae il valore testuale da un nodo XML [cite: 688] *)
  let value xml =
    try
      String.concat "\n" (List.map Xml.pcdata (Xml.children xml))
    with Xml.Not_element _ -> ""

  (** Ottiene un attributo da un nodo XML con un default [cite: 688] *)
  let attrib node name default =
    try Xml.attrib node name
    with Xml.No_attribute _ -> default

  (** Converte un attributo booleano [cite: 688, 689] *)
  let bool_attrib node name default =
    try bool_of_string (Xml.attrib node name)
    with Xml.No_attribute _ | Failure _ -> default

  (** Estrae i file sorgente da una stringa [cite: 690] *)
  let parse_files files_str =
    Utils.split_regexp "[ \t\r\n,]+" files_str
    |> List.filter (fun f ->
        Filename.check_suffix f ".ml" ||
        Filename.check_suffix f ".mli")

  (** Estrae i nomi dei moduli dai percorsi di file [cite: 690] *)
  let modules_from_files files =
    files
    |> List.map Filename.basename
    |> List.map Filename.chop_extension
    |> List.map String.capitalize_ascii
    |> List.sort_uniq String.compare

  (** Estrae i package findlib da una stringa [cite: 691] *)
  let parse_packages pkg_str =
    Utils.split_regexp "[, \t\r\n]+" pkg_str
    |> List.filter ((<>) "")

  (** Estrae le librerie da una stringa [cite: 691] *)
  let parse_libraries libs_str =
    Utils.split_regexp "[ \t\r\n]+" libs_str
    |> List.filter ((<>) "")

  (** Converte flags di compilazione OCamlEditor in flags Dune [cite: 691, 692] *)
  let convert_flags cflags lflags =
    let all_flags = (Utils.split_regexp "[ \t\r\n]+" cflags) @
                    (Utils.split_regexp "[ \t\r\n]+" lflags) in
    all_flags
    |> List.filter (fun f -> f <> "" && f <> "-g")
    |> List.sort_uniq String.compare

  (** Determina i mode di compilazione da byt/opt [cite: 692, 693, 694] *)
  let compilation_modes byt opt =
    match byt, opt with
    | true, true -> ["byte"; "native"]
    | true, false -> ["byte"]
    | false, true -> ["native"]
    | false, false -> ["native"]  (* default *)

  (** Legge un target dal nodo XML [cite: 694] *)
  let read_target target_node =
    let name = attrib target_node "name" "" in
    (* ... Omitting unused variables id, visible, readonly ... *)

    (* Leggi i campi del target [cite: 695] *)
    let target_type = ref "Executable" in
    let byt = ref true in
    let opt = ref true in
    let files = ref "" in
    let libs = ref "" in
    let package = ref "" in
    let cflags = ref "" in
    let lflags = ref "" in
    let thread = ref false in
    let pp = ref "" in
    let outname = ref "" in

    Xml.iter begin fun node ->
      match Xml.tag node with
      | "target_type" -> target_type := value node
      | "byt" -> byt := bool_of_string (value node)
      | "opt" -> opt := bool_of_string (value node)
      | "files" -> files := value node
      | "libs" -> libs := value node
      | "package" -> package := value node
      | "cflags" -> cflags := value node
      | "lflags" -> lflags := value node
      | "thread" -> thread := bool_of_string (value node)
      | "pp" -> pp := value node
      | "outname" -> outname := value node
      | _ -> ()
    end target_node;

    (* Determina il tipo di target [cite: 707] *)
    let kind = match !target_type with
      | "Library" -> `Library
      | "Plugin" -> `Plugin
      | "Executable" | _ -> `Executable
    in

    (* Estrai moduli e dipendenze [cite: 709] *)
    let file_list = parse_files !files in
    let modules = modules_from_files file_list in
    let libraries =
      (parse_libraries !libs) @
      (parse_packages !package)
      |> List.sort_uniq String.compare
    in

    (* Aggiungi threads se necessario [cite: 710] *)
    let libraries =
      if !thread then "threads.posix" :: libraries
      else libraries
    in

    (* Converti flags [cite: 710] *)
    let flags = convert_flags !cflags !lflags in

    (* Preprocessor [cite: 710] *)
    let preprocess =
      match Utils.string_opt !pp with
      | Some pp_cmd -> ["pps"; pp_cmd]
      | None -> []
    in

    (* Nome pubblico per librerie [cite: 712] *)
    let public_name =
      (* Rimosso il check `not readonly` dato che la variabile `readonly` non è usata dopo l'inizializzazione *)
      if kind = `Library then Some name
      else None
    in

    (* Mode di compilazione [cite: 713] *)
    let modes = compilation_modes !byt !opt in

    Some {
      name = Utils.sanitize_name name;
      kind;
      public_name;
      modules;
      libraries;
      flags;
      preprocess;
      wrapped = kind = `Library;
      modes;
    }

  (** Legge il progetto completo dal file XML [cite: 714] *)
  let read_project xml_file =
    (* Per la compilazione, il parser è un mock e ignora il file.
       In un ambiente reale, questo richiamerebbe la logica di lettura XML. *)
    let parser = XmlParser.make () in
    let xml = XmlParser.parse parser (XmlParser.SFile xml_file) in

    let proj_name = ref "project" in
    let proj_version = ref "0.1.0" in
    let targets = ref [] in

    Xml.iter begin fun node ->
      match Xml.tag node with
      | "name" -> proj_name := value node
      | "version" -> proj_version := value node
      | "targets" ->
          Xml.iter begin fun target_node ->
            if Xml.tag target_node = "target" then begin
              match read_target target_node with
              | Some tgt -> targets := tgt :: !targets
              | None -> ()
            end
          end node
      | _ -> ()
    end xml;

    (* Estrai tutti i package unici [cite: 720] *)
    let all_packages =
      !targets
      |> List.map (fun t -> t.libraries)
      |> List.flatten
      |> List.sort_uniq String.compare
    in

    {
      name = Utils.sanitize_name !proj_name;
      version = !proj_version;
      lang_version = "3.7";
      targets = List.rev !targets;
      packages = all_packages;
    }
end

(** Generatore di file Dune [cite: 722] *)
module DuneGen = struct
  (** Indenta una stringa con il numero specificato di spazi [cite: 722] *)
  let indent n str =
    let spaces = String.make n ' ' in
    spaces ^ str

  (** Formatta una lista S-expression con elementi su righe separate [cite: 722, 723, 724] *)
  let format_list name items =
    match items with
    | [] -> ""
    | [item] -> sprintf "(%s %s)" name item
    | items ->
        let items_str = String.concat "\n" (List.map (indent 2) items) in
        sprintf "(%s\n%s)" name items_str

  (** Genera una stanza library per Dune [cite: 724] *)
  let gen_library target =
    let name_field = sprintf "(name %s)" target.name in
    let public_field =
      match target.public_name with
      | Some pname -> sprintf "(public_name %s)" pname
      | None -> ""
    in
    let modules_field =
      if target.modules = [] then ""
      else format_list "modules" target.modules
    in
    let libraries_field =
      if target.libraries = [] then ""
      else format_list "libraries" target.libraries
    in
    let flags_field =
      if target.flags = [] then ""
      else format_list "flags" (List.map (sprintf "%S") target.flags)
    in
    let preprocess_field =
      if target.preprocess = [] then ""
      else format_list "preprocess" ["(pps " ^ String.concat " " (List.tl target.preprocess) ^ ")"]
    in
    let modes_field =
      if target.modes = ["byte"; "native"] then ""
      else format_list "modes" target.modes
    in
    let wrapped_field =
      if target.wrapped then "" else "(wrapped false)"
    in

    let fields = [
      name_field; public_field; modules_field; libraries_field;
      flags_field; preprocess_field; modes_field; wrapped_field
    ] |> List.filter ((<>) "") in

    "(library\n" ^
    String.concat "\n" (List.map (indent 1) fields) ^
    ")\n"

  (** Genera una stanza executable per Dune [cite: 730] *)
  let gen_executable target =
    let name_field = sprintf "(name %s)" target.name in
    let modules_field =
      if target.modules = [] then ""
      else format_list "modules" target.modules
    in
    let libraries_field =
      if target.libraries = [] then ""
      else format_list "libraries" target.libraries
    in
    let flags_field =
      if target.flags = [] then ""
      else format_list "flags" (List.map (sprintf "%S") target.flags)
    in
    let modes_field =
      if target.modes = ["byte"; "native"] then ""
      else format_list "modes" target.modes
    in

    let fields = [
      name_field; modules_field; libraries_field;
      flags_field; modes_field
    ] |> List.filter ((<>) "") in

    "(executable\n" ^
    String.concat "\n" (List.map (indent 1) fields) ^
    ")\n"

  (** Genera la stanza appropriata per un target [cite: 733] *)
  let gen_target target =
    match target.kind with
    | `Library -> gen_library target
    | `Executable -> gen_executable target
    | `Plugin ->
        (* I plugin richiedono configurazione speciale [cite: 734] *)
        sprintf "; TODO: Plugin %s richiede configurazione manuale\n" target.name ^
        gen_library target

  (** Genera il contenuto del file dune [cite: 734] *)
  let gen_dune_file targets =
    let header = "; Generato automaticamente da ocamleditor_to_dune\n\n" in
    header ^ String.concat "\n" (List.map gen_target targets)

  (** Genera il contenuto del file dune-project [cite: 735] *)
  let gen_dune_project project =
    sprintf "(lang dune %s)\n\n" project.lang_version ^
    sprintf "(name %s)\n\n" project.name ^
    sprintf "(version %s)\n\n" project.version ^
    "(generate_opam_files true)\n\n" ^
    "(source (github username/repo))\n\n" ^
    "(authors \"Author Name\")\n\n" ^
    "(maintainers \"maintainer@example.com\")\n\n" ^
    "(license LICENSE)\n\n" ^
    "(package\n" ^
    sprintf " (name %s)\n" project.name ^
    " (synopsis \"Short description\")\n" ^
    " (description \"Longer description\")\n" ^
    " (depends\n" ^
    "  (ocaml (>= 4.08))\n" ^
    String.concat "\n" (List.map (sprintf "  %s") project.packages) ^ ")\n" ^
    ")\n"

  (** Genera il contenuto del file dune-workspace (opzionale) [cite: 736] *)
  let gen_dune_workspace () =
    "(lang dune 3.7)\n\n" ^
    "(context default)\n"
end

(** Funzione principale di conversione [cite: 737] *)
let convert_project ?(output_dir=".") project_file =
  try
    Log.info "Lettura progetto OCamlEditor: %s" project_file;
    let project = OEProject.read_project project_file in

    Log.info "Progetto: %s v%s" project.name project.version;
    Log.info "Target trovati: %d" (List.length project.targets);

    (* Crea directory di output se non esiste [cite: 738] *)
    if not (Sys.file_exists output_dir) then begin
      Log.info "Creazione directory di output %s" output_dir;
      Unix.mkdir output_dir 0o755;
    end;

    (* Genera dune-project [cite: 739] *)
    let dune_project_file = Filename.concat output_dir "dune-project" in
    Log.info "Generazione %s..." dune_project_file;
    let oc = open_out dune_project_file in
    output_string oc (DuneGen.gen_dune_project project);
    close_out oc;

    (* Genera dune file [cite: 741] *)
    let dune_file = Filename.concat output_dir "dune" in
    Log.info "Generazione %s..." dune_file;
    let oc = open_out dune_file in
    output_string oc (DuneGen.gen_dune_file project.targets);
    close_out oc;

    (* Genera dune-workspace (opzionale) [cite: 743] *)
    let workspace_file = Filename.concat output_dir "dune-workspace" in
    Log.info "Generazione %s..." workspace_file;
    let oc = open_out workspace_file in
    output_string oc (DuneGen.gen_dune_workspace ());
    close_out oc;

    Log.info "Conversione completata con successo!";
    Log.info "";
    Log.info "Passi successivi:";
    Log.info "1. Rivedi i file generati e adatta se necessario";
    Log.info "2. Esegui: dune build";
    Log.info "3. Per installare: dune install";

    Ok ()
  with
  | Sys_error msg -> Error (sprintf "Errore di sistema: %s" msg)
  | Xml.Error err -> Error (sprintf "Errore XML: %s" (Xml.error err))
  | XmlParser.Error err -> Error (sprintf "Errore parsing XML: %s"
                                    (XmlParser.error_msg err))
  | ex -> Error (sprintf "Errore imprevisto: %s" (Printexc.to_string ex))

(** Entry point da riga di comando [cite: 750] *)
let main () =
  let usage = "Usage: ocamleditor_to_dune <project.project> [output_dir]" in

  match Array.to_list Sys.argv with
  | _ :: project_file :: [] ->
      begin match convert_project project_file with
      | Ok () -> exit 0
      | Error msg ->
          Log.error "%s" msg;
          exit 1
      end
  | _ :: project_file :: output_dir :: _ ->
      begin match convert_project ~output_dir project_file with
      | Ok () -> exit 0
      | Error msg ->
          Log.error "%s" msg;
          exit 1
      end
  | _ ->
      prerr_endline usage;
      exit 1

let () = main ()
