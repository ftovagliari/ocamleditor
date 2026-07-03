open Printf

module Log = Common.Log.Make(struct let prefix = "MERLIN" end)
let _ =
  Log.set_print_timestamp true;
  Log.set_verbosity `ERROR

type 'a result = Ok of 'a | Failure of string | Error of string

type json_string = string

let execute_async filename source_code command =
  let cmd_line = "ocamlmerlin" :: "server" :: command @ [ "-thread"; "-filename"; filename ] in
  Log.println `INFO "%s" (cmd_line |> String.concat " ");
  let (ic, oc, _) as channels = Unix.open_process_full (cmd_line |> String.concat " ") (Unix.environment ()) in
  output_string oc source_code;
  close_out_noerr oc;
  Async.create ~name:(List.hd command) begin fun () ->
    let json = In_channel.input_all ic in
    Unix.close_process_full channels |> ignore;
    (json : json_string)
  end

let as_cps merlin_func ?name ~filename ~buffer cont =
  merlin_func ~filename ~buffer |> Async.start_with_continuation ?name cont

let check_configuration ~filename ~buffer =
  [ "check-configuration" ] |> execute_async filename buffer

let errors ~filename ~buffer =
  [ "errors" ] |> execute_async filename buffer

[@@@warning "-42"]
[@@@warning "-40"]

let case_analysis ~(start : GText.iter) ~(stop : GText.iter) ~filename ~buffer =
  let start = sprintf "%d:%d" (start#line + 1) start#line_offset in
  let stop = sprintf "%d:%d" (stop#line + 1) stop#line_offset in
  [ "case-analysis"; "-start"; start; "-end"; stop ]
  |> execute_async filename buffer
  |> Async.map ~name:__FUNCTION__ begin fun json ->
    match Merlin_j.case_analysis_answer_of_string json with
    | Return case_analysis ->
        Log.println `DEBUG "%s" (Yojson.Safe.prettify json);
        Ok case_analysis.value
    | Failure msg ->
        Log.println `ERROR "%s" msg.value;
        Failure msg.value
    | Error msg | Exception msg ->
        Log.println `ERROR "%s" msg.value;
        Error msg.value
  end

let locate ~position:(line, col) ?prefix ?look_for ~filename ~buffer () =
  let position = sprintf "%d:%d" line col in
  "locate" :: "-position" :: position ::
  (match prefix with None -> "" | Some prefix -> sprintf "-prefix \"%s\"" prefix) ::
  (match look_for with
   | None -> ""
   | Some `Interface -> "-look-for interface"
   | Some `Implementation -> "-look-for implementation") :: []
  |> execute_async filename buffer
  |> Async.map ~name:"locate-result" begin fun json ->
    match Merlin_j.locate_answer_of_string json with
    | Return locate ->
        Log.println `DEBUG "%s" (Yojson.Safe.prettify json);
        Ok locate.value
    | Failure msg ->
        Log.println `ERROR "%s" msg.value;
        Failure msg.value
    | Error msg | Exception msg ->
        Log.println `ERROR "%s" msg.value;
        Error msg.value
  end

let occurrences ~identifier_at:(line, col) ?scope ?index_file ~filename ~buffer () =
  let identifier_at = sprintf "%d:%d" line col in
  "occurrences" :: "-identifier-at" :: identifier_at ::
  (match index_file with None -> "-index-file project.ocaml-index" | Some file -> sprintf "-index-file %s" file) ::
  (match scope with
   | None -> ""
   | Some `Buffer -> "-scope buffer"
   | Some `Project -> "-scope project"
   | Some `Renaming -> "-scope renaming") :: []
  |> execute_async filename buffer
  |> Async.map ~name:__FUNCTION__ begin fun json ->
    match Merlin_j.occurrences_answer_of_string json with
    | Return occurrences ->
        Log.println `INFO "%s" (Yojson.Safe.prettify json);
        Ok occurrences.value
    | Failure msg ->
        Log.println `ERROR "%s" msg.value;
        Failure msg.value
    | Error msg | Exception msg ->
        Log.println `ERROR "%s" msg.value;
        Error msg.value
  end

let locate_type ~position:(line, col) ~filename ~buffer =
  let position = sprintf "%d:%d" line col in
  "locate-type" :: "-position" :: position :: []
  |> execute_async filename buffer
  |> Async.map ~name:__FUNCTION__ begin fun json ->
    match Merlin_j.locate_type_answer_of_string json with
    | Return document ->
        Log.println `DEBUG "%s" (Yojson.Safe.prettify json);
        Ok document.value
    | Failure msg ->
        Log.println `ERROR "%s" msg.value;
        Failure msg.value
    | Error msg | Exception msg ->
        Log.println `ERROR "%s" msg.value;
        Error msg.value
  end

let enclosing ~position:(line, col) ~filename ~buffer () =
  let position = sprintf "%d:%d" line col in
  [ "enclosing"; "-position"; position ]
  |> execute_async filename buffer
  |> Async.map ~name:"enclosing-result" begin fun json ->
    match Merlin_j.enclosing_answer_of_string json with
    | Return enclosing ->
        Log.println `DEBUG "%s" (Yojson.Safe.prettify json);
        Ok enclosing.value
    | Failure msg ->
        Log.println `ERROR "%s" msg.value;
        Failure msg.value
    | Error msg | Exception msg ->
        Log.println `ERROR "%s" msg.value;
        Error msg.value
  end

let list_modules ?(ext=[".ml"]) ?(filename="dummy") ?(buffer="") () =
  "list-modules" :: (match ext with [] -> [] | _ -> ext |> List.map (sprintf "-ext %s"))
  |> execute_async filename buffer
  |> Async.map ~name:"list_modules-result" begin fun json ->
    match Merlin_j.list_modules_answer_of_string json with
    | Return list_modules ->
        Log.println `DEBUG "%s" (Yojson.Safe.prettify json);
        Ok list_modules.value
    | Failure msg ->
        Log.println `ERROR "%s" msg.value;
        Failure msg.value
    | Error msg | Exception msg ->
        Log.println `ERROR "%s" msg.value;
        Error msg.value
  end

let document ~position:(line, col) ?identifier ~filename ~buffer () =
  let position = sprintf "%d:%d" line col in
  "document" :: "-position" :: position ::
  (match identifier with None -> [] | Some identifier -> ["-identifier"; sprintf "\"%s\"" identifier])
  |> execute_async filename buffer
  |> Async.map ~name:__FUNCTION__ begin fun json ->
    match Merlin_j.document_answer_of_string json with
    | Return document ->
        Log.println `DEBUG "%s" (Yojson.Safe.prettify json);
        Ok document.value
    | Failure msg ->
        Log.println `ERROR "%s" msg.value;
        Failure msg.value
    | Error msg | Exception msg ->
        Log.println `ERROR "%s" msg.value;
        Error msg.value
  end

let type_enclosing ~position:(line, col) ?expression ?cursor ?verbosity ?index ~filename ~buffer () =
  let position = sprintf "%d:%d" line col in
  [
    ["type-enclosing"; "-position"; position ];
    (match expression with Some e -> ["-expression"; sprintf "\"%s\"" e ] | _ -> []);
    (match cursor with Some c -> ["-cursor"; string_of_int c ] | _ -> []);
    (match verbosity with
     | Some `Smart -> ["-verbosity"; "smart" ]
     | Some (`Int n) -> ["-verbosity"; string_of_int n ]
     | _ -> []);
    (match index with Some i -> ["-index"; string_of_int i ] | _ -> [])
  ] |> List.concat
  |> execute_async filename buffer
  |> Async.map ~name:__FUNCTION__ begin fun json ->
    match Merlin_j.type_enclosing_answer_of_string json with
    | Return types ->
        Log.println `DEBUG "%s" (Yojson.Safe.prettify json);
        Ok types.value
    | Failure msg ->
        Log.println `ERROR "%s" msg.value;
        Failure msg.value
    | Error msg | Exception msg ->
        Log.println `ERROR "%s" msg.value;
        Error msg.value
  end

let complete_prefix ~position:(line, col) ~prefix ~filename ~buffer =
  let position = sprintf "%d:%d" line col in
  [ "complete-prefix"; "-position"; position; "-prefix"; sprintf "\"%s\"" prefix; "-doc true -types true" ]
  |> execute_async filename buffer
  |> Async.map ~name:__FUNCTION__ begin fun json ->
    match Merlin_j.complete_prefix_answer_of_string json with
    | Return complete ->
        Log.println `DEBUG "%s" (Yojson.Safe.prettify json);
        Ok complete.value
    | Failure msg ->
        Log.println `ERROR "%s" msg.value;
        Failure msg.value
    | Error msg | Exception msg ->
        Log.println `ERROR "%s" msg.value;
        Error msg.value
  end

let expand_prefix ~position:(line, col) ~prefix ~filename ~buffer =
  let position = sprintf "%d:%d" line col in
  [ "expand-prefix"; "-position"; position; "-prefix";  sprintf "\"%s\"" prefix; "-doc true -types true" ]
  |> execute_async filename buffer
  |> Async.map ~name:__FUNCTION__ begin fun json ->
    match Merlin_j.complete_prefix_answer_of_string json with
    | Return complete ->
        Log.println `DEBUG "%s" (Yojson.Safe.prettify json);
        Ok complete.value
    | Failure msg ->
        Log.println `ERROR "%s" msg.value;
        Failure msg.value
    | Error msg | Exception msg ->
        Log.println `ERROR "%s" msg.value;
        Error msg.value
  end

let type_expression ~position:(line, col) ~expression ~filename ~buffer =
  let position = sprintf "%d:%d" line col in
  [ "type-expression"; "-position"; position; "-expression"; sprintf "\"%s\"" expression ]
  |> execute_async filename buffer
  |> Async.map ~name:__FUNCTION__ begin fun json ->
    match Merlin_j.type_expression_answer_of_string json with
    | Return type_expression ->
        Log.println `INFO "%s" (Yojson.Safe.prettify json);
        Ok type_expression.value
    | Failure msg ->
        Log.println `ERROR "%s" msg.value;
        Failure msg.value
    | Error msg | Exception msg ->
        Log.println `ERROR "%s" msg.value;
        Error msg.value
  end

let outline ~filename ~buffer =
  [ "outline" ]
  |> execute_async filename buffer
  |> Async.map ~name:__FUNCTION__ begin fun json ->
    match Merlin_j.outline_answer_of_string json with
    | Return outline ->
        Log.println `DEBUG "%s" (Yojson.Safe.prettify json);
        Ok outline.value
    | Failure msg ->
        Log.println `ERROR "%s" msg.value;
        Failure msg.value
    | Error msg | Exception msg ->
        Log.println `ERROR "%s" msg.value;
        Error msg.value
  end
