let origin_to_string (origin : Luast__parser.Parser.Origin.t) =
  match origin with
  | Lexer msg -> msg
  | Parser -> "Syntax error"

let error_to_string error =
  let {Luast__parser.Parser.Parser_error.position = {line; column}; origin} =
    error
  in
  Printf.sprintf "%d:%d: %s" line column (origin_to_string origin)

let () =
  let file = Sys.argv.(1) in
  let source = CCIO.with_in file (fun channel -> CCIO.read_all channel) in
  match Luast__parser.Parser.parse_chunk source with
  | Ok {tree; comments = _; locations = _} ->
    print_string (Luast__fmt.Fmt.format_chunk tree)
  | Error error ->
    Printf.eprintf "%s\n" (error_to_string error);
    exit 1
