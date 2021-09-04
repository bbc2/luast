let next_token buffer () =
  let (begin_, end_) = Sedlexing.lexing_positions buffer in
  Printf.printf "Position: %s\n%!" (Sedlexing.Utf8.lexeme buffer);
  (Luast__parsing.Lexer.parse_token buffer, begin_, end_)

let parse buffer parser =
  MenhirLib.Convert.Simplified.traditional2revised parser (next_token buffer)

let () =
  let buffer =
    Sedlexing.Utf8.from_string {|if 1 "\"foo\\bar\"" "été" [=[[[foo]]]=] 23|}
  in
  let rec print () =
    match Luast__parsing.Lexer.parse_token buffer with
    | Eof -> ()
    | token ->
      Printf.printf "%s\n%!" (Luast__parsing.Token.to_string token);
      print ()
  in
  print ()

let () =
  let buffer = Sedlexing.Utf8.from_string {|abc = nil|} in
  parse buffer Luast__parsing.Parser.chunk
  |> [%show: Luast__ast.Ast.Chunk.t]
  |> Printf.printf "Parsed: %s\n"
