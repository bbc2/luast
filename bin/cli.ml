let next_token buffer () =
  let (begin_, end_) = Sedlexing.lexing_positions buffer in
  Printf.printf "Position: %s\n%!" (Sedlexing.Utf8.lexeme buffer);
  (Lib.Lexer.parse_token buffer, begin_, end_)

let parse buffer parser =
  MenhirLib.Convert.Simplified.traditional2revised parser (next_token buffer)

let () =
  let buffer =
    Sedlexing.Utf8.from_string {|if 1 "\"foo\\bar\"" "été" [=[[[foo]]]=] 23|}
  in
  let rec print () =
    match Lib.Lexer.parse_token buffer with
    | Eof -> ()
    | token ->
      Printf.printf "%s\n%!" (Lib.Token.to_string token);
      print ()
  in
  print ()

let () =
  let buffer = Sedlexing.Utf8.from_string {|abc = nil|} in
  parse buffer Lib.Parser.chunk
  |> [%show: Lib.Ast.Chunk.t]
  |> Printf.printf "Parsed: %s\n"
