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
  {|abc = nil|}
  |> Luast__parser.Parser.parse_chunk
  |> [%show: Luast__ast.Ast.Chunk.t]
  |> Printf.printf "Parsed: %s\n"
