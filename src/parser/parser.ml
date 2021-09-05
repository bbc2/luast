let next_token buffer () =
  let (begin_, end_) = Sedlexing.lexing_positions buffer in
  Printf.printf "Position: %s\n%!" (Sedlexing.Utf8.lexeme buffer);
  (Luast__parsing.Lexer.parse_token buffer, begin_, end_)

let parse buffer parser =
  MenhirLib.Convert.Simplified.traditional2revised parser (next_token buffer)

let parse_chunk str =
  let buffer = Sedlexing.Utf8.from_string str in
  parse buffer Luast__parsing.Parser.chunk
