let next_token buffer () =
  let (begin_, end_) = Sedlexing.lexing_positions buffer in
  (Luast__parsing.Lexer.parse_token buffer, begin_, end_)

let parse buffer parser =
  MenhirLib.Convert.Simplified.traditional2revised parser (next_token buffer)

let parse_chunk str =
  let buffer = Sedlexing.Utf8.from_string str in
  try Ok (parse buffer Luast__parsing.Parser.chunk) with
  | Luast__parsing.Parser.Error -> Error ()

let print str =
  str
  |> parse_chunk
  |> [%show: (Luast__ast.Ast.Chunk.t, unit) result]
  |> print_string

let%expect_test _ =
  print {|a = nil|};
  [%expect
    {|
      (Ok { Ast.Block.stats =
            [Ast.Stat.Assignment {vars = [(Ast.Var.Name "a")]; exps = [Ast.Exp.Nil]}
              ];
            ret = None }) |}]

let%expect_test _ =
  print {|a|};
  [%expect {| (Error ()) |}]
