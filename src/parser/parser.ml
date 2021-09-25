module Origin = struct
  type t =
    | Lexer of string
    | Parser
  [@@deriving eq, ord, show]
end

module Parser_error = struct
  type t =
    { position : Luast__ast.Position.t
    ; origin : Origin.t }
  [@@deriving eq, ord, show]
end

let next_token ~comments buffer () =
  let (begin_, end_) = Sedlexing.lexing_positions buffer in
  let {Luast__parsing.Lexer.Step.token; comments = new_comments} =
    Luast__parsing.Lexer.parse_token buffer
  in
  comments := CCList.append !comments new_comments;
  (token, begin_, end_)

let parse buffer parser =
  let comments = ref [] in
  let tree =
    MenhirLib.Convert.Simplified.traditional2revised parser
      (next_token ~comments buffer)
  in
  (tree, !comments)

let parse_chunk str =
  let buffer = Sedlexing.Utf8.from_string str in

  (* Make Sedlex track the position in the input string. *)
  Sedlexing.set_position buffer
    {pos_lnum = 1; pos_cnum = 0; pos_bol = 0; pos_fname = ""};

  match parse buffer Luast__parsing.Parser.chunk with
  | (tree, comments) -> Ok {Luast__ast.Chunk_with_comments.tree; comments}
  | exception Luast__parsing.Parser.Error ->
    Error
      { Parser_error.position = (Luast__parsing.Util.get_location buffer).begin_
      ; origin = Parser }
  | exception Luast__parsing.Lexer.Lexer_error str ->
    Error
      { position = (Luast__parsing.Util.get_location buffer).begin_
      ; origin = Lexer str }

let print str =
  str
  |> parse_chunk
  |> [%show: (Luast__ast.Chunk_with_comments.t, Parser_error.t) result]
  |> print_string

let%expect_test _ =
  print {|a = nil|};
  [%expect
    {|
      (Ok { Chunk_with_comments.tree =
            { Ast.Block.stats =
              [Ast.Stat.Assignment {vars = [(Ast.Var.Name "a")];
                 exps = [Ast.Exp.Nil]}
                ];
              ret = None };
            comments = [] }) |}]

let%expect_test _ =
  print {|a = 0|};
  [%expect
    {|
      (Ok { Chunk_with_comments.tree =
            { Ast.Block.stats =
              [Ast.Stat.Assignment {vars = [(Ast.Var.Name "a")];
                 exps = [(Ast.Exp.Numeral (Ast.Numeral.Integer 0L))]}
                ];
              ret = None };
            comments = [] }) |}]

let%expect_test _ =
  print {|a = "a"|};
  [%expect
    {|
      (Ok { Chunk_with_comments.tree =
            { Ast.Block.stats =
              [Ast.Stat.Assignment {vars = [(Ast.Var.Name "a")];
                 exps = [(Ast.Exp.Str (Ast.Str.Short "a"))]}
                ];
              ret = None };
            comments = [] }) |}]

let%expect_test _ =
  print {|a = {}|};
  [%expect
    {|
      (Ok { Chunk_with_comments.tree =
            { Ast.Block.stats =
              [Ast.Stat.Assignment {vars = [(Ast.Var.Name "a")];
                 exps = [(Ast.Exp.Table [])]}
                ];
              ret = None };
            comments = [] }) |}]

let%expect_test _ =
  print {|a = {;}|};
  [%expect
    {|
      (Error { Parser.Parser_error.position = { Position.line = 1; column = 6 };
               origin = Parser.Origin.Parser }) |}]

let%expect_test _ =
  print {|a = {0}|};
  [%expect
    {|
      (Ok { Chunk_with_comments.tree =
            { Ast.Block.stats =
              [Ast.Stat.Assignment {vars = [(Ast.Var.Name "a")];
                 exps =
                 [(Ast.Exp.Table
                     [(Ast.Field.Exp (Ast.Exp.Numeral (Ast.Numeral.Integer 0L)))])
                   ]}
                ];
              ret = None };
            comments = [] }) |}]

let%expect_test _ =
  print {|a = {0;}|};
  [%expect
    {|
      (Ok { Chunk_with_comments.tree =
            { Ast.Block.stats =
              [Ast.Stat.Assignment {vars = [(Ast.Var.Name "a")];
                 exps =
                 [(Ast.Exp.Table
                     [(Ast.Field.Exp (Ast.Exp.Numeral (Ast.Numeral.Integer 0L)))])
                   ]}
                ];
              ret = None };
            comments = [] }) |}]

let%expect_test _ =
  print {|a = {0; 1}|};
  [%expect
    {|
      (Ok { Chunk_with_comments.tree =
            { Ast.Block.stats =
              [Ast.Stat.Assignment {vars = [(Ast.Var.Name "a")];
                 exps =
                 [(Ast.Exp.Table
                     [(Ast.Field.Exp (Ast.Exp.Numeral (Ast.Numeral.Integer 0L)));
                       (Ast.Field.Exp (Ast.Exp.Numeral (Ast.Numeral.Integer 1L)))])
                   ]}
                ];
              ret = None };
            comments = [] }) |}]

let%expect_test _ =
  print {|a = {0, 1;}|};
  [%expect
    {|
      (Ok { Chunk_with_comments.tree =
            { Ast.Block.stats =
              [Ast.Stat.Assignment {vars = [(Ast.Var.Name "a")];
                 exps =
                 [(Ast.Exp.Table
                     [(Ast.Field.Exp (Ast.Exp.Numeral (Ast.Numeral.Integer 0L)));
                       (Ast.Field.Exp (Ast.Exp.Numeral (Ast.Numeral.Integer 1L)))])
                   ]}
                ];
              ret = None };
            comments = [] }) |}]

let%expect_test _ =
  print {|a, b = 0, 1|};
  [%expect
    {|
    (Ok { Chunk_with_comments.tree =
          { Ast.Block.stats =
            [Ast.Stat.Assignment {
               vars = [(Ast.Var.Name "a"); (Ast.Var.Name "b")];
               exps =
               [(Ast.Exp.Numeral (Ast.Numeral.Integer 0L));
                 (Ast.Exp.Numeral (Ast.Numeral.Integer 1L))]}
              ];
            ret = None };
          comments = [] }) |}]

let%expect_test _ =
  print {|return|};
  [%expect
    {|
    (Ok { Chunk_with_comments.tree = { Ast.Block.stats = []; ret = (Some []) };
          comments = [] }) |}]

let%expect_test _ =
  print {|return;|};
  [%expect
    {|
    (Ok { Chunk_with_comments.tree = { Ast.Block.stats = []; ret = (Some []) };
          comments = [] }) |}]

let%expect_test _ =
  print {|return 0|};
  [%expect
    {|
    (Ok { Chunk_with_comments.tree =
          { Ast.Block.stats = [];
            ret = (Some [(Ast.Exp.Numeral (Ast.Numeral.Integer 0L))]) };
          comments = [] }) |}]

let%expect_test _ =
  print {|return 0, 1|};
  [%expect
    {|
    (Ok { Chunk_with_comments.tree =
          { Ast.Block.stats = [];
            ret =
            (Some [(Ast.Exp.Numeral (Ast.Numeral.Integer 0L));
                    (Ast.Exp.Numeral (Ast.Numeral.Integer 1L))])
            };
          comments = [] }) |}]

let%expect_test _ =
  print {|a|};
  [%expect
    {|
    (Error { Parser.Parser_error.position = { Position.line = 1; column = 2 };
             origin = Parser.Origin.Parser }) |}]

let%expect_test _ =
  print "a = 1\nb";
  [%expect
    {|
    (Error { Parser.Parser_error.position = { Position.line = 2; column = 2 };
             origin = Parser.Origin.Parser }) |}]

let%expect_test _ =
  print {|?|};
  [%expect
    {|
    (Error { Parser.Parser_error.position = { Position.line = 1; column = 1 };
             origin = (Parser.Origin.Lexer "Unexpected beginning of token") }) |}]
