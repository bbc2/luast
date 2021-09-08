module Origin = struct
  type t =
    | Lexer of string
    | Parser
  [@@deriving eq, ord, show]
end

module Position = struct
  type t =
    { line : int
    ; column : int }
  [@@deriving eq, ord, show]
end

module Parser_error = struct
  type t =
    { position : Position.t
    ; origin : Origin.t }
  [@@deriving eq, ord, show]
end

let next_token buffer () =
  let (begin_, end_) = Sedlexing.lexing_positions buffer in
  (Luast__parsing.Lexer.parse_token buffer, begin_, end_)

let parse buffer parser =
  MenhirLib.Convert.Simplified.traditional2revised parser (next_token buffer)

let get_position buffer =
  let ({Lexing.pos_fname = _; pos_lnum; pos_bol; pos_cnum}, _) =
    Sedlexing.lexing_positions buffer
  in
  let column = pos_cnum - pos_bol in
  {Position.line = pos_lnum; column}

let parse_chunk str =
  let buffer = Sedlexing.Utf8.from_string str in

  (* Make Sedlex track the position in the input string. *)
  Sedlexing.set_position buffer
    {pos_lnum = 1; pos_cnum = 1; pos_bol = 0; pos_fname = ""};

  try Ok (parse buffer Luast__parsing.Parser.chunk) with
  | Luast__parsing.Parser.Error ->
    Error {Parser_error.position = get_position buffer; origin = Parser}
  | Luast__parsing.Lexer.Lexer_error str ->
    Error {position = get_position buffer; origin = Lexer str}

let print str =
  str
  |> parse_chunk
  |> [%show: (Luast__ast.Ast.Chunk.t, Parser_error.t) result]
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
  print {|a = 0|};
  [%expect
    {|
      (Ok { Ast.Block.stats =
            [Ast.Stat.Assignment {vars = [(Ast.Var.Name "a")];
               exps = [(Ast.Exp.Numeral (Ast.Numeral.Integer 0L))]}
              ];
            ret = None }) |}]

let%expect_test _ =
  print {|a = {}|};
  [%expect
    {|
      (Ok { Ast.Block.stats =
            [Ast.Stat.Assignment {vars = [(Ast.Var.Name "a")];
               exps = [(Ast.Exp.Table [])]}
              ];
            ret = None }) |}]

let%expect_test _ =
  print {|a = {;}|};
  [%expect
    {|
      (Error { Parser.Parser_error.position =
               { Parser.Position.line = 1; column = 6 };
               origin = Parser.Origin.Parser }) |}]

let%expect_test _ =
  print {|a = {0}|};
  [%expect
    {|
      (Ok { Ast.Block.stats =
            [Ast.Stat.Assignment {vars = [(Ast.Var.Name "a")];
               exps =
               [(Ast.Exp.Table
                   [(Ast.Field.Exp (Ast.Exp.Numeral (Ast.Numeral.Integer 0L)))])
                 ]}
              ];
            ret = None }) |}]

let%expect_test _ =
  print {|a = {0;}|};
  [%expect
    {|
      (Ok { Ast.Block.stats =
            [Ast.Stat.Assignment {vars = [(Ast.Var.Name "a")];
               exps =
               [(Ast.Exp.Table
                   [(Ast.Field.Exp (Ast.Exp.Numeral (Ast.Numeral.Integer 0L)))])
                 ]}
              ];
            ret = None }) |}]

let%expect_test _ =
  print {|a = {0; 1}|};
  [%expect
    {|
      (Ok { Ast.Block.stats =
            [Ast.Stat.Assignment {vars = [(Ast.Var.Name "a")];
               exps =
               [(Ast.Exp.Table
                   [(Ast.Field.Exp (Ast.Exp.Numeral (Ast.Numeral.Integer 0L)));
                     (Ast.Field.Exp (Ast.Exp.Numeral (Ast.Numeral.Integer 1L)))])
                 ]}
              ];
            ret = None }) |}]

let%expect_test _ =
  print {|a = {0, 1;}|};
  [%expect
    {|
      (Ok { Ast.Block.stats =
            [Ast.Stat.Assignment {vars = [(Ast.Var.Name "a")];
               exps =
               [(Ast.Exp.Table
                   [(Ast.Field.Exp (Ast.Exp.Numeral (Ast.Numeral.Integer 0L)));
                     (Ast.Field.Exp (Ast.Exp.Numeral (Ast.Numeral.Integer 1L)))])
                 ]}
              ];
            ret = None }) |}]

let%expect_test _ =
  print {|a, b = 0, 1|};
  [%expect
    {|
    (Ok { Ast.Block.stats =
          [Ast.Stat.Assignment {vars = [(Ast.Var.Name "a"); (Ast.Var.Name "b")];
             exps =
             [(Ast.Exp.Numeral (Ast.Numeral.Integer 0L));
               (Ast.Exp.Numeral (Ast.Numeral.Integer 1L))]}
            ];
          ret = None }) |}]

let%expect_test _ =
  print {|return|};
  [%expect {| (Ok { Ast.Block.stats = []; ret = (Some []) }) |}]

let%expect_test _ =
  print {|return;|};
  [%expect {| (Ok { Ast.Block.stats = []; ret = (Some []) }) |}]

let%expect_test _ =
  print {|return 0|};
  [%expect
    {|
    (Ok { Ast.Block.stats = [];
          ret = (Some [(Ast.Exp.Numeral (Ast.Numeral.Integer 0L))]) }) |}]

let%expect_test _ =
  print {|return 0, 1|};
  [%expect
    {|
    (Ok { Ast.Block.stats = [];
          ret =
          (Some [(Ast.Exp.Numeral (Ast.Numeral.Integer 0L));
                  (Ast.Exp.Numeral (Ast.Numeral.Integer 1L))])
          }) |}]

let%expect_test _ =
  print {|a|};
  [%expect
    {|
    (Error { Parser.Parser_error.position =
             { Parser.Position.line = 1; column = 2 };
             origin = Parser.Origin.Parser }) |}]

let%expect_test _ =
  print "a = 1\nb";
  [%expect
    {|
    (Error { Parser.Parser_error.position =
             { Parser.Position.line = 2; column = 1 };
             origin = Parser.Origin.Parser }) |}]

let%expect_test _ =
  print {|?|};
  [%expect
    {|
    (Error { Parser.Parser_error.position =
             { Parser.Position.line = 1; column = 1 };
             origin = (Parser.Origin.Lexer "Unexpected beginning of token") }) |}]
