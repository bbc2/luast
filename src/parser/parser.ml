module Origin = struct
  type t =
    | Lexer of string
    | Parser
  [@@deriving eq, ord, show]
end

module Parser_error = struct
  type t =
    { position : Luast__tree.Position.t
    ; origin : Origin.t }
  [@@deriving eq, ord, show]
end

let next_token ~comments ~empty_spaces buffer () =
  let { Luast__parsing.Lexer.Step.token
      ; comments = new_comments
      ; empty_spaces = new_empty_spaces } =
    Luast__parsing.Lexer.parse_token buffer
  in
  let (begin_, end_) = Sedlexing.lexing_positions buffer in
  comments := CCList.append !comments new_comments;
  empty_spaces := CCList.append !empty_spaces new_empty_spaces;
  (token, begin_, end_)

let parse buffer parser =
  let comments = ref [] in
  let empty_spaces = ref [] in
  let tree =
    MenhirLib.Convert.Simplified.traditional2revised parser
      (next_token ~comments ~empty_spaces buffer)
  in
  (tree, !comments, !empty_spaces)

let parse_chunk str =
  let buffer = Sedlexing.Utf8.from_string str in

  (* Make Sedlex track the position in the input string. *)
  Sedlexing.set_position buffer
    {pos_lnum = 1; pos_cnum = 0; pos_bol = 0; pos_fname = ""};

  match parse buffer Luast__parsing.Parser.chunk with
  | (tree, comments, empty_spaces) ->
    Ok {Luast__tree.Chunk_with_comments.tree; comments; empty_spaces}
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
  |> [%show: (Luast__tree.Chunk_with_comments.t, Parser_error.t) result]
  |> print_string

let%expect_test _ =
  print {|a = nil|};
  [%expect
    {|
    (Ok { Chunk_with_comments.tree =
          { Located.value =
            { Cst.Block.stats =
              [{ Located.value =
                 Cst.Stat.Assignment {vars = [(Cst.Var.Name "a")];
                   exps = [Cst.Exp.Nil]};
                 loc =
                 { Location.begin_ = { Position.line = 1; column = 1 };
                   end_ = { Position.line = 1; column = 8 } }
                 }
                ];
              ret = None };
            loc =
            { Location.begin_ = { Position.line = 1; column = 1 };
              end_ = { Position.line = 1; column = 8 } }
            };
          comments = []; empty_spaces = [] }) |}]

let%expect_test _ =
  print {|a = 0|};
  [%expect
    {|
    (Ok { Chunk_with_comments.tree =
          { Located.value =
            { Cst.Block.stats =
              [{ Located.value =
                 Cst.Stat.Assignment {vars = [(Cst.Var.Name "a")];
                   exps = [(Cst.Exp.Numeral (Cst.Numeral.Integer 0L))]};
                 loc =
                 { Location.begin_ = { Position.line = 1; column = 1 };
                   end_ = { Position.line = 1; column = 6 } }
                 }
                ];
              ret = None };
            loc =
            { Location.begin_ = { Position.line = 1; column = 1 };
              end_ = { Position.line = 1; column = 6 } }
            };
          comments = []; empty_spaces = [] }) |}]

let%expect_test _ =
  print {|a = "a"|};
  [%expect
    {|
    (Ok { Chunk_with_comments.tree =
          { Located.value =
            { Cst.Block.stats =
              [{ Located.value =
                 Cst.Stat.Assignment {vars = [(Cst.Var.Name "a")];
                   exps = [(Cst.Exp.Str (Cst.Str.Short "a"))]};
                 loc =
                 { Location.begin_ = { Position.line = 1; column = 1 };
                   end_ = { Position.line = 1; column = 8 } }
                 }
                ];
              ret = None };
            loc =
            { Location.begin_ = { Position.line = 1; column = 1 };
              end_ = { Position.line = 1; column = 8 } }
            };
          comments = []; empty_spaces = [] }) |}]

let%expect_test _ =
  print {|a = {}|};
  [%expect
    {|
    (Ok { Chunk_with_comments.tree =
          { Located.value =
            { Cst.Block.stats =
              [{ Located.value =
                 Cst.Stat.Assignment {vars = [(Cst.Var.Name "a")];
                   exps = [(Cst.Exp.Table [])]};
                 loc =
                 { Location.begin_ = { Position.line = 1; column = 1 };
                   end_ = { Position.line = 1; column = 7 } }
                 }
                ];
              ret = None };
            loc =
            { Location.begin_ = { Position.line = 1; column = 1 };
              end_ = { Position.line = 1; column = 7 } }
            };
          comments = []; empty_spaces = [] }) |}]

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
          { Located.value =
            { Cst.Block.stats =
              [{ Located.value =
                 Cst.Stat.Assignment {vars = [(Cst.Var.Name "a")];
                   exps =
                   [(Cst.Exp.Table
                       [{ Located.value =
                          (Cst.Field.Exp
                             (Cst.Exp.Numeral (Cst.Numeral.Integer 0L)));
                          loc =
                          { Location.begin_ = { Position.line = 1; column = 6 };
                            end_ = { Position.line = 1; column = 7 } }
                          }
                         ])
                     ]};
                 loc =
                 { Location.begin_ = { Position.line = 1; column = 1 };
                   end_ = { Position.line = 1; column = 8 } }
                 }
                ];
              ret = None };
            loc =
            { Location.begin_ = { Position.line = 1; column = 1 };
              end_ = { Position.line = 1; column = 8 } }
            };
          comments = []; empty_spaces = [] }) |}]

let%expect_test _ =
  print {|a = {0;}|};
  [%expect
    {|
    (Ok { Chunk_with_comments.tree =
          { Located.value =
            { Cst.Block.stats =
              [{ Located.value =
                 Cst.Stat.Assignment {vars = [(Cst.Var.Name "a")];
                   exps =
                   [(Cst.Exp.Table
                       [{ Located.value =
                          (Cst.Field.Exp
                             (Cst.Exp.Numeral (Cst.Numeral.Integer 0L)));
                          loc =
                          { Location.begin_ = { Position.line = 1; column = 6 };
                            end_ = { Position.line = 1; column = 7 } }
                          }
                         ])
                     ]};
                 loc =
                 { Location.begin_ = { Position.line = 1; column = 1 };
                   end_ = { Position.line = 1; column = 9 } }
                 }
                ];
              ret = None };
            loc =
            { Location.begin_ = { Position.line = 1; column = 1 };
              end_ = { Position.line = 1; column = 9 } }
            };
          comments = []; empty_spaces = [] }) |}]

let%expect_test _ =
  print {|a = {0; 1}|};
  [%expect
    {|
    (Ok { Chunk_with_comments.tree =
          { Located.value =
            { Cst.Block.stats =
              [{ Located.value =
                 Cst.Stat.Assignment {vars = [(Cst.Var.Name "a")];
                   exps =
                   [(Cst.Exp.Table
                       [{ Located.value =
                          (Cst.Field.Exp
                             (Cst.Exp.Numeral (Cst.Numeral.Integer 0L)));
                          loc =
                          { Location.begin_ = { Position.line = 1; column = 6 };
                            end_ = { Position.line = 1; column = 7 } }
                          };
                         { Located.value =
                           (Cst.Field.Exp
                              (Cst.Exp.Numeral (Cst.Numeral.Integer 1L)));
                           loc =
                           { Location.begin_ = { Position.line = 1; column = 9 };
                             end_ = { Position.line = 1; column = 10 } }
                           }
                         ])
                     ]};
                 loc =
                 { Location.begin_ = { Position.line = 1; column = 1 };
                   end_ = { Position.line = 1; column = 11 } }
                 }
                ];
              ret = None };
            loc =
            { Location.begin_ = { Position.line = 1; column = 1 };
              end_ = { Position.line = 1; column = 11 } }
            };
          comments = []; empty_spaces = [] }) |}]

let%expect_test _ =
  print {|a = {0, 1;}|};
  [%expect
    {|
    (Ok { Chunk_with_comments.tree =
          { Located.value =
            { Cst.Block.stats =
              [{ Located.value =
                 Cst.Stat.Assignment {vars = [(Cst.Var.Name "a")];
                   exps =
                   [(Cst.Exp.Table
                       [{ Located.value =
                          (Cst.Field.Exp
                             (Cst.Exp.Numeral (Cst.Numeral.Integer 0L)));
                          loc =
                          { Location.begin_ = { Position.line = 1; column = 6 };
                            end_ = { Position.line = 1; column = 7 } }
                          };
                         { Located.value =
                           (Cst.Field.Exp
                              (Cst.Exp.Numeral (Cst.Numeral.Integer 1L)));
                           loc =
                           { Location.begin_ = { Position.line = 1; column = 9 };
                             end_ = { Position.line = 1; column = 10 } }
                           }
                         ])
                     ]};
                 loc =
                 { Location.begin_ = { Position.line = 1; column = 1 };
                   end_ = { Position.line = 1; column = 12 } }
                 }
                ];
              ret = None };
            loc =
            { Location.begin_ = { Position.line = 1; column = 1 };
              end_ = { Position.line = 1; column = 12 } }
            };
          comments = []; empty_spaces = [] }) |}]

let%expect_test _ =
  print {|a, b = 0, 1|};
  [%expect
    {|
    (Ok { Chunk_with_comments.tree =
          { Located.value =
            { Cst.Block.stats =
              [{ Located.value =
                 Cst.Stat.Assignment {
                   vars = [(Cst.Var.Name "a"); (Cst.Var.Name "b")];
                   exps =
                   [(Cst.Exp.Numeral (Cst.Numeral.Integer 0L));
                     (Cst.Exp.Numeral (Cst.Numeral.Integer 1L))]};
                 loc =
                 { Location.begin_ = { Position.line = 1; column = 1 };
                   end_ = { Position.line = 1; column = 12 } }
                 }
                ];
              ret = None };
            loc =
            { Location.begin_ = { Position.line = 1; column = 1 };
              end_ = { Position.line = 1; column = 12 } }
            };
          comments = []; empty_spaces = [] }) |}]

let%expect_test _ =
  print "a = 0\nb = 1";
  [%expect
    {|
      (Ok { Chunk_with_comments.tree =
            { Located.value =
              { Cst.Block.stats =
                [{ Located.value =
                   Cst.Stat.Assignment {vars = [(Cst.Var.Name "a")];
                     exps = [(Cst.Exp.Numeral (Cst.Numeral.Integer 0L))]};
                   loc =
                   { Location.begin_ = { Position.line = 1; column = 1 };
                     end_ = { Position.line = 1; column = 6 } }
                   };
                  { Located.value =
                    Cst.Stat.Assignment {vars = [(Cst.Var.Name "b")];
                      exps = [(Cst.Exp.Numeral (Cst.Numeral.Integer 1L))]};
                    loc =
                    { Location.begin_ = { Position.line = 2; column = 1 };
                      end_ = { Position.line = 2; column = 6 } }
                    }
                  ];
                ret = None };
              loc =
              { Location.begin_ = { Position.line = 1; column = 1 };
                end_ = { Position.line = 2; column = 6 } }
              };
            comments = []; empty_spaces = [] })
      |}]

let%expect_test _ =
  print {|return|};
  [%expect
    {|
    (Ok { Chunk_with_comments.tree =
          { Located.value =
            { Cst.Block.stats = [];
              ret =
              (Some { Located.value = [];
                      loc =
                      { Location.begin_ = { Position.line = 1; column = 1 };
                        end_ = { Position.line = 1; column = 7 } }
                      })
              };
            loc =
            { Location.begin_ = { Position.line = 1; column = 1 };
              end_ = { Position.line = 1; column = 7 } }
            };
          comments = []; empty_spaces = [] }) |}]

let%expect_test _ =
  print {|return;|};
  [%expect
    {|
    (Ok { Chunk_with_comments.tree =
          { Located.value =
            { Cst.Block.stats = [];
              ret =
              (Some { Located.value = [];
                      loc =
                      { Location.begin_ = { Position.line = 1; column = 1 };
                        end_ = { Position.line = 1; column = 8 } }
                      })
              };
            loc =
            { Location.begin_ = { Position.line = 1; column = 1 };
              end_ = { Position.line = 1; column = 8 } }
            };
          comments = []; empty_spaces = [] }) |}]

let%expect_test _ =
  print {|return 0|};
  [%expect
    {|
    (Ok { Chunk_with_comments.tree =
          { Located.value =
            { Cst.Block.stats = [];
              ret =
              (Some { Located.value =
                      [(Cst.Exp.Numeral (Cst.Numeral.Integer 0L))];
                      loc =
                      { Location.begin_ = { Position.line = 1; column = 1 };
                        end_ = { Position.line = 1; column = 9 } }
                      })
              };
            loc =
            { Location.begin_ = { Position.line = 1; column = 1 };
              end_ = { Position.line = 1; column = 9 } }
            };
          comments = []; empty_spaces = [] }) |}]

let%expect_test _ =
  print {|return 0, 1|};
  [%expect
    {|
    (Ok { Chunk_with_comments.tree =
          { Located.value =
            { Cst.Block.stats = [];
              ret =
              (Some { Located.value =
                      [(Cst.Exp.Numeral (Cst.Numeral.Integer 0L));
                        (Cst.Exp.Numeral (Cst.Numeral.Integer 1L))];
                      loc =
                      { Location.begin_ = { Position.line = 1; column = 1 };
                        end_ = { Position.line = 1; column = 12 } }
                      })
              };
            loc =
            { Location.begin_ = { Position.line = 1; column = 1 };
              end_ = { Position.line = 1; column = 12 } }
            };
          comments = []; empty_spaces = [] }) |}]

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
