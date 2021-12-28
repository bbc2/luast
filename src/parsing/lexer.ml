exception Lexer_error of string

module Step = struct
  type t =
    { token : Token.t
    ; comments : Luast__tree.Comment.t list
    ; empty_spaces : Luast__tree.Empty_space.t list }
  [@@deriving eq, ord, show]
end

let digit = [%sedlex.regexp? '0' .. '9']
let letter = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z']
let integer = [%sedlex.regexp? Plus digit]
let identifier = [%sedlex.regexp? letter, Star (letter | digit)]

let control_char = function
  | 'a' -> "\007"
  | 'b' -> "\008"
  | 'f' -> "\012"
  | 'n' -> "\n"
  | 'r' -> "\r"
  | 't' -> "\t"
  | 'v' -> "\011"
  | _ -> failwith "Unexpected control character"

module Quoting = struct
  type t =
    | Single
    | Double

  let delimiter = function
    | Single -> '\''
    | Double -> '"'
end

let rec quoted_string ~quote buf acc =
  match%sedlex buf with
  | Chars {|'"|} ->
    let chr = (Sedlexing.Utf8.lexeme buf).[0] in
    if Quoting.delimiter quote = chr then acc
    else quoted_string ~quote buf (acc ^ CCString.of_char chr)
  | "\\", Chars "\\\"'\n" ->
    let escaped = CCString.of_char (Sedlexing.Utf8.lexeme buf).[1] in
    quoted_string ~quote buf (acc ^ escaped)
  | "\\", Chars "abfnrtv" ->
    let escaped = (Sedlexing.Utf8.lexeme buf).[1] in
    quoted_string ~quote buf (acc ^ control_char escaped)
  | "\\z", Opt white_space -> quoted_string ~quote buf acc
  | "\n" -> raise (Lexer_error "Unescaped newline in quoted string")
  | Compl (Chars "\\") ->
    quoted_string ~quote buf (acc ^ Sedlexing.Utf8.lexeme buf)
  | _ -> raise (Lexer_error "Unexpected double quoted string character")

let potentially_closing_long_string_bracket ~level buf =
  let rec aux ~remaining ~acc =
    if remaining = 0 then
      match%sedlex buf with
      | "]" -> `Matched
      | _ -> `Not_matched acc
    else
      match%sedlex buf with
      | "=" -> aux ~remaining:(remaining - 1) ~acc:(acc ^ "=")
      | _ -> `Not_matched acc
  in
  aux ~remaining:level ~acc:""

let rec long_string ~level buf acc =
  match%sedlex buf with
  | "]" -> (
    match potentially_closing_long_string_bracket ~level buf with
    | `Matched -> acc
    | `Not_matched lexeme -> long_string ~level buf (acc ^ "]" ^ lexeme))
  | any -> long_string ~level buf (acc ^ Sedlexing.Utf8.lexeme buf)
  | _ ->
    raise
      (Lexer_error (Printf.sprintf "Unexpected long string/comment character"))

let short_comment buf =
  match%sedlex buf with
  | Star (Compl "\n") -> Sedlexing.Utf8.lexeme buf
  | _ -> failwith "unreachable"

let rec parse_token_with_comments
    ~(comments : Luast__tree.Comment.t list ref)
    ~(empty_spaces : Luast__tree.Empty_space.t list ref)
    buf : Token.t =
  match%sedlex buf with
  | Plus white_space ->
    let location = Util.get_location buf in
    let line_count = location.end_.line - location.begin_.line - 1 in
    if line_count > 0 then
      empty_spaces :=
        {first_line = location.begin_.line + 1; line_count} :: !empty_spaces;
    parse_token_with_comments ~comments ~empty_spaces buf
  | "and" -> And
  | "break" -> Break
  | "do" -> Do
  | "else" -> Else
  | "elseif" -> Elseif
  | "end" -> End
  | "false" -> False
  | "for" -> For
  | "function" -> Function
  | "goto" -> Goto
  | "if" -> If
  | "in" -> In
  | "local" -> Local
  | "nil" -> Nil
  | "not" -> Not
  | "or" -> Or
  | "return" -> Return
  | "repeat" -> Repeat
  | "then" -> Then
  | "true" -> True
  | "until" -> Until
  | "while" -> While
  | "+" -> Plus
  | "-" -> Minus
  | "*" -> Star
  | "/" -> Slash
  | "%" -> Percent
  | "^" -> Caret
  | "#" -> Hash
  | "&" -> Amp
  | "~" -> Tilde
  | "|" -> Pipe
  | "<<" -> Double_lt
  | ">>" -> Double_gt
  | "//" -> Double_slash
  | "==" -> Double_equal
  | "~=" -> Tilde_equal
  | "<=" -> Lte
  | ">=" -> Gte
  | "<" -> Lt
  | ">" -> Gt
  | "=" -> Equal
  | "(" -> Left_paren
  | ")" -> Right_paren
  | "{" -> Left_curly
  | "}" -> Right_curly
  | "[" -> Left_square
  | "]" -> Right_square
  | "::" -> Double_colon
  | ";" -> Semi_colon
  | ":" -> Colon
  | "," -> Comma
  | "." -> Dot
  | ".." -> Double_dot
  | "..." -> Triple_dot
  | identifier -> Id (Sedlexing.Utf8.lexeme buf)
  | integer -> (
    let str = Sedlexing.Utf8.lexeme buf in
    try Integer (Int64.of_string str)
    with Failure _ ->
      raise (Lexer_error (Printf.sprintf "Not an integer: %s" str)))
  | Plus (Chars "'") -> Str (Short (quoted_string ~quote:Single buf ""))
  | Plus (Chars "\"") -> Str (Short (quoted_string ~quote:Double buf ""))
  | "[", Star "=", "[", "\n" ->
    let level = CCString.length (Sedlexing.Utf8.lexeme buf) - 3 in
    Str
      (Long {level; leading_newline = true; value = long_string ~level buf ""})
  | "[", Star "=", "[" ->
    let level = CCString.length (Sedlexing.Utf8.lexeme buf) - 2 in
    Str
      (Long {level; leading_newline = false; value = long_string ~level buf ""})
  | "--[", Star "=", "[" ->
    let level = CCString.length (Sedlexing.Utf8.lexeme buf) - 4 in
    let begin_ = (Util.get_location buf).begin_ in
    let str = long_string ~level buf "" in
    let end_ = (Util.get_location buf).end_ in
    comments :=
      {type_ = Long {level}; str; location = {begin_; end_}} :: !comments;
    parse_token_with_comments ~comments ~empty_spaces buf
  | "--" ->
    let begin_ = (Util.get_location buf).begin_ in
    let str = short_comment buf in
    let end_ = (Util.get_location buf).end_ in
    comments := {type_ = Short; str; location = {begin_; end_}} :: !comments;
    parse_token_with_comments ~comments ~empty_spaces buf
  | eof -> Eof
  | _ -> raise (Lexer_error "Unexpected beginning of token")

let parse_token buf =
  let comments = ref [] in
  let empty_spaces = ref [] in
  let token = parse_token_with_comments ~comments ~empty_spaces buf in
  {Step.token; comments = !comments; empty_spaces = !empty_spaces}

let lex str =
  let buf = Sedlexing.Utf8.from_string str in

  (* Make Sedlex track the position in the input string. *)
  Sedlexing.set_position buf
    {pos_lnum = 1; pos_cnum = 0; pos_bol = 0; pos_fname = ""};

  try
    let token = parse_token buf in
    let _, {Lexing.pos_cnum; _} = Sedlexing.lexing_positions buf in
    Ok (token, pos_cnum)
  with Lexer_error str -> Error str

let print str =
  lex str |> [%show: (Step.t * int, string) result] |> print_endline

let%expect_test _ =
  print "";
  [%expect
    {| (Ok ({ Lexer.Step.token = Token.Eof; comments = []; empty_spaces = [] }, 0)) |}]

let%expect_test _ =
  print "--";
  [%expect
    {|
      (Ok ({ Lexer.Step.token = Token.Eof;
             comments =
             [{ Comment.type_ = Comment.Type.Short; str = "";
                location =
                { Location.begin_ = { Position.line = 1; column = 1 };
                  end_ = { Position.line = 1; column = 3 } }
                }
               ];
             empty_spaces = [] },
           2)) |}]

let%expect_test _ =
  print "--a";
  [%expect
    {|
      (Ok ({ Lexer.Step.token = Token.Eof;
             comments =
             [{ Comment.type_ = Comment.Type.Short; str = "a";
                location =
                { Location.begin_ = { Position.line = 1; column = 1 };
                  end_ = { Position.line = 1; column = 4 } }
                }
               ];
             empty_spaces = [] },
           3)) |}]

let%expect_test _ =
  print "--a\nb";
  [%expect
    {|
    (Ok ({ Lexer.Step.token = (Token.Id "b");
           comments =
           [{ Comment.type_ = Comment.Type.Short; str = "a";
              location =
              { Location.begin_ = { Position.line = 1; column = 1 };
                end_ = { Position.line = 1; column = 4 } }
              }
             ];
           empty_spaces = [] },
         5)) |}]

let%expect_test _ =
  print "--[[a]]";
  [%expect
    {|
      (Ok ({ Lexer.Step.token = Token.Eof;
             comments =
             [{ Comment.type_ = Comment.Type.Long {level = 0}; str = "a";
                location =
                { Location.begin_ = { Position.line = 1; column = 1 };
                  end_ = { Position.line = 1; column = 8 } }
                }
               ];
             empty_spaces = [] },
           7)) |}]

let%expect_test _ =
  print "--[==[a]==]";
  [%expect
    {|
      (Ok ({ Lexer.Step.token = Token.Eof;
             comments =
             [{ Comment.type_ = Comment.Type.Long {level = 2}; str = "a";
                location =
                { Location.begin_ = { Position.line = 1; column = 1 };
                  end_ = { Position.line = 1; column = 12 } }
                }
               ];
             empty_spaces = [] },
           11)) |}]

let%expect_test _ =
  print "--[==[a]]\n]==]";
  [%expect
    {|
    (Ok ({ Lexer.Step.token = Token.Eof;
           comments =
           [{ Comment.type_ = Comment.Type.Long {level = 2}; str = "a]]\n";
              location =
              { Location.begin_ = { Position.line = 1; column = 1 };
                end_ = { Position.line = 2; column = 5 } }
              }
             ];
           empty_spaces = [] },
         14)) |}]

let%expect_test _ =
  print "\n";
  [%expect
    {|
    (Ok ({ Lexer.Step.token = Token.Eof; comments = []; empty_spaces = [] }, 1)) |}]

let%expect_test _ =
  print {|a|};
  [%expect
    {|
    (Ok ({ Lexer.Step.token = (Token.Id "a"); comments = []; empty_spaces = [] },
         1)) |}]

let%expect_test _ =
  print {| a|};
  [%expect
    {|
    (Ok ({ Lexer.Step.token = (Token.Id "a"); comments = []; empty_spaces = [] },
         2)) |}]

let%expect_test _ =
  print {|1|};
  [%expect
    {|
      (Ok ({ Lexer.Step.token = (Token.Integer 1L); comments = [];
             empty_spaces = [] },
           1)) |}]

let%expect_test _ =
  print {|10000000000000000000|};
  [%expect {| (Error "Not an integer: 10000000000000000000") |}]

let%expect_test _ =
  print {|=|};
  [%expect
    {| (Ok ({ Lexer.Step.token = Token.Equal; comments = []; empty_spaces = [] }, 1)) |}]

let%expect_test _ =
  print {|"a"|};
  [%expect
    {|
      (Ok ({ Lexer.Step.token = (Token.Str (Cst.Short "a")); comments = [];
             empty_spaces = [] },
           3)) |}]

let%expect_test _ =
  print {|"a'b"|};
  [%expect
    {|
    (Ok ({ Lexer.Step.token = (Token.Str (Cst.Short "a'b")); comments = [];
           empty_spaces = [] },
         5)) |}]

let%expect_test _ =
  print {|"a\'b"|};
  [%expect
    {|
    (Ok ({ Lexer.Step.token = (Token.Str (Cst.Short "a'b")); comments = [];
           empty_spaces = [] },
         6)) |}]

let%expect_test _ =
  print {|"a\nb"|};
  [%expect
    {|
    (Ok ({ Lexer.Step.token = (Token.Str (Cst.Short "a\nb")); comments = [];
           empty_spaces = [] },
         6)) |}]

let%expect_test _ =
  print {|"a\vb"|};
  [%expect
    {|
    (Ok ({ Lexer.Step.token = (Token.Str (Cst.Short "a\011b")); comments = [];
           empty_spaces = [] },
         6)) |}]

let%expect_test _ =
  print {|"a\zb"|};
  [%expect
    {|
    (Ok ({ Lexer.Step.token = (Token.Str (Cst.Short "ab")); comments = [];
           empty_spaces = [] },
         6)) |}]

let%expect_test _ =
  print {|"a\z b"|};
  [%expect
    {|
    (Ok ({ Lexer.Step.token = (Token.Str (Cst.Short "ab")); comments = [];
           empty_spaces = [] },
         7)) |}]

let%expect_test _ =
  print {|"a\xb"|};
  [%expect {| (Error "Unexpected double quoted string character") |}]

let%expect_test _ =
  print {|"a\"b"|};
  [%expect
    {|
    (Ok ({ Lexer.Step.token = (Token.Str (Cst.Short "a\"b")); comments = [];
           empty_spaces = [] },
         6)) |}]

let%expect_test _ =
  print {|"a'b"|};
  [%expect
    {|
    (Ok ({ Lexer.Step.token = (Token.Str (Cst.Short "a'b")); comments = [];
           empty_spaces = [] },
         5)) |}]

let%expect_test _ =
  print {|'a'|};
  [%expect
    {|
      (Ok ({ Lexer.Step.token = (Token.Str (Cst.Short "a")); comments = [];
             empty_spaces = [] },
           3)) |}]

let%expect_test _ =
  print {|'a"b'|};
  [%expect
    {|
    (Ok ({ Lexer.Step.token = (Token.Str (Cst.Short "a\"b")); comments = [];
           empty_spaces = [] },
         5)) |}]

let%expect_test _ =
  print "\"a\nb\"";
  [%expect {| (Error "Unescaped newline in quoted string") |}]

let%expect_test _ =
  print "\"a\\\nb\"";
  [%expect
    {|
    (Ok ({ Lexer.Step.token = (Token.Str (Cst.Short "a\nb")); comments = [];
           empty_spaces = [] },
         6)) |}]

let%expect_test _ =
  print {|[[a]]|};
  [%expect
    {|
    (Ok ({ Lexer.Step.token =
           (Token.Str Cst.Long {level = 0; leading_newline = false; value = "a"});
           comments = []; empty_spaces = [] },
         5)) |}]

let%expect_test _ =
  print "[[\na]]";
  [%expect
    {|
    (Ok ({ Lexer.Step.token =
           (Token.Str Cst.Long {level = 0; leading_newline = true; value = "a"});
           comments = []; empty_spaces = [] },
         6)) |}]

let%expect_test _ =
  print {|[==[a[b]==]|};
  [%expect
    {|
    (Ok ({ Lexer.Step.token =
           (Token.Str
              Cst.Long {level = 2; leading_newline = false; value = "a[b"});
           comments = []; empty_spaces = [] },
         11)) |}]

let%expect_test _ =
  print {|"é"|};
  [%expect
    {|
    (Ok ({ Lexer.Step.token = (Token.Str (Cst.Short "\195\169")); comments = [];
           empty_spaces = [] },
         3)) |}]

let%expect_test _ =
  print {|[|};
  [%expect
    {|
      (Ok ({ Lexer.Step.token = Token.Left_square; comments = []; empty_spaces = []
             },
           1)) |}]

let%expect_test _ =
  print {|.|};
  [%expect
    {| (Ok ({ Lexer.Step.token = Token.Dot; comments = []; empty_spaces = [] }, 1)) |}]

let%expect_test _ =
  print {|..|};
  [%expect
    {|
      (Ok ({ Lexer.Step.token = Token.Double_dot; comments = []; empty_spaces = []
             },
           2)) |}]

let%expect_test _ =
  print {|...|};
  [%expect
    {|
      (Ok ({ Lexer.Step.token = Token.Triple_dot; comments = []; empty_spaces = []
             },
           3)) |}]
