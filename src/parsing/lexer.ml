exception Lexer_error of string

let digit = [%sedlex.regexp? '0' .. '9']

let letter = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z']

let number = [%sedlex.regexp? Plus digit]

let identifier = [%sedlex.regexp? (letter, Star (letter | digit))]

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
    if Quoting.delimiter quote = chr then
      acc
    else
      quoted_string ~quote buf (acc ^ CCString.of_char chr)
  | ("\\", Chars "\\\"'\n") ->
    let escaped = CCString.of_char (Sedlexing.Utf8.lexeme buf).[1] in
    quoted_string ~quote buf (acc ^ escaped)
  | ("\\", Chars "abfnrtv") ->
    let escaped = (Sedlexing.Utf8.lexeme buf).[1] in
    quoted_string ~quote buf (acc ^ control_char escaped)
  | ("\\z", Opt white_space) -> quoted_string ~quote buf acc
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
  | _ -> raise (Lexer_error "Unexpected long string character")

let rec parse_token buf : Token.t =
  match%sedlex buf with
  | white_space -> parse_token buf
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
  | number -> Number (Sedlexing.Utf8.lexeme buf)
  | Plus (Chars " ") -> parse_token buf
  | Plus (Chars "'") -> String (quoted_string ~quote:Single buf "")
  | Plus (Chars "\"") -> String (quoted_string ~quote:Double buf "")
  | ("[", Star "=", "[") ->
    let level = CCString.length (Sedlexing.Utf8.lexeme buf) - 2 in
    String (long_string ~level buf "")
  | eof -> Eof
  | _ -> raise (Lexer_error "Unexpected beginning of token")

let lex str =
  let buf = Sedlexing.Utf8.from_string str in
  try
    let token = parse_token buf in
    let (_, {Lexing.pos_cnum; _}) = Sedlexing.lexing_positions buf in
    Ok (token, pos_cnum)
  with
  | Lexer_error str -> Error str

let print str =
  lex str |> [%show: (Token.t * int, string) result] |> print_endline

let%expect_test _ =
  print "";
  [%expect {| (Ok (Token.Eof, 0)) |}]

let%expect_test _ =
  print "\n";
  [%expect {| (Ok (Token.Eof, 1)) |}]

let%expect_test _ =
  print {|a|};
  [%expect {| (Ok ((Token.Id "a"), 1)) |}]

let%expect_test _ =
  print {| a|};
  [%expect {| (Ok ((Token.Id "a"), 2)) |}]

let%expect_test _ =
  print {|1|};
  [%expect {| (Ok ((Token.Number "1"), 1)) |}]

let%expect_test _ =
  print {|=|};
  [%expect {| (Ok (Token.Equal, 1)) |}]

let%expect_test _ =
  print {|"a"|};
  [%expect {| (Ok ((Token.String "a"), 3)) |}]

let%expect_test _ =
  print {|"a'b"|};
  [%expect {| (Ok ((Token.String "a'b"), 5)) |}]

let%expect_test _ =
  print {|"a\'b"|};
  [%expect {| (Ok ((Token.String "a'b"), 6)) |}]

let%expect_test _ =
  print {|"a\nb"|};
  [%expect {| (Ok ((Token.String "a\nb"), 6)) |}]

let%expect_test _ =
  print {|"a\vb"|};
  [%expect {| (Ok ((Token.String "a\011b"), 6)) |}]

let%expect_test _ =
  print {|"a\zb"|};
  [%expect {| (Ok ((Token.String "ab"), 6)) |}]

let%expect_test _ =
  print {|"a\z b"|};
  [%expect {| (Ok ((Token.String "ab"), 7)) |}]

let%expect_test _ =
  print {|"a\xb"|};
  [%expect {| (Error "Unexpected double quoted string character") |}]

let%expect_test _ =
  print {|"a\"b"|};
  [%expect {| (Ok ((Token.String "a\"b"), 6)) |}]

let%expect_test _ =
  print {|"a'b"|};
  [%expect {| (Ok ((Token.String "a'b"), 5)) |}]

let%expect_test _ =
  print {|'a'|};
  [%expect {| (Ok ((Token.String "a"), 3)) |}]

let%expect_test _ =
  print {|'a"b'|};
  [%expect {| (Ok ((Token.String "a\"b"), 5)) |}]

let%expect_test _ =
  print "\"a\nb\"";
  [%expect {| (Error "Unescaped newline in quoted string") |}]

let%expect_test _ =
  print "\"a\\\nb\"";
  [%expect {| (Ok ((Token.String "a\nb"), 6)) |}]

let%expect_test _ =
  print {|[[a]]|};
  [%expect {| (Ok ((Token.String "a"), 5)) |}]

let%expect_test _ =
  print {|[==[a[b]==]|};
  [%expect {| (Ok ((Token.String "a[b"), 11)) |}]

let%expect_test _ =
  print {|"é"|};
  [%expect {| (Ok ((Token.String "\195\169"), 3)) |}]

let%expect_test _ =
  print {|[|};
  [%expect {| (Ok (Token.Left_square, 1)) |}]

let%expect_test _ =
  print {|.|};
  [%expect {| (Ok (Token.Dot, 1)) |}]

let%expect_test _ =
  print {|..|};
  [%expect {| (Ok (Token.Double_dot, 2)) |}]

let%expect_test _ =
  print {|...|};
  [%expect {| (Ok (Token.Triple_dot, 3)) |}]
