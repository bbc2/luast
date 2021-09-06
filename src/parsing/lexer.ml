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

let rec double_quoted_string buf acc =
  match%sedlex buf with
  | "\"" -> acc
  | ("\\", Chars "\\\"'") ->
    let escaped = CCString.of_char (Sedlexing.Utf8.lexeme buf).[1] in
    double_quoted_string buf (acc ^ escaped)
  | ("\\", Chars "abfnrtv") ->
    let escaped = (Sedlexing.Utf8.lexeme buf).[1] in
    double_quoted_string buf (acc ^ control_char escaped)
  | Compl (Chars "\\") ->
    double_quoted_string buf (acc ^ Sedlexing.Utf8.lexeme buf)
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
  | "if" -> If
  | "then" -> Then
  | "else" -> Else
  | "nil" -> Nil
  | identifier -> Id (Sedlexing.Utf8.lexeme buf)
  | "=" -> Equal
  | number -> Number (Sedlexing.Utf8.lexeme buf)
  | Plus (Chars " ") -> parse_token buf
  | Plus (Chars "\"") -> String (double_quoted_string buf "")
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
  print {|"a\xb"|};
  [%expect {| (Error "Unexpected double quoted string character") |}]

let%expect_test _ =
  print "\"a\nb\"";
  [%expect {| (Ok ((Token.String "a\nb"), 5)) |}]

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
  [%expect {| (Error "Unexpected beginning of token") |}]
