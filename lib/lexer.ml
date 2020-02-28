let digit = [%sedlex.regexp? '0' .. '9']

let letter = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z']

let number = [%sedlex.regexp? Plus digit]

let identifier = [%sedlex.regexp? (letter, Star (letter | digit))]

let rec double_quoted_string buf acc =
  match%sedlex buf with
  | "\"" -> acc
  | ("\\", Chars "\\\"'") ->
    let escaped = CCString.of_char (Sedlexing.Utf8.lexeme buf).[1] in
    double_quoted_string buf (acc ^ escaped)
  | Compl (Chars "\\") ->
    double_quoted_string buf (acc ^ Sedlexing.Utf8.lexeme buf)
  | _ -> failwith "Unexpected character"

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
    | `Not_matched lexeme -> long_string ~level buf (acc ^ "]" ^ lexeme) )
  | any -> long_string ~level buf (acc ^ Sedlexing.Utf8.lexeme buf)
  | _ -> failwith "Unexpected character"

let rec parse_token buf : Token.t =
  match%sedlex buf with
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
  | _ -> failwith "next: Unexpected character"
