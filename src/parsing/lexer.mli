exception Lexer_error of string

val parse_token : Sedlexing.lexbuf -> Token.t
(** Parse one token and update the lexing buffer.

    Raises [Lexer_error] if no token can be recognized. *)
