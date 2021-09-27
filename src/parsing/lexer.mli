exception Lexer_error of string

module Step : sig
  type t =
    { token : Token.t
    ; comments : Luast__ast.Comment.t list
    ; empty_spaces : Luast__ast.Empty_space.t list }
  [@@deriving eq, ord, show]
end

val parse_token : Sedlexing.lexbuf -> Step.t
(** Parse one token and update the lexing buffer.

    Raises [Lexer_error] if no token can be recognized. *)
