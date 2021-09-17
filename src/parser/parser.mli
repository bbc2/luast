module Origin : sig
  type t =
    | Lexer of string
    | Parser
  [@@deriving eq, ord, show]
end

module Parser_error : sig
  type t =
    { position : Luast__ast.Position.t
    ; origin : Origin.t }
  [@@deriving eq, ord, show]
end

val parse_chunk :
  string -> (Luast__ast.Chunk_with_comments.t, Parser_error.t) result
