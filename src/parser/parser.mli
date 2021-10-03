module Origin : sig
  type t =
    | Lexer of string
    | Parser
  [@@deriving eq, ord, show]
end

module Parser_error : sig
  type t =
    { position : Luast__tree.Position.t
    ; origin : Origin.t }
  [@@deriving eq, ord, show]
end

val parse_chunk :
  string -> (Luast__tree.Chunk_with_comments.t, Parser_error.t) result
