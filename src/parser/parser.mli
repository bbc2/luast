module Origin : sig
  type t =
    | Lexer of string
    | Parser
  [@@deriving eq, ord, show]
end

module Position : sig
  type t =
    { line : int
    ; column : int }
  [@@deriving eq, ord, show]
end

module Parser_error : sig
  type t =
    { position : Position.t
    ; origin : Origin.t }
  [@@deriving eq, ord, show]
end

val parse_chunk : string -> (Luast__ast.Ast.Chunk.t, Parser_error.t) result
