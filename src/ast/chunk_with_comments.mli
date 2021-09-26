type t =
  { tree : Ast.Chunk.t
  ; locations : Location.t list
  ; comments : Comment.t list }
[@@deriving eq, ord, show]
