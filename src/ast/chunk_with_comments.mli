type t =
  { tree : Ast.Chunk.t
  ; comments : Comment.t list
  ; empty_spaces : Empty_space.t list }
[@@deriving eq, ord, show]
