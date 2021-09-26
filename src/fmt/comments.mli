type t

val init :
     code_locations:Luast__ast.Location.t list
  -> comments:Luast__ast.Comment.t list
  -> t

val pop_comments_after : Luast__ast.Position.t -> t -> Luast__ast.Comment.t list
