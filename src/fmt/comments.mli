type t

val init :
     code_locations:Location_with_depth.t list
  -> comments:Luast__ast.Comment.t list
  -> t

val pop_comments_before :
  Luast__ast.Position.t -> t -> Luast__ast.Comment.t list

val pop_comment_after :
  Luast__ast.Position.t -> t -> Luast__ast.Comment.t option

val pop_comments_after : Luast__ast.Position.t -> t -> Luast__ast.Comment.t list
