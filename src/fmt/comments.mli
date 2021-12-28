type t

val init :
     code_locations:Location_with_depth.t list
  -> comments:Luast__tree.Comment.t list
  -> t

val has_comments_around : Luast__tree.Position.t -> t -> bool

val pop_comments_before :
  Luast__tree.Position.t -> t -> Luast__tree.Comment.t list

val pop_comment_after :
  Luast__tree.Position.t -> t -> Luast__tree.Comment.t option

val pop_comments_after :
  Luast__tree.Position.t -> t -> Luast__tree.Comment.t list
