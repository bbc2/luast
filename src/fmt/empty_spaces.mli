type t

val init : empty_spaces:Luast__ast.Empty_space.t list -> t

val empty_lines : line:int -> t -> int
