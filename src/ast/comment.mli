module Type : sig
  type t =
    | Short
    | Long of {level : int}
  [@@deriving eq, ord, show]
end

type t =
  { type_ : Type.t
  ; str : string
  ; location : Location.t }
[@@deriving eq, ord, show]

val compare_begin_positions : t -> t -> int
