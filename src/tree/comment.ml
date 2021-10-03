module Type = struct
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

let compare_begin_positions c0 c1 =
  Position.compare c0.location.begin_ c1.location.begin_
