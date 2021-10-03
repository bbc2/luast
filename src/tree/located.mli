type 'value t =
  { value : 'value
  ; loc : Location.t }
[@@deriving eq, ord, show]
