module Var : sig
  type t = Name of string [@@deriving eq, ord, show]
end

module Numeral : sig
  type t = Integer of Int64.t [@@deriving eq, ord, show]
end

module Str : sig
  type t =
    | Short of string
    | Long of
        { level : int  (** Number of equal signs *)
        ; leading_newline : bool
        ; value : string }
  [@@deriving eq, ord, show]
end

module rec Field : sig
  type t = Exp of Exp.t [@@deriving eq, ord, show]
end

and Exp : sig
  type t =
    | Nil
    | Numeral of Numeral.t
    | Str of Str.t
    | Table of Field.t Located.t list
  [@@deriving eq, ord, show]
end

module Stat : sig
  type t =
    | Assignment of
        { vars : Var.t list
        ; exps : Exp.t list }
  [@@deriving eq, ord, show]
end

module Retstat : sig
  type t = Exp.t list [@@deriving eq, ord, show]
end

module Block : sig
  type t =
    { stats : Stat.t Located.t list
    ; ret : Retstat.t Located.t option }
  [@@deriving eq, ord, show]
end

module Chunk : sig
  type t = Block.t Located.t [@@deriving eq, ord, show]
end
