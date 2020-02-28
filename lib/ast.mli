module Var : sig
  type t = Name of string [@@deriving eq, ord, show]
end

module Exp : sig
  type t = Nil [@@deriving eq, ord, show]
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
    { stats : Stat.t list
    ; ret : Retstat.t option }
  [@@deriving eq, ord, show]
end

module Chunk : sig
  type t = Block.t [@@deriving eq, ord, show]
end
