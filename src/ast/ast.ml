module Var = struct
  type t = Name of string [@@deriving eq, ord, show]
end

module Numeral = struct
  type t = Integer of Int64.t [@@deriving eq, ord, show]
end

module Exp = struct
  type t =
    | Nil
    | Numeral of Numeral.t
  [@@deriving eq, ord, show]
end

module Stat = struct
  type t =
    | Assignment of
        { vars : Var.t list
        ; exps : Exp.t list }
  [@@deriving eq, ord, show]
end

module Retstat = struct
  type t = Exp.t list [@@deriving eq, ord, show]
end

module Block = struct
  type t =
    { stats : Stat.t list
    ; ret : Retstat.t option }
  [@@deriving eq, ord, show]
end

module Chunk = struct
  type t = Block.t [@@deriving eq, ord, show]
end
