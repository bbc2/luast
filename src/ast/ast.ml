module Var = struct
  type t = Name of string [@@deriving eq, ord, show]
end

module Numeral = struct
  type t = Integer of Int64.t [@@deriving eq, ord, show]
end

module Str = struct
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
end = struct
  type t = Exp of Exp.t [@@deriving eq, ord, show]
end

and Exp : sig
  type t =
    | Nil
    | Numeral of Numeral.t
    | Str of Str.t
    | Table of Field.t list
  [@@deriving eq, ord, show]
end = struct
  type t =
    | Nil
    | Numeral of Numeral.t
    | Str of Str.t
    | Table of Field.t list
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
    { stats : Stat.t Located.t list
    ; ret : Retstat.t Located.t option }
  [@@deriving eq, ord, show]
end

module Chunk = struct
  type t = Block.t [@@deriving eq, ord, show]
end
