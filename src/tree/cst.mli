(** Concrete syntax tree

    This tree, starting at `chunk`, is directly obtained from the parser.  It contains
    details about the syntax used in the source code. *)

type var = Name of string [@@deriving eq, ord, show]

type numeral = Integer of Int64.t [@@deriving eq, ord, show]

type str =
  | Short of string
  | Long of
      { level : int  (** Number of equal signs *)
      ; leading_newline : bool
      ; value : string }
[@@deriving eq, ord, show]

type field = Exp of exp [@@deriving eq, ord, show]

and exp =
  | Nil
  | Numeral of numeral
  | Str of str
  | Table of field Located.t list
[@@deriving eq, ord, show]

type stat =
  | Assignment of
      { vars : var list
      ; exps : exp list }
[@@deriving eq, ord, show]

type retstat = exp list [@@deriving eq, ord, show]

type block =
  { stats : stat Located.t list
  ; ret : retstat Located.t option }
[@@deriving eq, ord, show]

type chunk = block Located.t [@@deriving eq, ord, show]
