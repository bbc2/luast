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

type retstat = exp list [@@deriving eq, ord, show]

type params =
  { names : string list
  ; ellipsis : bool }
[@@deriving eq, ord, show]

type func_name =
  { prefix : string list
  ; name : string
  ; method_ : bool }
[@@deriving eq, ord, show]

type body =
  { params : params
  ; block : block Located.t }
[@@deriving eq, ord, show]

and stat =
  | Assignment of
      { vars : var list
      ; exps : exp list }
  | Function_def of
      { name : func_name
      ; body : body }
[@@deriving eq, ord, show]

and block =
  { stats : stat Located.t list
  ; ret : retstat Located.t option }
[@@deriving eq, ord, show]

type chunk = block Located.t [@@deriving eq, ord, show]
