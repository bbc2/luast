%{
  open Luast__ast.Ast
%}

(* %token If Then Else *)
%token Nil Return
%token Comma Semi_colon Equal
%token <string> Id
%token <Int64.t> Integer
%token Left_curly Right_curly
(*
%token <string> Number
%token <string> String
*)
%token Eof

%start <Chunk.t> chunk

%%

let chunk :=
  | ~ = block; Eof; <>

let block :=
  | stats = list(stat); ret = option(retstat); {{Block.stats; ret}}

let stat :=
  | vars = varlist; Equal; exps = explist; {Stat.Assignment {vars; exps}}

let varlist :=
  | ~ = separated_nonempty_list(Comma, var); <>

let explist :=
  | ~ = separated_nonempty_list(Comma, exp); <>

let retstat :=
  | Return; {[]}

let var :=
  | ~ = Id; <Var.Name>

let exp :=
  | Nil; {Nil}
  | integer = Integer; {Numeral (Integer integer)}
  | tableconstructor

let tableconstructor :=
  | Left_curly; ~ = fields; Right_curly; <Exp.Table>

let fields :=
  | {[]}
  | field = field; {[field]}
  | field = field; fieldsep; fields = fields; {field :: fields}

let field :=
  | ~ = exp; <Field.Exp>

let fieldsep :=
  | Comma
  | Semi_colon

%%
