%{
  open Luast__ast.Ast
%}

(* %token If Then Else *)
%token Nil Return
%token Comma Equal
%token <string> Id
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
  | Nil; {Exp.Nil}

%%
