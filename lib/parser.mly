%{
  (* Workaround for https://gitlab.inria.fr/fpottier/menhir/issues/26 *)
  module Lib = struct end
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

%start <Ast.Chunk.t> chunk

%%

let chunk :=
  | ~ = block; Eof; <>

let block :=
  | stats = list(stat); ret = option(retstat); {{Ast.Block.stats; ret}}

let stat :=
  | vars = varlist; Equal; exps = explist; {Ast.Stat.Assignment {vars; exps}}

let varlist :=
  | ~ = separated_nonempty_list(Comma, var); <>

let explist :=
  | ~ = separated_nonempty_list(Comma, exp); <>

let retstat :=
  | Return; {[]}

let var :=
  | ~ = Id; <Ast.Var.Name>

let exp :=
  | Nil; {Ast.Exp.Nil}

%%
