%{
  open Luast__ast.Ast

  let loc (begin_, end_) =
    { Luast__ast.Location.begin_ = Util.get_position begin_
      ; end_ = Util.get_position end_
    }
%}

(* %token If Then Else *)
%token Nil Return
%token Comma Semi_colon Equal
%token <string> Id
%token <Int64.t> Integer
%token <Str.t> Str
%token Left_curly Right_curly
%token Eof

%start <Chunk.t> chunk

%%

let chunk :=
  | block = block; Eof; <>

let block :=
  | located(stats = list(stat); ret = option(retstat); {{Block.stats; ret}})

let stat :=
  | located(vars = varlist; Equal; exps = explist; {Stat.Assignment {vars; exps}})

let varlist :=
  | ~ = separated_nonempty_list(Comma, var); <>

let explist :=
  | ~ = separated_nonempty_list(Comma, exp); <>

let retstat :=
  | located(
      Return; exps = option(explist); option(Semi_colon);
      {exps |> CCOpt.get_or ~default:[]}
    )

let var :=
  | ~ = Id; <Var.Name>

let exp :=
  | Nil; {Nil}
  | integer = Integer; {Numeral (Integer integer)}
  | ~ = Str; <Exp.Str>
  | tableconstructor

let tableconstructor :=
  | Left_curly; ~ = fields; Right_curly; <Exp.Table>

let fields :=
  | {[]}
  | field = located(field); {[field]}
  | field = located(field); fieldsep; fields = fields; {field :: fields}

let field :=
  | ~ = exp; <Field.Exp>

let fieldsep :=
  | Comma
  | Semi_colon

let located(value) ==
  ~ = value; {{Luast__ast.Located.value; loc = loc $sloc }}
