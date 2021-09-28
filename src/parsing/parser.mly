%parameter<Param : sig
  val add_loc : Luast__ast.Location.t -> unit
end>

%{
  open Luast__ast.Ast

  let loc (begin_, end_) =
    let loc =
      { Luast__ast.Location.begin_ = Util.get_position begin_
      ; end_ = Util.get_position end_
      } in
    Param.add_loc loc;
    loc
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
  | stats = list(stat); ret = option(retstat);
    {{Luast__ast.Located.value = {Block.stats; ret}; loc = loc $sloc}}

let stat :=
  | vars = varlist; Equal; exps = explist;
  {{Luast__ast.Located.value = Stat.Assignment {vars; exps}; loc = loc $sloc}}

let varlist :=
  | ~ = separated_nonempty_list(Comma, var); <>

let explist :=
  | ~ = separated_nonempty_list(Comma, exp); <>

let retstat :=
  | Return; exps = option(explist); option(Semi_colon);
  {{Luast__ast.Located.value = exps |> CCOpt.get_or ~default:[]; loc = loc $sloc}}

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
  | field = field; {[field]}
  | field = field; fieldsep; fields = fields; {field :: fields}

let field :=
  | ~ = exp; <Field.Exp>

let fieldsep :=
  | Comma
  | Semi_colon

%%
