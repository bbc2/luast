%{
  open Luast__tree.Cst

  let loc (begin_, end_) =
    { Luast__tree.Location.begin_ = Util.get_position begin_
      ; end_ = Util.get_position end_
    }
%}

(* %token If Then Else *)
%token Nil Return
%token Comma Semi_colon Equal
%token <string> Id
%token <Int64.t> Integer
%token <str> Str
%token Left_curly Right_curly
%token Eof

%start <chunk> chunk

%%

let chunk :=
  | block = block; Eof; <>

let block :=
  | located(stats = list(stat); ret = option(retstat); {{stats; ret}})

let stat :=
  | located(vars = varlist; Equal; exps = explist; {Assignment {vars; exps}})

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
  | ~ = Id; <Name>

let exp :=
  | Nil; {Nil}
  | integer = Integer; {Numeral (Integer integer)}
  | ~ = Str; <Str>
  | tableconstructor

let tableconstructor :=
  | Left_curly; ~ = fields; Right_curly; <Table>

let fields :=
  | {[]}
  | field = located(field); {[field]}
  | field = located(field); fieldsep; fields = fields; {field :: fields}

let field :=
  | ~ = exp; <Exp>

let fieldsep :=
  | Comma
  | Semi_colon

let located(value) ==
  ~ = value; {{Luast__tree.Located.value; loc = loc $sloc }}
