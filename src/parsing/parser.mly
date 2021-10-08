%{
  open Luast__tree.Cst

  let loc (begin_, end_) =
    { Luast__tree.Location.begin_ = Util.get_position begin_
      ; end_ = Util.get_position end_
    }
%}

(* %token If Then Else *)
%token Nil Return Function End
%token Comma Colon Semi_colon Equal Triple_dot
%token <string> Id
%token <Int64.t> Integer
%token <str> Str
%token Left_curly Right_curly
%token Left_paren Right_paren
%token Eof

%start <chunk> chunk

%%

let chunk :=
  | block = block; Eof; <>

let block :=
  | located(stats = list(stat); ret = option(retstat); {{stats; ret}})

let stat :=
  | located(vars = varlist; Equal; exps = explist; {Assignment {vars; exps}})
  | located(Function; name = funcname; body = funcbody; {Function_def {name; body}})

let varlist :=
  | ~ = separated_nonempty_list(Comma, var); <>

let explist :=
  | ~ = separated_nonempty_list(Comma, exp); <>

let retstat :=
  | located(
      Return; exps = option(explist); option(Semi_colon);
      {exps |> CCOpt.get_or ~default:[]}
    )

let funcname :=
  | list = separated_nonempty_list(Comma, Id); method_ = option(Colon; Id);
    {
      let names = CCList.append (CCOpt.to_list method_) (CCList.rev list) in
      { prefix = CCList.rev (CCList.tl names)
      ; name = CCList.hd names
      ; method_ = CCOpt.is_some method_
      }
    }

let funcbody :=
  | Left_paren; params = option(parlist); Right_paren; block = block; End;
    {{params = params |> CCOpt.get_or ~default:{names = []; ellipsis = false}; block}}

let parlist :=
  | Triple_dot; {{names = []; ellipsis = true}}
  | name = Id; {{names = [name]; ellipsis = false}}
  | name = Id; Comma; params = parlist; {{params with names = name :: params.names}}

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
