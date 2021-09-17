type t =
  | And
  | Break
  | Do
  | Else
  | Elseif
  | End
  | False
  | For
  | Function
  | Goto
  | If
  | In
  | Local
  | Nil
  | Not
  | Or
  | Repeat
  | Return
  | Then
  | True
  | Until
  | While
  | Plus (* + *)
  | Minus (* - *)
  | Star (* * *)
  | Slash (* / *)
  | Percent (* % *)
  | Caret (* ^ *)
  | Hash (* # *)
  | Amp (* & *)
  | Tilde (* ~ *)
  | Pipe (* | *)
  | Double_lt (* << *)
  | Double_gt (* >> *)
  | Double_slash (* // *)
  | Double_equal (* == *)
  | Tilde_equal (* ~= *)
  | Lte (* <= *)
  | Gte (* >= *)
  | Lt (* < *)
  | Gt (* > *)
  | Equal (* = *)
  | Left_paren (* ( *)
  | Right_paren (* ) *)
  | Left_curly (* { *)
  | Right_curly (* } *)
  | Left_square (* [ *)
  | Right_square (* ] *)
  | Double_colon (* :: *)
  | Semi_colon (* ; *)
  | Colon (* : *)
  | Comma (* , *)
  | Dot (* . *)
  | Double_dot (* .. *)
  | Triple_dot (* ... *)
  | Id of string
  | Integer of Int64.t
  | Str of Luast__ast.Ast.Str.t
  | Eof
[@@deriving show]

type token = t
