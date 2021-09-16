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
  | Plus
  | Minus
  | Star
  | Slash
  | Percent
  | Caret
  | Hash
  | Amp
  | Tilde
  | Pipe
  | Double_lt
  | Double_gt
  | Double_slash
  | Double_equal
  | Tilde_equal
  | Lte
  | Gte
  | Lt
  | Gt
  | Equal
  | Left_paren
  | Right_paren
  | Left_curly
  | Right_curly
  | Left_square
  | Right_square
  | Double_colon
  | Semi_colon
  | Colon
  | Comma
  | Dot
  | Double_dot
  | Triple_dot
  | Id of string
  | Integer of Int64.t
  | Str of Luast__ast.Ast.Str.t
  | Eof
[@@deriving show]

type token = t

let to_string = function
  | And -> "And"
  | Break -> "Break"
  | Do -> "Do"
  | Else -> "Else"
  | Elseif -> "Elseif"
  | End -> "End"
  | False -> "False"
  | For -> "For"
  | Function -> "Function"
  | Goto -> "Goto"
  | If -> "If"
  | In -> "In"
  | Local -> "Local"
  | Nil -> "Nil"
  | Not -> "Not"
  | Or -> "Or"
  | Repeat -> "Repeat"
  | Return -> "Return"
  | Then -> "Then"
  | True -> "True"
  | Until -> "Until"
  | While -> "While"
  | Plus -> "Plus"
  | Minus -> "Minus"
  | Star -> "Star"
  | Slash -> "Slash"
  | Percent -> "Percent"
  | Caret -> "Caret"
  | Hash -> "Hash"
  | Amp -> "Amp"
  | Tilde -> "Tilde"
  | Pipe -> "Pipe"
  | Double_lt -> "Double_lt"
  | Double_gt -> "Double_gt"
  | Double_slash -> "Double_slash"
  | Double_equal -> "Double_equal"
  | Tilde_equal -> "Tilde_equal"
  | Lte -> "Lte"
  | Gte -> "Gte"
  | Lt -> "Lt"
  | Gt -> "Gt"
  | Equal -> "Equal"
  | Left_paren -> "Left_paren"
  | Right_paren -> "Right_paren"
  | Left_curly -> "Left_curly"
  | Right_curly -> "Rigth_curly"
  | Left_square -> "Left_square"
  | Right_square -> "Right_square"
  | Double_colon -> "Double_colon"
  | Semi_colon -> "Semi_colon"
  | Colon -> "Colon"
  | Comma -> "Comma"
  | Dot -> "Dot"
  | Double_dot -> "Double_dot"
  | Triple_dot -> "Triple_dot"
  | Id id -> Printf.sprintf "Id: %s" id
  | Integer integer -> Printf.sprintf "Integer: %Ld" integer
  | Str str -> Printf.sprintf "String: %s" ([%show: Luast__ast.Ast.Str.t] str)
  | Eof -> "Eof"
