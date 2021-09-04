type t =
  (* | And *)
  (* | Break *)
  (* | Do *)
  | Else
  (* | Elseif *)
  (* | End *)
  (* | False *)
  (* | For *)
  (* | Function *)
  (* | Goto *)
  | If
  (* | In *)
  (* | Local *)
  | Nil
  (* | Not *)
  (* | Or *)
  (* | Repeat *)
  | Return
  | Then
  (* | True *)
  (* | Until *)
  (* | While *)
  | Id of string
  | Number of string
  | String of string
  | Comma
  | Semi_colon
  | Equal
  | Eof
[@@deriving show]

type token = t

let to_string = function
  | If -> "If"
  | Then -> "Then"
  | Else -> "Else"
  | Nil -> "Nil"
  | Return -> "Return"
  | Id id -> Printf.sprintf "Id: %s" id
  | Number number -> Printf.sprintf "Number: %s" number
  | String str -> Printf.sprintf "String: %s" str
  | Comma -> "Comma"
  | Semi_colon -> "Semi_colon"
  | Equal -> "Equal"
  | Eof -> "Eof"
