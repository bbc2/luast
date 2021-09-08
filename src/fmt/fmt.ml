let var_to_string (var : Luast__ast.Ast.Var.t) =
  match var with
  | Name str -> str

let rec field_to_string (field : Luast__ast.Ast.Field.t) =
  match field with
  | Exp exp -> exp_to_string exp

and exp_to_string (exp : Luast__ast.Ast.Exp.t) =
  match exp with
  | Nil -> "nil"
  | Numeral (Integer n) -> Printf.sprintf "%Ld" n
  | Table fields ->
    "{" ^ CCString.concat ", " (fields |> CCList.map field_to_string) ^ "}"

let stat_to_string (stat : Luast__ast.Ast.Stat.t) =
  match stat with
  | Assignment {vars; exps} ->
    let vs = CCString.concat ", " (vars |> CCList.map var_to_string) in
    let es = CCString.concat ", " (exps |> CCList.map exp_to_string) in
    Printf.sprintf "%s = %s" vs es

let stats_to_string stats =
  let concatenated =
    CCString.concat "\n" (stats |> CCList.map stat_to_string)
  in
  if stats = [] then
    concatenated
  else
    concatenated ^ "\n"

let ret_to_string ret =
  match ret with
  | None -> ""
  | Some exps ->
    let exps = CCString.concat ", " (exps |> CCList.map exp_to_string) in
    Printf.sprintf "return %s" exps

let format_chunk chunk =
  let {Luast__ast.Ast.Block.stats; ret} = chunk in
  Printf.sprintf "%s%s" (stats_to_string stats) (ret_to_string ret)

let print str = str |> format_chunk |> print_string

let%expect_test _ =
  print {stats = []; ret = None};
  [%expect {| |}]

let%expect_test _ =
  print {stats = []; ret = Some [Numeral (Integer 0L)]};
  [%expect {| return 0 |}]

let%expect_test _ =
  print
    { stats = [Assignment {vars = [Name "a"]; exps = [Numeral (Integer 0L)]}]
    ; ret = Some [Numeral (Integer 1L)] };
  [%expect {|
    a = 0
    return 1 |}]
