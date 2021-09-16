let format_comma_sep fmt () = Format.fprintf fmt ",@;<1 2>"

let trailing_sep sep =
  Format.pp_print_custom_break ~fits:("", 0, "") ~breaks:(sep, 0, "")

let var_to_string (var : Luast__ast.Ast.Var.t) =
  match var with
  | Name str -> str

let rec format_field fmt (field : Luast__ast.Ast.Field.t) =
  match field with
  | Exp exp ->
    Format.pp_open_hvbox fmt 0;
    format_exp fmt exp;
    Format.pp_close_box fmt ()

and format_exp fmt (exp : Luast__ast.Ast.Exp.t) =
  match exp with
  | Nil -> Format.fprintf fmt "nil"
  | Numeral (Integer n) -> Format.fprintf fmt "%Ld" n
  | Str (Short str) -> Format.fprintf fmt "%S" str
  | Str (Long {level; leading_newline; value}) ->
    let equal_signs = String.init level (fun _ -> '=') in
    let newline =
      if leading_newline then
        "\n"
      else
        ""
    in
    Format.fprintf fmt "[%s[%s%s]%s]" equal_signs newline value equal_signs
  | Table fields ->
    if fields = [] then
      Format.fprintf fmt "{}"
    else (
      Format.fprintf fmt "{@;<0 2>";
      Format.pp_print_list ~pp_sep:format_comma_sep format_field fmt fields;
      Format.fprintf fmt "%t}" (trailing_sep ",")
    )

let format_exps fmt exps = Format.pp_print_list format_exp fmt exps

let format_stat fmt (stat : Luast__ast.Ast.Stat.t) =
  match stat with
  | Assignment {vars; exps} ->
    let vs = CCString.concat ", " (vars |> CCList.map var_to_string) in
    Format.pp_open_hvbox fmt 0;
    Format.fprintf fmt "%s = " vs;
    format_exps fmt exps;
    Format.pp_close_box fmt ()

let format_stats fmt stats =
  Format.pp_open_vbox fmt 0;
  Format.pp_print_list format_stat fmt stats;
  Format.pp_close_box fmt ()

let format_ret fmt ret =
  match ret with
  | None -> ()
  | Some exps ->
    Format.fprintf fmt "return";
    if exps != [] then (
      Format.pp_print_space fmt ();
      format_exps fmt exps
    )

let format_chunk chunk =
  let buffer = Buffer.create 0 in
  let fmt = Format.formatter_of_buffer buffer in
  Format.pp_set_margin fmt 90;
  let {Luast__ast.Ast.Block.stats; ret} = chunk in
  Format.pp_open_vbox fmt 0;
  format_stats fmt stats;
  if stats != [] && CCOpt.is_some ret then Format.pp_print_cut fmt ();
  format_ret fmt ret;
  if stats != [] || CCOpt.is_some ret then Format.pp_print_cut fmt ();
  Format.pp_close_box fmt ();
  Format.pp_print_flush fmt ();
  Buffer.contents buffer

let print str = str |> format_chunk |> print_string

let%expect_test _ =
  print {stats = []; ret = None};
  [%expect {| |}]

let%expect_test _ =
  print {stats = []; ret = Some [Numeral (Integer 0L)]};
  [%expect {|
    return
    0 |}]

let%expect_test _ =
  print
    { stats = [Assignment {vars = [Name "a"]; exps = [Numeral (Integer 0L)]}]
    ; ret = Some [Numeral (Integer 1L)] };
  [%expect {|
    a = 0
    return
    1 |}]
