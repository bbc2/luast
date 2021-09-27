let format_comma_sep fmt () = Format.fprintf fmt ",@;<1 2>"

let trailing_sep sep =
  Format.pp_print_custom_break ~fits:("", 0, "") ~breaks:(sep, 0, "")

let var_to_string (var : Luast__ast.Ast.Var.t) =
  match var with
  | Name str -> str

let comment_to_string (comment : Luast__ast.Comment.t) =
  match comment.type_ with
  | Short -> Printf.sprintf "--%s" comment.str
  | Long {level} ->
    let equal_signs = String.init level (fun _ -> '=') in
    Printf.sprintf "--[%s[%s]%s]--" equal_signs comment.str equal_signs

let format_empty_space_after
    fmt
    ~empty_spaces
    ~(position : Luast__ast.Position.t) =
  let line_count =
    Empty_spaces.empty_lines ~line:(position.line + 1) empty_spaces
  in
  if line_count > 0 then Format.pp_print_cut fmt ()

let format_comments_after
    fmt
    ~comments
    ~empty_spaces
    ~(position : Luast__ast.Position.t) =
  let current_line = ref position.line in
  comments
  |> Comments.pop_comments_after position
  |> CCList.iter (fun comment ->
         let {Luast__ast.Comment.location = {begin_; end_}; _} = comment in
         if begin_.line > !current_line then
           Format.pp_print_cut fmt ()
         else
           Format.fprintf fmt " ";
         current_line := begin_.line;
         Format.pp_print_string fmt (comment_to_string comment);
         format_empty_space_after fmt ~empty_spaces ~position:end_)

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

let format_located_stat
    fmt
    (stat : Luast__ast.Ast.Stat.t Luast__ast.Located.t)
    ~comments
    ~empty_spaces =
  format_stat fmt stat.value;
  format_empty_space_after fmt ~empty_spaces ~position:stat.loc.end_;
  format_comments_after fmt ~comments ~empty_spaces ~position:stat.loc.end_

let format_stats fmt stats ~comments ~empty_spaces =
  Format.pp_open_vbox fmt 0;
  Format.pp_print_list (format_located_stat ~comments ~empty_spaces) fmt stats;
  Format.pp_close_box fmt ()

let format_ret fmt (ret : Luast__ast.Ast.Retstat.t Luast__ast.Located.t option)
    =
  match ret with
  | None -> ()
  | Some {value = exps; loc = _} ->
    Format.fprintf fmt "return";
    if exps != [] then (
      Format.pp_print_space fmt ();
      format_exps fmt exps
    )

let format_chunk chunk_with_comments =
  let { Luast__ast.Chunk_with_comments.tree = chunk
      ; locations
      ; comments
      ; empty_spaces } =
    chunk_with_comments
  in
  let comments = Comments.init ~code_locations:locations ~comments in
  let empty_spaces = Empty_spaces.init ~empty_spaces in
  let buffer = Buffer.create 0 in
  let fmt = Format.formatter_of_buffer buffer in
  Format.pp_set_margin fmt 90;
  let {Luast__ast.Ast.Block.stats; ret} = chunk in
  Format.pp_open_vbox fmt 0;
  format_stats fmt stats ~comments ~empty_spaces;
  if stats != [] && CCOpt.is_some ret then Format.pp_print_cut fmt ();
  format_ret fmt ret;
  if stats != [] || CCOpt.is_some ret then Format.pp_print_cut fmt ();
  Format.pp_close_box fmt ();
  Format.pp_print_flush fmt ();
  Buffer.contents buffer
