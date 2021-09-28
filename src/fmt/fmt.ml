let var_to_string (var : Luast__ast.Ast.Var.t) =
  match var with
  | Name str -> str

let comment_to_string (comment : Luast__ast.Comment.t) =
  match comment.type_ with
  | Short -> Printf.sprintf "--%s" comment.str
  | Long {level} ->
    let equal_signs = String.init level (fun _ -> '=') in
    Printf.sprintf "--[%s[%s]%s]--" equal_signs comment.str equal_signs

let format_comment fmt (comment : Luast__ast.Comment.t) =
  match comment.type_ with
  | Short -> Format.fprintf fmt "@<999>--%s" comment.str
  | Long {level} ->
    let equal_signs = String.init level (fun _ -> '=') in
    Format.fprintf fmt "@<999>--[%s[%s]%s]--" equal_signs comment.str
      equal_signs

let format_empty_space_after
    fmt
    ~empty_spaces
    ~(position : Luast__ast.Position.t) =
  let line_count =
    Empty_spaces.empty_lines ~line:(position.line + 1) empty_spaces
  in
  if line_count > 0 then Format.pp_print_cut fmt ()

let format_comments_before
    fmt
    ~comments
    ~empty_spaces
    ~(position : Luast__ast.Position.t) =
  let current_line = ref None in
  comments
  |> Comments.pop_comments_before position
  |> CCList.iter (fun comment ->
         let {Luast__ast.Comment.location = {begin_; end_}; _} = comment in
         (match !current_line with
         | None -> ()
         | Some line when line < begin_.line -> Format.pp_print_cut fmt ()
         | _ -> Format.fprintf fmt " ");
         current_line := Some begin_.line;
         format_comment fmt comment;
         format_empty_space_after fmt ~empty_spaces ~position:end_)

let format_comments_after
    fmt
    ~comments
    ~empty_spaces
    ~(position : Luast__ast.Position.t)
    ~line =
  let last_line = ref line in
  comments
  |> Comments.pop_comments_after position
  |> CCList.iter (fun comment ->
         let {Luast__ast.Comment.location = {begin_; end_}; _} = comment in
         if begin_.line > line then
           Format.pp_print_break fmt 1 0
         else
           Format.fprintf fmt " ";
         last_line := end_.line;
         format_comment fmt comment;
         format_empty_space_after fmt ~empty_spaces ~position:end_)

let rec format_located_list pp_value fmt ~comments ~empty_spaces :
    'a Luast__ast.Located.t list -> unit = function
  | [] -> ()
  | [v] ->
    format_comments_before fmt ~comments ~empty_spaces ~position:v.loc.begin_;
    pp_value fmt v.value;
    let comment = Comments.pop_comment_after v.loc.end_ comments in

    (* Ensure we split the line after a short comment. *)
    (match comment with
    | Some {type_ = Short; _} -> Format.pp_print_as fmt 999 ""
    | _ -> ());

    (* The separator contains the first potential comment so that it is attached to the
       trailing comma. *)
    let first_comment_str =
      match comment with
      | None -> ""
      | Some comment -> Printf.sprintf " %s" (comment_to_string comment)
    in
    let fits = (first_comment_str, 0, "") in
    (* Decrease indentation here for the closing bracket, which will follow the last
       list item or comment. *)
    let breaks = ("," ^ first_comment_str, -2, "") in
    Format.pp_print_custom_break fmt ~fits ~breaks;
    format_comments_after fmt ~comments ~empty_spaces ~position:v.loc.end_
      ~line:v.loc.end_.line
  | v :: vs ->
    format_comments_before fmt ~comments ~empty_spaces ~position:v.loc.begin_;
    pp_value fmt v.value;
    Format.fprintf fmt ",";
    format_comments_after fmt ~comments ~empty_spaces ~position:v.loc.end_
      ~line:v.loc.end_.line;
    Format.pp_print_break fmt 1 0;
    format_located_list pp_value fmt ~comments ~empty_spaces vs

let rec format_field
    fmt
    ~comments
    ~empty_spaces
    (field : Luast__ast.Ast.Field.t) =
  match field with
  | Exp exp ->
    Format.pp_open_hvbox fmt 0;
    format_exp fmt ~comments ~empty_spaces exp;
    Format.pp_close_box fmt ()

and format_exp ~comments ~empty_spaces fmt (exp : Luast__ast.Ast.Exp.t) =
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
      Format.fprintf fmt "{@;<0 2>@[<hv 0>";
      format_located_list
        (format_field ~comments ~empty_spaces)
        fmt ~comments ~empty_spaces fields;
      Format.fprintf fmt "@]}"
    )

let format_exps fmt ~comments ~empty_spaces exps =
  Format.pp_print_list (format_exp ~comments ~empty_spaces) fmt exps

let format_stat fmt ~comments ~empty_spaces (stat : Luast__ast.Ast.Stat.t) =
  match stat with
  | Assignment {vars; exps} ->
    let vs = CCString.concat ", " (vars |> CCList.map var_to_string) in
    Format.pp_open_hvbox fmt 0;
    Format.fprintf fmt "%s = " vs;
    format_exps fmt ~comments ~empty_spaces exps;
    Format.pp_close_box fmt ()

let format_located_stat
    fmt
    (stat : Luast__ast.Ast.Stat.t Luast__ast.Located.t)
    ~comments
    ~empty_spaces =
  format_comments_before fmt ~comments ~empty_spaces ~position:stat.loc.begin_;
  format_stat fmt ~comments ~empty_spaces stat.value;
  format_empty_space_after fmt ~empty_spaces ~position:stat.loc.end_;
  format_comments_after fmt ~comments ~empty_spaces ~position:stat.loc.end_
    ~line:stat.loc.end_.line

let format_stats fmt stats ~comments ~empty_spaces =
  Format.pp_open_vbox fmt 0;
  Format.pp_print_list (format_located_stat ~comments ~empty_spaces) fmt stats;
  Format.pp_close_box fmt ()

let format_ret
    fmt
    ~comments
    ~empty_spaces
    (ret : Luast__ast.Ast.Retstat.t Luast__ast.Located.t option) =
  match ret with
  | None -> ()
  | Some {value = exps; loc = _} ->
    Format.fprintf fmt "return";
    if exps != [] then (
      Format.pp_print_space fmt ();
      format_exps fmt ~comments ~empty_spaces exps
    )

let format_block fmt block ~comments ~empty_spaces =
  let {Luast__ast.Ast.Block.stats; ret} = block in
  Format.pp_open_vbox fmt 0;
  format_stats fmt stats ~comments ~empty_spaces;
  if stats != [] && CCOpt.is_some ret then Format.pp_print_cut fmt ();
  format_ret fmt ~comments ~empty_spaces ret;
  if stats != [] || CCOpt.is_some ret then Format.pp_print_cut fmt ();
  Format.pp_close_box fmt ()

let format_located_block
    fmt
    (block : Luast__ast.Ast.Block.t Luast__ast.Located.t)
    ~comments
    ~empty_spaces =
  format_block fmt block.value ~comments ~empty_spaces;
  format_comments_after fmt ~comments ~empty_spaces ~position:block.loc.end_
    ~line:block.loc.end_.line

let format_chunk chunk_with_comments =
  let {Luast__ast.Chunk_with_comments.tree = chunk; comments; empty_spaces} =
    chunk_with_comments
  in
  let comments =
    Comments.init ~code_locations:(Locations.get_locations chunk) ~comments
  in
  let empty_spaces = Empty_spaces.init ~empty_spaces in
  let buffer = Buffer.create 0 in
  let fmt = Format.formatter_of_buffer buffer in
  Format.pp_set_margin fmt 90;
  format_located_block fmt chunk ~comments ~empty_spaces;
  Format.pp_print_flush fmt ();
  Buffer.contents buffer
