let get_position lexing_position =
  let {Lexing.pos_fname = _; pos_lnum; pos_bol; pos_cnum} = lexing_position in
  let column = pos_cnum - pos_bol + 1 in
  {Luast__tree.Position.line = pos_lnum; column}

let get_location buffer =
  let (begin_, end_) = Sedlexing.lexing_positions buffer in
  {Luast__tree.Location.begin_ = get_position begin_; end_ = get_position end_}
