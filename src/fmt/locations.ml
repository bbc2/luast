module State = struct
  type t = {mutable locs : Location_with_depth.t list}

  let init () = {locs = []}

  let add loc state = state.locs <- loc :: state.locs
end

let next depth = depth + 1

let rec do_field
    ~state
    ~depth
    (field : Luast__tree.Cst.field Luast__tree.Located.t) =
  let depth = next depth in
  State.add {value = field.loc; depth} state;
  match field.value with
  | Exp exp -> do_exp ~state ~depth exp

and do_exp ~state ~depth (exp : Luast__tree.Cst.exp) =
  let depth = next depth in
  match exp with
  | Nil
  | Numeral _
  | Str _ ->
    ()
  | Table fields -> CCList.iter (do_field ~state ~depth) fields

let do_retstat ~state ~depth (retstat : 'a Luast__tree.Located.t) =
  let depth = next depth in
  State.add {value = retstat.loc; depth} state;
  CCList.iter (do_exp ~state ~depth) retstat.value

let rec do_stat
    ~state
    ~depth
    (stat : Luast__tree.Cst.stat Luast__tree.Located.t) =
  let depth = next depth in
  State.add {value = stat.loc; depth} state;
  match stat.value with
  | Assignment {vars = _; exps} -> CCList.iter (do_exp ~state ~depth) exps
  | Function_def {body; _} -> do_chunk ~state ~depth body.block

and do_stats ~state ~depth stats =
  let depth = next depth in
  CCList.iter (do_stat ~state ~depth) stats

and do_ret ~state ~depth ret =
  let depth = next depth in
  CCOpt.iter (do_retstat ~state ~depth) ret

and do_block ~state ~depth {Luast__tree.Cst.stats; ret} =
  let depth = next depth in
  do_stats ~state ~depth stats;
  do_ret ~state ~depth ret

and do_chunk ~state ~depth (chunk : Luast__tree.Cst.chunk) =
  let depth = next depth in
  State.add {value = chunk.loc; depth} state;
  do_block ~state ~depth chunk.value

let get_locations chunk =
  let state = State.init () in
  do_chunk ~state ~depth:0 chunk;
  state.locs
