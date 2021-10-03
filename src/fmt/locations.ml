module State = struct
  type t = {mutable locs : Location_with_depth.t list}

  let init () = {locs = []}

  let add loc state = state.locs <- loc :: state.locs
end

let next depth = depth + 1

let rec do_field
    ~state
    ~depth
    (field : Luast__tree.Cst.Field.t Luast__tree.Located.t) =
  let depth = next depth in
  State.add {value = field.loc; depth} state;
  match field.value with
  | Exp exp -> do_exp ~state ~depth exp

and do_exp ~state ~depth (exp : Luast__tree.Cst.Exp.t) =
  let depth = next depth in
  match exp with
  | Nil
  | Numeral _
  | Str _ ->
    ()
  | Table fields -> CCList.iter (do_field ~state ~depth) fields

let do_stat ~state ~depth (stat : Luast__tree.Cst.Stat.t Luast__tree.Located.t)
    =
  let depth = next depth in
  State.add {value = stat.loc; depth} state;
  match stat.value with
  | Assignment {vars = _; exps} -> CCList.iter (do_exp ~state ~depth) exps

let do_stats ~state ~depth stats =
  let depth = next depth in
  CCList.iter (do_stat ~state ~depth) stats

let do_retstat ~state ~depth (retstat : 'a Luast__tree.Located.t) =
  let depth = next depth in
  State.add {value = retstat.loc; depth} state;
  CCList.iter (do_exp ~state ~depth) retstat.value

let do_ret ~state ~depth ret =
  let depth = next depth in
  CCOpt.iter (do_retstat ~state ~depth) ret

let do_block ~state ~depth {Luast__tree.Cst.Block.stats; ret} =
  let depth = next depth in
  do_stats ~state ~depth stats;
  do_ret ~state ~depth ret

let do_chunk ~state ~depth (chunk : Luast__tree.Cst.Chunk.t) =
  let depth = next depth in
  State.add {value = chunk.loc; depth} state;
  do_block ~state ~depth chunk.value

let get_locations chunk =
  let state = State.init () in
  do_chunk ~state ~depth:0 chunk;
  state.locs
