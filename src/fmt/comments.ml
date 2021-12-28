module Map = CCMap.Make (Luast__tree.Position)

module Value = struct
  type t =
    { depth : int
    ; comments : Luast__tree.Comment.t list ref }
end

type t =
  { before_map : Value.t Map.t
  ; after_map : Value.t Map.t }

let init ~code_locations ~comments =
  let before_map =
    code_locations
    |> List.to_seq
    |> Seq.map (fun {Location_with_depth.value = {begin_; end_ = _}; depth} ->
           (begin_, {Value.depth; comments = ref []}))
    |> Map.of_seq
  in
  let after_map =
    code_locations
    |> List.to_seq
    |> Seq.map (fun {Location_with_depth.value = {begin_ = _; end_}; depth} ->
           (end_, {Value.depth; comments = ref []}))
    |> Map.of_seq
  in
  (* Thanks to the binary tree backing the map, finding the right position is logarithmic
     in complexity, which means we probably achieve O(n * log(n)) for this function. *)
  comments
  |> CCList.sort (fun c0 c1 ->
         Luast__tree.Comment.compare_begin_positions c1 c0)
  |> CCList.iter (fun (comment : Luast__tree.Comment.t) ->
         let before_opt =
           Map.find_first_opt
             (fun position -> comment.location.end_ <= position)
             before_map
         in
         let after_opt =
           Map.find_last_opt
             (fun position -> comment.location.begin_ >= position)
             after_map
         in
         match (before_opt, after_opt) with
         | (Some (_, before), Some (_, after)) when before.depth <= after.depth
           ->
           after.comments := comment :: !(after.comments)
         | (Some (_, before), _) ->
           before.comments := comment :: !(before.comments)
         | (_, Some (_, after)) ->
           after.comments := comment :: !(after.comments)
         | (None, None) -> failwith "Unmatched comment");
  {before_map; after_map}

let has_comments_around position {before_map; after_map} =
  let {Value.comments = before; _} = Map.find position before_map in
  let {Value.comments = after; _} = Map.find position after_map in
  !before <> [] || !after <> []

let pop_comments_before position {before_map; _} =
  let {Value.comments; _} = Map.find position before_map in
  let list = !comments in
  comments := [];
  list

let pop_comment_after position {after_map; _} =
  let {Value.comments; _} = Map.find position after_map in
  match !comments with
  | [] -> None
  | comment :: rest ->
    comments := rest;
    Some comment

let pop_comments_after position {after_map; _} =
  let {Value.comments; _} = Map.find position after_map in
  let list = !comments in
  comments := [];
  list
