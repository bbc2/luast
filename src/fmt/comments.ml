module Map = CCMap.Make (Luast__ast.Position)

type t = Luast__ast.Comment.t list ref Map.t

let init ~code_locations ~comments =
  let map =
    code_locations
    |> List.to_seq
    |> Seq.map (fun {Luast__ast.Location.begin_ = _; end_} -> (end_, ref []))
    |> Map.of_seq
  in
  (* Thanks to the binary tree backing the map, finding the right position is logarithmic
     in complexity, which means we probably achieve O(n * log(n)) for this function. *)
  comments
  |> CCList.iter (fun (comment : Luast__ast.Comment.t) ->
         let (_, comments) =
           Map.find_last
             (fun position -> comment.location.begin_ >= position)
             map
         in
         comments := comment :: !comments);
  map

let pop_comments_after position map =
  let list = Map.find position map in
  let comments = !list in
  list := [];
  comments |> CCList.sort Luast__ast.Comment.compare_begin_positions
