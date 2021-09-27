module Map = CCMap.Make (Int)

type t = int Map.t

let init ~empty_spaces =
  empty_spaces
  |> CCList.to_seq
  |> Seq.map (fun {Luast__ast.Empty_space.first_line; line_count} ->
         (first_line, line_count))
  |> Map.of_seq

let empty_lines ~line map = map |> Map.find_opt line |> CCOpt.get_or ~default:0
