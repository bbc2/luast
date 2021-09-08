let extract_name file =
  file |> Str.full_split (Str.regexp {|\.lua$|}) |> function
  | [Str.Text name; Delim _] -> Some name
  | _ -> None

let gen_case name =
  Printf.sprintf
    {|(rule
 (with-stdout-to %s.out (run %%{bin:luafmt} %%{dep:cases/%s.lua})))

(rule
 (alias runtest)
 (action (diff cases/%s.exp %s.out)))|}
    name name name name

let () =
  Sys.readdir "cases"
  |> Array.to_seq
  |> Seq.filter_map extract_name
  |> Seq.map gen_case
  |> List.of_seq
  |> String.concat "\n\n"
  |> print_string
