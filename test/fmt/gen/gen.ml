let extract_name file =
  file |> Str.full_split (Str.regexp {|\.in\.lua$|}) |> function
  | [Str.Text name; Delim _] -> Some name
  | _ -> None

let gen_case name =
  Printf.sprintf
    {|(rule
 (with-stdout-to %s.out.lua (run %%{bin:luafmt} %%{dep:cases/%s.in.lua})))

(rule
 (alias runtest)
 (action (diff cases/%s.exp.lua %s.out.lua)))|}
    name name name name

let () =
  let cases = Sys.readdir "cases" in
  Array.sort String.compare cases;
  cases
  |> Array.to_seq
  |> Seq.filter_map extract_name
  |> Seq.map gen_case
  |> List.of_seq
  |> String.concat "\n\n"
  |> print_string
