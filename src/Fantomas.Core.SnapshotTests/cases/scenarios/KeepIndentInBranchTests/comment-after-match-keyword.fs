(*---
fsharp_max_infix_operator_expression = 50
fsharp_experimental_keep_indent_in_branch = true
---*)
match  // comment
    Stream.peel rest with
| None -> failwith "oh no"
| Some longName ->

longName
|> Map.map (fun _ -> TypedTerm.force<int>)
