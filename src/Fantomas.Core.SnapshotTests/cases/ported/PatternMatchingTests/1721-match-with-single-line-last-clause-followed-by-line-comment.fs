(*---
indent_size = 2
---*)
  let select p =
    match p with
    | voo _ -> "v_"
    | dd -> "dd_"
    | q -> "q_" // comment
    |> List.singleton
    |> instruction "s"
