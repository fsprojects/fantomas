(*---
indent_size = 2
---*)
  let select px =
    match px with
    | Shared.Foo _ -> "foo"
    | Shared.LongerFoobarFoo -> "lf"
    | Shared.Barry ->
        match () with
        | _ -> "meh"
    |> List.singleton
    |> instr "ziggy"
