(*---
fsharp_multi_line_lambda_closing_newline = true
fsharp_experimental_keep_indent_in_branch = true
---*)
  let nextModel, objectsRemoved =
    List.fold
      (fun acc item ->
        match entityInCurrentModel with
        | None ->
          // look it's a tuple but wrapped in parenthesis
          (nextModel, objectsRemoved)
        | Some subjectToRemove ->
        let a = 5
        let b = 6
        someFunctionApp a b |> ignore
        acc)
      state
      []
