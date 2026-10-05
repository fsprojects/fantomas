(*---
fsharp_max_infix_operator_expression = 50
---*)
let f () =
  async {
    let x = 2
    return! some rather long |> stuff that |> uses piping |> to' demonstrate |> the issue
  }
