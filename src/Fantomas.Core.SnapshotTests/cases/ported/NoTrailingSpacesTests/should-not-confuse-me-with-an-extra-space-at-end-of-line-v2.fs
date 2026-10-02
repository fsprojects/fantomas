(*---
fsharp_max_infix_operator_expression = 90
fsharp_max_array_or_list_width = 40
fsharp_multiline_bracket_style = cramped
---*)
let ``should not extrude without positive distance`` () =
    let args = [| "-i"; "input.dxf"; "-o"; "output.pdf"; "--op"; "extrude"; |]
    (fun () -> parseCmdLine args |> ignore)
    |> should throw typeof<Argu.ArguParseException>