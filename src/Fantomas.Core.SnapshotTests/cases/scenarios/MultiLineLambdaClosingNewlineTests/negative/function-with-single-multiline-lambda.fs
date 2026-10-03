(*---
fsharp_max_infix_operator_expression = 35
fsharp_multi_line_lambda_closing_newline = true
---*)
List.collect (fun (a, element) ->
    let path' =
        path
        |> someFunctionToCalculateThing

    innerFunc<'a, 'b>
        path'
        elementNameThatHasThisRatherLongVariableNameToForceTheWholeThingOnMultipleLines
        (foo >> bar value >> List.item a)
        shape
)
