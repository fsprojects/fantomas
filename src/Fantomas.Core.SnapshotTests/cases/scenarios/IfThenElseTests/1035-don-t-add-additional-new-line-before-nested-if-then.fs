(*---
fsharp_max_infix_operator_expression = 50
fsharp_max_value_binding_width = 50
fsharp_max_function_binding_width = 50
---*)
            if ast.ParseHadErrors then
                let errors =
                    ast.Errors
                    |> Array.filter (fun e -> e.Severity = FSharpErrorSeverity.Error)

                if not <| Array.isEmpty errors
                then log.LogError(sprintf "Parsing failed with errors: %A\nAnd options: %A" errors checkOptions)

                return Error ast.Errors
            else
                match ast.ParseTree with
                | Some tree -> return Result.Ok tree
                | _ -> return Error Array.empty // Not sure this branch can be reached.
