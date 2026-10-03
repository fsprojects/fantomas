(*---
fsharp_multi_line_lambda_closing_newline = true
---*)
module Foo =

    let blah =
        it
        |> List.iter (fun (_, output) ->
            thing
            |> Map.iter (fun key value ->
                match value with
                | Ok (TestResult.Failure f) -> failwith ""
                | Error e -> failwith ""
                | _ -> () // hi!
            )
        )
