(*---
max_line_length = 80
fsharp_multi_line_lambda_closing_newline = true
---*)
let mock () =
    Mock<IInstanceApi>()
        .Create()
        .Calls(fun (path: StepPath) (key: WellKnownStepMetadata) (value: string) -> metadata.Add(key, value))
