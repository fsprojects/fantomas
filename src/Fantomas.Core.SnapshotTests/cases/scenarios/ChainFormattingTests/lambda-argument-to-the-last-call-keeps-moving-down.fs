(*---
max_line_length = 80
---*)
let mock () =
    Mock<IInstanceApi>()
        .Create()
        .Calls(fun (path: StepPath) (key: WellKnownStepMetadata) (value: string) -> metadata.Add(key, value))
