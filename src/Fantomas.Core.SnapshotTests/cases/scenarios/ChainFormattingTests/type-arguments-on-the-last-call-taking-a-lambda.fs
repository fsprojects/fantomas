(*---
max_line_length = 80
---*)
let mock () =
    Mock<IInstanceApi>()
        .Create()
        .Calls<StepPath * WellKnownStepMetadata>(fun (path: StepPath) (key: WellKnownStepMetadata) -> metadata.Add key)
