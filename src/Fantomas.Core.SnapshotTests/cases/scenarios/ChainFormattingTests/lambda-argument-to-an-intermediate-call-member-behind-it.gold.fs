let mock () =
    Mock<IInstanceApi>()
        .Calls(
            fun (path: StepPath) (key: WellKnownStepMetadata) (value: string) ->
                metadata.Add(key, value))
        .Value
