let instanceMetadata =
            Mock<IInstanceApi>()
                .Calls<StepPath * AttemptNumber * JobNumber * DateTime option * WellKnownStepMetadata * string>(fun
                                                                                                                    (path,
                                                                                                                     _,
                                                                                                                     _,
                                                                                                                     _,
                                                                                                                     key,
                                                                                                                     value) ->
                    path |> shouldEqual (StepPath.Parse "/Foo")
                    metadata.Add (key, value)
                )
                .Create ()
