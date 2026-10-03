builder
    .Configure(fun v ->
        handleSomeValue v |> andThenSomethingElse v
    )
    .Build()
    .Result
