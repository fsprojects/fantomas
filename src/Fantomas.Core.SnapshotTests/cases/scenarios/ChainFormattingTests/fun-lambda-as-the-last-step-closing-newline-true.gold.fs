builder
    .Build()
    .Configure(fun v ->
        handleSomeValue v |> andThenSomethingElse v
    )
