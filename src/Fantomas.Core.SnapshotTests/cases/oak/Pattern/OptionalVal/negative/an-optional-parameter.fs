type Greeter() =
    member _.Greet(?name) = defaultArg name "world"
