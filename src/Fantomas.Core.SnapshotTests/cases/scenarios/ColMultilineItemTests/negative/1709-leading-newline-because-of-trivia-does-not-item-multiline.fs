[<Fact>]
let ``first lamba`` () =
    // You can use the `fun` keyword to write lambda's.
    // Mind the -> instead of C#'s =>
    // TODO: complete the lambda so that the value is returned in uppercase.
    let toUpperCase = fun a -> a
    // ref: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/functions/lambda-expressions-the-fun-keyword

    let name = "Joey"
    let uppercased = toUpperCase name
    Assert.Equal("JOEY", uppercased)
