module ``member``

let ``abstract`` = "abstract"

type SomeType() =
    member this.``new``() = System.Console.WriteLine("Hello World!")
