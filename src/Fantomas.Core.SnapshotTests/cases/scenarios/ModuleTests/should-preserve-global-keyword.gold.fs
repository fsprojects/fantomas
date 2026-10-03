namespace global

type SomeType() =
    member this.Print() = global.System.Console.WriteLine("Hello World!")
