open System

type MyClass(a: int, b: int) =

    member val PropA = a with get, set
    member val PropB = b with get, set

    [<Obsolete("Do not use")>]
    new(x: int) = MyClass(x, x)
