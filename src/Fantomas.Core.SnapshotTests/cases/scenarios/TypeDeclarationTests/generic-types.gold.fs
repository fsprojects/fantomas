type public MyClass<'a> public (x, y) as this =
    static let PI = 3.14
    static do printfn "static constructor"
    let mutable z = x + y

    do
        printfn "%s" (this.ToString())
        printfn "more constructor effects"

    internal new(a) = MyClass(a, a)
    static member StaticProp = PI
    static member StaticMethod a = a + 1
    member internal self.Prop1 = x

    member self.Prop2
        with get () = z
        and set (a) = z <- a

    member self.Method(a, b) = x + y + z + a + b
