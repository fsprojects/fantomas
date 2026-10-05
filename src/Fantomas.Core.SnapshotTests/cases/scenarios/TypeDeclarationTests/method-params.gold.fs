type Test() =
    member this.Function1<'a>(x, y) = printfn "%A, %A" x y

    abstract AbstractMethod<'a, 'b> : 'a * 'b -> unit
    override this.AbstractMethod<'a, 'b>(x: 'a, y: 'b) = printfn "%A, %A" x y
