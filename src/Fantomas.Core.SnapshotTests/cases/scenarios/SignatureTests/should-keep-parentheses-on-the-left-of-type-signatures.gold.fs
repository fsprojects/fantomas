type IA =
    abstract F: (unit -> Option<'T>) -> Option<'T>

type A() =
    interface IA with
        member x.F(f: unit -> _) = f ()
