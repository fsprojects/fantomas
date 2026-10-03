[<Extension>]
type Funcs =
    [<Extension>]
    static member ToFunc(f: Action<_, _, _>) = Func<_, _, _, _>(fun a b c -> f.Invoke(a, b, c))
