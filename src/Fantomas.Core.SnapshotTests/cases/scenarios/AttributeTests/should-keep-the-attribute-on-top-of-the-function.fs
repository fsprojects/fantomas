(*---
fsharp_max_function_binding_width = 120
---*)
[<Extension>]
type Funcs =
    [<Extension>]
    static member ToFunc (f: Action<_,_,_>) =
        Func<_,_,_,_>(fun a b c -> f.Invoke(a,b,c))
    