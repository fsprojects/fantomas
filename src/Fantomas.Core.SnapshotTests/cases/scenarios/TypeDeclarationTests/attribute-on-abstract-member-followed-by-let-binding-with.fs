[<AllowNullLiteral>]
type SubGroupStackOptions =
    [<Emit "$0[$1]{{=$2}}">]
    abstract Item: name:string -> bool with get, set

[<AllowNullLiteral>]
let foo bar = zero
