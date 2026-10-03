[<AllowNullLiteral>]
type SubGroupStackOptions =
    [<Emit "$0[$1]{{=$2}}">]
    abstract Item: name:string -> bool with get, set

[<AllowNullLiteral>]
type DataGroup =
    abstract className: string option with get, set
