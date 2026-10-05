[<AllowNullLiteral>]
type TimelineOptionsGroupCallbackFunction =
    [<Emit "$0($1...)">]
    abstract Invoke: group: TimelineGroup * callback: (TimelineGroup option -> unit) -> unit

let myBinding a = 7
