[<AllowNullLiteral>]
type TimelineOptionsGroupCallbackFunction =
    [<Emit "$0($1...)">]
    abstract Invoke: group: TimelineGroup * callback: (TimelineGroup option -> unit) -> unit

type TimelineOptionsGroupEditableType = U2<bool, TimelineGroupEditableOption>
