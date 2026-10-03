(*---
fsharp_multiline_bracket_style = cramped
---*)
// ts2fable 0.8.0
module rec Xterm

type [<AllowNullLiteral>] Terminal =
    abstract onKey: IEvent<{| key: string; domEvent: KeyboardEvent |}> with get, set
    abstract onLineFeed: IEvent<unit> with get, set
