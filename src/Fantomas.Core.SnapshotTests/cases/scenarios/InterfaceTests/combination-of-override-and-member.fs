(*---
max_line_length = 119
fsharp_max_if_then_else_short_width = 80
fsharp_max_function_binding_width = 120
---*)
type LogInterface =
    abstract member Print: string -> unit
    abstract member GetLogFile: string -> string
    abstract member Info: unit -> unit
    abstract member Version: unit -> unit

type MyLogInteface() =
    interface LogInterface with
        member x.Print msg = printfn "%s" msg
        override x.GetLogFile environment =
            if environment = "DEV" then
                "dev.log"
            else
                sprintf "date-%s.log" environment
        member x.Info () = ()
        override x.Version () = ()