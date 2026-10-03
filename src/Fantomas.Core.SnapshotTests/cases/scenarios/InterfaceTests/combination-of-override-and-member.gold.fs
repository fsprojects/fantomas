type LogInterface =
    abstract member Print: string -> unit
    abstract member GetLogFile: string -> string
    abstract member Info: unit -> unit
    abstract member Version: unit -> unit

type MyLogInteface() =
    interface LogInterface with
        member x.Print msg = printfn "%s" msg

        override x.GetLogFile environment =
            if environment = "DEV" then "dev.log" else sprintf "date-%s.log" environment

        member x.Info() = ()
        override x.Version() = ()
