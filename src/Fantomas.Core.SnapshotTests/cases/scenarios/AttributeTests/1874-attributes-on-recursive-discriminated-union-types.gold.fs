module test

open System.Diagnostics

type Correct =
    | A of unit

    [<DebuggerStepThrough>]
    override this.ToString() = ""

    [<DebuggerStepThrough>]
    member this.f = ()

    [<DebuggerStepThrough>]
    static member this.f = ()

and Wrong =
    | B of unit

    [<DebuggerStepThrough>]
    override this.ToString() = ""

    [<DebuggerStepThrough>]
    member this.f = ()

    [<DebuggerStepThrough>]
    static member this.f = ()
