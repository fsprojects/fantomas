type Cont = SuccessCont

[<Struct>]
type ContStackFrame =
    val Cont: Cont
    new cont = { Cont = cont }
