namespace global

#if DEBUG

module Dbg =

    open System
    open System.Text

    let seq fn = Seq.iter fn

    let iff condition fn =
        if condition () then
            fn ()

    let tee fn a =
        fn a
        a

    let teePrint x = tee (printfn "%A") x
    let print x = printfn "%A" x
#else
#endif
