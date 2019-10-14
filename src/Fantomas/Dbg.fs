namespace global

#if DEBUG

module Dbg =

    open System
    open System.Text
    
    let write fmt =
        let sb = StringBuilder()
        let mutable n = 0
        let prefix() =
            sb.Append("DEBUG ")
              .Append(DateTime.Now.ToString("dd MMM HH:mm:ss.fffffff"))
              .Append("> ") |> ignore
            n <- n + 1
        prefix()
        Printf.kbprintf (fun () ->
            
            let old = Console.ForegroundColor
            try
                Console.ForegroundColor <- ConsoleColor.Red
                if n<>1 then sb.Append(" (").Append(n).Append(")") |> ignore
                sb.ToString() |> Console.Error.WriteLine
                sb.Clear() |> ignore
                prefix()
            finally
                Console.ForegroundColor <- old
        ) sb fmt
    
    let seq fn = Seq.iter fn

    let iff condition fn = if condition() then fn()

    let tee fn a =
        fn a
        a

    let teeSeq fn s =
        let s = Seq.cache s
        fn s
        s
    
    let fun1 fn (f:'a->'b) a =
        let b = f a
        fn a b
        b

    let fun2 fn (f:'a->'b->'c) a b =
        let c = f a b
        fn a b c
        c

    let fun3 fn (f:'a->'b->'c->'d) a b c =
        let d = f a b c
        fn a b c d
        d

    let mutable private countMap = Map.empty
    let count (key:string) =
        match Map.tryFind key countMap with
        | Some i ->
            incr i
            !i
        | None ->
            countMap <- Map.add key (ref 1) countMap
            1

    let mutable private functionMap = Map.empty
    let addFun (key:string) (fn:unit->unit) = functionMap <- Map.add key fn functionMap
    let runFun (key:string) = Map.find key functionMap ()
    
    let teePrint x = tee (printfn "%A") x
    let print x = printfn "%A" x
#else
module Dbg =
    let teePrint x = x
    let print _ = ()
#endif