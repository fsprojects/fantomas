///  The Computer Language Benchmarks Game
///    http://shootout.alioth.debian.org/
///
///    Contributed by Dmitry Lomov & Jomo Fisher
///
///    Uses F# asyncs (lightweight threads) with customized auto reset cell 
///    as semaphore.

let ringLength = 503

type AutoResetCell() =
    let mutable value = -1
    let mutable run = None
    
    member this.RegisterResult res =
        let grabbed = 
            lock this (fun () ->
                match run with
                | None -> value <- res; None
                | grabbed -> run <- None; grabbed)
        match grabbed with
        | None -> ()
        | Some run -> run res

    member this.AsyncResult = 
        Async.FromContinuations(fun (success,_,_) -> 
            let runNow = 
                lock this (fun () ->
                    if value = -1 then
                        run <- Some success
                        false                        
                    else true)                        
            if runNow then 
                let r = value
                value <- -1 // Autoreset
                success r) 

let createCell _ = AutoResetCell()

let createThread (cells:AutoResetCell array) i =
    let next = if i = ringLength-1 then 0 else i + 1
    async {
            let more = ref true
            while !more do
                let! msg = cells.[i].AsyncResult 
                cells.[next].RegisterResult(msg-1)           
                more := msg>0
                if msg = 0 then                    
                    printfn "%d" (i+1) }

[<EntryPoint>]
let main args = 
    let count = if args.Length>0 then int args.[0] else 50000000
    
    let cells = Array.init ringLength createCell

    let threads = Array.init ringLength (createThread cells)

    cells.[0].RegisterResult(count) 

    threads
        |> Async.Parallel // Run all the asyncs at once
        |> Async.Ignore // Ignore the results
        |> Async.RunSynchronously // Block the main thread until work is done
    
    0
