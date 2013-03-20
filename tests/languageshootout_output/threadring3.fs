///  The Computer Language Benchmarks Game
///    http://shootout.alioth.debian.org/
///
///    Contributed by Jomo Fisher
///
///    Uses F# asyncs. Asyncs are triggered by tailcall in sequence as progress is 
///    made around the ring. 
module Threadring

let ringLength = 503

let cells = Array.zeroCreate ringLength

let threads = Array.zeroCreate ringLength

let answer = ref -1

let createWorker i = 
  let next = (i + 1) % ringLength
  async { 
    let value = cells.[i]
    if false then ()
    match value with
    | 0 -> answer := i + 1
    | _ -> 
      cells.[next] <- value - 1
      return! threads.[next] }

[<EntryPoint>]
let main args = 
  cells.[0] <- if args.Length > 0 then int args.[0] else 50000000
  for i in 0..ringLength - 1 do
    threads.[i] <- createWorker i
  let result = Async.StartImmediate(threads.[0])
  printfn "%d" ! answer
  0
