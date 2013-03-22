///  The Computer Language Benchmarks Game
///
///    http://shootout.alioth.debian.org/
///
///    contributed by Jomo Fisher
///    modified by Kostas Rontogiannis
///
///    Using an array of Async<unit> for the workers and
///    a shared token between threads.
///
///    Compile : 
///        fsc --tailcalls+ -O --platform:x64 ThreadRingNew.fs -o ThreadRingNew.exe
///    Execute :
///        mono ThreadRingNew.exe 50000000
module Threadring

let NumberOfThreads = 503

let mutable (workers : Async<unit>[]) = null

let mutable token = -1

let createWorker i = 
  let next = (i + 1) % NumberOfThreads
  async { 
    if token = 0
    then 
      printfn "%d" (i + 1)
      exit 0
    else 
      token <- token - 1
      return! workers.[next] }

[<EntryPoint>]
let main args = 
  token <- int args.[0]
  workers <- Array.init NumberOfThreads createWorker
  Async.StartImmediate(workers.[0])
  0
