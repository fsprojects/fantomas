/// The Computer Language Benchmarks Game
/// http://shootout.alioth.debian.org/
///
/// Based on C# version by Isaac Gouy, The Anh Tran, Alan McGovern
/// Contributed by Don Syme
module Spectralnorm

open System

open System.Threading

type BarrierHandle(threads : int) = 
  let mutable current = threads
  let mutable handle = new ManualResetEvent(false)
  member x.WaitOne() = 
    let h = handle
    if Interlocked.Decrement(&current) > 0
    then h.WaitOne()
         |> ignore
    else 
      handle <- new ManualResetEvent(false)
      Interlocked.Exchange(&current, threads)
      |> ignore
      h.Set()
      |> ignore
      h.Close()

let Approximate(u : double[], v : double[], tmp : double[], rbegin, rend, barrier : BarrierHandle) = 
  let mutable vBv = 0.0
  let mutable vv = 0.0
  let A i j = 1.0 / float((i + j) * (i + j + 1) / 2 + i + 1)
  let multiplyAv(v : double[], Av : double[]) = 
    for i = rbegin to rend - 1 do
      let mutable sum = 0.0
      for j = 0 to v.Length - 1 do
        sum <- sum + A i j * v.[j]
      Av.[i] <- sum
  let multiplyAtv(v : double[], atv : double[]) = 
    for i = rbegin to rend - 1 do
      let mutable sum = 0.0
      for j = 0 to v.Length - 1 do
        sum <- sum + A j i * v.[j]
      atv.[i] <- sum
  let multiplyatAv(v : double[], tmp : double[], atAv : double[]) = 
    multiplyAv(v, tmp)
    barrier.WaitOne()
    multiplyAtv(tmp, atAv)
    barrier.WaitOne()
  for i = 0 to 9 do
    multiplyatAv(u, tmp, v)
    multiplyatAv(v, tmp, u)
  for i = rbegin to rend - 1 do
    vBv <- vBv + u.[i] * v.[i]
    vv <- vv + v.[i] * v.[i]
  (vBv, vv)

let RunGame n = 
  let u = Array.create n 1.0
  let tmp = Array.zeroCreate n
  let v = Array.zeroCreate n
  let nthread = Environment.ProcessorCount
  let barrier = new BarrierHandle(nthread)
  let chunk = n / nthread
  let aps = 
    Async.Parallel [for i in 0..nthread - 1 do
                      let r1 = i * chunk
                      let r2 = if (i < (nthread - 1))
                               then r1 + chunk
                               else n
                      yield async { return Approximate(u, v, tmp, r1, r2, barrier) }]
    |> Async.RunSynchronously
  let vBv = aps
            |> Array.sumBy fst
  let vv = aps
           |> Array.sumBy snd
  Math.Sqrt(vBv / vv)

[<EntryPoint>]
let main args = 
  let n = 
    try 
      int <| args.[0]
    with
    | _ -> 2500
  System.Console.WriteLine("{0:f9}", RunGame n)
  0
