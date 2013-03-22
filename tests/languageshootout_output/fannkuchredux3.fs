/// The Computer Language Benchmarks Game
///
///   http://shootout.alioth.debian.org/
///
///   from Scala version by Otto Bommer, 13 Dec 2011
///
/// Modified by Vassil Keremidchiev by simplifying and parallelizing in F#, January 24, 2012
module Fannkuchredux

open System.Threading

let rec fac x = if x < 2
                then 1L
                else (int64 x) * fac(x - 1)

let F = [0..20]
        |> Seq.map fac
        |> Seq.toArray

type fannkuch(n) = 
  let p = Array.create n 0
  let pp = Array.create n 0
  let count = Array.create n 0
  let mutable flips = 0
  let mutable cksum = 0
  let rec direct idx i = 
    if i > 0
    then 
      let d = int(idx / F.[i])
      count.[i] <- d
      for j = 0 to d - 1 do
        pp.[j] <- p.[j]
      for j = 0 to i - d do
        p.[j] <- p.[j + d]
      for j = 0 to d - 1 do
        p.[j + i + 1 - d] <- pp.[j]
      direct (idx % F.[i]) (i - 1)
  let permute() = 
    let mutable first = p.[1]
    p.[1] <- p.[0]
    p.[0] <- first
    let mutable i = 1
    count.[i] <- count.[i] + 1
    while count.[i] > i do
      count.[i] <- 0
      i <- i + 1
      p.[0] <- p.[1]
      let next = p.[1]
      for j = 1 to i - 1 do
        p.[j] <- p.[j + 1]
      p.[i] <- first
      first <- next
      count.[i] <- count.[i] + 1
  let fcount() = 
    let mutable flips = 1
    let mutable first = p.[0]
    if p.[first] <> 0
    then 
      for i = 0 to n - 1 do
        pp.[i] <- p.[i]
      while pp.[first] <> 0 do
        flips <- flips + 1
        let mutable lo = 1
        let mutable hi = first - 1
        while lo < hi do
          let t = pp.[lo]
          pp.[lo] <- pp.[hi]
          pp.[hi] <- t
          lo <- lo + 1
          hi <- hi - 1
        let t = pp.[first]
        pp.[first] <- first
        first <- t
    flips
  member x.runTask(task, chunk) = 
    let lo = int64(task) * chunk
    let hi = min F.[n] (lo + chunk)
    for j = 0 to p.Length - 1 do
      p.[j] <- j
    direct lo (p.Length - 1)
    let last = int(hi - lo - 1L)
    for j = 0 to last do
      if p.[0] <> 0
      then 
        let f = fcount()
        flips <- max flips f
        cksum <- cksum + if (int64(j) + lo) % 2L = 0L
                         then f
                         else -f
      if j < last
      then permute()
    (cksum, flips)

let _ = 
  let nthreads = System.Environment.ProcessorCount
  let n = 
    try 
      int((System.Environment.GetCommandLineArgs()).[1])
    with
    | _ -> 7
  let split(i : int64) = (F.[n] + i - 1L) / i
  let chunk = split(int64(nthreads * 4))
  let ntasks = int(split chunk)
  let (c, fl) = 
    [0..ntasks]
    |> Seq.map(fun i -> 
      async { 
        let thread = fannkuch(n)
        return thread.runTask(i, chunk) })
    |> Async.Parallel
    |> Async.RunSynchronously
    |> Array.fold (fun (_cksum, _flips) (cksum, flips) -> (_cksum + cksum, max _flips flips)) (0, 0)
  Printf.printf "%d\nPfannkuchen(%d) = %d\n" c n fl
