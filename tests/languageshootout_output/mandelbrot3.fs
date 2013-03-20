/// The Computer Language Benchmarks Game
/// http://shootout.alioth.debian.org/
///
/// Adapted by Antti Lankila from the earlier Isaac Gouy's implementation
/// Add multithread & tweaks from C++ by The Anh Tran
/// Translate to F# by Jomo Fisher
module Mandelbrot

open System
open System.Threading
open System.IO
let mutable N = 200

let mutable width_bytes = 0

let mutable data : byte array array = null

let mutable nbyte_each_line : int array = null

let current_line = ref -1

let Calculate() = 
  let inverse_n = 2.0 / float N
  let mutable y = Interlocked.Increment(&current_line.contents)
  while y < N do
    let pdata = data.[y]
    let mutable byte_count = 0
    let mutable bit_num = 0
    let mutable byte_acc = 0
    let Civ = float y * inverse_n - 1.0
    for x in 0..N - 1 do
      let Crv = float x * inverse_n - 1.5
      let mutable Zrv = Crv
      let mutable Ziv = Civ
      let mutable Trv = Crv * Crv
      let mutable Tiv = Civ * Civ
      let mutable i = 49
      let mutable more = true
      while more do
        Ziv <- (Zrv * Ziv) + (Zrv * Ziv) + Civ
        Zrv <- Trv - Tiv + Crv
        Trv <- Zrv * Zrv
        Tiv <- Ziv * Ziv
        more <- (Trv + Tiv) <= 4.0
        if more then 
          i <- i - 1
          more <- i > 0
      byte_acc <- byte_acc <<< 1
      byte_acc <- byte_acc ||| (if i = 0 then 1 else 0)
      bit_num <- bit_num + 1
      if bit_num = 8 then 
        pdata.[byte_count] <- byte byte_acc
        byte_count <- byte_count + 1
        bit_num <- 0
        byte_acc <- 0
    if bit_num <> 0 then 
      byte_acc <- byte_acc <<< (8 - (N &&& 7))
      pdata.[byte_count] <- byte byte_acc
      byte_count <- byte_count + 1
    nbyte_each_line.[y] <- byte_count
    y <- Interlocked.Increment(&current_line.contents)

[<EntryPoint>]
let main args = 
  if args.Length > 0 then N <- int args.[0]
  Console.Out.WriteLine("P4\n{0} {0}", N)
  width_bytes <- N / 8
  if width_bytes * 8 < N then width_bytes <- width_bytes + 1
  nbyte_each_line <- Array.zeroCreate N
  data <- Array.zeroCreate N
  for i in 0..N - 1 do
    data.[i] <- Array.zeroCreate width_bytes
  let threads = Array.init(Environment.ProcessorCount - 1) (fun (i) -> new Thread(Calculate))
  for thread in threads do
    thread.Start()
  Calculate()
  for thread in threads do
    thread.Join()
  let s = Console.OpenStandardOutput()
  for y in 0..N - 1 do
    s.Write(data.[y], 0, nbyte_each_line.[y])
  0
