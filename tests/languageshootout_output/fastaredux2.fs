///  The Computer Language Benchmarks Game
///    http://shootout.alioth.debian.org/
///
///    contributed by Jimmy Tang
module Fastaredux

open System
open System.IO

let IM, IA, IC = 139968, 3877, 29573
let cols, LUTLEN = 60, 1 <<< 9
let mutable s = 42
let alu = "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG\
    GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA\
    CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT\
    ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA\
    GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG\
    AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC\
    AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"B
let iubvalues = [ 0.27; 0.12; 0.12; 0.27 ] @ List.replicate 12 0.02
let iub = Seq.zip "acgtBDHKMNRSVWY"B iubvalues

let homosapien = 
  [ 'a'B, 0.3029549426680
    'c'B, 0.1979883004921
    'g'B, 0.1975473066391
    't'B, 0.3015094502008 ]

let os = new BufferedStream(Console.OpenStandardOutput(), 1 <<< 16)

let repeatFasta alu n = 
  let r = Array.length alu
  let s = Array.concat [ alu; alu ]
  for j in 0..cols..n - cols do
    os.Write(s, j % r, cols)
    os.WriteByte('\n'B)
  os.Write(s, (n / cols * cols) % r, n % cols)
  if n % cols <> 0 then os.WriteByte('\n'B)
  os.Flush()

let randomFasta src n = 
  /// cumulative probability for each nucleotide
  let cumuArray = 
    let f (a, c, d) (x, y) = (x, c + y, d + 1)
    src
    |> Seq.scan f (0uy, 0.0, 0)
    |> Seq.toArray
  
  /// lookup table optimization
  let lut = 
    let arr = Array.zeroCreate LUTLEN
    let mutable j = 0
    for key, cum, i in cumuArray do
      let v = j
      while j <= int (float (LUTLEN - 1) * cum) do
        arr.[j] <- key, i
        j <- j + 1
      if j <> v then arr.[j - 1] <- 0uy, i
    arr
  
  let lookup x = 
    match lut.[x * (LUTLEN - 1) / IM] with
    | 0uy, p -> 
      let a, b, c = cumuArray.[p]
      let i, j, k = cumuArray.[p + 1]
      if float (x) / float (IM) < b then a
      else i
    | c, p -> c
  
  /// write output one line at a time
  let buf = Array.zeroCreate (cols + 1)
  
  for x in n..(-cols)..1 do
    let e = 
      if x < cols then x
      else cols
    buf.[e] <- '\n'B
    for y in 0..e - 1 do
      s <- (s * IA + IC) % IM
      buf.[y] <- lookup s
    os.Write(buf, 0, e + 1)
  os.Flush()

[<EntryPoint>]
let main args = 
  let n = 
    try 
      int args.[0]
    with _ -> 2500000
  printfn ">ONE Homo sapiens alu"
  repeatFasta alu (2 * n)
  printfn ">TWO IUB ambiguity codes"
  randomFasta iub (3 * n)
  printfn ">THREE Homo sapiens frequency"
  randomFasta homosapien (5 * n)
  0
