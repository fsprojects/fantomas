/// The Computer Language Benchmarks Game
/// http://shootout.alioth.debian.org/
///
/// Contributed by Valentin Kraevskiy
module Fasta

let im = 139968

let ia = 3877

let ic = 29573

let mutable seed = 42

let inline random max = 
  seed <- (seed * ia + ic) % im
  max * float seed / float im

let alu = "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG\
     GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA\
     CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT\
     ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA\
     GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG\
     AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC\
     AGCCTGGGCGACAGAGCGAGACTCCGTCTCAA

let makeCumulative = List.fold(fun (cp, res) -> fun (c, p) -> (cp + p, ::((c, cp + p), res))) (0.0, []) >> snd >> List.toArray

let homoSapiens = 
  makeCumulative [(97uy, 0.3029549427)
   (99uy, 0.1979883005)
   (103uy, 0.1975473066)
   (116uy, 0.3015094502)]

let iub = 
  makeCumulative [(97uy, 0.27)
   (99uy, 0.12)
   (103uy, 0.12)
   (116uy, 0.27)
   (66uy, 0.02)
   (68uy, 0.02)
   (72uy, 0.02)
   (75uy, 0.02)
   (77uy, 0.02)
   (78uy, 0.02)
   (82uy, 0.02)
   (83uy, 0.02)
   (86uy, 0.02)
   (87uy, 0.02)
   (89uy, 0.02)]

let inline selectRandom(f : _[]) = 
  let r = random 1.0
  let rec find = 
    function 
    | 0 -> fst f.[0]
    | n when r < snd f.[n] -> fst f.[n]
    | n -> find(n - 1)
  find <| f.Length - 1

let width = 60

let stream = System.Console.OpenStandardOutput()

let buffer = Array.create 1024 0uy

let mutable index = 0

let inline flush() = 
  stream.Write(buffer, 0, index)
  index <- 0

let inline write b = 
  buffer.[index] <- b
  index <- index + 1
  if index = buffer.Length then flush()

let randomFasta desc table n = 
  Array.iter write desc
  for i in 1..n do
    write <| selectRandom table
    if i % width = 0 then write 10uy
  if n % width <> 0 then write 10uy

let repeatFasta desc (table : byte[]) n = 
  Array.iter write desc
  for i in 1..n do
    write <| table.[(i - 1) % table.Length]
    if i % width = 0 then write 10uy
  if n % width <> 0 then write 10uy

[<EntryPoint>]
let main args = 
  let n = 
    try 
      int args.[0]
    with
    | _ -> 1000
  repeatFasta ">ONE Homo sapiens alu\n"B alu (2 * n)
  randomFasta ">TWO IUB ambiguity codes\n"B iub (3 * n)
  randomFasta ">THREE Homo sapiens frequency\n"B homoSapiens (5 * n)
  flush()
  0
