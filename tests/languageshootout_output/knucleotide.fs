/// knucleotide.fs
///
/// The Computer Language Benchmarks Game
/// http://shootout.alioth.debian.org/
///
/// contributed by Jimmy Tang
module Knucleotide

open System
open System.IO
open System.Collections.Generic

/// make our hashtable using System.Collections.Generic.Dictionary
let maketable (dna : string) (length : int) = 
  let d = new Dictionary<_, _>()
  for start in 0..(dna.Length - length) do
    let substr = dna.Substring(start, length)
    let x = ref (ref 0)
    if d.TryGetValue(substr, x) then x.Value := ! !x + 1
    else d.[substr] <- ref 1
  d

/// frequency for all substrings of a given length
let frequencies (dna : string) (length : int) = 
  [let d = maketable dna length
   let total = 
     d.Values
     |> Seq.map (!)
     |> Seq.sum
   yield! [for pair in d -> 
             pair.Key.ToUpper(), 
             (float (pair.Value.Value) * 100.0 / float (total))]
          |> List.sortBy (snd >> (~-))
          |> List.map (fun (s, c) -> sprintf "%s %.3f" s c)
   yield ""]

// frequency of occurrence for a particular substring
let countSubstring dna (substring : string) = 
  [let d = maketable dna (substring.Length)
   yield (sprintf "%d\t%s" (if d.ContainsKey(substring) then !d.[substring]
                            else 0) (substring.ToUpper()))]

let input = Console.In

let dna = 
  seq { 
    while true do
      yield input.ReadLine() }
  |> Seq.takeWhile (fun x -> x <> null)
  |> Seq.skipWhile (fun x -> not (x.StartsWith(">THREE")))
  |> Seq.skip 1
  |> String.concat ""

[for len in [1; 2] -> async { return frequencies dna len }] 
@ [for str in ["ggt"; "ggta"; "ggtatt"; "ggtattttaatt"; "ggtattttaatttatagt"] -> 
     async { return countSubstring dna str }]
|> List.rev
|> Async.Parallel
|> Async.RunSynchronously
|> Array.rev
|> Seq.concat
|> Seq.iter (printfn "%s")
