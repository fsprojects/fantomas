/// The Computer Language Benchmarks Game
/// http://shootout.alioth.debian.org/
///
/// Modified version of Valentin Kraevskiy
/// Contributed by Vassil Keremidchiev
module Regexdna

open System.Text.RegularExpressions

open System.Threading

let regex s = Regex(s, RegexOptions.Compiled)

let input = stdin.ReadToEnd()

let withoutComments = (regex ">.*\n").Replace(input, "")

let text = (regex "\n").Replace(withoutComments, "")

let textSize = text.Length

let blockSize = textSize / 2

let onblocks overlapSize blockSize = 
  let rec onblocks' res = 
    function 
    | "" -> res
    | s when s.Length <= blockSize -> res @ [s]
    | s -> onblocks' (res @ [s.Substring(0, blockSize)]) (s.Substring(blockSize - overlapSize))
  onblocks' []

let onProcBlocks = onblocks 0 ((textSize / System.Environment.ProcessorCount) + 1)

let DNAcodes = 
  ["agggtaaa|tttaccct"; "[cgt]gggtaaa|tttaccc[acg]"; "a[act]ggtaaa|tttacc[agt]t"; "ag[act]gtaaa|tttac[agt]ct"; "agg[act]taaa|ttta[agt]cct"; "aggg[acg]aaa|ttt[cgt]ccct"; "agggt[cgt]aa|tt[acg]accct"; "agggta[cgt]a|t[acg]taccct"; "agggtaa[cgt]|[acg]ttaccct"]

/// Calculate all chunks in parallel
let chunksCounts = 
  let chunkedMatch(matchStr : string) = text |> onblocks (matchStr.Length - 1) blockSize |> List.map(fun (t) -> async { return (matchStr, ((regex matchStr).Matches t).Count) })
  DNAcodes |> List.collect chunkedMatch |> Async.Parallel |> Async.RunSynchronously

DNAcodes |> List.map(fun (key) -> 
  (key, chunksCounts |> Array.fold (fun (S) -> 
    fun (k, cnt) -> 
      match S with
      | S -> if k = key then S + cnt else S) 0)) |> List.iter(fun (key, cnt) -> printfn "%s %i" key cnt)
/// Gather result counts by summing them per DNA code
let lengthAfterReplace text = 
  [("B", "(c|g|t)");
   ("D", "(a|g|t)");
   ("H", "(a|c|t)");
   ("K", "(g|t)");
   ("M", "(a|c)");
   ("N", "(a|c|g|t)");
   ("R", "(a|g)");
   ("S", "(c|g)");
   ("V", "(a|c|g)");
   ("W", "(a|t)");
   ("Y", "(c|t)")] |> List.fold (fun (s) -> fun (code, alt) -> (regex code).Replace(s, alt)) text |> String.length

let replacedSize = text |> onProcBlocks |> List.map(fun (chunk) -> async { return lengthAfterReplace chunk }) |> Async.Parallel |> Async.RunSynchronously |> Array.sum

printf "\n%i\n%i\n%i\n" input.Length textSize replacedSize