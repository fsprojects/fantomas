/// The Computer Language Benchmarks Game
/// http://shootout.alioth.debian.org/
///
/// Contributed by Valentin Kraevskiy
module Regexdna

open System.Text.RegularExpressions

let regex s = Regex(s, RegexOptions.Compiled)
let input = stdin.ReadToEnd()
let text = (regex ">.*\n|\n").Replace(input, "")

["agggtaaa|tttaccct"
 "[cgt]gggtaaa|tttaccc[acg]"
 "a[act]ggtaaa|tttacc[agt]t"
 "ag[act]gtaaa|tttac[agt]ct"
 "agg[act]taaa|ttta[agt]cct"
 "aggg[acg]aaa|ttt[cgt]ccct"
 "agggt[cgt]aa|tt[acg]accct"
 "agggta[cgt]a|t[acg]taccct"
 "agggtaa[cgt]|[acg]ttaccct"]
|> List.iter(fun s -> printf "%s %i\n" s ((regex s).Matches text).Count)

let newText = 
  ["B", "(c|g|t)"
   "D", "(a|g|t)"
   "H", "(a|c|t)"
   "K", "(g|t)"
   "M", "(a|c)"
   "N", "(a|c|g|t)"
   "R", "(a|g)"
   "S", "(c|g)"
   "V", "(a|c|g)"
   "W", "(a|t)"
   "Y", "(c|t)"]
  |> List.fold (fun s (code, alt) -> (regex code).Replace(s, alt)) text

printf "\n%i\n%i\n%i\n" input.Length text.Length newText.Length
