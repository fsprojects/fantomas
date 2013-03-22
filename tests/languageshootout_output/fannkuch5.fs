/// The Computer Language Benchmarks Game
/// http://shootout.alioth.debian.org/
/// contributed by Alex Peake
/// Literal translation of Andrei Formiga's Scala solution
module Fannkuch

[<EntryPoint>]
let main(args) = 
  let n = if args.Length > 0 then int args.[0] else 7
  let mutable maxFlips = 0
  let mutable permN = 0
  let mutable j = 0
  let mutable k = 0
  let mutable temp = 0
  let mutable first = 0
  let mutable flips = 0
  let mutable perm0 = Array.create n 0
  let mutable perm = Array.create n 0
  let mutable rot = Array.create n 0
  while (k < n) do
    perm.[k] <- k + 1
    rot.[k] <- 0
    k <- k + 1
  while (rot.[n - 1] < n) do
    if (permN < 30) then 
      k <- 0
      while (k < n) do
        printf "%d" perm.[k]
        k <- k + 1
      printf "\n"
      permN <- permN + 1
    flips <- 0
    k <- 0
    while (k < n) do
      perm0.[k] <- perm.[k]
      k <- k + 1
    first <- perm0.[0]
    while (first <> 1) do
      k <- 0
      while (k < (first >>> 1)) do
        temp <- perm0.[k]
        perm0.[k] <- perm0.[first - 1 - k]
        perm0.[first - 1 - k] <- temp
        k <- k + 1
      first <- perm0.[0]
      flips <- flips + 1
    if (flips > maxFlips) then maxFlips <- flips
    temp <- perm.[0]
    perm.[0] <- perm.[1]
    perm.[1] <- temp
    rot.[1] <- rot.[1] + 1
    j <- 1
    while (j < n - 1 && rot.[j] > j) do
      rot.[j] <- 0
      j <- j + 1
      k <- 0
      while (k < j) do
        temp <- perm.[k]
        perm.[k] <- perm.[k + 1]
        perm.[k + 1] <- temp
        k <- k + 1
      rot.[j] <- rot.[j] + 1
  printfn "Pfannkuchen(%d) = %d" n maxFlips
  0
