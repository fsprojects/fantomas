///
/// The Computer Language Benchmarks Game
/// http://shootout.alioth.debian.org/
///
/// Based on contribution of Isaac Gouy
/// Based on contribution of Eckehard Berns
/// Based on code by Heiner Marxen
/// and the ATS version by Hongwei Xi
/// convert to C# by The Anh Tran
/// convert to F# by Jomo Fisher
module Fannkuch

open System
open System.Threading
let mutable n = 0

let mutable flip_max_arr : int array = null

let remain_task = ref -1

let threads = Environment.ProcessorCount

let next_perm(permutation : int array, position) = 
  let perm0 = permutation.[0]
  for i in 0..position - 1 do
    permutation.[i] <- permutation.[i + 1]
  permutation.[position] <- perm0

/// In order to divide tasks 'equally' for many threads, permut generation
/// strategy is different than that of original single thread.
/// this function will 'correctly' print first 30 permutations
let print_30_permut() = 
  let /// declare and initialize
  permutation : int array = 
    Array.init n (fun (i) -> 
       Console.Write(i + 1)
       i)
  Console.WriteLine()
  let perm_remain = Array.init n (fun (i) -> i + 1)
  let mutable numPermutationsPrinted = 1
  let mutable finished = false
  let mutable pos_right = 2
  while not finished && pos_right <= n do
    let mutable pos_left = pos_right - 1
    while not finished && pos_left < pos_right do
      next_perm(permutation, pos_left)
      perm_remain.[pos_left] <- perm_remain.[pos_left] - 1
      if perm_remain.[pos_left] > 0 then 
        numPermutationsPrinted <- numPermutationsPrinted + 1
        if numPermutationsPrinted < 31 then 
          for i in 0..n - 1 do
            Console.Write("{0}", (1 + permutation.[i]))
          Console.WriteLine()
        else 
          finished <- true
          pos_right <- n
        if not finished then 
          while pos_left <> 1 do
            perm_remain.[pos_left - 1] <- pos_left
            pos_left <- pos_left - 1
      else pos_left <- pos_left + 1
    pos_right <- pos_right + 1

/// rotate down perm[0..prev] by one
/// Take a permut array, continuously flipping until first element is '1'
/// Return flipping times
let public count_flip(perm_flip : int array) = 
  let mutable v0 = perm_flip.[0]
  let mutable tmp = 0
  let mutable flip_count = 0
  let mutable finished = false
  while not finished do
    let mutable i = 1
    let mutable j = v0 - 1
    while i < j do
      tmp <- perm_flip.[i]
      perm_flip.[i] <- perm_flip.[j]
      perm_flip.[j] <- tmp
      i <- i + 1
      j <- j - 1
    tmp <- perm_flip.[v0]
    perm_flip.[v0] <- v0
    v0 <- tmp
    flip_count <- flip_count + 1
    finished <- v0 = 0
  flip_count

let worker() = 
  let permutation = Array.zeroCreate n
  let perm_remain = Array.zeroCreate n
  let perm_flip = Array.zeroCreate n
  let mutable pos_right = Interlocked.Increment(&remain_task.contents)
  while (pos_right < n - 1) do
    let mutable flip_max = 0
    for i in 0..n - 2 do
      permutation.[i] <- i
    permutation.[pos_right] <- n - 1
    permutation.[n - 1] <- pos_right
    for i in 1..n do
      perm_remain.[i - 1] <- i
    let mutable pos_left = n - 2
    while pos_left < n - 1 do
      next_perm(permutation, pos_left)
      perm_remain.[pos_left] <- perm_remain.[pos_left] - 1
      if perm_remain.[pos_left] > 0 then 
        while pos_left <> 1 do
          perm_remain.[pos_left - 1] <- pos_left
          pos_left <- pos_left - 1
        if permutation.[0] <> 0 && permutation.[n - 1] <> n - 1 then 
          for ip in 0..n - 1 do
            perm_flip.[ip] <- permutation.[ip]
          let flipcount = count_flip(perm_flip)
          if flip_max < flipcount then flip_max <- flipcount
      else pos_left <- pos_left + 1
    flip_max_arr.[pos_right] <- flip_max
    pos_right <- Interlocked.Increment(&remain_task.contents)

let fank_game() = 
  let th : Thread array = 
    Array.init threads (fun (i) -> 
       let th = Thread(worker)
       th.Start()
       th)
  print_30_permut()
  for t in th do
    t.Join()
  let mutable mx = 0
  for i in flip_max_arr do
    if (mx < i) then mx <- i
  mx

[<EntryPoint>]
let main args = 
  n <- if args.Length > 0 then int args.[0] else 7
  flip_max_arr <- Array.zeroCreate n
  Console.WriteLine("Pfannkuchen({0}) = {1}", n, fank_game())
  0
