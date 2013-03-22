/// The Computer Language Benchmarks Game
/// http://shootout.alioth.debian.org/
///
/// from Scala version by Otto Bommer, August 2010
/// Modified by Faisal Waris by removing ref's and using mutable variables, April 25, 2011
module Fannkuchredux

let fannkuch n = 
  (let perm1 = Array.create n 0
   for i = 0 to (n - 1) do
     perm1.[i] <- i;
   let perm = Array.create n 0
   let count = Array.create n 0
   let mutable flips = 0
   let mutable maxflips = 0
   let mutable checksum = 0
   let mutable nperm = 0
   let mutable r = n
   while r > 0 do
     for i = 0 to n - 1 do
       perm.[i] <- perm1.[i];
     while r <> 1 do
       count.[r - 1] <- r;
       r <- r - 1;
     flips <- 0;
     let mutable k = perm.[0]
     while k <> 0 do
       let mutable t = 0
       for i = 0 to k / 2 do
         t <- perm.[i];
         perm.[i] <- perm.[k - i];
         perm.[k - i] <- t;
       k <- perm.[0];
       flips <- flips + 1;
     maxflips <- max maxflips flips;
     if nperm &&& 1 = 0 then checksum <- checksum + flips else checksum <- checksum - flips;
     let mutable go = true
     let mutable t = 0
     while go do
       if r = n then 
         (go <- false;
          r <- 0)
       else 
         (t <- perm1.[0];
          for i = 0 to r - 1 do
            perm1.[i] <- perm1.[i + 1];
          perm1.[r] <- t;
          count.[r] <- count.[r] - 1;
          if count.[r] > 0 then go <- false else r <- r + 1);
     nperm <- nperm + 1;
   (maxflips, checksum))

let _ = 
  let n = 
    try 
      int((System.Environment.GetCommandLineArgs()).[1])
    with
    | _ -> 7
  let (maxflips, checksum) = fannkuch n
  Printf.printf "%d\nPfannkuchen(%d) = %d\n" checksum n maxflips
