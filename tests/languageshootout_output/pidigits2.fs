/// The Computer Language Benchmarks Game
/// http://shootout.alioth.debian.org/
///
/// contributed by Valentin Kraevskiy
/// fixed by Joel Mueller
/// updated by Anh-Dung Phan
module Pidigits

open System

open System.Numerics

let id = 1I, 0I, 0I, 1I

let inline compose (q, r, s, t) (u, v, x) = (q * u, q * v + r * x, s * u, s * v + t * x)

let inline extract (q, r, s, t) x = BigInteger.Divide(q * x + r, s * x + t)

let inline next z = extract z 3I

let inline safe z y = extract z 4I = y

let inline product (u, v, w, x) n = 
  let neg = -10I * n
  (10I * u + neg * w, 10I * v + neg * x, w, x)

let inline lfts n = n, 4I * n + 2I, 2I * n + 1I

let rec digits n z = 
  let y = next z
  if safe z y
  then y, n, product z y
  else digits (n + 1I) (compose z <| lfts n)

let rec loop n s i z total = 
  if total = 0
  then 
    for _ in 1..n do
      Console.Write(" ")
    Console.Write("\t:{0}\n", s + 10 - n)
  elif n = 0
  then 
    Console.Write("\t:{0}\n", s + 10)
    loop 10 (s + 10) i z total
  else 
    let (y, i', z') = digits i z
    Console.Write("{0}", int y)
    loop (n - 1) s i' z' (total - 1)

loop 10 0 1I id <| try 
                     int(System.Environment.GetCommandLineArgs().[1])
                   with
                   | _ -> 27