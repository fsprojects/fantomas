/// The Computer Language Benchmarks Game
/// http://shootout.alioth.debian.org/
///
/// contributed by Valentin Kraevskiy
/// fixed by Joel Mueller
module Pidigits

let id = (1I, 0I, 0I, 1I)

let comp (q, r, s, t) (u, v, x) = (q * u, q * v + r * x, s * u, s * v + t * x)

let div x y = 
  let rec next w n = 
    if w > x
    then n
    else next (w + y) (n + 1I)
  next y 0I

let extr (q, r, s, t) x = div (q * x + r) (s * x + t)

let next z = extr z 3I

let safe z = (=)(extr z 4I)

let prod (u, v, w, x) n = 
  let neg = -10I * n
  (10I * u + neg * w, 10I * v + neg * x, w, x)

let digits = 
  let z = ref id
  let lfts = 
    let n = ref 0I
    fun () -> 
      n := !n + 1I
      (!n, 4I * !n + 2I, 2I * !n + 1I)
  let rec digits() = 
    let y = next !z
    if safe !z y
    then 
      z := (prod !z y)
      y
    else 
      z := (comp !z <| lfts())
      digits()
  digits

let rec loop n s total = 
  if total = 0
  then 
    for _ in 1..n do
      printf " "
    printf ("\t:%i\n") (s + 10 - n)
  elif n = 0
  then 
    printf "\t:%i\n" <| s + 10
    loop 10 (s + 10) total
  else 
    printf "%i" <| int(digits())
    loop (n - 1) s (total - 1)

loop 10 0 <| try 
               int(System.Environment.GetCommandLineArgs().[1])
             with
             | _ -> 27