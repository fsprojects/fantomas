/// The program is from http://codereview.stackexchange.com/q/12643
module Mergesort

open System
open System.Windows
open System.Collections.Generic

let shuffle(l : 'a array) = 
  let ileft = LinkedList<int>(seq { 0..(l.Length - 1) })
  let rec pick (ar : 'a array) r = 
    match ileft.Count with
    | 0 -> r
    | n -> 
      let ik = ileft |> Seq.nth(rnd.Next(n))
      ileft.Remove(ik) |> ignore
      pick ar (ar.[ik] :: r)
  pick l []

let rec merge (ar1 : 'a array) (ar2 : 'a array) = 
  let rec index(islastfromAr1, ilast, jlast) = 
    seq { 
      let inext, jnext = ilast + 1, jlast + 1
      match inext < ar1.Length, jnext < ar2.Length with
      | true, true -> 
        let indexnext = 
          if ar1.[inext] < ar2.[jnext]
          then (true, inext, jlast)
          else (false, ilast, jnext)
        yield Some(indexnext)
        yield! index indexnext
      | false, true -> 
        let indexnext = (false, ilast, jnext)
        yield Some(indexnext)
        yield! index indexnext
      | true, false -> 
        let indexnext = (true, inext, jlast)
        yield Some(indexnext)
        yield! index indexnext
      | false, false -> yield None }
  let mergeindex = index(false, -1, -1)
  [for (formar1, i, j) in mergeindex |> Seq.choose(id) -> 
     if formar1
     then ar1.[i]
     else ar2.[j]]

and mergesort = 
  function 
  | [||] -> [||]
  | [|a|] -> [|a|]
  | ar -> 
    let ar1 = ar.[0..ar.Length / 2 - 1]
    let ar2 = ar.[ar.Length / 2..ar.Length - 1]
    merge (mergesort ar1) (mergesort ar2) |> List.toArray

let testval = 
  ([|1..100|]
   |> shuffle
   |> List.toArray)

let test4 = mergesort testval
