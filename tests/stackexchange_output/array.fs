/// The program is from http://codereview.stackexchange.com/q/17882
module ArrayEx

open System

let private reorder cap (array : int[]) = 
  if Array.isEmpty array
  then array
  else 
    let max = array |> Array.max
    if max = cap
    then 
      let newArray = Array.create array.Length 1
      newArray.[array.Length - 1] <- cap
      newArray
    else 
      array |> Array.map(fun elem -> 
                   if elem = max
                   then max
                   else 1)

let private getZeroes todo = 
  todo
  |> Seq.takeWhile(fun elem -> elem = 0)
  |> Seq.toArray

let private getNumbers todo cap = 
  todo
  |> Seq.takeWhile(fun elem -> elem <> 0)
  |> Seq.toArray
  |> reorder cap

let GetEquivalentPermutation(array : int[], cap) = 
  let rec joinParts finished todo = 
    if Seq.isEmpty todo
    then finished |> Seq.toArray
    else 
      let zeroes = getZeroes todo
      let nextTodo = todo |> Seq.skip zeroes.Length
      let numbers = getNumbers nextTodo cap
      let finalTodo = nextTodo |> Seq.skip numbers.Length
      let newFinished = Seq.append (Seq.append finished zeroes) numbers
      joinParts newFinished finalTodo
  joinParts [] array
