/// The program is from http://codereview.stackexchange.com/q/15973
module Unblock

open System

open System.Collections.Generic

open System.IO

open System.Diagnostics

type Position = int * int

type Brick = char * int

type State = Position[]

type BrickInfo = Brick[]

type Direction = 
  | Up
  | Down
  | Left
  | Right
  override this.ToString() = 
    match this with
    | Up -> "Up"
    | Down -> "Down"
    | Left -> "Left"
    | Right -> "Right"

type Move = int * Direction

let checkBoth target length current = if current > target || current + length - 1 < target then true else false

let test (initialState : State) (brickInfo : Brick[]) (horizontalBricks : int[][], verticalBricks : int[][]) (rowNum, columnNum) = 
  let (redRowNum, _) = initialState.[0]
  let (_, redLength) = brickInfo.[0]
  let mySet = new HashSet<_>(HashIdentity.Structural)
  let horizontalBricksAll = horizontalBricks |> Array.concat
  let verticalBricksAll = verticalBricks |> Array.concat
  let rec solveDFS (currentState : State) (lastBrick, lastDirection) depth = 
    let generateState i (newPosition : Position) = 
      let nextState = currentState |> Array.copy
      nextState.[i] <- newPosition
      nextState
    let isDuplicated(set : HashSet<_>) = 
      if set.Contains(currentState) then true
      else 
        ignore(set.Add currentState)
        false
    let checkVacancy(rowID, columnID) = 
      let checkHorizontal i = 
        let (_, length) = brickInfo.[i]
        let (_, startColumn) = currentState.[i]
        checkBoth columnID length startColumn
      let checkVertical i = 
        let (_, length) = brickInfo.[i]
        let (startRow, _) = currentState.[i]
        checkBoth rowID length startRow
      (horizontalBricks.[rowID] |> Array.forall checkHorizontal) && (verticalBricks.[columnID] |> Array.forall checkVertical)
    let generateRight i = 
      let (_, length) = brickInfo.[i]
      let (rowID, columnID) = currentState.[i]
      if columnID + length >= columnNum then None else Some((rowID, columnID + length), (rowID, columnID + 1))
    let generateLeft i = 
      let (_, length) = brickInfo.[i]
      let (rowID, columnID) = currentState.[i]
      if columnID = 0 then None else Some((rowID, columnID - 1), (rowID, columnID - 1))
    let generateUp i = 
      let (_, length) = brickInfo.[i]
      let (rowID, columnID) = currentState.[i]
      if rowID + length >= rowNum then None else Some((rowID + length, columnID), (rowID + 1, columnID))
    let generateDown i = 
      let (_, length) = brickInfo.[i]
      let (rowID, columnID) = currentState.[i]
      if rowID = 0 then None else Some((rowID - 1, columnID), (rowID - 1, columnID))
    if depth < 7 && not(isDuplicated mySet) && (let (_, columnLeft) = currentState.[0]
                                                [|columnLeft + redLength..columnNum - 1|] |> Array.forall(fun (columnID) -> checkVacancy(redRowNum, columnID)) || horizontalBricksAll |> Seq.tryFind(fun (elem) -> 
                                                  (match generateRight elem with
                                                   | Some(a, b) when checkVacancy a -> solveDFS(generateState elem b) (elem, Right) (depth + 1)
                                                   | _ -> false) || (match generateLeft elem with
                                                                    | Some(a, b) when checkVacancy a -> solveDFS(generateState elem b) (elem, Left) (depth + 1)
                                                                    | _ -> false)) |> Option.isSome || verticalBricksAll |> Seq.tryFind(fun (elem) -> 
                                                  (match generateUp elem with
                                                   | Some(a, b) when checkVacancy a -> solveDFS(generateState elem b) (elem, Up) (depth + 1)
                                                   | _ -> false) || (match generateDown elem with
                                                                    | Some(a, b) when checkVacancy a -> solveDFS(generateState elem b) (elem, Down) (depth + 1)
                                                                    | _ -> false)) |> Option.isSome) then 
      Console.WriteLine("Brick Name {0} Direction {1}", fst brickInfo.[lastBrick], lastDirection)
      true
    else 
      ignore(mySet.Remove(currentState))
      false
  Console.WriteLine(solveDFS initialState (0, Right) 0)

let x0 = ('a', 2)

let x1 = ('b', 2)

let x2 = ('c', 3)

let x3 = ('d', 2)

let x4 = ('e', 2)

let x5 = ('f', 1)

let x6 = ('g', 2)

let x7 = ('h', 2)

let x8 = ('i', 2)

let x9 = ('j', 2)

let x10 = ('k', 3)

let x11 = ('l', 3)

let brickInfo = 
  [|x0
    x1
    x2
    x3
    x4
    x5
    x6
    x7
    x8
    x9
    x10
    x11|]

let horizontalBricks = 
  [|[|4|]
    [|7|]
    [|6|]
    [|0|]
    [|2|]
    [|1|]|]

let verticalBricks = 
  [|[|3|]
    [||]
    [|5|]
    [|10|]
    [|8
      9|]
    [|11|]|]

let rowNum = 6

let columnNum = 6

let p0 = (3, 0)

let p1 = (5, 0)

let p2 = (4, 0)

let p3 = (1, 0)

let p4 = (0, 0)

let p5 = (0, 2)

let p6 = (2, 2)

let p7 = (1, 3)

let p8 = (2, 4)

let p9 = (4, 4)

let p10 = (3, 3)

let p11 = (1, 5)

let initialState = 
  [|p0
    p1
    p2
    p3
    p4
    p5
    p6
    p7
    p8
    p9
    p10
    p11|]

[<EntryPoint>]
let main argv = 
  printfn "%A" argv
  let sw = new Stopwatch()
  sw.Start()
  ignore(test (initialState) (brickInfo) (horizontalBricks, verticalBricks) (rowNum, columnNum))
  sw.Stop()
  Console.WriteLine(sw.Elapsed)
  0
