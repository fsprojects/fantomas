module Fantomas.Tests.ControlStructureTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

[<Test>]
let ``if/then/else block``() =
    formatSourceString false """
let rec tryFindMatch pred list =
    match list with
    | head :: tail -> if pred(head)
                        then Some(head)
                        else tryFindMatch pred tail
    | [] -> None

let test x y =
  if x = y then "equals" 
  elif x < y then "is less than" 
  else if x > y then "is greater than"
  else "Don't know"

if age < 10
then printfn "You are only %d years old and already learning F#? Wow!" age""" config
    |> prepend newline
    |> should equal """
let rec tryFindMatch pred list =
    match list with
    | head :: tail -> if pred (head) then Some(head) else tryFindMatch pred tail
    | [] -> None

let test x y =
    if x = y then "equals"
    elif x < y then "is less than"
    else if x > y then "is greater than"
    else "Don't know"

if age < 10
then printfn "You are only %d years old and already learning F#? Wow!" age
"""

[<Test>]
let ``for loops``() =
    formatSourceString false """
    let function1() =
        for i = 1 to 10 do
            printf "%d " i
        printfn ""
    let function2() =
      for i = 10 downto 1 do
        printf "%d " i
      printfn ""
    """ config
    |> prepend newline
    |> should equal """
let function1 () =
    for i = 1 to 10 do
        printf "%d " i
    printfn ""

let function2 () =
    for i = 10 downto 1 do
        printf "%d " i
    printfn ""
"""

[<Test>]
let ``while loop``() =
    formatSourceString false """
open System
let lookForValue value maxValue =
  let mutable continueLooping = true 
  let randomNumberGenerator = new Random()
  while continueLooping do 
    let rand = randomNumberGenerator.Next(maxValue)
    printf "%d " rand
    if rand = value then 
       printfn "\nFound a %d!" value
       continueLooping <- false
lookForValue 10 20""" config
    |> prepend newline
    |> should equal """
open System

let lookForValue value maxValue =
    let mutable continueLooping = true
    let randomNumberGenerator = new Random()
    while continueLooping do
        let rand = randomNumberGenerator.Next(maxValue)
        printf "%d " rand
        if rand = value then
            printfn "\nFound a %d!" value
            continueLooping <- false

lookForValue 10 20
"""

[<Test>]
let ``try/with block``() =
    formatSourceString false """
let divide1 x y =
   try
      Some (x / y)
   with
      | :? System.DivideByZeroException -> printfn "Division by zero!"; None

let result1 = divide1 100 0
    """ config
    |> prepend newline
    |> should equal """
let divide1 x y =
    try
        Some(x / y)
    with :? System.DivideByZeroException ->
        printfn "Division by zero!"
        None

let result1 = divide1 100 0
"""

[<Test>]
let ``try/with and finally``() =
    formatSourceString false """
    let function1 x y =
       try 
         try 
            if x = y then raise (InnerError("inner"))
            else raise (OuterError("outer"))
         with
          | Failure _ -> ()
          | InnerError(str) -> printfn "Error1 %s" str
       finally
          printfn "Always print this."
    """ config
    |> prepend newline
    |> should equal """
let function1 x y =
    try
        try
            if x = y then raise (InnerError("inner")) else raise (OuterError("outer"))
        with
        | Failure _ -> ()
        | InnerError (str) -> printfn "Error1 %s" str
    finally
        printfn "Always print this."
"""

[<Test>]
let ``range expressions``() =
    formatSourceString false """
    let function2() =
      for i in 1 .. 2 .. 10 do
         printf "%d " i
      printfn ""
    function2()""" config
    |> prepend newline
    |> should equal """
let function2 () =
    for i in 1 .. 2 .. 10 do
        printf "%d " i
    printfn ""

function2 ()
"""

[<Test>]
let ``use binding``() =
    formatSourceString false """
    let writetofile filename obj =
     use file1 = File.CreateText(filename)
     file1.WriteLine("{0}", obj.ToString())
    """ config
    |> prepend newline
    |> should equal """
let writetofile filename obj =
    use file1 = File.CreateText(filename)
    file1.WriteLine("{0}", obj.ToString())
"""

[<Test>]
let ``access modifiers``() =
    formatSourceString false """
    let private myPrivateObj = new MyPrivateType()
    let internal myInternalObj = new MyInternalType()""" config
    |> prepend newline
    |> should equal """
let private myPrivateObj = new MyPrivateType()
let internal myInternalObj = new MyInternalType()
"""

[<Test>]
let ``keyworded expressions``() =
    formatSourceString false """
    assert (3 > 2)
    let result = lazy (x + 10)
    do printfn "Hello world"
    """ config
    |> prepend newline
    |> should equal """
assert (3 > 2)
let result = lazy (x + 10)
do printfn "Hello world"
"""

[<Test>]
let ``should break lines on multiline if conditions``() =
    formatSourceString false """
let x = 
    if try
        true
       with 
       | Failure _ -> false 
    then ()
    else ()
    """ config
    |> prepend newline
    |> should equal """
let x =
    if try
        true
       with Failure _ -> false then
        ()
    else
        ()
"""

[<Test>]
let ``should not escape some specific keywords``() =
    formatSourceString false """
base.Initializer()
global.Test()
    """ config
    |> prepend newline
    |> should equal """
base.Initializer()
global.Test()
"""

[<Test>]
let ``should handle delimiters before comments``() =
    formatSourceString false """
let handle = 
    if n<weakThreshhold then 
        assert onStrongDiscard.IsNone; // it disappeared
        Weak(WeakReference(v)) 
    else 
        Strong(v)
    """ config
    |> prepend newline
    |> should equal """
let handle =
    if n < weakThreshhold then
        assert onStrongDiscard.IsNone // it disappeared
        Weak(WeakReference(v))
    else
        Strong(v)
"""

[<Test>]
let ``should handle infix operators in pattern matching``() =
    formatSourceString false """
let url = 
  match x with
  | A -> "a"
  | B -> "b"
  + "/c"
    """ config
    |> prepend newline
    |> should equal """
let url =
    match x with
    | A -> "a"
    | B -> "b"
    + "/c"
"""

[<Test>]
let ``if/elif without else``() =
    formatSourceString false """
if true then ()
elif true then ()
    """ config
    |> prepend newline
    |> should equal """
if true then ()
elif true then ()
"""

[<Test>]
let ``multiline if in tuple``() =
    formatSourceString false """
(if true then 1 else 2 
 ,3)
    """ config
    |> prepend newline
    |> should equal """
((if true then 1 else 2), 3)
"""

// https://docs.microsoft.com/en-us/dotnet/fsharp/style-guide/formatting#formatting-if-expressions
[<Test>]
let ``else branch should be on newline in case if branch is long`` () =
    formatSourceString false """
if cond then
    match foo with
    | Some f -> ()
    | None -> printfn "%s" "meh"
else ()
"""  config
    |> prepend newline
    |> should equal """
if cond then
    match foo with
    | Some f -> ()
    | None -> printfn "%s" "meh"
else
    ()
"""

[<Test>]
let ``if branch should be on newline in case else branch is long`` () =
    formatSourceString false """
if not cond then
    ()
else
    match foo with
    | Some f -> ()
    | None -> printfn "%s" "meh"
"""  config
    |> prepend newline
    |> should equal """
if not cond then
    ()
else
    match foo with
    | Some f -> ()
    | None -> printfn "%s" "meh"
"""

[<Test>]
let ``elif branch should on newline if else branch is long`` () =
    formatSourceString false """
if not cond then
    ()
elif false then ()
else
    match foo with
    | Some f -> ()
    | None -> printfn "%s" "meh"
"""  config
    |> prepend newline
    |> should equal """
if not cond then
    ()
elif false then
    ()
else
    match foo with
    | Some f -> ()
    | None -> printfn "%s" "meh"
"""

[<Test>]
let ``multiline elif branch should result in newline for if and else`` () =
    formatSourceString false """
if foo then ()
elif bar then
    match foo with
    | Some f -> ()
    | None -> printfn "%s" "meh"
else ()
"""  config
    |> prepend newline
    |> should equal """
if foo then
    ()
elif bar then
    match foo with
    | Some f -> ()
    | None -> printfn "%s" "meh"
else
    ()
"""

[<Test>]
let ``else keyword should be on separate line, #483`` () =
    formatSourceString false """  if i.OpCode = OpCodes.Switch then
    AccumulateSwitchTargets i targets
    c
  else
    let branch = i.Operand :?> Cil.Instruction
    c + (Option.nullable branch.Previous)
"""  config
    |> prepend newline
    |> should equal """
if i.OpCode = OpCodes.Switch then
    AccumulateSwitchTargets i targets
    c
else
    let branch = i.Operand :?> Cil.Instruction
    c + (Option.nullable branch.Previous)
"""

[<Test>]
let ``relaxation in for loops`` () =
    formatSourceString false """
for _ in 1..10 do ()
"""  config
    |> prepend newline
    |> should equal """
for _ in 1 .. 10 do
    ()
"""

[<Test>]
let ``if elif if with trivia doesn't glitch elif conditional`` () =
    formatSourceString false """
let a ex =
    if null = ex then
        fooo ()
        None
        // this was None
    elif ex.GetType() = typeof<obj> then
        Some ex
    else
        None
"""  config
    |> prepend newline
    |> should equal """
let a ex =
    if null = ex then
        fooo ()
        None
    // this was None
    elif ex.GetType() = typeof<obj> then
        Some ex
    else
        None
"""
