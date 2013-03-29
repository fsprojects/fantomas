module Fantomas.Tests.CodeFormatterTests

open NUnit.Framework
open FsUnit

open Fantomas.FormatConfig
open Fantomas.CodeFormatter

let config = FormatConfig.Default
let newline = System.Environment.NewLine

let inline prepend s content = s + content
let inline append s content = content + s

[<Test>]
let ``module abbreviation``() =
    formatSourceString "module ES = Microsoft.FSharp.Quotations.ExprShape" config
    |> should equal "module ES = Microsoft.FSharp.Quotations.ExprShape"

[<Test>]
let ``attributes on expressions``() =
    formatSourceString """
    [<Dependency("FSharp.Compiler", LoadHint.Always)>]
    do ()""" config
    |> prepend newline
    |> should equal """
[<Dependency("FSharp.Compiler", LoadHint.Always)>]
do ()"""

[<Test>]
let ``module with functions``() =
    formatSourceString "module internal MyModule = let x = 42" config
    |> prepend newline
    |> should equal """
module internal MyModule = 
    let x = 42
    """

[<Test>]
let ``open modules``() =
    formatSourceString """
    open System
    open System.IO""" config
    |> prepend newline
    |> should equal """
open System

open System.IO
"""

[<Test>]
let ``recursive functions``() =
    formatSourceString """
    let rec f x = g x
    and g x = x""" config
    |> prepend newline
    |> should equal """
let rec f x = g x

and g x = x
"""

[<Test>]
let ``hash directives``() =
    formatSourceString """
    #r "Fantomas.Tests.dll"
    #load "CodeFormatterTests.fs"
    """ config
    |> prepend newline
    |> append newline
    |> should equal """
#r "Fantomas.Tests.dll"
#load "CodeFormatterTests.fs"
"""

[<Test>]
let ``discriminated unions declaration``() =
    formatSourceString "type X = private | A of AParameters | B" config
    |> prepend newline
    |> should equal """
type X = 
    private
    | A of AParameters
    | B
"""

[<Test>]
let ``record declaration``() =
    formatSourceString "type AParameters = { a : int }" config
    |> prepend newline
    |> should equal """
type AParameters = 
    { a : int }
"""

[<Test>]
let ``enums declaration``() =
    formatSourceString """
    type FontVariant =
    | [<Description("small-caps")>] SmallCaps = 0""" config
    |> prepend newline
    |> should equal """
type FontVariant = 
    | [<Description("small-caps")>] SmallCaps = 0
"""

[<Test>]
let ``units of measures declaration``() =
    formatSourceString """
    [<Measure>] type m
    [<Measure>] type kg
    [<Measure>] type s
    [<Measure>] type N = kg m / s^2
    [<Measure>] type Pa = N / m^2""" config
    |> prepend newline
    |> should equal """
[<Measure>]
type m

[<Measure>]
type kg

[<Measure>]
type s

[<Measure>]
type N = kg m * s^2

[<Measure>]
type Pa = N * m^2
"""

[<Test>]
let ``typed quotations``() =
    formatSourceString """
    <@ 
        let f x = x + 10
        f 20
    @>""" config
    |> prepend newline
    |> should equal """
<@ let f x = x + 10
   f 20 @>"""

[<Test>]
let ``untyped quotations``() =
    formatSourceString "<@@ 2 + 3 @@>" config
    |> should equal "<@@ 2 + 3 @@>"

[<Test>]
let ``exception declations``() =
    formatSourceString "exception Error2 of string * int" config
    |> should equal """exception Error2 of string * int
"""

[<Test>]
let ``for loops``() =
    formatSourceString """
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
let function1() = 
    for i = 1 to 10 do
        printf "%d " i
    printfn ""

let function2() = 
    for i = 10 downto 1 do
        printf "%d " i
    printfn ""
"""

[<Test>]
let ``object expressions``() =
    formatSourceString """let obj1 = { new System.Object() with member x.ToString() = "F#" }""" config
    |> prepend newline
    |> should equal """
let obj1 = 
    { new System.Object() with
          member x.ToString() = "F#" }
"""

[<Test>]
let ``object expressions and interfaces``() =
    formatSourceString """
    let implementer() = 
        { new ISecond with 
            member this.H() = ()
            member this.J() = ()
          interface IFirst with 
            member this.F() = ()
            member this.G() = () }""" config
    |> prepend newline
    |> should equal """
let implementer() = 
    { new ISecond with
          member this.H() = ()
          member this.J() = ()
      interface IFirst with
          member this.F() = ()
          member this.G() = () }
"""

[<Test>]
let ``type annotations``() =
    formatSourceString """
    let iterate1 (f : unit -> seq<int>) =
        for e in f() do printfn "%d" e
    let iterate2 (f : unit -> #seq<int>) =
        for e in f() do printfn "%d" e""" config
    |> prepend newline
    |> should equal """
let iterate1(f : unit -> seq<int>) = 
    for e in f() do
        printfn "%d" e

let iterate2(f : unit -> #seq<int>) = 
    for e in f() do
        printfn "%d" e
"""

[<Test>]
let ``upcast and downcast``() =
    formatSourceString """
    let base1 = d1 :> Base1
    let derived1 = base1 :?> Derived1""" config
    |> prepend newline
    |> should equal """
let base1 = d1 :> Base1

let derived1 = base1 :?> Derived1
"""

[<Test>]
let ``use binding``() =
    formatSourceString """
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
let ``range expressions``() =
    formatSourceString """
    let function2() =
      for i in 1 .. 2 .. 10 do
         printf "%d " i
      printfn ""
    function2()""" config
    |> prepend newline
    |> should equal """
let function2() = 
    for i in 1..2..10 do
        printf "%d " i
    printfn ""

function2()"""

[<Test>]
let ``access modifiers``() =
    formatSourceString """
    let private myPrivateObj = new MyPrivateType()
    let internal myInternalObj = new MyInternalType()""" config
    |> prepend newline
    |> should equal """
let private myPrivateObj = new MyPrivateType()

let internal myInternalObj = new MyInternalType()
"""

[<Test>]
let ``keyworded expressions``() =
    formatSourceString """
    assert (3 > 2)
    let result = lazy (x + 10)
    do printfn "Hello world"
    """ config
    |> prepend newline
    |> append newline
    |> should equal """
assert (3 > 2)
let result = lazy (x + 10)

do printfn "Hello world"
"""

[<Test>]
let ``match expressions``() =
    formatSourceString """
    let filter123 x =
        match x with
        | 1 | 2 | 3 -> printfn "Found 1, 2, or 3!"
        | a -> printfn "%d" a""" config
    |> prepend newline
    |> should equal """
let filter123 x = 
    match x with
    | 1 | 2 | 3 -> printfn "Found 1, 2, or 3!"
    | a -> printfn "%d" a
"""

[<Test>]
let ``function keyword``() =
    formatSourceString """
    let filterNumbers =
        function | 1 | 2 | 3 -> printfn "Found 1, 2, or 3!"
                 | a -> printfn "%d" a""" config
    |> prepend newline
    |> should equal """
let filterNumbers = 
    function 
    | 1 | 2 | 3 -> printfn "Found 1, 2, or 3!"
    | a -> printfn "%d" a
"""

[<Test>]
let ``try/with block``() =
    formatSourceString """
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
    with
    | :? System.DivideByZeroException -> 
        printfn "Division by zero!"
        None

let result1 = divide1 100 0
"""

[<Test>]
let ``try/with and finally``() =
    formatSourceString """
    let function1 x y =
       try 
         try 
            if x = y then raise (InnerError("inner"))
            else raise (OuterError("outer"))
         with
          | InnerError(str) -> printfn "Error1 %s" str
       finally
          printfn "Always print this."
    """ config
    |> prepend newline
    |> should equal """
let function1 x y = 
    try 
        try 
            if x = y
            then raise(InnerError("inner"))
            else raise(OuterError("outer"))
        with
        | InnerError(str) -> printfn "Error1 %s" str
    finally
        printfn "Always print this."
"""

[<Test>]
let ``verbose syntax``() =
    formatSourceString """
    #light "off"

    let div2 = 2;;

    let f x = 
        let r = x % div2 in
          if r = 1 then 
            begin "Odd"  end 
          else 
            begin "Even" end
    """ config
    |> prepend newline
    |> should equal """
let div2 = 2

let f x = 
    let r = x % div2
    if r = 1
    then ("Odd")
    else ("Even")
"""

[<Test>]
let ``while loop``() =
    formatSourceString """
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
        if rand = value
        then 
            printfn "\nFound a %d!" value
            continueLooping <- false

lookForValue 10 20"""

[<Test>]
let ``triple-quoted strings``() =
    formatSourceString "let xmlFragment2 = \"\"\"<book author=\"Milton, John\" title=\"Paradise Lost\">\"\"\"" config
    |> should equal ("let xmlFragment2 = \"\"\"<book author=\"Milton, John\" title=\"Paradise Lost\">\"\"\"" + newline)

[<Test>]
let ``string literals``() =
    formatSourceString """
let xmlFragment1 = @"<book author=""Milton, John"" title=""Paradise Lost"">"
let str1 = "abc"
    """ config 
    |> prepend newline
    |> should equal """
let xmlFragment1 = @"<book author=""Milton, John"" title=""Paradise Lost"">"

let str1 = "abc"
"""

[<Test>]
let ``line, file and path identifiers``() =
    formatSourceString """
    let printSourceLocation() =
        printfn "Line: %s" __LINE__
        printfn "Source Directory: %s" __SOURCE_DIRECTORY__
        printfn "Source File: %s" __SOURCE_FILE__
    printSourceLocation()
    """ config
    |> prepend newline
    |> should equal """
let printSourceLocation() = 
    printfn "Line: %s" __LINE__
    printfn "Source Directory: %s" __SOURCE_DIRECTORY__
    printfn "Source File: %s" __SOURCE_FILE__

printSourceLocation()"""

[<Test>]
let ``enums conversion``() =
    formatSourceString """
type uColor =
   | Red = 0u
   | Green = 1u
   | Blue = 2u
let col3 = Microsoft.FSharp.Core.LanguagePrimitives.EnumOfValue<uint32, uColor>(2u)""" config
    |> prepend newline
    |> should equal """
type uColor = 
    | Red = 0u
    | Green = 1u
    | Blue = 2u

let col3 = Microsoft.FSharp.Core.LanguagePrimitives.EnumOfValue<uint32, uColor>(2u)
"""

[<Test>]
let ``if/then/else block``() =
    formatSourceString """
let rec tryFindMatch pred list =
    match list with
    | head :: tail -> if pred(head)
                        then Some(head)
                        else tryFindMatch pred tail
    | [] -> None

let test x y =
  if x = y then "equals" 
  elif x < y then "is less than" 
  else "is greater than"

if age < 10
then printfn "You are only %d years old and already learning F#? Wow!" age""" config
    |> prepend newline
    |> should equal """
let rec tryFindMatch pred list = 
    match list with
    | head :: tail -> 
        if pred(head)
        then Some(head)
        else tryFindMatch pred tail
    | [] -> None

let test x y = 
    if x = y
    then "equals"
    elif x < y
    then "is less than"
    else "is greater than"

if age < 10
then printfn "You are only %d years old and already learning F#? Wow!" age"""