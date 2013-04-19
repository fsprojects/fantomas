module Fantomas.Tests.CodeFormatterTests

open NUnit.Framework
open FsUnit

open Fantomas.CodeFormatter
open Fantomas.Tests.TestHelper

[<Test>]
let ``hash directives``() =
    formatSourceString false """
    #r "Fantomas.Tests.dll"
    #load "CodeFormatterTests.fs"
    """ config
    |> prepend newline
    |> should equal """
#r "Fantomas.Tests.dll"
#load "CodeFormatterTests.fs"
"""

[<Test>]
let ``typed quotations``() =
    formatSourceString false """
    <@ 
        let f x = x + 10
        f 20
    @>""" config
    |> prepend newline
    |> should equal """
<@ let f x = x + 10
   f 20 @>
"""

[<Test>]
let ``untyped quotations``() =
    formatSourceString false "<@@ 2 + 3 @@>" config
    |> should equal """<@@ 2 + 3 @@>
"""

[<Test>]
let ``exception declations``() =
    formatSourceString false "exception Error2 of string * int" config
    |> should equal """exception Error2 of string * int
"""

[<Test>]
let ``object expressions``() =
    formatSourceString false """let obj1 = { new System.Object() with member x.ToString() = "F#" }""" config
    |> prepend newline
    |> should equal """
let obj1 = 
    { new System.Object() with
          member x.ToString() = "F#" }
"""

[<Test>]
let ``object expressions and interfaces``() =
    formatSourceString false """
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
    formatSourceString false """
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
    formatSourceString false """
    let base1 = d1 :> Base1
    let derived1 = base1 :?> Derived1""" config
    |> prepend newline
    |> should equal """
let base1 = d1 :> Base1

let derived1 = base1 :?> Derived1
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
let ``range expressions``() =
    formatSourceString false """
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

function2()
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
let ``match expressions``() =
    formatSourceString false """
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
    formatSourceString false """
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
let ``triple-quoted strings``() =
    formatSourceString false "let xmlFragment2 = \"\"\"<book author=\"Milton, John\" title=\"Paradise Lost\">\"\"\"" config
    |> should equal ("let xmlFragment2 = \"\"\"<book author=\"Milton, John\" title=\"Paradise Lost\">\"\"\"" + newline)

[<Test>]
let ``string literals``() =
    formatSourceString false """
let xmlFragment1 = @"<book author=""Milton, John"" title=""Paradise Lost"">"
let str1 = "abc"
    """ config 
    |> prepend newline
    |> should equal """
let xmlFragment1 = @"<book author=""Milton, John"" title=""Paradise Lost"">"

let str1 = "abc"
"""

