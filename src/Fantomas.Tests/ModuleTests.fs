module Fantomas.Tests.ModuleTests

open NUnit.Framework
open FsUnit

open Fantomas.CodeFormatter
open Fantomas.Tests.TestHelper

[<Test>]
let ``module abbreviation``() =
    formatSourceString false "module ES = Microsoft.FSharp.Quotations.ExprShape" config
    |> should equal """module ES = Microsoft.FSharp.Quotations.ExprShape
"""

[<Test>]
let ``module with functions``() =
    formatSourceString false "module internal MyModule = let x = 42" config
    |> prepend newline
    |> should equal """
module internal MyModule = 
    let x = 42
"""

[<Test>]
let ``open modules``() =
    formatSourceString false """
    // comment1
    open System.IO
    // comment2
    open System""" config
    |> prepend newline
    |> should equal """
// comment2
open System
// comment1
open System.IO
"""

[<Test>]
let ``sort open modules doesn't mess comments up``() =
    formatSourceString false """
module internal Fantomas.CodePrinter

open System
open System.Collections.Generic
open Microsoft.FSharp.Compiler.Ast
open Fantomas.FormatConfig
open Fantomas.SourceParser
open Fantomas.SourceTransformer

// comment1
let sortAndDedup by l =
    // comment2
    l |> Seq.distinctBy by |> Seq.sortBy by |> List.ofSeq""" config
    |> prepend newline
    |> should equal """
module internal Fantomas.CodePrinter

open Fantomas.FormatConfig
open Fantomas.SourceParser
open Fantomas.SourceTransformer
open Microsoft.FSharp.Compiler.Ast
open System
open System.Collections.Generic

// comment1
let sortAndDedup by l =
    // comment2
    l |> Seq.distinctBy by |> Seq.sortBy by |> List.ofSeq
"""

[<Test>]
let ``nested modules``() =
    formatSourceString false """
module Y =
    let x = 1 

    module Z =
        let z = 5""" config
    |> prepend newline
    |> should equal """
module Y = 
    let x = 1
    
    module Z = 
        let z = 5
"""

[<Test>]
let ``sibling modules``() =
    formatSourceString false """
module TopLevel

let topLevelX = 5

module Inner1 =
    let inner1X = 1
module Inner2 =
    let inner2X = 5""" config
    |> prepend newline
    |> should equal """
module TopLevel

let topLevelX = 5

module Inner1 = 
    let inner1X = 1

module Inner2 = 
    let inner2X = 5
"""

[<Test>]
let ``module signatures``() =
    formatSourceString true """
module Utils

val turnTracingOn : unit -> unit
val turnTracingOff : unit -> unit
val isTraced : unit -> bool

module Random = begin
    val exponential : mean:float -> float
    val nextInt : max:int -> int
    val nextInt64 : max:int64 -> int64
    val next : max:float -> float
end""" config
    |> prepend newline
    |> should equal """
module Utils

val turnTracingOn : unit -> unit
val turnTracingOff : unit -> unit
val isTraced : unit -> bool

module Random = 
    val exponential : mean:float -> float
    val nextInt : max:int -> int
    val nextInt64 : max:int64 -> int64
    val next : max:float -> float
"""

[<Test>]
let ``namespace declaration``() =
    formatSourceString false """
namespace Widgets

type MyWidget1 =
    member this.WidgetName = "Widget1" 

module WidgetsModule =
    let widgetName = "Widget2"
    """ config
    |> prepend newline
    |> should equal """
namespace Widgets

type MyWidget1 = 
    member this.WidgetName = "Widget1"

module WidgetsModule = 
    let widgetName = "Widget2"
"""

[<Test>]
let ``should preserve global keyword``() =
    formatSourceString false """
namespace global

type SomeType() =
    member this.Print() = 
        global.System.Console.WriteLine("Hello World!")
    """ config
    |> prepend newline
    |> should equal """
namespace global

type SomeType() = 
    member this.Print() = global.System.Console.WriteLine("Hello World!")
"""

[<Test>]
let ``should escape keywords correctly``() =
    formatSourceString false """
module ``method``

let ``abstract`` = "abstract"

type SomeType() =
    member this.``new``() = 
        System.Console.WriteLine("Hello World!")
    """ config
    |> prepend newline
    |> should equal """
module ``method``

let ``abstract`` = "abstract"

type SomeType() = 
    member this.``new``() = System.Console.WriteLine("Hello World!")
"""
