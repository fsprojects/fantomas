module Fantomas.Tests.ModuleTests

open Fantomas
open NUnit.Framework
open FsUnit
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
// comment1
open System.IO
// comment2
open System
"""

[<Test>]
let ``sort open modules doesn't mess comments up``() =
    formatSourceString false """
module internal Fantomas.CodePrinter

// comment0
let x = 0

open System
open System.Collections.Generic
open FSharp.Compiler.SyntaxTree
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

// comment0
let x = 0

open System
open System.Collections.Generic
open FSharp.Compiler.SyntaxTree
open Fantomas.FormatConfig
open Fantomas.SourceParser
open Fantomas.SourceTransformer

// comment1
let sortAndDedup by l =
    // comment2
    l
    |> Seq.distinctBy by
    |> Seq.sortBy by
    |> List.ofSeq
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

val turnTracingOn: unit -> unit
val turnTracingOff: unit -> unit
val isTraced: unit -> bool

module Random =
    val exponential: mean:float -> float
    val nextInt: max:int -> int
    val nextInt64: max:int64 -> int64
    val next: max:float -> float
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
let ``should retain rec in namespace``() =
    formatSourceString false """
namespace rec Test

type Add = Expr * Expr

type Expr =
    | Add of Add
    | Value of int
    """ config
    |> prepend newline
    |> should equal """
namespace rec Test

type Add = Expr * Expr

type Expr =
    | Add of Add
    | Value of int
"""

[<Test>]
let ``should retain rec in nested module``() =
    formatSourceString false """
namespace Test

module rec Expression =
    type Add = Expr * Expr

    type Expr =
        | Add of Add
        | Value of int
    """ config
    |> prepend newline
    |> should equal """
namespace Test

module rec Expression =
    type Add = Expr * Expr

    type Expr =
        | Add of Add
        | Value of int
"""

[<Test>]
let ``should preserve global keyword``() =
    formatSourceString false """
namespace global

type SomeType() =
    member this.Print() = 
        global.System.Console.WriteLine("Hello World!")
    """ { config with MaxFunctionBindingWidth = 120 }
    |> prepend newline
    |> should equal """
namespace global

type SomeType() =
    member this.Print() = global.System.Console.WriteLine("Hello World!")
"""

[<Test>]
let ``should escape keywords correctly``() =
    formatSourceString false """
module ``member``

let ``abstract`` = "abstract"

type SomeType() =
    member this.``new``() = 
        System.Console.WriteLine("Hello World!")
    """ { config with MaxFunctionBindingWidth = 120 }
    |> prepend newline
    |> should equal """
module ``member``

let ``abstract`` = "abstract"

type SomeType() =
    member this.``new``() = System.Console.WriteLine("Hello World!")
"""

[<Test>]
let ``should escape base keyword correctly``() =
    formatSourceString false """
open System
open RDotNet
open RDotNet.NativeLibrary
open RDotNet.Internals
open RProvider
open RProvider.``base``
open RProvider.stats

[<EntryPoint>]
let main argv = 
    let a = R.rnorm(1000)
    0
    """ config
    |> prepend newline
    |> should equal """
open System
open RDotNet
open RDotNet.NativeLibrary
open RDotNet.Internals
open RProvider
open RProvider.``base``
open RProvider.stats

[<EntryPoint>]
let main argv =
    let a = R.rnorm (1000)
    0
"""

[<Test>]
let ``should retain rec in modules``() =
    formatSourceString false """
module rec Test =
    let test = 42
    """ config
    |> prepend newline
    |> should equal """
module rec Test =
    let test = 42
"""

[<Test>]
let ``should retain order when access and rec present in module declaration``() =
    formatSourceString false """
module private rec Test =
    let test = 42
    """ config
    |> prepend newline
    |> should equal """
module private rec Test =
    let test = 42
"""

[<Test>]
let ``Implicit module should not be added to code`` () =
    let fileName = "60Seconds.fsx"
    let sourceCode = """open System

type T() =
    interface IDisposable with
        override x.Dispose() = ()"""
    
    Fantomas.CodeFormatter.FormatDocumentAsync(fileName, SourceOrigin.SourceString sourceCode, config,
                                               FakeHelpers.createParsingOptionsFromFile fileName, sharedChecker.Value)
    |> Async.RunSynchronously
    |> fun s -> s.Replace("\r\n", "\n")
    |> should equal """open System

type T() =
    interface IDisposable with
        override x.Dispose() = ()
"""

[<Test>]
let ``attribute on module after namespace`` () =
    formatSourceString false """namespace SomeNamespace

[<AutoOpen>]
module Types =
    let a = 5
"""  config
    |> prepend newline
    |> should equal """
namespace SomeNamespace

[<AutoOpen>]
module Types =
    let a = 5
"""

[<Test>]
let ``single line and multiline module decls`` () =
    formatSourceString false """let a =  5
let b =  8
type Model =
    { ActiveTab : ActiveTab
      Trivia : Trivia list
      TriviaNodes: TriviaNode list
      Exception: exn option
      IsLoading: bool
      ActiveByTriviaNodeIndex: int
      ActiveByTriviaIndex: int
      Defines: string
      FSCVersion: string
      IsFsi: bool
      KeepNewlineAfter: bool }
type UrlModel =
    { IsFsi: bool
      KeepNewlineAfter: bool
      Defines: string }
"""  config
    |> prepend newline
    |> should equal """
let a = 5
let b = 8

type Model =
    { ActiveTab: ActiveTab
      Trivia: Trivia list
      TriviaNodes: TriviaNode list
      Exception: exn option
      IsLoading: bool
      ActiveByTriviaNodeIndex: int
      ActiveByTriviaIndex: int
      Defines: string
      FSCVersion: string
      IsFsi: bool
      KeepNewlineAfter: bool }

type UrlModel =
    { IsFsi: bool
      KeepNewlineAfter: bool
      Defines: string }
"""

[<Test>]
let ``single line and multiline module decls with newline trivia`` () =
    formatSourceString false """let a =  5
let b =  8

type Model =
    { ActiveTab : ActiveTab
      Trivia : Trivia list
      TriviaNodes: TriviaNode list
      Exception: exn option
      IsLoading: bool
      ActiveByTriviaNodeIndex: int
      ActiveByTriviaIndex: int
      Defines: string
      FSCVersion: string
      IsFsi: bool
      KeepNewlineAfter: bool }

type UrlModel =
    { IsFsi: bool
      KeepNewlineAfter: bool
      Defines: string }
"""  config
    |> prepend newline
    |> should equal """
let a = 5
let b = 8

type Model =
    { ActiveTab: ActiveTab
      Trivia: Trivia list
      TriviaNodes: TriviaNode list
      Exception: exn option
      IsLoading: bool
      ActiveByTriviaNodeIndex: int
      ActiveByTriviaIndex: int
      Defines: string
      FSCVersion: string
      IsFsi: bool
      KeepNewlineAfter: bool }

type UrlModel =
    { IsFsi: bool
      KeepNewlineAfter: bool
      Defines: string }
"""

[<Test>]
let ``comment is first trivia in module should not add newline, 784`` () =
    formatSourceString false """
module foo

// bar
// baz
"""  config
    |> prepend newline
    |> should equal """
module foo

// bar
// baz
"""

[<Test>]
let ``comment is first trivia in module in signature file should not add newline, 784`` () =
    formatSourceString true """
module foo

// bar
// baz
"""  config
    |> prepend newline
    |> should equal """
module foo

// bar
// baz
"""

[<Test>]
let ``comment is first trivia in namespace should not add newline, 784`` () =
    formatSourceString false """
namespace foo.quz

// bar
// baz
"""  config
    |> prepend newline
    |> should equal """
namespace foo.quz

// bar
// baz
"""

[<Test>]
let ``comment is first trivia in namespace in signature file should not add newline, 784`` () =
    formatSourceString true """
namespace foo.quz

// bar
// baz
"""  config
    |> prepend newline
    |> should equal """
namespace foo.quz

// bar
// baz
"""
