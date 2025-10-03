module Fantomas.Core.Tests.ModuleTests

open NUnit.Framework
open FsUnit
open Fantomas.Core
open Fantomas.Core.Tests.TestHelpers

[<Test>]
let ``module abbreviation`` () =
    formatSourceString "module ES = Microsoft.FSharp.Quotations.ExprShape" config
    |> should
        equal
        """module ES = Microsoft.FSharp.Quotations.ExprShape
"""

[<Test>]
let ``module with functions`` () =
    formatSourceString "module internal MyModule = let x = 42" config
    |> prepend newline
    |> should
        equal
        """
module internal MyModule =
    let x = 42
"""

[<Test>]
let ``open modules`` () =
    formatSourceString
        """
    // comment1
    open System.IO
    // comment2
    open System"""
        config
    |> prepend newline
    |> should
        equal
        """
// comment1
open System.IO
// comment2
open System
"""

[<Test>]
let ``sort open modules doesn't mess comments up`` () =
    formatSourceString
        """
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
    l |> Seq.distinctBy by |> Seq.sortBy by |> List.ofSeq"""
        { config with
            MaxInfixOperatorExpression = 50 }
    |> prepend newline
    |> should
        equal
        """
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
let ``nested modules`` () =
    formatSourceString
        """
module Y =
    let x = 1

    module Z =
        let z = 5"""
        config
    |> prepend newline
    |> should
        equal
        """
module Y =
    let x = 1

    module Z =
        let z = 5
"""

[<Test>]
let ``sibling modules`` () =
    formatSourceString
        """
module TopLevel

let topLevelX = 5

module Inner1 =
    let inner1X = 1
module Inner2 =
    let inner2X = 5"""
        config
    |> prepend newline
    |> should
        equal
        """
module TopLevel

let topLevelX = 5

module Inner1 =
    let inner1X = 1

module Inner2 =
    let inner2X = 5
"""

[<Test>]
let ``module signatures`` () =
    formatSignatureString
        """
module Utils

val turnTracingOn : unit -> unit
val turnTracingOff : unit -> unit
val isTraced : unit -> bool

module Random = begin
    val exponential : mean:float -> float
    val nextInt : max:int -> int
    val nextInt64 : max:int64 -> int64
    val next : max:float -> float
end"""
        config
    |> prepend newline
    |> should
        equal
        """
module Utils

val turnTracingOn: unit -> unit
val turnTracingOff: unit -> unit
val isTraced: unit -> bool

module Random =
    val exponential: mean: float -> float
    val nextInt: max: int -> int
    val nextInt64: max: int64 -> int64
    val next: max: float -> float
"""

[<Test>]
let ``namespace declaration`` () =
    formatSourceString
        """
namespace Widgets

type MyWidget1 =
    member this.WidgetName = "Widget1"

module WidgetsModule =
    let widgetName = "Widget2"
    """
        config
    |> prepend newline
    |> should
        equal
        """
namespace Widgets

type MyWidget1 =
    member this.WidgetName = "Widget1"

module WidgetsModule =
    let widgetName = "Widget2"
"""

[<Test>]
let ``should retain rec in namespace`` () =
    formatSourceString
        """
namespace rec Test

type Add = Expr * Expr

type Expr =
    | Add of Add
    | Value of int
    """
        config
    |> prepend newline
    |> should
        equal
        """
namespace rec Test

type Add = Expr * Expr

type Expr =
    | Add of Add
    | Value of int
"""

[<Test>]
let ``should retain rec in nested module`` () =
    formatSourceString
        """
namespace Test

module rec Expression =
    type Add = Expr * Expr

    type Expr =
        | Add of Add
        | Value of int
    """
        config
    |> prepend newline
    |> should
        equal
        """
namespace Test

module rec Expression =
    type Add = Expr * Expr

    type Expr =
        | Add of Add
        | Value of int
"""

[<Test>]
let ``should preserve global keyword`` () =
    formatSourceString
        """
namespace global

type SomeType() =
    member this.Print() =
        global.System.Console.WriteLine("Hello World!")
    """
        { config with
            MaxFunctionBindingWidth = 120 }
    |> prepend newline
    |> should
        equal
        """
namespace global

type SomeType() =
    member this.Print() = global.System.Console.WriteLine("Hello World!")
"""

[<Test>]
let ``should escape keywords correctly`` () =
    formatSourceString
        """
module ``member``

let ``abstract`` = "abstract"

type SomeType() =
    member this.``new``() =
        System.Console.WriteLine("Hello World!")
    """
        { config with
            MaxFunctionBindingWidth = 120 }
    |> prepend newline
    |> should
        equal
        """
module ``member``

let ``abstract`` = "abstract"

type SomeType() =
    member this.``new``() = System.Console.WriteLine("Hello World!")
"""

[<Test>]
let ``should escape base keyword correctly`` () =
    formatSourceString
        """
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
    """
        config
    |> prepend newline
    |> should
        equal
        """
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
let ``should retain rec in modules`` () =
    formatSourceString
        """
module rec Test =
    let test = 42
    """
        config
    |> prepend newline
    |> should
        equal
        """
module rec Test =
    let test = 42
"""

[<Test>]
let ``should retain order when access and rec present in module declaration`` () =
    formatSourceString
        """
module private rec Test =
    let test = 42
    """
        config
    |> prepend newline
    |> should
        equal
        """
module private rec Test =
    let test = 42
"""

[<Test>]
let ``implicit module should not be added to code`` () =
    let sourceCode =
        """open System

type T() =
    interface IDisposable with
        override x.Dispose() = ()"""

    CodeFormatter.FormatDocumentAsync(false, sourceCode, config)
    |> Async.RunSynchronously
    |> fun s -> s.Code.Replace("\r\n", "\n")
    |> should
        equal
        """open System

type T() =
    interface IDisposable with
        override x.Dispose() = ()
"""

[<Test>]
let ``attribute on module after namespace`` () =
    formatSourceString
        """namespace SomeNamespace

[<AutoOpen>]
module Types =
    let a = 5
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace SomeNamespace

[<AutoOpen>]
module Types =
    let a = 5
"""

[<Test>]
let ``single line and multiline module decls`` () =
    formatSourceString
        """let a =  5
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
"""
        config
    |> prepend newline
    |> should
        equal
        """
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
    formatSourceString
        """let a =  5
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
"""
        config
    |> prepend newline
    |> should
        equal
        """
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
    formatSourceString
        """
module foo

// bar
// baz
"""
        config
    |> prepend newline
    |> should
        equal
        """
module foo

// bar
// baz
"""

[<Test>]
let ``comment is first trivia in module in signature file should not add newline, 784`` () =
    formatSignatureString
        """
module foo

// bar
// baz
"""
        config
    |> prepend newline
    |> should
        equal
        """
module foo

// bar
// baz
"""

[<Test>]
let ``comment is first trivia in namespace should not add newline, 784`` () =
    formatSourceString
        """
namespace foo.quz

// bar
// baz
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace foo.quz

// bar
// baz
"""

[<Test>]
let ``comment is first trivia in namespace in signature file should not add newline, 784`` () =
    formatSignatureString
        """
namespace foo.quz

// bar
// baz
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace foo.quz

// bar
// baz
"""

[<Test>]
let ``don't add extra new lines between comments and attributes, 1108`` () =
    formatSourceString
        """
namespace Foo

// First
[<someAnnotation>]

// Second
[<someAnnotation>]

// Third
[<someAnnotation>]

do ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace Foo

// First
[<someAnnotation>]

// Second
[<someAnnotation>]

// Third
[<someAnnotation>]

do ()
"""

[<Test>]
let ``keep correct indentation for let binding inside nested module, 1122`` () =
    formatSourceString
        """
namespace Test

module App =
    type Msg = B of C

    let a = "test"
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace Test

module App =
    type Msg = B of C

    let a = "test"
"""

[<Test>]
let ``keep correct indentation for let binding inside nested module, signature file`` () =
    formatSignatureString
        """
namespace Test

module App =
    type Msg = B of C

    val a : string
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace Test

module App =
    type Msg = B of C

    val a: string
"""

[<Test>]
let ``keep correct indentation for let binding after match lambda inside nested module, 2214`` () =
    formatSourceString
        """
module Outer

let sort fallback (f: int -> string list) = ()

module Inner =

    let f =
        sort
            "Name"
            (function
             | 1 -> ["One"]
             | _ -> ["Not One"])

    let g () = 23
"""
        config
    |> prepend newline
    |> should
        equal
        """
module Outer

let sort fallback (f: int -> string list) = ()

module Inner =

    let f =
        sort "Name" (function
            | 1 -> [ "One" ]
            | _ -> [ "Not One" ])

    let g () = 23
"""

[<Test>]
let ``nested nested module with single union DU, 1123`` () =
    formatSourceString
        """
module Test =
  module Foo =
    type t = T of bool
    let foo = true
"""
        config
    |> prepend newline
    |> should
        equal
        """
module Test =
    module Foo =
        type t = T of bool
        let foo = true
"""

[<Test>]
let ``always add new line between named module and first declaration, 1139`` () =
    formatSourceString
        """
module Input
    let modules = [109024;137172;80445;80044]
"""
        config
    |> prepend newline
    |> should
        equal
        """
module Input

let modules = [ 109024; 137172; 80445; 80044 ]
"""

[<Test>]
let ``comment after equals sign in named module`` () =
    formatSourceString
        """
module Foo =   // comment
    let bar = 9
"""
        config
    |> prepend newline
    |> should
        equal
        """
module Foo = // comment
    let bar = 9
"""

[<Test>]
let ``comment after equals sign in named module, signature file`` () =
    formatSignatureString
        """
namespace Meh

module Foo =   // comment
    val bar : int
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace Meh

module Foo = // comment
    val bar: int
"""

[<Test>]
let ``comment above named module with xml doc, 2141`` () =
    formatSourceString
        """
// Boring copyright notice

(* Some other preamble *)

/// This module is amazing it's full of helpful lookup queries
module Queries
"""
        config
    |> prepend newline
    |> should
        equal
        """
// Boring copyright notice

(* Some other preamble *)

/// This module is amazing it's full of helpful lookup queries
module Queries
"""

[<Test>]
let ``comment before declared namespace`` () =
    formatSourceString
        """
// some comment
namespace Blah
let a = 0
"""
        config
    |> prepend newline
    |> should
        equal
        """
// some comment
namespace Blah

let a = 0
"""

[<Test>]
let ``comment before global namespace`` () =
    formatSourceString
        """
// some comment
namespace global
let a = 0
"""
        config
    |> prepend newline
    |> should
        equal
        """
// some comment
namespace global

let a = 0
"""

[<Test>]
let ``comment before declared namespace in signature file`` () =
    formatSignatureString
        """
// some comment
namespace Blah
val a : int
"""
        config
    |> prepend newline
    |> should
        equal
        """
// some comment
namespace Blah

val a: int
"""

[<Test>]
let ``comment before global namespace in signature file`` () =
    formatSignatureString
        """
// some comment
namespace global
val a : int
"""
        config
    |> prepend newline
    |> should
        equal
        """
// some comment
namespace global

val a: int
"""

[<Test>]
let ``comment before named module in signature file`` () =
    formatSignatureString
        """
// some comment
module Meh
val a : int
"""
        config
    |> prepend newline
    |> should
        equal
        """
// some comment
module Meh

val a: int
"""

[<Test>]
let ``xml comment above module with nested module`` () =
    formatSourceString
        """
/// this file contains patches to the F# Compiler Service that have not yet made it into
/// published nuget packages.  We source-copy them here to have a consistent location for our to-be-removed extensions

module FsAutoComplete.FCSPatches

open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FsAutoComplete.UntypedAstUtils
open FSharp.Compiler.CodeAnalysis

module internal SynExprAppLocationsImpl =
    let a = 42
"""
        config
    |> prepend newline
    |> should
        equal
        """
/// this file contains patches to the F# Compiler Service that have not yet made it into
/// published nuget packages.  We source-copy them here to have a consistent location for our to-be-removed extensions

module FsAutoComplete.FCSPatches

open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FsAutoComplete.UntypedAstUtils
open FSharp.Compiler.CodeAnalysis

module internal SynExprAppLocationsImpl =
    let a = 42
"""

[<Test>]
let ``xml comment above namespace with nested module`` () =
    formatSourceString
        """
/// this file contains patches to the F# Compiler Service that have not yet made it into
/// published nuget packages.  We source-copy them here to have a consistent location for our to-be-removed extensions

namespace FsAutoComplete.FCSPatches

open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FsAutoComplete.UntypedAstUtils
open FSharp.Compiler.CodeAnalysis

module internal SynExprAppLocationsImpl =
    let a = 42
"""
        config
    |> prepend newline
    |> should
        equal
        """
/// this file contains patches to the F# Compiler Service that have not yet made it into
/// published nuget packages.  We source-copy them here to have a consistent location for our to-be-removed extensions

namespace FsAutoComplete.FCSPatches

open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FsAutoComplete.UntypedAstUtils
open FSharp.Compiler.CodeAnalysis

module internal SynExprAppLocationsImpl =
    let a = 42
"""

[<Test>]
let ``global keyword in open statement, 2366`` () =
    formatSourceString
        """
namespace Ionide.VSCode.FSharp

open global.Node
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace Ionide.VSCode.FSharp

open global.Node
"""

[<Test>]
let ``global keyword in open statement, signature file`` () =
    formatSignatureString
        """
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Ionide.VSCode.FSharp

open global.Node.ChildProcess
"""
        config
    |> prepend newline
    |> should
        equal
        """
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Ionide.VSCode.FSharp

open global.Node.ChildProcess
"""

[<Test>]
let ``empty nested module, 2721`` () =
    formatSignatureString
        """
module Graph =
    begin end
"""
        config
    |> prepend newline
    |> should
        equal
        """
module Graph =
    begin end
"""

[<Test>]
let ``module abbreviation followed by nested module, 2792`` () =
    formatSignatureString
        """
module Foo

open System
open System.Text

module A = B

module Bar =
    type a =
        | Ex
        | Why
        | Zed
"""
        config
    |> prepend newline
    |> should
        equal
        """
module Foo

open System
open System.Text

module A = B

module Bar =
    type a =
        | Ex
        | Why
        | Zed
"""

[<Test>]
let ``namespace with ticks, 2959`` () =
    formatSourceString
        """
namespace ``G-Research``.``FSharp X``.``Analyzers Y``

module StringAnalyzers =
    ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace ``G-Research``.``FSharp X``.``Analyzers Y``

module StringAnalyzers =
    ()
"""

[<Test>]
let ``hash directives around access modifier in module, 3188`` () =
    formatSourceString
        """
[<RequireQualifiedAccess>]
module
    #if !MCP
    internal
    #endif
        Fantomas.Core.CodeFormatterImpl
"""
        config
    |> prepend newline
    |> should
        equal
        """
[<RequireQualifiedAccess>]
module
#if !MCP
    internal
#endif
        Fantomas.Core.CodeFormatterImpl
"""
