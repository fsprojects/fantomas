module Fantomas.Core.Tests.FormatAstTests

open NUnit.Framework
open FsUnit
open Fantomas.Core
open Fantomas.Core.Tests.TestHelper

let parseAndFormat sourceCode =
    let ast =
        CodeFormatter.ParseAsync(false, source = sourceCode)
        |> Async.RunSynchronously
        |> Seq.head
        |> fst

    let formattedCode =
        CodeFormatter.FormatASTAsync(ast, source = sourceCode)
        |> Async.RunSynchronously
        |> String.normalizeNewLine
        |> fun s -> s.TrimEnd('\n')

    formattedCode

let formatAstWithSourceCode code = parseAndFormat code
let formatAst code = parseAndFormat code

[<Test>]
let ``format the ast works correctly with no source code`` () = formatAst "()" |> should equal "()"

[<Test>]
let ``let in should be used`` () =
    formatAst "let x = 1 in ()" |> should equal """let x = 1 in ()"""

[<Test>]
let ``elif keyword is present in raw AST`` () =
    let source =
        """
    if a then ()
    elif b then ()
    else ()"""

    formatAst source
    |> should
        equal
        """if a then ()
elif b then ()
else ()"""

/// There is no dead code in this test
/// The trivia (newline on line 2) is kept in tact after formatting

[<Test>]
let ``create F# code with existing AST and source code`` () =
    """let a =   42

let b =   1"""
    |> formatAstWithSourceCode
    |> should
        equal
        """let a = 42

let b = 1"""

[<Test>]
let ``default implementations in abstract classes should be emited as override from AST without origin source, 742``
    ()
    =
    """[<AbstractClass>]
type Foo =
    abstract foo: int
    default __.foo = 1"""
    |> formatAst
    |> should
        equal
        """[<AbstractClass>]
type Foo =
    abstract foo: int
    default __.foo = 1"""

[<Test>]
let ``default implementations in abstract classes with `default` keyword should be emited as it was before from AST with origin source, 742``
    ()
    =
    """[<AbstractClass>]
type Foo =
    abstract foo: int
    default __.foo = 1"""
    |> formatAstWithSourceCode
    |> should
        equal
        """[<AbstractClass>]
type Foo =
    abstract foo: int
    default __.foo = 1"""

[<Test>]
let ``default implementations in abstract classes with `override` keyword should be emitted as it was before from AST with origin source, 742``
    ()
    =
    """[<AbstractClass>]
type Foo =
    abstract foo: int
    override __.foo = 1"""
    |> formatAstWithSourceCode
    |> should
        equal
        """[<AbstractClass>]
type Foo =
    abstract foo: int
    override __.foo = 1"""

[<Test>]
let ``object expression should emit override keyword on AST formatting without origin source, 742`` () =
    """{ new System.IDisposable with
    member __.Dispose() = () }"""
    |> formatAst
    |> should
        equal
        """{ new System.IDisposable with
    member __.Dispose() = () }"""

[<Test>]
let ``object expression should preserve member keyword on AST formatting with origin source, 742`` () =
    """{ new System.IDisposable with
    member __.Dispose() = () }"""
    |> formatAstWithSourceCode
    |> should
        equal
        """{ new System.IDisposable with
    member __.Dispose() = () }"""

[<Test>]
let ``object expression should preserve override keyword on AST formatting with origin source, 742`` () =
    """{ new System.IDisposable with
    override __.Dispose() = () }"""
    |> formatAstWithSourceCode
    |> should
        equal
        """{ new System.IDisposable with
    override __.Dispose() = () }"""
