module Fantomas.Core.Tests.AstExtensionsTests

open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open Fantomas.FCS.Parse
open NUnit.Framework
open FsUnit
open Fantomas.Core.AstExtensions

let private getAst (isSignature: bool) (source: string) : ParsedInput =
    let ast, _ = parseFile isSignature (SourceText.ofString source) []
    ast

let private getImplAst = getAst false
let private getSigAst = getAst true

let assertRange
    (expectedStartLine: int, expectedStartColumn: int)
    (expectedEndLine: int, expectedEndColumn: int)
    (actualRange: range)
    : unit =
    Assert.AreEqual(Position.mkPos expectedStartLine expectedStartColumn, actualRange.Start)
    Assert.AreEqual(Position.mkPos expectedEndLine expectedEndColumn, actualRange.End)

[<Test>]
let ``range of anonymous file with leading and trailing blank lines`` () =
    let ast =
        getImplAst
            """





let a = 0

let b = 1



"""

    assertRange (7, 0) (9, 9) ast.FullRange

[<Test>]
[<Ignore "https://github.com/dotnet/fsharp/issues/13205">]
let ``range of anonymous signature with leading and trailing blank lines`` () =
    let ast =
        getSigAst
            """


val x : int

val y : string = ""


"""

    assertRange (4, 0) (6, 19) ast.FullRange

[<Test>]
let ``range of multiple namespaces`` () =
    let ast =
        getImplAst
            """


namespace Foo

namespace Bar

let x = 0


"""

    assertRange (4, 0) (8, 9) ast.FullRange

[<Test>]
let ``range of multiple namespaces in signature files`` () =
    let ast =
        getSigAst
            """


namespace Foo

namespace Bar

val y : int


"""

    assertRange (4, 0) (8, 11) ast.FullRange

[<Test>]
let ``range of parsed hash directives and namespace`` () =
    let ast =
        getImplAst
            """

#nowarn "66"

namespace Foo

let y : string = ""



"""

    assertRange (3, 0) (7, 19) ast.FullRange

[<Test>]
let ``range of parsed hash directives and namespace in signature file`` () =
    let ast =
        getSigAst
            """

#nowarn "66"

namespace Foo

val y : int



"""

    assertRange (3, 0) (7, 11) ast.FullRange
