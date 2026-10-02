module Fantomas.Core.Tests.FormatAstTests

open Fantomas.FCS.Text
open Fantomas.FCS.Syntax
open Fantomas.FCS.SyntaxTrivia
open Fantomas.FCS.Xml
open NUnit.Framework
open FsUnit
open Fantomas.Core
open Fantomas.Core.Tests.TestHelpers

let parseAndFormat sourceCode =
    let ast =
        CodeFormatter.ParseAsync(false, source = sourceCode)
        |> Async.RunSynchronously
        |> Array.head
        |> fst

    let config =
        { config with
            MultilineBracketStyle = Cramped
        }

    let formattedCode =
        CodeFormatter.FormatASTAsync(ast, source = sourceCode, config = config)
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

[<Test>]
let ``attribute above extern keyword, 562`` () =
    formatAST
        false
        """
module C =
  [<DllImport("")>]
  extern IntPtr f()
"""
        config
    |> prepend newline
    |> should
        equal
        """
module C =
    [<DllImport("")>]
    extern IntPtr f()
"""

[<Test>]
let ``interpolation in strict mode`` () =
    formatAST
        false
        """
let text = "foo"
let s = $"%s{text} bar"
"""
        config
    |> prepend newline
    |> should
        equal
        """
let text = "foo"
let s = $"%s{text} bar"
"""

[<Test>]
let ``interpolation from AST with multiple fillExprs`` () =
    formatAST
        false
        """
$"%s{text} %i{bar} %f{meh}"
"""
        config
    |> prepend newline
    |> should
        equal
        """
$"%s{text} %i{bar} %f{meh}"
"""

[<Test>]
let ``alignment in string interpolation from AST`` () =
    formatAST
        false
        """
$"{x,10:N2} then {y}"
"""
        config
    |> prepend newline
    |> should
        equal
        """
$"{x, 10:N2} then {y}"
"""

// Without the source text, a number is printed from its value. Each must come back as the same type
// and, for a float, the same number.
[<Test>]
let ``numeric literals without the source text`` () =
    formatAST
        false
        """
let a = 1uy
let b = 1s
let c = -1
let d = 1L
let e = 1u
let f = 1UL
let g = 1n
let h = 1un
let i = 2.
let j = 0.30000000000000004
let k = 123456789012.0
let l = 1e-7
let m = 3.1415927f
let n = 2.0M
let o = 0.10m
    """
        config
    |> prepend newline
    |> should
        equal
        """
let a = 1uy
let b = 1s
let c = -1
let d = 1L
let e = 1u
let f = 1UL
let g = 1n
let h = 1un
let i = 2.0
let j = 0.30000000000000004
let k = 123456789012.0
let l = 1e-07
let m = 3.1415927f
let n = 2.0M
let o = 0.10M
"""

[<Test>]
let ``uncommon literals strict mode`` () =
    formatAST
        false
        """
let a = 0xFFy
let c = 0b0111101us
let d = 0o0777
let e = 1.40e10f
let f = 23.4M
let g = '\n'
    """
        config
    |> prepend newline
    |> should
        equal
        """
let a = -1y
let c = 61us
let d = 511
let e = 1.4e+10f
let f = 23.4M
let g = '\n'
"""

[<Test>]
let ``quotes should be escaped in strict mode`` () =
    formatAST
        false
        """
    let formatter =
        // escape commas left in invalid entries
        sprintf "%i,\"%s\""
"""
        config
    |> should
        equal
        """let formatter = sprintf "%i,\"%s\""
"""

[<Test>]
let ``character quotes should be preserved, 3076`` () =
    formatAST false "let s = 'A'" config |> should equal "let s = 'A'\n"

[<Test>]
let ``verbatim string in AST is preserved, 560`` () =
    formatAST
        false
        """
let s = @"\"
"""
        config
    |> prepend newline
    |> should
        equal
        """
let s = @"\"
"""

[<Test>]
let ``enums conversion with strict mode`` () =
    formatAST
        false
        """
type uColor =
   | Red = 0u
   | Green = 1u
   | Blue = 2u
let col3 = Microsoft.FSharp.Core.LanguagePrimitives.EnumOfValue<uint32, uColor>(2u)"""
        config
    |> prepend newline
    |> should
        equal
        """
type uColor =
    | Red = 0u
    | Green = 1u
    | Blue = 2u

let col3 = Microsoft.FSharp.Core.LanguagePrimitives.EnumOfValue<uint32, uColor>(2u)
"""

[<Test>]
let ``backticks can be added from AST only scenarios`` () =
    let tree =
        let testIdent = Ident("Test", Range.range0)

        ParsedInput.ImplFile(
            ParsedImplFileInput(
                "Test.fsx",
                true,
                QualifiedNameOfFile testIdent,
                [],
                [
                    SynModuleOrNamespace(
                        [ testIdent ],
                        false,
                        SynModuleOrNamespaceKind.AnonModule,
                        [
                            SynModuleDecl.Expr(
                                SynExpr.LongIdent(
                                    false,
                                    SynLongIdent(
                                        [ Ident("new", Range.range0) ],
                                        [],
                                        [ Some(IdentTrivia.OriginalNotation "``new``") ]
                                    ),
                                    None,
                                    Range.range0
                                ),
                                Range.range0
                            )
                        ],
                        PreXmlDoc.Empty,
                        [],
                        None,
                        Range.range0,
                        {
                            LeadingKeyword = SynModuleOrNamespaceLeadingKeyword.None
                        }
                    )
                ],
                (true, false),
                ParsedInputTrivia.Empty,
                Set.empty
            )
        )

    CodeFormatter.FormatASTAsync(
        tree,
        config =
            { config with
                InsertFinalNewline = false
            }
    )
    |> Async.RunSynchronously
    |> should equal "``new``"
