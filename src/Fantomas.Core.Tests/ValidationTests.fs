module Fantomas.Core.Tests.ValidationTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers

[<Test>]
let ``naked ranges are valid outside for..in.do`` () =
    isValidFSharpCode
        false
        """
let factors number = 2L..number / 2L
                     |> Seq.filter (fun x -> number % x = 0L)"""
    |> should equal true

[<Test>]
let ``misplaced comments should give parser errors`` () =
    isValidFSharpCode
        false
        """
module ServiceSupportMethods =
    let toDisposable (xs : seq<'t // Sleep to give time for printf to succeed
                                  when 't :> IDisposable>) =
        { new IDisposable with
              member x.Dispose() = xs |> Seq.iter (fun x -> x.Dispose()) }"""
    |> should equal false

[<Test>]
let ``should fail on uncompilable extern functions`` () =
    isValidFSharpCode
        false
        """
[<System.Runtime.InteropServices.DllImport("user32.dll")>]
let GetWindowLong hwnd : System.IntPtr, index : int : int = failwith )"""
    |> should equal false

[<Test>]
let ``interface with static abstract members is valid, 3396`` () =
    isValidFSharpCode
        false
        """
type IWSAMTest<'e> =
    static abstract member Test: int -> 'e
"""
    |> should equal true

[<Test>]
let ``interface with static abstract members is valid in a signature file`` () =
    isValidFSharpCode
        true
        """
module Foo

type IWSAMTest<'e> =
    static abstract member Test: int -> 'e
"""
    |> should equal true

// InvariantViolationException marks a state the transformer's own model says is impossible.
// It must derive from FormatException: the CLI matches on that type to decide what to print,
// and anything else falls through to an empty message at normal verbosity.

let private sampleRange =
    Fantomas.FCS.Text.Range.mkRange
        "Sample.fs"
        (Fantomas.FCS.Text.Position.mkPos 7 4)
        (Fantomas.FCS.Text.Position.mkPos 7 20)

[<Test>]
let ``InvariantViolationException is reported as a FormatException`` () =
    let ex = Fantomas.Core.InvariantViolationException("chain head is Foo", sampleRange)
    ex |> should be instanceOfType<Fantomas.Core.FormatException>

[<Test>]
let ``InvariantViolationException keeps the bare invariant and points at the issue tracker`` () =
    let ex = Fantomas.Core.InvariantViolationException("chain head is Foo", sampleRange)
    ex.Invariant |> should equal "chain head is Foo"
    ex.Message |> should haveSubstring "chain head is Foo"
    ex.Message |> should haveSubstring "fsprojects.github.io/fantomas-tools"

[<Test>]
let ``InvariantViolationException reports where in the source the violation happened`` () =
    let ex = Fantomas.Core.InvariantViolationException("chain head is Foo", sampleRange)
    ex.Range |> should equal sampleRange
    // The location has to survive into the message, because that is all the CLI prints.
    ex.Message |> should haveSubstring "line 7"
    ex.Message |> should haveSubstring "column 4"
    ex.Message |> should haveSubstring "Sample.fs"

// The invariant stays on one line and the source is not quoted into it: positioning the violation
// against the source is the reporter's job, and the reporter that draws a parse failure does it.
[<Test>]
let ``InvariantViolationException keeps the invariant on one line`` () =
    let ex =
        Fantomas.Core.InvariantViolationException(
            "no Oak node is defined for this type: SynType.App",
            sampleRange,
            "App (LongIdent ...)"
        )

    ex.Invariant |> should equal "no Oak node is defined for this type: SynType.App"

[<Test>]
let ``InvariantViolationException keeps the syntax tree node off the message`` () =
    let ex =
        Fantomas.Core.InvariantViolationException("chain head is Foo", sampleRange, "App (LongIdent ...)")

    ex.SyntaxNode |> should equal "App (LongIdent ...)"
    ex.Message |> should not' (haveSubstring "App (LongIdent ...)")

[<Test>]
let ``InvariantViolationException carries no syntax tree node when it was not given one`` () =
    let ex = Fantomas.Core.InvariantViolationException("chain head is Foo", sampleRange)

    ex.SyntaxNode |> should equal ""

// Naming the union case is what replaces the %A dump of a syntax tree node in an error message.
[<Test>]
let ``UnionCase.name qualifies the case with the type it belongs to`` () =
    let t: Fantomas.FCS.Syntax.SynType =
        Fantomas.FCS.Syntax.SynType.Anon(Fantomas.FCS.Text.Range.range0)

    Fantomas.Core.UnionCase.name t |> should equal "SynType.Anon"

[<Test>]
let ``UnionCase.name falls back to the type name for something that is not a union`` () =
    Fantomas.Core.UnionCase.name 42 |> should equal "Int32"
