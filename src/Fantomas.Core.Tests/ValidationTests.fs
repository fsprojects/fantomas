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

[<Test>]
let ``use binding at the top level of a script is valid, 3478`` () =
    // The parser warns that a top-level `use` is treated as `let`, which is a remark about the
    // author's source and nothing Fantomas changed.
    isValidFSharpCode
        false
        """
use model = new System.IO.MemoryStream()
printfn "%d" model.Length
"""
    |> should equal true

// What the verdict is built from. `isValidFSharpCode` above reads `IsValid` off the same result, so
// these are about the half of it a caller could not see before.

let private validate (isSignature: bool) (source: string) : Fantomas.Core.ValidationResult =
    Fantomas.Core.CodeFormatter.ValidateFSharpCodeAsync(isSignature, source)
    |> Async.RunSynchronously

[<Test>]
let ``source Fantomas accepts has nothing to report about it`` () =
    let result = validate false "let a = 1\n"

    result.IsValid |> should equal true
    result.Diagnostics |> should be Empty

[<Test>]
let ``source Fantomas refuses says what it refused`` () =
    let result = validate false "let a = (1\n"

    result.IsValid |> should equal false
    result.Diagnostics |> should not' (be Empty)

    // Positioned, because positioning it against the source is the whole reason a caller asks.
    let diagnostic = List.head result.Diagnostics
    diagnostic.Range |> should not' (equal None)

[<Test>]
let ``a warning Fantomas tolerates is not a reason to refuse, 3396`` () =
    // The set of tolerated warnings is what makes `IsValid` more than "the parser had nothing to
    // say", and the diagnostics have to be filtered by it too, or a report points at a warning that
    // was never the reason.
    let result =
        validate
            false
            """
type IWSAMTest<'e> =
    static abstract member Test: int -> 'e
"""

    result.IsValid |> should equal true
    result.Diagnostics |> should be Empty

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

// A case without fields is an instance of the union itself rather than a class of its own, so this
// is the case only F# reflection can name.
[<Test>]
let ``UnionCase.name names a case without fields`` () =
    Fantomas.Core.UnionCase.name Fantomas.FCS.Syntax.SynTypeDefnKind.Unspecified
    |> should equal "SynTypeDefnKind.Unspecified"

// `UnionCase.name` reads a case with fields off the class the compiler makes for it, which is
// nested in the union and named after the case, because F# reflection may be trimmed away under
// Native AOT. This holds it to what F# reflection says for every case of every union either
// assembly has, so a union the compiler lays out some other way cannot go unnoticed.
[<Test>]
let ``UnionCase.name agrees with F# reflection for every case of every union`` () =
    let flags: System.Reflection.BindingFlags =
        System.Reflection.BindingFlags.Public
        ||| System.Reflection.BindingFlags.NonPublic

    let unionTypes: System.Type list =
        [
            typeof<Fantomas.FCS.Syntax.SynExpr>.Assembly
            typeof<Fantomas.Core.SyntaxOak.Expr>.Assembly
        ]
        |> List.collect (fun assembly -> List.ofArray (assembly.GetTypes()))
        |> List.filter (fun (t: System.Type) ->
            not t.ContainsGenericParameters
            && Microsoft.FSharp.Reflection.FSharpType.IsUnion(t, flags)
            // The class of a single case counts as a union to F# reflection as well.
            && (isNull t.BaseType
                || not (Microsoft.FSharp.Reflection.FSharpType.IsUnion(t.BaseType, flags)))
        )

    let mismatches: string list =
        [
            for unionType in unionTypes do
                for case in Microsoft.FSharp.Reflection.FSharpType.GetUnionCases(unionType, flags) do
                    let fields: obj array =
                        case.GetFields()
                        |> Array.map (fun (field: System.Reflection.PropertyInfo) ->
                            if field.PropertyType.IsValueType then
                                System.Activator.CreateInstance field.PropertyType
                            else
                                null
                        )

                    let value: obj =
                        Microsoft.FSharp.Reflection.FSharpValue.MakeUnion(case, fields, flags)

                    let expected: string = $"%s{unionType.Name}.%s{case.Name}"

                    // A case represented by null, as `None` is, has nothing to read a name off.
                    if not (isNull value) then
                        let actual: string = Fantomas.Core.UnionCase.name value

                        if actual <> expected then
                            yield $"%s{expected} was named %s{actual}"
        ]

    List.length unionTypes |> should be (greaterThan 100)
    mismatches |> should be Empty
