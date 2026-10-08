module Fantomas.Core.Tests.ValidationTests

open NUnit.Framework
open FsUnit
open Fantomas.Core
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

// Formatting checks its result. Each test below formats a sample Fantomas gets wrong today and
// asserts on what `formatDocument` hands back. The day Fantomas formats one correctly, that test needs
// another sample.

// What the checks found, with each comment as the line it starts on in the source.
type private Found =
    | Missing of line: int * text: string
    | Invalid of defines: string list
    | Comments of defines: string list * missing: (int * string) list * added: string list
    | Directives of defines: string list * missing: string list * added: string list
    | NotIdempotent
    | CheckFailed of check: Validations

let private found (issue: ValidationIssue) : Found =
    let at (comment: SourceComment) : int * string = comment.Range.StartLine, comment.Text

    match issue with
    | ValidationIssue.MissingComment comment -> Missing(at comment)
    | ValidationIssue.NotValidFSharp(defines, _) -> Invalid defines
    | ValidationIssue.CommentsChanged(defines, missing, added) -> Comments(defines, List.map at missing, added)
    | ValidationIssue.DirectivesChanged(defines, missing, added) -> Directives(defines, missing, added)
    | ValidationIssue.NotIdempotent _ -> NotIdempotent
    | ValidationIssue.CheckFailed(check, _) -> CheckFailed check

let private checking (validations: Validations) (source: string) : Found list =
    let document: FormatResult =
        CodeFormatterImpl.formatDocument
            ignore
            FormatConfig.Default
            false
            (CodeFormatterImpl.getSourceText source)
            None
            validations
        |> Async.RunSynchronously

    List.map found document.Issues

let private searchAndComparison: Validations =
    Validations.CommentSearch ||| Validations.TriviaComparison

// A comment between an infix operator and a `let` is dropped, and formatting the result again joins
// it onto one line. Found in the F# compiler's `MethodOverrides.fs`.
let private dropped (comment: string) (binding: string) : string =
    $"a &&\n%s{comment}\nlet c = %s{binding}\nc\n"

// A comment before a parenthesised operand of `||` gives output that is offside. Found in the F#
// compiler's `Optimizer.fs`.
let private invalidOutput: string =
    """let ValueIsUsedOrHasEffect cenv fvs (b: Binding, binfo) =
    let v = b.Var
    // No discarding for debug code, except InlineIfLambda
    (not cenv.settings.EliminateUnusedBindings && not v.InlineIfLambda) ||
    // No discarding for members
    Option.isSome v.MemberInfo ||
    // No discarding for bindings that have an effect
    (binfo.HasEffect && not (IsDiscardableEffectExpr b.Expr)) ||
    // No discarding for 'fixed'
    v.IsFixed ||
    // No discarding for things that are used
    Zset.contains v (fvs())
"""

[<Test>]
let ``a comment the result does not keep is missed by the search and by the comparison`` () =
    checking searchAndComparison (dropped "// c" "b")
    |> should equal [ Missing(2, "// c"); Comments([], [ 2, "// c" ], []) ]

[<Test>]
let ``each check reports only its own issues`` () =
    checking Validations.CommentSearch (dropped "// c" "b")
    |> should equal [ Missing(2, "// c") ]

    checking Validations.None (dropped "// c" "b") |> should be Empty

// The `// x` the result has is the one at the end. Found there for the first, it would leave the
// comments after it nowhere to be found, and three would be missing. The comparison reads the same
// from the order of the comments.
[<Test>]
let ``a lost comment whose text is further on is the only one missing`` () =
    checking searchAndComparison "a &&\n// x\nlet c = b\n// y\nc\n// x\n"
    |> should equal [ Missing(2, "// x"); Comments([], [ 2, "// x" ], []) ]

// The `//` that is dropped is on a line of its own, and the one the result has is beside code, as
// the other `//` of the source is. Their order cannot tell the two apart; where they are does.
[<Test>]
let ``of two comments with the same text, where they are tells which is missing`` () =
    checking searchAndComparison "let g a b =\n    a &&\n    //\n    let c = b\n    c\n\nlet y = 2 //\n"
    |> should equal [ Missing(3, "//"); Comments([], [ 3, "//" ], []) ]

[<Test>]
let ``all is every check`` () =
    System.Enum.GetValues<Validations>()
    |> Array.filter (fun (check: Validations) -> check <> Validations.All)
    |> Array.fold (|||) Validations.None
    |> should equal Validations.All

[<Test>]
let ``a result that formats differently again is not idempotent`` () =
    checking Validations.Idempotency (dropped "// c" "b")
    |> should equal [ NotIdempotent ]

[<Test>]
let ``a rewritten comment is missing, and what the result has instead is added`` () =
    // A `#if` inside a block comment is taken for a directive and moved to column 0.
    let source =
        "module Sample\n\nlet greet name =\n#if DEBUG\n    (*\n        #if VERBOSE\n            printfn \"greeting %s\" name\n        #endif\n    *)\n#endif\n    printfn \"Hello %s\" name\n"

    checking Validations.TriviaComparison source
    |> List.exists (fun (each: Found) ->
        match each with
        | Comments(_, [ 5, missing ], [ added ]) ->
            missing.StartsWith("(*", System.StringComparison.Ordinal)
            && added.StartsWith("(*", System.StringComparison.Ordinal)
        | _ -> false
    )
    |> should equal true

[<Test>]
let ``a lost line comment is not found inside a URL`` () =
    checking Validations.CommentSearch (dropped "//" "\"https://x\"")
    |> should equal [ Missing(2, "//") ]

[<Test>]
let ``a lost line comment is not found inside a string`` () =
    checking Validations.CommentSearch (dropped "// end" "\"// end\"")
    |> should equal [ Missing(2, "// end") ]

[<Test>]
let ``a lost line comment is not found at the start of a longer one, which is not blamed`` () =
    checking Validations.CommentSearch (dropped "//" "b // x")
    |> should equal [ Missing(2, "//") ]

[<Test>]
let ``one of two comments with the same text going missing is noticed`` () =
    checking Validations.CommentSearch ("let x = 1 // c\n\n" + dropped "// c" "b")
    |> should equal [ Missing(4, "// c") ]

[<Test>]
let ``two comments with the same text both going missing are both reported`` () =
    checking Validations.CommentSearch (dropped "// c" "b" + "\n" + dropped "// c" "d")
    |> should equal [ Missing(2, "// c"); Missing(7, "// c") ]

[<Test>]
let ``a comment outside every branch is missed once by the search, and compared under every combination`` () =
    checking searchAndComparison ("#if DEBUG\nlet d = 1\n#endif\n\n" + dropped "// c" "b")
    |> should
        equal
        [
            Missing(6, "// c")
            Comments([], [ 6, "// c" ], [])
            Comments([ "DEBUG" ], [ 6, "// c" ], [])
        ]

[<Test>]
let ``a comment lost from a branch is reported under the defines that keep the branch`` () =
    checking searchAndComparison ("#if FOO\n" + dropped "// c" "b" + "#endif\n")
    |> should equal [ Missing(3, "// c"); Comments([ "FOO" ], [ 3, "// c" ], []) ]

[<Test>]
let ``output that is not valid F# is reported as such, and not compared`` () =
    // Its trivia is read through its Oak, which a tree with parse errors does not have.
    checking (Validations.Parse ||| Validations.TriviaComparison) invalidOutput
    |> should equal [ Invalid [] ]

[<Test>]
let ``output not valid F# under several define combinations is reported under each`` () =
    checking Validations.Parse ("#if DEBUG\nlet debug = true\n#endif\n\n" + invalidOutput)
    |> should equal [ Invalid []; Invalid [ "DEBUG" ] ]

// Formatting it again would only fail on the errors `Parse` reports.
[<Test>]
let ``output that is not valid F# is not formatted again`` () =
    checking Validations.Idempotency invalidOutput |> should be Empty

    checking (Validations.Parse ||| Validations.Idempotency) invalidOutput
    |> should equal [ Invalid [] ]

[<Test>]
let ``output that keeps every comment and directive has nothing to report`` () =
    checking Validations.All "#nowarn \"40\"\n\n// a\nlet a =   1 (* b *)\n"
    |> should be Empty

[<Test>]
let ``line endings inside a comment do not matter`` () =
    checking Validations.All "(*\r\n  a\r\n*)\r\nlet  a = 1\r\n" |> should be Empty

[<Test>]
let ``a line comment with trailing whitespace is found`` () =
    checking Validations.All "let  a = 1 // a   \n" |> should be Empty

[<Test>]
let ``a block comment beside code is found with code after it`` () =
    checking Validations.All "let a =  (* a *) 1\n" |> should be Empty

[<Test>]
let ``a file that is already formatted passes every check`` () =
    checking Validations.All "let a = 1\n" |> should be Empty

[<Test>]
let ``formatting reports a comment it rewrote, by default`` () =
    // A `#if` inside a block comment is taken for a directive and moved to column 0.
    let source =
        "#if FOO\n    (*\n        #if BAR\n                    printfn \"FOO\"\n        #endif\n    *)\n#else\n                ()\n#endif\n"

    CodeFormatter.FormatDocumentAsync(false, source)
    |> Async.RunSynchronously
    |> fun (result: FormatResult) -> List.map found result.Issues
    |> should
        equal
        [
            Missing(2, "(*\n        #if BAR\n                    printfn \"FOO\"\n        #endif\n    *)")
        ]

// The cursor is trivia of the formatted tree, as the comments are, and is no comment of the source.
[<Test>]
let ``a cursor is not taken for a comment, and still moves with the code`` () =
    let result: FormatResult =
        CodeFormatter.FormatDocumentWithValidationsAsync(
            false,
            "let  a =  1 // a\n",
            FormatConfig.Default,
            CodeFormatter.MakePosition(1, 10),
            Validations.All
        )
        |> Async.RunSynchronously

    result.Issues |> should be Empty
    result.Cursor |> should equal (Some(CodeFormatter.MakePosition(1, 8)))

// `val` at the top level is only valid F# in a signature file, so the result is parsed as one.
[<Test>]
let ``a signature file is checked as a signature file`` () =
    CodeFormatter.FormatDocumentWithValidationsAsync(
        true,
        "module M\n\n// a\nval x :  int (* b *)\n",
        FormatConfig.Default,
        Validations.All
    )
    |> Async.RunSynchronously
    |> fun (result: FormatResult) -> result.Issues
    |> should be Empty

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

// Trivia the parser recorded and assignment found no node for is caught on the Oak, before
// printing. A comment right after `when`, with the guard on the next line, is one: the tree gives
// `when` no range. The day Fantomas keeps it, this needs another sample.

[<Test>]
let ``a comment trivia assignment drops is caught on the Oak`` () =
    let failure: exn option =
        try
            formatSourceString "match x with\n| _\n    when // c\n        a -> b\n" FormatConfig.Default
            |> ignore

            None
        with error ->
            Some error

    match failure with
    | None -> failwith "Expected the dropped comment to be caught"
    | Some error ->

    error.Message
    |> should haveSubstring "Trivia the parser recorded is attached to no node of the Oak"

    error.Message |> should haveSubstring "// c"
