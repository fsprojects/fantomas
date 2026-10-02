module Fantomas.Core.AotSmokeTest.Program

open System
open System.Text
open Microsoft.FSharp.Reflection
open Fantomas.FCS.Diagnostics
open Fantomas.FCS.Parse
open Fantomas.FCS.Syntax
open Fantomas.FCS.Text
open Fantomas.Core
open Fantomas.Core.SyntaxOak

// Each check returns what it saw, which is printed so that a JIT run and a Native AOT run can be
// compared line by line, and raises when what it saw is wrong.

exception CheckFailed of string

let expect (condition: bool) (message: string) : unit =
    if not condition then
        raise (CheckFailed message)

let expectEqual (expected: string) (actual: string) : unit =
    if expected <> actual then
        raise (CheckFailed(String.Concat("expected:\n", expected, "\nactual:\n", actual)))

let run (computation: Async<'T>) : 'T = Async.RunSynchronously computation

let parse (isSignature: bool) (source: string) (defines: string list) : ParsedInput * FSharpParserDiagnostic list =
    parseFile isSignature (SourceText.ofString source) defines

let showRange (range: range) : string =
    $"(%i{range.StartLine},%i{range.StartColumn}-%i{range.EndLine},%i{range.EndColumn})"

let showSeverity (severity: FSharpDiagnosticSeverity) : string =
    match severity with
    | FSharpDiagnosticSeverity.Hidden -> "hidden"
    | FSharpDiagnosticSeverity.Info -> "info"
    | FSharpDiagnosticSeverity.Warning -> "warning"
    | FSharpDiagnosticSeverity.Error -> "error"

/// A diagnostic the way fantomas-tools sends one to the browser.
let showDiagnostic (diagnostic: FSharpParserDiagnostic) : string =
    let range: string =
        match diagnostic.Range with
        | None -> "(no range)"
        | Some range -> showRange range

    let errorNumber: string =
        match diagnostic.ErrorNumber with
        | None -> "-"
        | Some(number: int) -> $"%i{number}"

    $"%s{showSeverity diagnostic.Severity} FS%s{errorNumber} %s{diagnostic.SubCategory} %s{range}: %s{diagnostic.Message}"

/// An Oak the way the Oak viewer of fantomas-tools encodes one: the node's type name, its text when
/// it is a single token, its range, its trivia and its children.
let rec showNode (indent: int) (builder: StringBuilder) (node: Node) : unit =
    let text: string =
        match node with
        | :? SingleTextNode as stn -> String.Concat(" \"", stn.Text, "\"")
        | _ -> String.Empty

    let showTrivia (label: string) (trivia: TriviaNode) : unit =
        let content: string =
            match trivia.Content with
            | CommentOnSingleLine comment -> String.Concat("commentOnSingleLine ", comment)
            | LineCommentAfterSourceCode comment -> String.Concat("lineCommentAfterSourceCode ", comment)
            | BlockComment(comment, _, _) -> String.Concat("blockComment ", comment)
            | CommentOnSingleLineWithLeadingNewlines(newlines, comment) ->
                $"commentOnSingleLineWithLeadingNewlines %i{newlines} %s{comment}"
            | Newline -> "newline"
            | Directive directive -> String.Concat("directive ", directive)
            | Cursor -> "cursor"

        builder
            .Append(' ', indent + 2)
            .Append(label)
            .Append(' ')
            .Append(content)
            .Append(' ')
            .AppendLine(showRange trivia.Range)
        |> ignore

    builder.Append(' ', indent).Append(node.GetType().Name).Append(text).Append(' ').AppendLine(showRange node.Range)
    |> ignore

    for trivia in node.ContentBefore do
        showTrivia "before" trivia

    for child in node.Children do
        showNode (indent + 2) builder child

    for trivia in node.ContentAfter do
        showTrivia "after" trivia

let zeroRange: range = Range.range0
let stn (text: string) : SingleTextNode = SingleTextNode(text, zeroRange)

let constant (text: string) : Expr =
    Expr.Constant(Constant.FromText(stn text))

let identList (names: string list) : IdentListNode =
    let content: IdentifierOrDot list =
        names
        |> List.mapi (fun (index: int) (name: string) ->
            [
                if index > 0 then
                    IdentifierOrDot.UnknownDot
                IdentifierOrDot.Ident(stn name)
            ]
        )
        |> List.concat

    IdentListNode(content, zeroRange)

let oakOf (expr: Expr) : Oak =
    Oak([], [ ModuleOrNamespaceNode(None, [ ModuleDecl.DeclExpr expr ], zeroRange) ], zeroRange)

/// A source with every kind of trivia the Oak viewer shows.
let triviaSource: string =
    """module A

// A comment on its own line
let a = 1 // after code

(* block *)
#if DEBUG
let b = 2
#endif
"""

let libraryChecks: (string * (unit -> string)) list =
    [
        "Fantomas.FCS.Parse.parseFile, an implementation file",
        fun () ->
            let ast, diagnostics = parse false "let a = 1" []

            match ast with
            | ParsedInput.ImplFile _ -> ()
            | ParsedInput.SigFile _ -> raise (CheckFailed "parsed as a signature file")

            expect (List.isEmpty diagnostics) "diagnostics for valid code"
            "ImplFile, no diagnostics"

        "Fantomas.FCS.Parse.parseFile, a signature file with a define",
        fun () ->
            let ast, diagnostics =
                parse true "module A\n#if FOO\nval a: int\n#endif\n" [ "FOO" ]

            match ast with
            | ParsedInput.SigFile _ -> ()
            | ParsedInput.ImplFile _ -> raise (CheckFailed "parsed as an implementation file")

            expect (List.isEmpty diagnostics) "diagnostics for valid code"
            "SigFile, no diagnostics"

        "Fantomas.FCS.Parse.parseFile, the diagnostics of code that does not parse",
        fun () ->
            let _, diagnostics = parse false "let a =\nlet b = (1 +\n" []
            expect (not (List.isEmpty diagnostics)) "no diagnostics for invalid code"

            for diagnostic in diagnostics do
                expect (not (String.IsNullOrWhiteSpace diagnostic.Message)) "a diagnostic without a message"

            diagnostics |> List.map showDiagnostic |> String.concat "\n"

        "CodeFormatter.TransformAST with the source, as the Oak viewer does",
        fun () ->
            let ast, _ = parse false triviaSource [ "DEBUG" ]
            let oak: Oak = CodeFormatter.TransformAST(ast, triviaSource)
            let builder: StringBuilder = StringBuilder()
            showNode 0 builder oak
            let shown: string = builder.ToString()

            for expected in
                [
                    "commentOnSingleLine // A comment on its own line"
                    "lineCommentAfterSourceCode // after code"
                    "commentOnSingleLine (* block *)"
                    "directive #if DEBUG"
                    "SingleTextNode \"a\""
                ] do
                expect (shown.Contains(expected, StringComparison.Ordinal)) (String.Concat("the Oak lacks ", expected))

            shown

        "CodeFormatter.FormatDocumentAsync, the default settings",
        fun () ->
            let result: FormatResult =
                CodeFormatter.FormatDocumentAsync(false, "let  f x =   x+1\ntype R = { A: int; B: string }")
                |> run

            expectEqual "let f x = x + 1\ntype R = { A: int; B: string }\n" (result.Code.Replace("\r\n", "\n"))

            result.Code

        "CodeFormatter.FormatDocumentAsync, changed settings and a signature file",
        fun () ->
            let config: FormatConfig =
                { FormatConfig.Default with
                    IndentSize = 2
                    MaxLineLength = 40
                    EndOfLine = EndOfLineStyle.LF
                    MultilineBracketStyle = Aligned
                }

            let result: FormatResult =
                CodeFormatter.FormatDocumentAsync(
                    true,
                    "module A\nval f: aaaaaaaaaa: int -> bbbbbbbbbb: int -> cccccccccc: int -> int\n",
                    config
                )
                |> run

            expectEqual
                "module A\n\nval f:\n  aaaaaaaaaa: int ->\n  bbbbbbbbbb: int ->\n  cccccccccc: int ->\n    int\n"
                result.Code

            result.Code

        "CodeFormatter.ValidateFSharpCodeAsync, valid code",
        fun () ->
            let result: ValidationResult =
                CodeFormatter.ValidateFSharpCodeAsync(false, "let a = 1") |> run

            expect result.IsValid "valid code reported as invalid"
            "valid"

        "CodeFormatter.ValidateFSharpCodeAsync, code that only fails with a define",
        fun () ->
            let source: string = "#if FOO\nlet a =\n#else\nlet a = 1\n#endif\n"

            let result: ValidationResult =
                CodeFormatter.ValidateFSharpCodeAsync(false, source) |> run

            expect (not result.IsValid) "invalid code reported as valid"
            result.Diagnostics |> List.map showDiagnostic |> String.concat "\n"

        "CodeFormatter.FormatDocumentAsync raises ParseException for code that does not parse",
        fun () ->
            try
                CodeFormatter.FormatDocumentAsync(false, "let a =") |> run |> ignore
                raise (CheckFailed "no exception")
            with :? ParseException as parseException ->
                expect (not (List.isEmpty parseException.Diagnostics)) "no diagnostics on the exception"

                String.Concat(
                    parseException.Message,
                    "\n",
                    parseException.Diagnostics |> List.map showDiagnostic |> String.concat "\n"
                )

        "CodeFormatter.FormatDocumentAsync raises DefineParseException for code that only fails with a define",
        fun () ->
            try
                CodeFormatter.FormatDocumentAsync(false, "#if FOO\nlet a =\n#else\nlet a = 1\n#endif\n")
                |> run
                |> ignore

                raise (CheckFailed "no exception")
            with :? DefineParseException as defineParseException ->
                expect (List.contains [ "FOO" ] defineParseException.Combinations) "FOO is not among the combinations"

                let combinations: string =
                    defineParseException.Combinations
                    |> List.map (fun (defines: string list) -> String.Concat("[", String.concat ", " defines, "]"))
                    |> String.concat "; "

                String.Concat(defineParseException.Message, "\n", combinations)

        "CodeFormatter.FormatOakAsync of an Oak built by hand, as the expanded AST viewer does",
        fun () ->
            let field (name: string) (value: Expr) : ExprRecordFieldOrSpread =
                RecordFieldNode(identList [ name ], stn "=", value, zeroRange)
                |> ExprRecordFieldOrSpread.Field

            let record: Expr =
                ExprRecordNode(
                    stn "{",
                    None,
                    [
                        field "Name" (constant "\"a\"")
                        field "Range" (constant "R(\"(1,0--1,1)\")")
                    ],
                    stn "}",
                    zeroRange
                )
                |> Expr.Record

            let expr: Expr =
                ExprAppSingleParenArgNode(
                    Expr.OptVar(ExprOptVarNode(false, identList [ "SynExpr"; "Record" ], zeroRange)),
                    Expr.Paren(ExprParenNode(stn "(", record, stn ")", zeroRange)),
                    zeroRange
                )
                |> Expr.AppSingleParenArg

            let config: FormatConfig =
                { FormatConfig.Default with
                    MultilineBracketStyle = Stroustrup
                    MaxLineLength = 200
                }

            let formatted: string = CodeFormatter.FormatOakAsync(oakOf expr, config) |> run

            expectEqual
                "SynExpr.Record({ Name = \"a\"; Range = R(\"(1,0--1,1)\") })\n"
                (formatted.Replace("\r\n", "\n"))

            formatted

        "InvariantViolationException from CodeFormatter.FormatOakAsync, as fantomas-tools reports it",
        fun () ->
            // A call on a list is no Oak the transformer builds, and the printer says so.
            let expr: Expr =
                ExprAppSingleParenArgNode(
                    Expr.ArrayOrList(ExprArrayOrListNode(stn "[", [], stn "]", zeroRange)),
                    Expr.Paren(ExprParenNode(stn "(", constant "2", stn ")", zeroRange)),
                    zeroRange
                )
                |> Expr.AppSingleParenArg

            try
                CodeFormatter.FormatOakAsync(oakOf expr) |> run |> ignore
                raise (CheckFailed "no exception")
            with :? InvariantViolationException as invariantViolation ->
                expect (not (String.IsNullOrWhiteSpace invariantViolation.Invariant)) "no invariant"
                expect (not (String.IsNullOrWhiteSpace invariantViolation.SyntaxNode)) "no syntax node"

                String.Concat(
                    invariantViolation.Invariant,
                    "\n",
                    showRange invariantViolation.Range,
                    "\nSyntaxNode: ",
                    invariantViolation.SyntaxNode
                )

        "CodeFormatter.GetVersion",
        fun () ->
            let version: string = CodeFormatter.GetVersion()
            expect (not (String.IsNullOrWhiteSpace version)) "no version"
            version
    ]

/// Names every union case reached from `value`, a few levels deep, the way the expanded view of the
/// AST viewer walks a tree.
[<Diagnostics.CodeAnalysis.UnconditionalSuppressMessage("Trimming",
                                                        "IL2072",
                                                        Justification =
                                                            "This is fantomas-tools' reflection, here to show what Native AOT does with it.")>]
let rec walkUnionCases (caseNames: Collections.Generic.List<string>) (depth: int) (value: obj) : unit =
    if depth < 6 && not (isNull value) then
        let runtimeType: System.Type = value.GetType()

        if FSharpType.IsUnion(runtimeType, true) then
            let case, fields = FSharpValue.GetUnionFields(value, runtimeType, true)
            caseNames.Add(String.Concat(case.DeclaringType.Name, ".", case.Name))

            for field in fields do
                walkUnionCases caseNames (depth + 1) field
        elif FSharpType.IsRecord(runtimeType, true) then
            for field in FSharpValue.GetRecordFields(value, true) do
                walkUnionCases caseNames (depth + 1) field

/// What fantomas-tools does with the library's types on its own side. None of it is a library entry
/// point, and none of it is this repository's to keep working, but each decides whether one of its
/// lambdas can run on Native AOT. They count towards the exit code with `--all` only.
///
/// Under Native AOT the two over the syntax tree print the top union case and nothing below it, and
/// keeping all of Fantomas.FCS with `TrimmerRootAssembly` does not change that.
let toolChecks: (string * (unit -> string)) list =
    [
        "FSharpType.GetRecordFields and FSharpValue.GetRecordFields of FormatConfig, which lists the settings",
        fun () ->
            let names: string array =
                FSharpType.GetRecordFields(typeof<FormatConfig>)
                |> Array.map (fun property -> property.Name)

            let values: obj array = FSharpValue.GetRecordFields(FormatConfig.Default)
            expect (names.Length > 0) "no fields"
            expect (names.Length = values.Length) "as many names as values"
            $"%i{names.Length} settings, first %s{names.[0]} = %s{values.[0].ToString()}"

        "FSharpValue.MakeRecord of FormatConfig, which builds the settings a request asks for",
        fun () ->
            let values: obj array = FSharpValue.GetRecordFields(FormatConfig.Default)
            values.[0] <- box 2

            let config: FormatConfig =
                FSharpValue.MakeRecord(typeof<FormatConfig>, values) :?> FormatConfig

            expect (config.IndentSize = 2) "IndentSize was not set"
            $"IndentSize = %i{config.IndentSize}"

        "%A of a ParsedInput, the default view of the AST viewer",
        fun () ->
            let ast, _ = parse false "let a = 1" []
            // fsharpanalyzer: ignore-line-next FANTOMAS-PRINTF-001
            let shown: string = $"%A{ast}"

            expect (shown.Contains("Let", StringComparison.Ordinal)) (String.Concat("no Let in the dump:\n", shown))

            shown

        "FSharpValue.GetUnionFields over a ParsedInput, the expanded view of the AST viewer",
        fun () ->
            let ast, _ = parse false "let a = 1" []
            let caseNames: Collections.Generic.List<string> = Collections.Generic.List<string>()
            walkUnionCases caseNames 0 (box ast)

            expect
                (caseNames.Contains "SynModuleDecl.Let")
                (String.Concat("SynModuleDecl.Let was not reached: ", String.concat ", " caseNames))

            String.concat ", " caseNames
    ]

let runChecks (title: string) (checks: (string * (unit -> string)) list) : int =
    Console.Out.WriteLine(String.Concat("# ", title))

    checks
    |> List.sumBy (fun (name: string, check: unit -> string) ->
        Console.Out.WriteLine()
        Console.Out.WriteLine(String.Concat("## ", name))

        try
            let shown: string = check ()
            Console.Out.WriteLine("ok")
            Console.Out.WriteLine(shown)
            0
        with
        | CheckFailed message ->
            Console.Out.WriteLine(String.Concat("FAILED: ", message))
            1
        | ex ->
            Console.Out.WriteLine(String.Concat("FAILED: ", ex.ToString()))
            1
    )

[<EntryPoint>]
let main (args: string array) : int =
    let failedLibrary: int = runChecks "The library" libraryChecks
    Console.Out.WriteLine()

    let failedTools: int =
        runChecks "What fantomas-tools does with the library's types" toolChecks

    Console.Out.WriteLine()
    Console.Out.WriteLine($"%i{failedLibrary} library checks failed, %i{failedTools} fantomas-tools checks failed")

    // Only the library is this repository's to keep working. `--all` counts the rest too.
    if Array.contains "--all" args then
        failedLibrary + failedTools
    else
        failedLibrary
