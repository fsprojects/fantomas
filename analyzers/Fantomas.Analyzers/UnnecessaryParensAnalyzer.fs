module Fantomas.Analyzers.UnnecessaryParensAnalyzer

open System.Collections.Generic
open FSharp.Analyzers.SDK
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

[<Literal>]
let Code: string = "FANTOMAS-PARENS-001"

[<Literal>]
let Name: string = "UnnecessaryParensAnalyzer"

[<Literal>]
let ShortDescription: string =
    "Detects parentheses that can be removed without changing how the code parses, which are a pair of characters the reader has to match for nothing."

[<Literal>]
let HelpUri: string =
    "https://github.com/fsprojects/fantomas/blob/main/analyzers/AGENTS.md#fantomas-parens-001"

// What the two messages have to say beyond "remove them" is different, because the two edits are:
// an expression can span more than one line and have to be moved left, where a parenthesized
// pattern is a name and a pair of characters around it.
[<Literal>]
let ExpressionMessage: string =
    "Remove these parentheses. The expression parses the same without them. Re-indent the body if it spans more than one line."

[<Literal>]
let PatternMessage: string =
    "Remove these parentheses. The pattern parses the same without them, so `let f (x) = x` becomes `let f x = x`."

// Whether the opening parenthesis is written against whatever comes before it, which is how a call
// writes its argument list: `Some(x)`, `new StringBuilder(64)`, `s.TrimEnd('\n')`,
// `Dictionary<int, string>(comparer)`.
//
// Those pairs are removable and the rule stays quiet about them anyway. `Some x` and
// `new StringBuilder 64` compile, but the parentheses are how this repository writes a union case
// or a constructor, and half of what the rule would otherwise report is that shape. Reporting them
// turns the rule into a request to restyle the codebase, which is not what it is for.
//
// This is a question about text rather than about the tree, because the tree cannot tell
// `Some(x)` from `Some (x)` and the two are written for different reasons.
let isApplicationParen (sourceText: ISourceText) (range: range) : bool =
    let line: string = sourceText.GetLineString(range.StartLine - 1)

    if range.StartColumn = 0 || range.StartColumn > line.Length then
        false
    else

    // Everything that can end the thing being called: a name, the `>` closing a type application,
    // and the closing bracket of the call before it in a chain.
    let preceding: char = line[range.StartColumn - 1]
    System.Char.IsLetterOrDigit preceding || "_'`>)]".Contains preceding

// Every pair of parentheses the code parses the same without, mapped to what to say about it.
//
// Whether a pair is needed at all is `shouldBeParenthesizedInContext`, which is the same pair of
// calls FsAutoComplete makes for its own version of this diagnostic. The expression overload wants a
// one-based line getter where `ISourceText` counts from zero, and it consults those lines for the
// cases it cannot answer from the tree alone, so getting the bridge wrong makes the rule judge the
// wrong line rather than fail.
//
// The keys are ranges spanning the opening parenthesis through the closing one, which is the thing
// to delete. `rightParenRange` has to be there: a pair that was never closed is broken source, and
// nothing is gained by speaking about it.
let unnecessaryParens (sourceText: ISourceText) (parsedInput: ParsedInput) : Dictionary<range, string> =
    let getSourceLineStr (lineNumber: int) : string =
        sourceText.GetLineString(lineNumber - 1)

    (Dictionary<range, string>(Range.comparer), parsedInput)
    ||> ParsedInput.fold (fun (found: Dictionary<range, string>) (path: SyntaxVisitorPath) (node: SyntaxNode) ->
        match node with
        | SyntaxNode.SynExpr(SynExpr.Paren(expr = inner; rightParenRange = Some _; range = range)) when
            not (isApplicationParen sourceText range)
            && not (SynExpr.shouldBeParenthesizedInContext getSourceLineStr path inner)
            ->
            found[range] <- ExpressionMessage
            found
        | SyntaxNode.SynPat(SynPat.Paren(inner, range)) when
            not (isApplicationParen sourceText range)
            && not (SynPat.shouldBeParenthesizedInContext path inner)
            ->
            found[range] <- PatternMessage
            found
        | _ -> found
    )

let analyze (sourceText: ISourceText) (parsedInput: ParsedInput) : Message list =
    unnecessaryParens sourceText parsedInput
    // The fold reaches a node before the nodes under it but says nothing about the order of
    // siblings, and a dictionary says nothing about order at all. Sorting here is what makes the
    // findings read down the file the way the file does.
    |> Seq.sortBy (fun (pair: KeyValuePair<range, string>) -> pair.Key.StartLine, pair.Key.StartColumn)
    |> Seq.map (fun (pair: KeyValuePair<range, string>) ->
        {
            Type = Name
            Message = pair.Value
            Code = Code
            Severity = Severity.Warning
            Range = pair.Key
            Fixes = []
        }
    )
    |> Seq.toList

let cliAnalyzer (ctx: CliContext) : Async<Message list> =
    async { return analyze ctx.SourceText ctx.ParseFileResults.ParseTree }

let editorAnalyzer (ctx: EditorContext) : Async<Message list> =
    async { return analyze ctx.SourceText ctx.ParseFileResults.ParseTree }
