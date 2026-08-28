module Fantomas.Analyzers.UnnecessaryParensAnalyzer

open FSharp.Analyzers.SDK

[<Literal>]
val Code: string = "FANTOMAS-PARENS-001"

[<Literal>]
val Name: string = "UnnecessaryParensAnalyzer"

[<Literal>]
val ShortDescription: string =
    "Detects parentheses that can be removed without changing how the code parses, which are a pair of characters the reader has to match for nothing."

[<Literal>]
val HelpUri: string = "https://github.com/fsprojects/fantomas/blob/main/analyzers/AGENTS.md#fantomas-parens-001"

/// Reports a pair of parentheses, around an expression or around a pattern, that the code parses
/// the same without. The reported range spans the opening parenthesis through the closing one, so
/// it says exactly what to delete.
///
/// The compiler answers whether a pair is needed, through
/// `SynExpr.shouldBeParenthesizedInContext` and `SynPat.shouldBeParenthesizedInContext`, which is
/// the same pair of calls FsAutoComplete makes for the diagnostic it raises as `FSAC0004`. Both
/// read only the untyped tree, so the rule says the same thing in the editor as on the command
/// line.
[<CliAnalyzer(Name, ShortDescription, HelpUri)>]
val cliAnalyzer: ctx: CliContext -> Async<Message list>

[<EditorAnalyzer(Name, ShortDescription, HelpUri)>]
val editorAnalyzer: ctx: EditorContext -> Async<Message list>
