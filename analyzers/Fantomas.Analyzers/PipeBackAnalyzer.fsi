module Fantomas.Analyzers.PipeBackAnalyzer

open FSharp.Analyzers.SDK

[<Literal>]
val Code: string = "FANTOMAS-PIPEBACK-001"

[<Literal>]
val Name: string = "PipeBackAnalyzer"

[<Literal>]
val ShortDescription: string =
    "Detects the backward pipe operator, which reads against the direction the surrounding code is written in."

[<Literal>]
val HelpUri: string = "https://github.com/fsprojects/fantomas/blob/main/analyzers/AGENTS.md#fantomas-pipeback-001"

/// Reports every backward pipe in the file, on the operator itself rather than on the expression
/// around it, so that the range points at the thing to remove.
[<CliAnalyzer(Name, ShortDescription, HelpUri)>]
val cliAnalyzer: ctx: CliContext -> Async<Message list>

[<EditorAnalyzer(Name, ShortDescription, HelpUri)>]
val editorAnalyzer: ctx: EditorContext -> Async<Message list>
