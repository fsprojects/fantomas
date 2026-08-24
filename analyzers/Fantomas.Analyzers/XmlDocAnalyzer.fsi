module Fantomas.Analyzers.XmlDocAnalyzer

open FSharp.Analyzers.SDK

[<Literal>]
val Code: string = "FANTOMAS-XMLDOC-001"

[<Literal>]
val Name: string = "XmlDocAnalyzer"

[<Literal>]
val ShortDescription: string =
    "Detects a documentation comment in an implementation file that has a signature file, where the signature is the copy readers and tooling see."

[<Literal>]
val HelpUri: string = "https://github.com/fsprojects/fantomas/blob/main/analyzers/AGENTS.md#fantomas-xmldoc-001"

/// Reports every documentation comment in a file that has a signature file.
///
/// This takes the looser of the two readings of the rule. It cannot tell whether the signature
/// documents the same binding, so a helper that appears in neither is reported too, and the answer
/// there is an ordinary `//` comment rather than a `///` one.
[<CliAnalyzer(Name, ShortDescription, HelpUri)>]
val cliAnalyzer: ctx: CliContext -> Async<Message list>

[<EditorAnalyzer(Name, ShortDescription, HelpUri)>]
val editorAnalyzer: ctx: EditorContext -> Async<Message list>
