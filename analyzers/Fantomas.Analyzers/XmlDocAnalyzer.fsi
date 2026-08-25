module Fantomas.Analyzers.XmlDocAnalyzer

open FSharp.Analyzers.SDK

[<Literal>]
val Code: string = "FANTOMAS-XMLDOC-001"

[<Literal>]
val Name: string = "XmlDocAnalyzer"

[<Literal>]
val ShortDescription: string =
    "Detects a documentation comment that is duplicated in an implementation file and its signature file, where the signature is the copy readers and tooling see."

[<Literal>]
val HelpUri: string = "https://github.com/fsprojects/fantomas/blob/main/analyzers/AGENTS.md#fantomas-xmldoc-001"

/// Reports a documentation comment on a declaration that the file's signature file also declares.
///
/// A declaration the signature does not carry is left alone, doc comment and all: there is no second
/// copy to keep in step, so there is nothing for the rule to be about. The signature is asked for
/// through the symbol's `SignatureLocation` rather than guessed at from the name.
[<CliAnalyzer(Name, ShortDescription, HelpUri)>]
val cliAnalyzer: ctx: CliContext -> Async<Message list>

[<EditorAnalyzer(Name, ShortDescription, HelpUri)>]
val editorAnalyzer: ctx: EditorContext -> Async<Message list>
