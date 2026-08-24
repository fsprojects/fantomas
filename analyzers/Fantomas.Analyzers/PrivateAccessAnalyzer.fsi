module Fantomas.Analyzers.PrivateAccessAnalyzer

open FSharp.Analyzers.SDK

[<Literal>]
val Code: string = "FANTOMAS-PRIVATE-001"

[<Literal>]
val Name: string = "PrivateAccessAnalyzer"

[<Literal>]
val ShortDescription: string =
    "Detects a private let binding in an implementation file that has a signature file, where the signature file is already the visibility boundary."

[<Literal>]
val HelpUri: string = "https://github.com/fsprojects/fantomas/blob/main/analyzers/AGENTS.md#fantomas-private-001"

/// Reports every `let private` in a file whose signature file already hides it, on the `private`
/// keyword, which is the thing to delete.
///
/// Whether there is a signature file is answered by the project's own source list rather than by a
/// look at the filesystem.
[<CliAnalyzer(Name, ShortDescription, HelpUri)>]
val cliAnalyzer: ctx: CliContext -> Async<Message list>

[<EditorAnalyzer(Name, ShortDescription, HelpUri)>]
val editorAnalyzer: ctx: EditorContext -> Async<Message list>
