module Fantomas.Analyzers.UnusedOpensAnalyzer

open FSharp.Analyzers.SDK

[<Literal>]
val Code: string = "FANTOMAS-OPENS-001"

[<Literal>]
val Name: string = "UnusedOpensAnalyzer"

[<Literal>]
val ShortDescription: string =
    "Detects an open declaration that nothing in the file needs, which is a name the reader has to hold and a dependency the file does not have."

[<Literal>]
val HelpUri: string = "https://github.com/fsprojects/fantomas/blob/main/analyzers/AGENTS.md#fantomas-opens-001"

/// Reports an `open` declaration that no symbol in the file resolves through.
///
/// The compiler answers this, through `FSharp.Compiler.EditorServices.UnusedOpens`, which walks
/// every symbol use of the file and keeps the opens that were needed to write a name the way it is
/// written. The untyped tree cannot say any of that, so the rule is quiet without check results.
[<CliAnalyzer(Name, ShortDescription, HelpUri)>]
val cliAnalyzer: ctx: CliContext -> Async<Message list>

[<EditorAnalyzer(Name, ShortDescription, HelpUri)>]
val editorAnalyzer: ctx: EditorContext -> Async<Message list>
