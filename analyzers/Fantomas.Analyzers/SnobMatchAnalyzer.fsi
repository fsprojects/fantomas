module Fantomas.Analyzers.SnobMatchAnalyzer

open FSharp.Analyzers.SDK

[<Literal>]
val Code: string = "FANTOMAS-SNOBMATCH-001"

[<Literal>]
val Name: string = "SnobMatchAnalyzer"

[<Literal>]
val ShortDescription: string =
    "Detects a two armed match on a boolean, which is an if expression dressed up as pattern matching."

[<Literal>]
val HelpUri: string = "https://github.com/fsprojects/fantomas/blob/main/analyzers/AGENTS.md#fantomas-snobmatch-001"

/// Reports a `match` on a boolean, on the whole expression, because the whole of it is what an
/// `if` replaces.
///
/// This is the narrowest shape of the rule and the only one where the rewrite is mechanical: the
/// scrutinee becomes the condition and is written once, exactly where it was. It stays quiet on a
/// guard, on anything other than two arms, on `match!` and `function`, which cannot be rewritten
/// without inventing a `let!` or a parameter, and on a conditional directive inside the match. No
/// fix is offered, as with every rule here.
[<CliAnalyzer(Name, ShortDescription, HelpUri)>]
val cliAnalyzer: ctx: CliContext -> Async<Message list>

[<EditorAnalyzer(Name, ShortDescription, HelpUri)>]
val editorAnalyzer: ctx: EditorContext -> Async<Message list>
