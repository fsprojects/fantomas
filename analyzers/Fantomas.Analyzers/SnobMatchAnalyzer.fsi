module Fantomas.Analyzers.SnobMatchAnalyzer

open FSharp.Analyzers.SDK

[<Literal>]
val Code: string = "FANTOMAS-SNOBMATCH-001"

[<Literal>]
val Name: string = "SnobMatchAnalyzer"

[<Literal>]
val ShortDescription: string =
    "Detects a two armed match on a boolean, or on a constant against a wildcard, which is an if expression dressed up as pattern matching."

[<Literal>]
val HelpUri: string = "https://github.com/fsprojects/fantomas/blob/main/analyzers/AGENTS.md#fantomas-snobmatch-001"

/// Reports a `match` on a boolean, or on a constant against a wildcard, on the whole expression,
/// because the whole of it is what an `if` replaces.
///
/// These are the shapes where the rewrite is mechanical: the scrutinee becomes the condition, or
/// one side of an `=` against the constant, and is written once, exactly where it was. It stays
/// quiet on a guard, on anything other than two arms, on a constant against a binder, on `match!`
/// and `function`, which cannot be rewritten without inventing a `let!` or a parameter, and on a
/// conditional directive inside the match. No fix is offered, as with every rule here.
[<CliAnalyzer(Name, ShortDescription, HelpUri)>]
val cliAnalyzer: ctx: CliContext -> Async<Message list>

[<EditorAnalyzer(Name, ShortDescription, HelpUri)>]
val editorAnalyzer: ctx: EditorContext -> Async<Message list>
