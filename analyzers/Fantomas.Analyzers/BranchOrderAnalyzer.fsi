module Fantomas.Analyzers.BranchOrderAnalyzer

open FSharp.Analyzers.SDK

[<Literal>]
val Code: string = "FANTOMAS-BRANCHORDER-001"

[<Literal>]
val Name: string = "BranchOrderAnalyzer"

[<Literal>]
val ShortDescription: string =
    "Detects an if expression whose long branch comes first, where negating the condition would put the one line branch first instead."

[<Literal>]
val HelpUri: string = "https://github.com/fsprojects/fantomas/blob/main/analyzers/AGENTS.md#fantomas-branchorder-001"

/// Reports an if expression whose branches should be the other way around, on the one line branch,
/// which is the one that moves.
///
/// This is `FANTOMAS-ARMORDER-001` for an `if`, and it asks for more than that rule does: a match
/// arm can only be moved, where a condition has to be negated as well. The two branches of an `if`
/// are exclusive by construction, so there is nothing to prove about overlap and the swap is always
/// sound. What is not always an improvement is the condition it leaves behind, so the rule stays
/// quiet where the condition is joined by `&&` or `||` and negating it would mean wrapping the whole
/// of it in `not`.
///
/// It speaks only for a plain `if`/`then`/`else` with no `elif`, since a chain has no single swap
/// that puts the short branch first, and only where the `then` runs to more than one line while the
/// `else` fits on one. It stays quiet on a conditional directive inside the expression. No fix is
/// offered: rewriting a condition is a thing to read before doing.
[<CliAnalyzer(Name, ShortDescription, HelpUri)>]
val cliAnalyzer: ctx: CliContext -> Async<Message list>

[<EditorAnalyzer(Name, ShortDescription, HelpUri)>]
val editorAnalyzer: ctx: EditorContext -> Async<Message list>
