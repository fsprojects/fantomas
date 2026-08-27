module Fantomas.Analyzers.ArmOrderAnalyzer

open FSharp.Analyzers.SDK

[<Literal>]
val Code: string = "FANTOMAS-ARMORDER-001"

[<Literal>]
val Name: string = "ArmOrderAnalyzer"

[<Literal>]
val ShortDescription: string =
    "Detects a two armed match whose long arm comes first, where the arms are disjoint and swapping them cannot change meaning."

[<Literal>]
val HelpUri: string = "https://github.com/fsprojects/fantomas/blob/main/analyzers/AGENTS.md#fantomas-armorder-001"

/// Reports a two armed match whose arms should be the other way around, on the one line arm, which
/// is the one that moves.
///
/// Match arm order is semantically significant, so this speaks only where reordering provably
/// cannot change meaning and where which arm is shorter is not a judgement call. It stays quiet on
/// a guard, on a wildcard or a bare binder, on anything other than two arms, and where a comment
/// between the arms or a conditional directive inside the match would make a swap something other
/// than a swap. No fix is offered: the point is to make a person look.
///
/// Once the arms are the right way around, `FANTOMAS-KEEPINDENT-001` is what asks for the other
/// half of the reshape.
[<CliAnalyzer(Name, ShortDescription, HelpUri)>]
val cliAnalyzer: ctx: CliContext -> Async<Message list>

[<EditorAnalyzer(Name, ShortDescription, HelpUri)>]
val editorAnalyzer: ctx: EditorContext -> Async<Message list>
