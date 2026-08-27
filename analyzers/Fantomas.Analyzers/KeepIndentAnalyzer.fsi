module Fantomas.Analyzers.KeepIndentAnalyzer

open FSharp.Analyzers.SDK

[<Literal>]
val Code: string = "FANTOMAS-KEEPINDENT-001"

[<Literal>]
val Name: string = "KeepIndentAnalyzer"

[<Literal>]
val ShortDescription: string =
    "Detects a last branch whose body is a block indented a level past the expression it belongs to, where that indentation could be kept instead."

[<Literal>]
val HelpUri: string = "https://github.com/fsprojects/fantomas/blob/main/analyzers/AGENTS.md#fantomas-keepindent-001"

/// Reports the last arm of a match whose body could keep the indentation of the match, on the body,
/// which is the part that moves.
///
/// `fsharp_experimental_keep_indent_in_branch` only holds a body in the column of the `|` when the
/// source already put it there, so a de-indent that would read better is one nothing asks for until
/// somebody writes it. This is the rule that asks, and the other half of what
/// `FANTOMAS-ARMORDER-001` starts.
///
/// It speaks only for the last arm, whose body is a block already on a line of its own, and only
/// where nothing follows the match in the same block, since keeping the indentation past that would
/// take the following code into the arm. It stays quiet on a `when` guard and on a conditional
/// directive inside the match. No fix is offered: re-indenting a block means leaving the multiline
/// strings inside it exactly where they are, which is not a thing to do blind.
[<CliAnalyzer(Name, ShortDescription, HelpUri)>]
val cliAnalyzer: ctx: CliContext -> Async<Message list>

[<EditorAnalyzer(Name, ShortDescription, HelpUri)>]
val editorAnalyzer: ctx: EditorContext -> Async<Message list>
