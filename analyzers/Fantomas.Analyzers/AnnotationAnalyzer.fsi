module Fantomas.Analyzers.AnnotationAnalyzer

open FSharp.Analyzers.SDK

[<Literal>]
val Code: string = "FANTOMAS-ANNOTATE-001"

[<Literal>]
val Name: string = "AnnotationAnalyzer"

[<Literal>]
val ShortDescription: string = "Detects a let binding without a type annotation, where a written type would say what the name holds."

[<Literal>]
val HelpUri: string = "https://github.com/fsprojects/fantomas/blob/main/analyzers/AGENTS.md#fantomas-annotate-001"

/// Reports every let binding that is missing a type, on the name rather than on the whole binding
/// so that the range stays small.
///
/// Signature files are skipped whole, since a `val` already states the type. So is the unit
/// parameter, which has nowhere to put one. So is any binding carrying a test attribute, along with
/// everything nested inside it: annotating a test says nothing a reader did not already know, and
/// the locals in a test body are scaffolding.
[<CliAnalyzer(Name, ShortDescription, HelpUri)>]
val cliAnalyzer: ctx: CliContext -> Async<Message list>

[<EditorAnalyzer(Name, ShortDescription, HelpUri)>]
val editorAnalyzer: ctx: EditorContext -> Async<Message list>
