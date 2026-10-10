module Fantomas.Analyzers.PrintfAnalyzer

open FSharp.Analyzers.SDK

[<Literal>]
val Code: string = "FANTOMAS-PRINTF-001"

[<Literal>]
val Name: string = "PrintfAnalyzer"

[<Literal>]
val ShortDescription: string =
    "Detects printf in code the command line tool runs, which fails at runtime in a Native AOT build."

[<Literal>]
val HelpUri: string = "https://github.com/fsprojects/fantomas/blob/main/analyzers/AGENTS.md#fantomas-printf-001"

/// Reports printf in `src/Fantomas` and `src/Fantomas.Core`: every function that takes a printf
/// format, on its name, and every interpolated string the compiler does not lower to
/// `String.Concat`, on the string.
///
/// printf specializes its formatters at runtime through `MethodInfo.MakeGenericMethod`, which a
/// Native AOT build of the tool does not have, and which formats survive depends on what the AOT
/// compiler happened to generate. So the rule is about all of them. It reads the untyped tree and
/// states the compiler's lowering itself, because the typed tree comes from the analyzer SDK's own
/// compiler, which lowers less than the one that builds the tool. It reports at error severity, so
/// that the full run fails on it. No fix is offered, as with every rule here.
[<CliAnalyzer(Name, ShortDescription, HelpUri)>]
val cliAnalyzer: ctx: CliContext -> Async<Message list>

[<EditorAnalyzer(Name, ShortDescription, HelpUri)>]
val editorAnalyzer: ctx: EditorContext -> Async<Message list>
