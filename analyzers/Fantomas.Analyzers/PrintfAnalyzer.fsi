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

/// Reports printf in `src/Fantomas` and `src/Fantomas.Core`: every `PrintfFormat` constructed, on
/// the format, which covers `sprintf` and its kin and every interpolated string the compiler does
/// not lower to `String.Concat`, and every call into FSharp.Core handed a format as a value, which
/// is a format forwarded to `kprintf` or one of its kin.
///
/// printf specializes its formatters at runtime through `MethodInfo.MakeGenericMethod`, which a
/// Native AOT build of the tool does not have, and which formats survive depends on what the AOT
/// compiler happened to generate. So the rule is about all of them. It reads the typed tree, where
/// the lowering is already done, and says nothing in the editor until the file is checked. It
/// reports at error severity, so that the full run fails on it. No fix is offered, as with every
/// rule here.
[<CliAnalyzer(Name, ShortDescription, HelpUri)>]
val cliAnalyzer: ctx: CliContext -> Async<Message list>

[<EditorAnalyzer(Name, ShortDescription, HelpUri)>]
val editorAnalyzer: ctx: EditorContext -> Async<Message list>
