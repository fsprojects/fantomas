module Fantomas.Analyzers.UnusedOpensAnalyzer

open FSharp.Analyzers.SDK
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.EditorServices
open FSharp.Compiler.Text

[<Literal>]
let Code: string = "FANTOMAS-OPENS-001"

[<Literal>]
let Name: string = "UnusedOpensAnalyzer"

[<Literal>]
let ShortDescription: string =
    "Detects an open declaration that nothing in the file needs, which is a name the reader has to hold and a dependency the file does not have."

[<Literal>]
let HelpUri: string =
    "https://github.com/fsprojects/fantomas/blob/main/analyzers/AGENTS.md#fantomas-opens-001"

// The opens no symbol use of the file resolves through.
//
// All of the work is `UnusedOpens.getUnusedOpens`, which is the same call FsAutoComplete makes for
// its own version of this diagnostic. It wants a one-based line getter where `ISourceText` counts
// from zero, and it reads those lines to find where each open's declaration starts, so getting the
// bridge wrong moves every reported range by a line rather than failing.
let analyze (checkResults: FSharpCheckFileResults) (sourceText: ISourceText) : Async<Message list> =
    async {
        let getSourceLineStr (lineNumber: int) : string =
            sourceText.GetLineString(lineNumber - 1)

        let! unused: range list = UnusedOpens.getUnusedOpens (checkResults, getSourceLineStr)

        return
            unused
            |> List.map (fun (range: range) ->
                {
                    Type = Name
                    Message =
                        "Remove this `open` declaration. Nothing in the file resolves through it, so it is a name the reader has to hold and a dependency the file does not have."
                    Code = Code
                    Severity = Severity.Warning
                    Range = range
                    Fixes = []
                }
            )
    }

let cliAnalyzer (ctx: CliContext) : Async<Message list> =
    analyze ctx.CheckFileResults ctx.SourceText

// Without check results there is no symbol use to walk, and an open cannot be told from an unused
// one. It says nothing rather than guessing.
let editorAnalyzer (ctx: EditorContext) : Async<Message list> =
    async {
        match ctx.CheckFileResults with
        | None -> return []
        | Some checkResults -> return! analyze checkResults ctx.SourceText
    }
