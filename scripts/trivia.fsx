#load "shared.fsx"

open System.IO
open Fantomas.Core
open Fantomas.Core.SnapshotTests
open Shared

/// Every piece of trivia in the input and where it landed, one line each in source order: its
/// position, its kind, which side of the node, and the node class with the token slot it fills.
/// `(whole node)` is the node itself, a property name the token that property holds. It reads the Oaks a
/// snapshot case is printed from, and names things the way the reports do. A source with
/// conditional directives is listed once per define combination.
let listTrivia (input: string) (isSignature: bool) (config: FormatConfig) : string =
    let combinations: Formatting.ForDefines list =
        (Formatting.formatEach config isSignature input).Combinations

    combinations
    |> List.map (fun (each: Formatting.ForDefines) ->
        let lines: string list =
            OakFacts.attachments each.Oak
            |> List.sortBy (fun (attachment: OakFacts.Attachment) ->
                attachment.Range.StartLine, attachment.Range.StartColumn
            )
            |> List.map (fun (attachment: OakFacts.Attachment) ->
                let (owner: System.Type), (slot: string) = OakFacts.slotOf attachment.Visit
                let side: string = if attachment.IsBefore then "before" else "after"

                $"(%d{attachment.Range.StartLine},%d{attachment.Range.StartColumn})  %s{attachment.Kind} %s{side} %s{owner.Name}.%s{slot}"
            )

        let body: string =
            if lines.IsEmpty then
                "No trivia."
            else
                String.concat "\n" lines

        if combinations.Length = 1 then
            body
        else

        $"## %s{Case.combinationName each.Defines}\n%s{body}"
    )
    |> String.concat "\n\n"

match Array.tryHead fsi.CommandLineArgs with
| Some scriptPath ->
    let scriptFile: FileInfo = FileInfo(scriptPath)

    let sourceFile: FileInfo =
        FileInfo(Path.Combine(__SOURCE_DIRECTORY__, __SOURCE_FILE__))

    if scriptFile.FullName = sourceFile.FullName then
        let sample, isSignature, config, _ = parseArgs fsi.CommandLineArgs.[1..]
        listTrivia sample isSignature config |> printfn "%s"
| _ -> printfn "Usage: dotnet fsi trivia.fsx [--signature] [--editorconfig <content>] <input file>"
