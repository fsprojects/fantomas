/// Two reports over every case, to read while writing cases for a folder: which shapes of each node
/// some case produces, and where trivia ends up.
module Fantomas.Core.SnapshotTests.Reports

open System
open System.Collections.Generic
open System.Reflection
open System.Text
open Fantomas.Core.SyntaxOak

/// What every case's Oaks contain, gathered over all of them.
[<NoComparison; NoEquality>]
type private Observed =
    {
        Classes: HashSet<string>
        Shapes: Dictionary<string * string, HashSet<string>>
        Trivia: Dictionary<string * string, HashSet<string>>
    }

let private add (table: Dictionary<string * string, HashSet<string>>) (key: string * string) (value: string) : unit =
    match table.TryGetValue key with
    | true, values -> values.Add value |> ignore
    | false, _ -> table[key] <- HashSet<string>([ value ])

let private observe () : Observed =
    let observed: Observed =
        {
            Classes = HashSet<string>()
            Shapes = Dictionary<string * string, HashSet<string>>()
            Trivia = Dictionary<string * string, HashSet<string>>()
        }

    // Formatting is what takes the time, so the cases are formatted side by side, and only the
    // tables are filled one Oak at a time.
    let oaks: Oak array =
        Case.all ()
        |> Array.Parallel.collect (fun (relativePath: string) ->
            // A case that does not format is its own test's failure, not the report's.
            try
                let case: Case.Case = Case.read relativePath

                (Formatting.formatEach case.Config case.IsSignature case.Source).Combinations
                |> List.map (fun (each: Formatting.FormattedCombination) -> each.Oak)
                |> List.toArray
            with _ ->
                [||]
        )

    for oak in oaks do
        for visit in OakFacts.visits oak do
            let nodeClass: System.Type = visit.Node.GetType()
            observed.Classes.Add nodeClass.Name |> ignore

            for property in OakFacts.shapeProperties nodeClass do
                OakFacts.shapeOf property visit.Node
                |> Option.iter (add observed.Shapes (nodeClass.Name, property.Name))

        for attachment in OakFacts.attachments oak do
            let (owner: System.Type), (slot: string) = OakFacts.slotOf attachment.Visit
            let side: string = if attachment.IsBefore then "before" else "after"
            add observed.Trivia (owner.Name, slot) $"%s{side}: %s{attachment.Kind}"

    observed

let private seen (table: Dictionary<string * string, HashSet<string>>) (key: string * string) : Set<string> =
    match table.TryGetValue key with
    | true, values -> Set.ofSeq values
    | false, _ -> Set.empty

let private header (title: string) (explanation: string) : StringBuilder =
    StringBuilder()
        .Append($"# %s{title}\n\n")
        .Append(explanation)
        .Append("\n\nGenerated from every case under `cases/` by `dotnet fsi build.fsx -- -p SnapshotReports`.\n")

let private shapesReport (observed: Observed) : string =
    let report: StringBuilder =
        header
            "Node shapes"
            "For every node class some case produces: each optional part, whether some case has it and some\ncase leaves it out, and each list of parts, whether some case has none, one and several. A shape\nunder Missing is either a case still to write or one the parser cannot produce; which, is for\nwhoever writes the cases to judge."

    let unproduced: string list =
        OakFacts.nodeClasses
        |> List.choose (fun (t: System.Type) ->
            if observed.Classes.Contains t.Name then
                None
            else
                Some t.Name
        )

    report.Append("\n## Node classes no case produces\n\n") |> ignore

    for name in unproduced do
        report.Append($"- `%s{name}`\n") |> ignore

    for nodeClass in OakFacts.nodeClasses do
        if observed.Classes.Contains nodeClass.Name then
            report.Append($"\n## %s{nodeClass.Name}\n\n") |> ignore

            match OakFacts.shapeProperties nodeClass with
            | [] -> report.Append("No optional parts and no lists.\n") |> ignore
            | properties ->

            report.Append("| Property | Seen | Missing |\n|---|---|---|\n") |> ignore

            for property in properties do
                let shapes: string list =
                    if OakFacts.isOption property.PropertyType then
                        [ "Some"; "None" ]
                    else
                        [ "none"; "one"; "several" ]

                let seenShapes: Set<string> = seen observed.Shapes (nodeClass.Name, property.Name)

                let seenCell: string =
                    shapes |> List.filter seenShapes.Contains |> String.concat ", "

                let missingCell: string =
                    shapes |> List.filter (seenShapes.Contains >> not) |> String.concat ", "

                report.Append($"| %s{property.Name} | %s{seenCell} | %s{missingCell} |\n")
                |> ignore

    report.ToString()

let private triviaReport (observed: Observed) : string =
    let report: StringBuilder =
        header
            "Trivia"
            "For every node class some case produces: where trivia ended up. `(whole node)` is the node\nitself, a property name is the token that property holds, and `(token)` is a token the node holds\nsome other way, in a list for instance. An empty row is a place no case puts trivia yet."

    for nodeClass in OakFacts.nodeClasses do
        if observed.Classes.Contains nodeClass.Name then
            let slots: string list =
                let declared: string list =
                    "(whole node)"
                    :: (OakFacts.tokenProperties nodeClass
                        |> List.map (fun (property: PropertyInfo) -> property.Name))

                if observed.Trivia.ContainsKey(nodeClass.Name, "(token)") then
                    declared @ [ "(token)" ]
                else
                    declared

            report.Append($"\n## %s{nodeClass.Name}\n\n").Append("| Where | Before | After |\n|---|---|---|\n")
            |> ignore

            for slot in slots do
                let kinds: Set<string> = seen observed.Trivia (nodeClass.Name, slot)

                let side (prefix: string) : string =
                    kinds
                    |> Seq.choose (fun (kind: string) ->
                        if kind.StartsWith(prefix, StringComparison.Ordinal) then
                            Some(kind.Substring prefix.Length)
                        else
                            None
                    )
                    |> String.concat ", "

                let before: string = side "before: "
                let after: string = side "after: "
                report.Append($"| %s{slot} | %s{before} | %s{after} |\n") |> ignore

    report.ToString()

/// Both reports, as the text of `reports/shapes.md` and `reports/trivia.md`.
let render () : string * string =
    let observed: Observed = observe ()
    shapesReport observed, triviaReport observed
