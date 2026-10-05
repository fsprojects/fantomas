match x with
| NotificationEvent.Lint(file, warnings) ->
    let uri = Path.FilePathToUri file

    diagnosticCollections.AddOrUpdate((uri, "F# Linter"), [||], (fun _ _ -> [||]))
    |> ignore

    let fs =
        warnings
        |> List.choose (fun w ->
            w.Warning.Details.SuggestedFix
            |> Option.bind (fun f ->
                let f = f.Force()
                let range = fcsRangeToLsp w.Warning.Details.Range

                f
                |> Option.map (fun f -> range, { Range = range; NewText = f.ToText })))

    lintFixes.[uri] <- fs
