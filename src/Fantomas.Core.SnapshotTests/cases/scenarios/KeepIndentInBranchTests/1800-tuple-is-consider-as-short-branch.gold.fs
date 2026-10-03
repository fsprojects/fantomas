let nextModel, objectsRemoved =
    List.fold
        (fun acc item ->
            match entityInCurrentModel with
            | None ->
                // look it's a tuple
                nextModel, objectsRemoved
            | Some subjectToRemove ->

            let a = 5
            let b = 6
            someFunctionApp a b |> ignore
            acc
        )
        state
        []
