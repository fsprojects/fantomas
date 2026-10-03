node {
    let! cachedResults =
        node {
            let! builderOpt, creationDiags = getAnyBuilder (options, userOpName)

            match builderOpt with
            | Some builder ->
                match! bc.GetCachedCheckFileResult(builder, fileName, sourceText, options) with
                | Some (_, checkResults) ->
                    return Some(builder, creationDiags, Some(FSharpCheckFileAnswer.Succeeded checkResults))
                | _ -> return Some(builder, creationDiags, None)
            | _ -> return None // the builder wasn't ready
        }

    ()
}
