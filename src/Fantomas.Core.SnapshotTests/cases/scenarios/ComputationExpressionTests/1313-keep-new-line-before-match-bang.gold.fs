/// a codefix that generates union cases for an incomplete match expression
let generateUnionCases =
    ifDiagnosticByMessage
        (fun diagnostic codeActionParams ->
            asyncResult {
                let! (tyRes, line, lines) = getParseResultsForFile fileName pos

                match! generateCases tyRes pos lines line |> Async.map Ok with
                | CoreResponse.Res(insertString: string, insertPosition) ->
                    return
                        [ { SourceDiagnostic = Some diagnostic
                            File = codeActionParams.TextDocument
                            Title = "Generate union pattern match cases"
                            Edits = [| { Range = range; NewText = replaced } |]
                            Kind = Fix } ]

                | _ -> return []
            }
            |> AsyncResult.foldResult id (fun _ -> []))
        "Incomplete pattern matches on this expression. For example"
