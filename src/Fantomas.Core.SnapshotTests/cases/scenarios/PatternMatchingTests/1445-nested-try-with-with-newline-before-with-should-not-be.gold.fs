let private formatResponse<'options> () =
    async {
        use stream = new StreamReader(req.Body)
        let! json = stream.ReadToEndAsync() |> Async.AwaitTask
        let model = Decoders.decodeRequest json

        let configResult =
            Result.map (fun r -> r, mapFantomasOptionsToRecord r.Options) model

        match configResult with
        | Ok({ SourceCode = code; IsFsi = isFsi }, config) ->
            let fileName = if isFsi then "tmp.fsi" else "tmp.fsx"

            try
                let! formatted = format fileName code config
                let! validationResult = validateResult fileName formatted

                return sendBadRequest content

            with exn ->
                return sendBadRequest (sprintf "%A" exn)
        | Error err -> return sendInternalError (err)
    }
