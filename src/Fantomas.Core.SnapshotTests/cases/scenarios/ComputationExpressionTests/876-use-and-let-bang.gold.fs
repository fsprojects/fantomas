let private getAST log (req: HttpRequest) =
    async {
        use stream = new StreamReader(req.Body)
        let! json = stream.ReadToEndAsync() |> Async.AwaitTask
        let parseRequest = Decoders.decodeInputRequest json

        match parseRequest with
        | Result.Ok input when (input.SourceCode.Length < Const.sourceSizeLimit) ->
            let! astResult = parseAST log input

            match astResult with
            | Result.Ok ast ->
                let node =
                    match ast with
                    | ParsedInput.ImplFile(ParsedImplFileInput.ParsedImplFileInput(_, _, _, _, hds, mns, _)) ->
                        Fantomas.AstTransformer.astToNode hds mns

                    | ParsedInput.SigFile(ParsedSigFileInput.ParsedSigFileInput(_, _, _, _, mns)) ->
                        Fantomas.AstTransformer.sigAstToNode mns
                    |> Encoders.nodeEncoder

                let responseJson =
                    Encoders.encodeResponse node (sprintf "%A" ast)
                    |> Thoth.Json.Net.Encode.toString 2

                return sendJson responseJson

            | Error error -> return sendBadRequest (sprintf "%A" error)

        | Result.Ok _ -> return sendTooLargeError ()

        | Error err -> return sendInternalError (sprintf "%A" err)
    }
