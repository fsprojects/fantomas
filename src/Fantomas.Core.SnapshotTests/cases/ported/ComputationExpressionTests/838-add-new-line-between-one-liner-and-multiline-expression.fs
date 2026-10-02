let AddEvents (req: HttpRequest, log: ILogger) =
    task {
        let! user = req.Authenticate(log)
        let! response =
            user
            |> Result.mapError sendUnAuthorizedRequest
            |> Result.bind (decodeEvents req.Body)
            |> Result.bind (validateEvents)
            |> Result.bind (authenticateEvents)
            |> Result.map (persistEvents)
            |> Result.either
        return response
    }

let AddEventsX (req: HttpRequest, log: ILogger) =
        let user = req.Authenticate(log)
        let response =
            user
            |> Result.mapError sendUnAuthorizedRequest
            |> Result.bind (decodeEvents req.Body)
            |> Result.bind (validateEvents)
            |> Result.bind (authenticateEvents)
            |> Result.map (persistEvents)
            |> Result.either
        response
