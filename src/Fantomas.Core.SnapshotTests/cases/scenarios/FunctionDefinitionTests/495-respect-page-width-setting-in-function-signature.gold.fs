let fold
    (funcs : ResultFunc<'Input, 'Output, 'TError> seq)
    (input : 'Input)
    : Result<'Output list, 'TError list> =
    let mutable anyErrors = false
    let mutable collectedOutputs = []
    let mutable collectedErrors = []

    let runValidator (validator : ResultFunc<'Input, 'Output, 'TError>) input =
        let validatorResult = validator input

        match validatorResult with
        | Error error ->
            anyErrors <- true
            collectedErrors <- error :: collectedErrors
        | Ok output -> collectedOutputs <- output :: collectedOutputs

    funcs |> Seq.iter (fun validator -> runValidator validator input)

    match anyErrors with
    | true -> Error collectedErrors
    | false -> Ok collectedOutputs
