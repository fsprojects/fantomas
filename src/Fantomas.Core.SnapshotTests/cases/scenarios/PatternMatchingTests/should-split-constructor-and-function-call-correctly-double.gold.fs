let update msg model =
    let res =
        match msg with
        | AMessage ->
            { model with AFieldWithAVeryVeryVeryLooooooongName = 10 }
                .RecalculateTotal()
        | AnotherMessage -> model

    res
