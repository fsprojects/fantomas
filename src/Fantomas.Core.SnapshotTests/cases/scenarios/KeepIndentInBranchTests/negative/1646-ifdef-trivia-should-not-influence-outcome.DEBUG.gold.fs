module Foo =
    [<Foo>]
    let blah<'a> config : Type =
        #if DEBUG
        failwith ""
        #endif
        DoThing.doIt ()
        let result = Runner.Run<'a> config

        if successful |> List.isEmpty then
            result
        else

        let errors =
            unsuccessful
            |> List.filter (fun report ->
                not report.BuildResult.IsBuildSuccess
                || not report.BuildResult.IsGenerateSuccess
            )
            |> List.map (fun report -> report.BuildResult.ErrorMessage)

        failwith ""
