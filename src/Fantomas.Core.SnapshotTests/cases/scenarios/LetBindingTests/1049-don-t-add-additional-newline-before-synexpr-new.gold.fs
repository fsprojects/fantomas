let getVersion () =
    let version =
        let assembly = typeof<FSharp.Compiler.SourceCodeServices.FSharpChecker>.Assembly

        let version = assembly.GetName().Version
        sprintf "%i.%i.%i" version.Major version.Minor version.Revision

    new HttpResponseMessage(
        HttpStatusCode.OK,
        Content = new StringContent(version, System.Text.Encoding.UTF8, "application/text")
    )
