let private additionalRefs =
    let refs =
        Directory.EnumerateFiles(Path.GetDirectoryName(typeof<System.Object>.Assembly.Location))
        |> Seq.filter (fun path -> Array.contains (Path.GetFileName(path)) assemblies)
        |> Seq.map (sprintf "-r:%s")

    [| "--simpleresolution"
       "--noframework"
       yield! refs |]
