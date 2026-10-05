(*---
fsharp_max_array_or_list_width = 40
fsharp_multiline_bracket_style = cramped
---*)
    let private additionalRefs =
        let refs =
            Directory.EnumerateFiles(Path.GetDirectoryName(typeof<System.Object>.Assembly.Location))
            |> Seq.filter (fun path -> Array.contains (Path.GetFileName(path)) assemblies)
            |> Seq.map (sprintf "-r:%s")

        [| "--simpleresolution"
           "--noframework"
           yield! refs |]
