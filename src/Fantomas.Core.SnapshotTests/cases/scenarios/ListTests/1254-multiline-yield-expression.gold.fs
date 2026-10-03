type FSharpCompilerServiceChecker(backgroundServiceEnabled) =
  member __.GetDependingProjects (file: FilePath) (options: seq<string * FSharpProjectOptions>) =
    let project =
      options
      |> Seq.tryFind (fun (k, _) -> k.ToUpperInvariant() = file.ToUpperInvariant())

    project
    |> Option.map (fun (_, option) ->
      option,
      [ yield!
          options
          |> Seq.map snd
          |> Seq.distinctBy (fun o -> o.ProjectFileName)
          |> Seq.filter (fun o ->
            o.ReferencedProjects
            |> Array.map (fun (_, v) -> Path.GetFullPath v.ProjectFileName)
            |> Array.contains option.ProjectFileName) ])
