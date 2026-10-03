type ProjectController(checker: FSharpChecker) =
  member x.LoadWorkspace (files: string list) (tfmForScripts: FSIRefs.TFM) onProjectLoaded (generateBinlog: bool) =
    async {
      match Environment.workspaceLoadDelay () with
      | delay when delay > TimeSpan.Zero ->
        do
          NonAsync.Sleep(
            Environment.workspaceLoadDelay().TotalMilliseconds
            |> int
          )
      | _ -> ()

      return true
    }
