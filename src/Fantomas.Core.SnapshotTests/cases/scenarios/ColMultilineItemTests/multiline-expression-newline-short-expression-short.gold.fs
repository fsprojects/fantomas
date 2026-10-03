asyncResult {
    let job =
        { JobType = EsriBoundaryImport
          FileToImport = filePath
          State = state
          DryRun = args.DryRun }

    importer.ApiMaster <! StartImportCmd job
    return Ok job
}
