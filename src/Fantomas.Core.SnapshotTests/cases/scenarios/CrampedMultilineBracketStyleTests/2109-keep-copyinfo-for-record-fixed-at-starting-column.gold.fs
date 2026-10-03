let defaultTestOptions fwk common (o: DotNet.TestOptions) =
    { o.WithCommon(
          (fun o2 -> { o2 with Verbosity = Some DotNet.Verbosity.Normal })
          >> common
      ) with
        NoBuild = true
        Framework = fwk // Some "netcoreapp3.0"
        Configuration = DotNet.BuildConfiguration.Debug }
