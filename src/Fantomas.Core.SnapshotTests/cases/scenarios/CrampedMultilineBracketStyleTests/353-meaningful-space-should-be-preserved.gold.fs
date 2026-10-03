to'
    .WithCommon(fun o' ->
        { dotnetOptions o' with
            WorkingDirectory = Path.getFullName "RegressionTesting/issue29"
            Verbosity = Some DotNet.Verbosity.Minimal })
    .WithParameters
