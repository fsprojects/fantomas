Target.create "Install" (fun _ ->
    Yarn.install (fun o -> { o with WorkingDirectory = clientDir })
    // Paket restore will already happen when the build.fsx dependencies are restored
)
