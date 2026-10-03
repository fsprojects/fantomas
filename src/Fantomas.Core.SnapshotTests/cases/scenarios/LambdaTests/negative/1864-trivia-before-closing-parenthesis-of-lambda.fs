Target.create "Install" (fun x ->
    Yarn.install (fun o -> { o with WorkingDirectory = clientDir })
    // Paket restore will already happen when the build.fsx dependencies are restored
)
