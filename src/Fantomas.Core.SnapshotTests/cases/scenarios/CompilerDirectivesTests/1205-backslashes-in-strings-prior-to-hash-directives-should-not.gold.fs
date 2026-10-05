let loadFile n =
    let file =
        System.IO.Path
            .Combine(
                contentDir,
                (n |> System.IO.Path.GetFileNameWithoutExtension)
                + ".md"
            )
            .Replace("\\", "/")

    ()

let loader (projectRoot: string) (siteContent: SiteContents) =
#if WATCH
    let disableLiveRefresh = false
#else
    let disableLiveRefresh = true
#endif
    disableLiveRefresh
