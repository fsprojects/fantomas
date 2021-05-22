namespace Fantomas.Extras

[<RequireQualifiedAccess>]
module IgnoreFile =

    open System.IO
    open MAB.DotIgnore

    [<Literal>]
    let IgnoreFileName = ".fantomasignore"

    let private getIgnoreFilePath () =
        Path.Combine(Directory.GetCurrentDirectory(), IgnoreFileName)

    let ignores = lazy (IgnoreList(getIgnoreFilePath ()))

    let private hasNoIgnoreFile () =
        let path = getIgnoreFilePath ()
        File.Exists path |> not

    let isIgnoredFile (file: string) =
        if hasNoIgnoreFile () then
            false
        else
            try
                let fullPath = Path.GetFullPath(file)
                ignores.Value.IsIgnored(fullPath, false)
            with
            | ex ->
                printfn "%A" ex
                false
