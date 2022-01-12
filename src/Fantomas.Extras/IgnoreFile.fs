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

    let private relativePathPrefix =
        sprintf ".%c" Path.DirectorySeparatorChar

    let private removeRelativePathPrefix (path: string) =
        if path.StartsWith(relativePathPrefix) then
            path.Substring(2)
        else
            path

    let isIgnoredFile (file: string) =
        if hasNoIgnoreFile () then
            false
        else
            try
                let path = removeRelativePathPrefix file
                ignores.Value.IsIgnored(path, false)
            with
            | ex ->
                printfn "%A" ex
                false
