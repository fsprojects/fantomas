namespace Fantomas.Extras

[<RequireQualifiedAccess>]
module IgnoreFile =

    open System.IO
    open MAB.DotIgnore

    [<Literal>]
    let IgnoreFileName = ".fantomasignore"

    let private getIgnoreFilePath () =  Path.Combine(Directory.GetCurrentDirectory(), IgnoreFileName).TrimStart('/');

    let ignores = lazy IgnoreList(getIgnoreFilePath ())

    let private hasNoIgnoreFile () =
        let path = getIgnoreFilePath ()
        File.Exists path
        |> not

    let isIgnoredFile (file: string) =
        if hasNoIgnoreFile () then
            false
        else
            try
                printfn ".fantomasignore at %s" (getIgnoreFilePath ())
                let fullPath = Path.GetFullPath(file)
                if isNull fullPath then
                    failwithf "Path.GetFullPath is null for %s" file
                ignores.Value.IsIgnored(fullPath, false)
            with
            | ex ->
                printfn "%A" ex
                false