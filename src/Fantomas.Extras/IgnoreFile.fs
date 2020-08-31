namespace Fantomas.Extras

[<RequireQualifiedAccess>]
module IgnoreFile =

    open System.IO
    open MAB.DotIgnore

    [<Literal>]
    let IgnoreFileName = ".fantomasignore"

    let private ignoreFile () =  Path.Combine(Directory.GetCurrentDirectory(), IgnoreFileName)

    let ignores = lazy IgnoreList(ignoreFile ())

    let private hasNoIgnoreFile () =
        let path = ignoreFile ()
        File.Exists path
        |> not

    let isIgnoredFile (file: string) =
        if hasNoIgnoreFile () then
            false
        else
            try
                let fullPath = Path.GetFullPath(file)
                if isNull fullPath then
                    failwithf "Path.GetFullPath is null for %s" file
                ignores.Value.IsIgnored(fullPath, false)
            with
            | ex ->
                printfn "%A" ex
                false