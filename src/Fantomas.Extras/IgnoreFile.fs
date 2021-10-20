namespace Fantomas.Extras

[<RequireQualifiedAccess>]
module IgnoreFile =

    open System.IO
    open MAB.DotIgnore

    [<Literal>]
    let IgnoreFileName = ".fantomasignore"

    let rec private findIgnoreFile (filePath: string) : string option =
        let allParents =
            let rec addParent (di: DirectoryInfo) (finalContinuation: string list -> string list) =
                if isNull di.Parent then
                    finalContinuation [ di.FullName ]
                else
                    addParent di.Parent (fun parents -> di.FullName :: parents |> finalContinuation)

            addParent (Directory.GetParent filePath) id

        allParents
        |> List.tryFind (fun p -> Path.Combine(p, IgnoreFileName) |> File.Exists)
        |> Option.map (fun p -> Path.Combine(p, IgnoreFileName))

    let private relativePathPrefix =
        sprintf ".%c" Path.DirectorySeparatorChar

    let private removeRelativePathPrefix (path: string) =
        if path.StartsWith(relativePathPrefix) then
            path.Substring(2)
        else
            path

    let isIgnoredFile (file: string) =
        let fullPath = Path.GetFullPath(file)

        match findIgnoreFile fullPath with
        | None -> false
        | Some ignoreFile ->
            let ignores = IgnoreList(ignoreFile)

            try
                let path = removeRelativePathPrefix file
                ignores.IsIgnored(path, false)
            with
            | ex ->
                printfn "%A" ex
                false
