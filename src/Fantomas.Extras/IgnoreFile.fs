namespace Fantomas.Extras

[<RequireQualifiedAccess>]
module IgnoreFile =

    open System.Collections.Concurrent
    open System.IO.Abstractions
    open MAB.DotIgnore

    [<Literal>]
    let IgnoreFileName = ".fantomasignore"

    let internal findIgnoreFile<'a>
        (fs: IFileSystem)
        (readFile: string -> 'a)
        (ignoreFiles: ConcurrentDictionary<string, 'a option>)
        (filePath: string)
        : 'a option =
        // Note that this function has side effects: it mutates the state `ignoreFiles`.
        let rec findIgnoreFileAbove (path: IDirectoryInfo) : 'a option =
            let potentialFile = fs.Path.Combine(path.FullName, IgnoreFileName)

            match ignoreFiles.TryGetValue path.FullName with
            | true, Some f -> Some f
            | true, None ->
                // A slight inefficiency here, in exchange for making the code a lot shorter:
                // if we've already computed that there is no .fantomasignore file immediately at this level,
                // we keep looking upwards in our knowledge of the file hierarchy.
                // (This is inefficient: we could in theory have stored the actual final result at all levels
                // once we computed it the first time.)
                if isNull path.Parent then
                    None
                else
                    findIgnoreFileAbove path.Parent
            | _, _ ->
                let result =
                    if fs.File.Exists potentialFile then
                        readFile potentialFile |> Some
                    else
                        None

                ignoreFiles.TryAdd(path.FullName, result)
                // We don't care if another thread beat us to this, they'll have
                // inserted exactly what we wanted to insert anyway
                |> ignore

                match result with
                | None ->
                    if isNull path.Parent then
                        None
                    else
                        findIgnoreFileAbove path.Parent
                | Some _ -> result

        findIgnoreFileAbove (fs.Directory.GetParent filePath)

    let private removeRelativePathPrefix (fs: IFileSystem) (path: string) =
        if
            path.StartsWith(sprintf ".%c" fs.Path.DirectorySeparatorChar)
            || path.StartsWith(sprintf ".%c" fs.Path.AltDirectorySeparatorChar)
        then
            path.Substring(2)
        else
            path

    /// Store of the IgnoreFiles present in each folder discovered so far.
    /// This is to save repeatedly hitting the disk for each file, and to save
    /// loading the IgnoreLists from the disk repeatedly (which is nontrivially expensive!).
    let private ignoreFiles: ConcurrentDictionary<string, IgnoreList option> =
        ConcurrentDictionary()

    let isIgnoredFile (file: string) =
        let fs = FileSystem()
        let fullPath = fs.Path.GetFullPath(file)

        match findIgnoreFile fs IgnoreList ignoreFiles fullPath with
        | None -> false
        | Some ignores ->

            try
                let path = removeRelativePathPrefix fs file
                ignores.IsIgnored(path, false)
            with
            | ex ->
                printfn "%A" ex
                false
