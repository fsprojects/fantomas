namespace Fantomas.Extras

open System.Collections.Immutable
open System.IO.Abstractions
open System.Threading

[<RequireQualifiedAccess>]
module internal IgnoreFileStore =

    [<Literal>]
    let IgnoreFileName = ".fantomasignore"

    let internal findIgnoreFile<'a>
        (fs: IFileSystem)
        (readFile: string -> 'a)
        (ignoreFiles: ImmutableDictionary<string, 'a option>)
        (filePath: string)
        : 'a option * ImmutableDictionary<string, 'a option> =
        // Note that this function has side effects: it mutates the state `ignoreFiles`.
        let rec findIgnoreFileAbove
            (ignoreFiles: ImmutableDictionary<string, 'a option>)
            (path: IDirectoryInfo)
            : 'a option * ImmutableDictionary<string, 'a option> =
            let potentialFile = fs.Path.Combine(path.FullName, IgnoreFileName)

            match ignoreFiles.TryGetValue path.FullName with
            | true, Some f -> Some f, ignoreFiles
            | true, None ->
                // A slight inefficiency here, in exchange for making the code a lot shorter:
                // if we've already computed that there is no .fantomasignore file immediately at this level,
                // we keep looking upwards in our knowledge of the file hierarchy.
                // (This is inefficient: we could in theory have stored the actual final result at all levels
                // once we computed it the first time.)
                if isNull path.Parent then
                    None, ignoreFiles
                else
                    findIgnoreFileAbove ignoreFiles path.Parent
            | _, _ ->
                let result =
                    if fs.File.Exists potentialFile then
                        readFile potentialFile |> Some
                    else
                        None

                let ignoreFiles = ignoreFiles.Add(path.FullName, result)

                match result with
                | None ->
                    if isNull path.Parent then
                        None, ignoreFiles
                    else
                        findIgnoreFileAbove ignoreFiles path.Parent
                | Some _ -> result, ignoreFiles

        findIgnoreFileAbove ignoreFiles (fs.Directory.GetParent filePath)

    type Message<'a> = RequestFilePath of string * AsyncReplyChannel<'a>

    let rec loop<'a>
        (fs: IFileSystem)
        (readFile: string -> 'a)
        (state: ImmutableDictionary<string, 'a option>)
        (mb: MailboxProcessor<_>)
        =
        async {
            let! message = mb.Receive()

            match message with
            | Message.RequestFilePath (path, reply) ->
                let found, state = findIgnoreFile fs readFile state path
                reply.Reply found
                return! loop fs readFile state mb
        }

    let removeRelativePathPrefix (fs: IFileSystem) (path: string) =
        if
            path.StartsWith(sprintf ".%c" fs.Path.DirectorySeparatorChar)
            || path.StartsWith(sprintf ".%c" fs.Path.AltDirectorySeparatorChar)
        then
            path.Substring(2)
        else
            path

type IgnoreFileStore<'a>
    (
        fs: IFileSystem,
        readFile: string -> 'a,
        isIgnored: 'a -> string -> bool,
        ?cancellationToken: CancellationToken
    ) =
    let mailbox: MailboxProcessor<_> =
        MailboxProcessor.Start(
            IgnoreFileStore.loop<'a> fs readFile ImmutableDictionary.Empty,
            ?cancellationToken = cancellationToken
        )

    member _.IsIgnoredFile(file: string) : bool =
        let fullPath = fs.Path.GetFullPath(file)

        match mailbox.PostAndReply(fun reply -> IgnoreFileStore.Message.RequestFilePath(fullPath, reply)) with
        | None -> false
        | Some ignores ->

            try
                let path = IgnoreFileStore.removeRelativePathPrefix fs file
                isIgnored ignores path
            with
            | ex ->
                printfn "%A" ex
                false

[<RequireQualifiedAccess>]
module IgnoreFile =

    open MAB.DotIgnore

    [<Literal>]
    let IgnoreFileName = IgnoreFileStore.IgnoreFileName

    let private defaultStore =
        lazy IgnoreFileStore(FileSystem(), IgnoreList, (fun ignoreList path -> ignoreList.IsIgnored(path, false)))

    let isIgnoredFile filePath =
        defaultStore.Force().IsIgnoredFile filePath
