module Fantomas.Core.Tests.IgnoreFileTests

open System.Collections.Generic
open NUnit.Framework
open FsUnitTyped
open Fantomas
open System.IO.Abstractions
open System.IO.Abstractions.TestingHelpers

let private makeFileHierarchy (fs: IFileSystem) (filePaths: string list) : unit =
    for path in filePaths do
        let fileInfo = fs.FileInfo.New path
        fileInfo.Directory.Create()
        fs.File.WriteAllText(fileInfo.FullName, "some text")

/// A helper method to create a `loadIgnoreList` function for injection into IgnoreFile;
/// this `loadIgnoreList` function will throw if it tries to load the same file twice.
let private oneShotLoader (isIgnored: IsPathIgnored) : (string -> IsPathIgnored) * (unit -> string Set) =
    let loadedFiles = HashSet()

    let load ignoreFilePath =
        let added = lock loadedFiles (fun () -> loadedFiles.Add ignoreFilePath)

        if added then
            isIgnored
        else
            failwithf "Attempted duplicate file load: %s" ignoreFilePath

    let freeze () =
        lock loadedFiles (fun () -> loadedFiles |> Set.ofSeq)

    load, freeze

[<Test>]
let ``IgnoreFile.find returns None if it can't find an ignorefile`` () =
    let fs = MockFileSystem()
    let root = fs.Path.GetTempPath() |> fs.Path.GetPathRoot

    let source = fs.Path.Combine(root, "folder1", "folder2", "SomeSource.fs")

    [ source ] |> makeFileHierarchy fs

    match IgnoreFile.find fs (fun _ -> failwith "not called") source with
    | None -> ()
    | Some ignoreFile -> failwithf "Unexpectedly found an ignorefile: %s" ignoreFile.Location.FullName

[<Test>]
let ``IgnoreFile.find does not crash at the root, ignore file present`` () =
    let fs = MockFileSystem()
    let root = fs.Path.GetTempPath() |> fs.Path.GetPathRoot

    let fileAtRoot = fs.Path.Combine(root, "SomeFile.fs")

    let loadIgnoreList, getLoads = oneShotLoader (fun _ -> failwith "never called")

    let target = fs.Path.Combine(root, ".fantomasignore")
    fs.File.WriteAllText(target, "some text")

    let ignoreFile = IgnoreFile.find fs loadIgnoreList fileAtRoot

    match ignoreFile with
    | None -> failwith "Failed to find the fantomasignore file at the root"
    | Some ignoreFile -> ignoreFile.Location.FullName |> shouldEqual target

    getLoads () |> shouldEqual (Set.ofList [ target ])

[<Test>]
let ``IgnoreFile.find does not crash at the root, no ignore file present`` () =
    let fs = MockFileSystem()
    let root = fs.Path.GetTempPath() |> fs.Path.GetPathRoot

    let fileAtRoot = fs.Path.Combine(root, "SomeFile.fs")

    let loadIgnoreList, getLoads = oneShotLoader (fun _ -> failwith "never called")

    let ignoreFile = IgnoreFile.find fs loadIgnoreList fileAtRoot

    match ignoreFile with
    | None -> ()
    | Some ignoreFile ->
        failwithf "Somehow found a fantomasignore file even though none was present: %s" ignoreFile.Location.FullName

    getLoads () |> shouldBeEmpty

[<Test>]
let ``IgnoreFile.find preferentially finds the fantomasignore next to the source file`` () =
    let fs = MockFileSystem()
    let root = fs.Path.GetTempPath() |> fs.Path.GetPathRoot

    let source = fs.Path.Combine(root, "folder1", "folder2", "SomeSource.fs")
    let target = fs.Path.Combine(root, "folder1", "folder2", ".fantomasignore")

    [
        source
        target
        // Another couple, at higher levels of the hierarchy
        fs.Path.Combine(root, "folder1", ".fantomasignore")
        fs.Path.Combine(root, ".fantomasignore")
    ]
    |> makeFileHierarchy fs

    let loadIgnoreList, getLoads = oneShotLoader (fun _ -> failwith "never called")

    let ignoreFile =
        match IgnoreFile.find fs loadIgnoreList source with
        | Some f -> f
        | None -> failwith $"calling %s{nameof IgnoreFile.find} failed"

    ignoreFile.Location.FullName |> shouldEqual target
    getLoads () |> shouldEqual (Set.ofList [ target ])

[<Test>]
let ``IgnoreFile.find can find the fantomasignore one layer up from the source file`` () =
    let fs = MockFileSystem()
    let root = fs.Path.GetTempPath() |> fs.Path.GetPathRoot

    let source = fs.Path.Combine(root, "folder1", "folder2", "SomeSource.fs")
    let target = fs.Path.Combine(root, "folder1", ".fantomasignore")

    [
        source
        target
        // Another one, at a higher level of the hierarchy
        fs.Path.Combine(root, ".fantomasignore")
    ]
    |> makeFileHierarchy fs

    let loadIgnoreList, getLoads = oneShotLoader (fun _ -> failwith "never called")

    let ignoreFile =
        match IgnoreFile.find fs loadIgnoreList source with
        | Some f -> f
        | None -> failwith $"calling %s{nameof IgnoreFile.find} failed"

    ignoreFile.Location.FullName |> shouldEqual target
    getLoads () |> shouldEqual (Set.ofList [ target ])

[<Test>]
let ``IgnoreFile.findInDirectory loads the ignore file above the directory, exactly once`` () =
    let fs = MockFileSystem()
    let root = fs.Path.GetTempPath() |> fs.Path.GetPathRoot

    let source = fs.Path.Combine(root, "folder1", "folder2", "SomeSource.fs")
    let target = fs.Path.Combine(root, "folder1", ".fantomasignore")

    [ source; target ] |> makeFileHierarchy fs

    let loadIgnoreList, getLoads = oneShotLoader (fun _ -> failwith "never called")

    let ignoreFile =
        IgnoreFile.findInDirectory fs (fs.Path.GetDirectoryName target) loadIgnoreList

    match ignoreFile with
    | Some found -> found.Location.FullName |> shouldEqual target
    | None -> failwith "Expected to find the ignore file above the directory"

    // oneShotLoader throws on a second load of the same file, so this says the walk up stopped at
    // the first ignore file it met rather than carrying on and loading another.
    getLoads () |> shouldEqual (Set.ofList [ target ])

// The command line resolves the ignore file through `cachedFinder` and the daemon resolves it
// through `find`. That they answer the same is the whole of the fix: they used not to, and the same
// file was skipped in an editor and formatted by CI. Nothing about the two call sites forces them
// together, so it is pinned here.
[<Test>]
let ``the command line and the daemon resolve the same ignore file for the same path`` () =
    let fs: IFileSystem = MockFileSystem()
    let root: string = fs.Path.GetTempPath() |> fs.Path.GetPathRoot
    let repo: string = fs.Path.Combine(root, "repo")
    let sub: string = fs.Path.Combine(repo, "sub")

    let atRoot: string = fs.Path.Combine(repo, ".fantomasignore")
    let atSub: string = fs.Path.Combine(sub, ".fantomasignore")

    let files: string list =
        [
            fs.Path.Combine(repo, "R.fs")
            fs.Path.Combine(sub, "S.fs")
            fs.Path.Combine(sub, "deeper", "D.fs")
        ]

    (atRoot :: atSub :: files) |> makeFileHierarchy fs

    // `find` is what the daemon calls, once per file. `cachedFinder` is what the command line
    // builds once and calls per file.
    let asTheDaemonDoes: string -> IgnoreFile option =
        IgnoreFile.find fs (IgnoreFile.loadIgnoreList fs)

    let asTheCommandLineDoes: string -> IgnoreFile option =
        IgnoreFile.cachedFinder fs (IgnoreFile.loadIgnoreList fs)

    let located (finder: string -> IgnoreFile option) (file: string) : string =
        match finder file with
        | Some found -> found.Location.FullName
        | None -> "none"

    for file in files do
        located asTheCommandLineDoes file |> shouldEqual (located asTheDaemonDoes file)

    // And the answer is the nearest one, which is the part that used to differ: a run started at
    // the repository root resolved the root file for everything below it.
    located asTheCommandLineDoes (fs.Path.Combine(repo, "R.fs"))
    |> shouldEqual atRoot

    located asTheCommandLineDoes (fs.Path.Combine(sub, "S.fs")) |> shouldEqual atSub

    located asTheCommandLineDoes (fs.Path.Combine(sub, "deeper", "D.fs"))
    |> shouldEqual atSub

[<Test>]
let ``the command line resolves an ignore file once per directory rather than once per file`` () =
    // A folder walk asks about every file in turn, and resolving each from scratch walks the tree
    // and compiles the patterns again. `oneShotLoader` throws on a second load of the same file.
    let fs: IFileSystem = MockFileSystem()
    let root: string = fs.Path.GetTempPath() |> fs.Path.GetPathRoot
    let repo: string = fs.Path.Combine(root, "repo")
    let target: string = fs.Path.Combine(repo, ".fantomasignore")

    [
        target
        fs.Path.Combine(repo, "A.fs")
        fs.Path.Combine(repo, "B.fs")
        fs.Path.Combine(repo, "sub", "C.fs")
    ]
    |> makeFileHierarchy fs

    let loadIgnoreList, getLoads = oneShotLoader (fun _ -> false)
    let finder: string -> IgnoreFile option = IgnoreFile.cachedFinder fs loadIgnoreList

    for file in [ "A.fs"; "B.fs" ] do
        finder (fs.Path.Combine(repo, file)) |> Option.isSome |> shouldEqual true

    finder (fs.Path.Combine(repo, "sub", "C.fs"))
    |> Option.isSome
    |> shouldEqual true

    getLoads () |> shouldEqual (Set.ofList [ target ])
