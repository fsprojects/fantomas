module Fantomas.Core.Tests.IgnoreFileTests

open System.Collections.Generic
open NUnit.Framework
open FsUnitTyped
open Fantomas
open System.IO.Abstractions
open System.IO.Abstractions.TestingHelpers

let private makeFileHierarchy (fs: IFileSystem) (filePaths: string list) : unit =
    for path in filePaths do
        let fileInfo = fs.FileInfo.FromFileName path
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
    | Some ignoreFile ->
        failwithf "Somehow found a fantomasignore file even though none was present: %s" ignoreFile.Location.FullName
    | None -> ()

    getLoads () |> shouldBeEmpty

[<Test>]
let ``IgnoreFile.find preferentially finds the fantomasignore next to the source file`` () =
    let fs = MockFileSystem()
    let root = fs.Path.GetTempPath() |> fs.Path.GetPathRoot

    let source = fs.Path.Combine(root, "folder1", "folder2", "SomeSource.fs")
    let target = fs.Path.Combine(root, "folder1", "folder2", ".fantomasignore")

    [ source
      target
      // Another couple, at higher levels of the hierarchy
      fs.Path.Combine(root, "folder1", ".fantomasignore")
      fs.Path.Combine(root, ".fantomasignore") ]
    |> makeFileHierarchy fs

    let loadIgnoreList, getLoads = oneShotLoader (fun _ -> failwith "never called")
    let ignoreFile = IgnoreFile.find fs loadIgnoreList source |> Option.get
    ignoreFile.Location.FullName |> shouldEqual target
    getLoads () |> shouldEqual (Set.ofList [ target ])

[<Test>]
let ``IgnoreFile.find can find the fantomasignore one layer up from the source file`` () =
    let fs = MockFileSystem()
    let root = fs.Path.GetTempPath() |> fs.Path.GetPathRoot

    let source = fs.Path.Combine(root, "folder1", "folder2", "SomeSource.fs")
    let target = fs.Path.Combine(root, "folder1", ".fantomasignore")

    [ source
      target
      // Another one, at a higher level of the hierarchy
      fs.Path.Combine(root, ".fantomasignore") ]
    |> makeFileHierarchy fs

    let loadIgnoreList, getLoads = oneShotLoader (fun _ -> failwith "never called")
    let ignoreFile = IgnoreFile.find fs loadIgnoreList source |> Option.get
    ignoreFile.Location.FullName |> shouldEqual target
    getLoads () |> shouldEqual (Set.ofList [ target ])

[<Test>]
let ``IgnoreFile.current' does not load more than once`` () =
    let fs = MockFileSystem()
    let root = fs.Path.GetTempPath() |> fs.Path.GetPathRoot

    let source = fs.Path.Combine(root, "folder1", "folder2", "SomeSource.fs")
    let target = fs.Path.Combine(root, "folder1", ".fantomasignore")

    [ source; target ] |> makeFileHierarchy fs

    let loadIgnoreList, getLoads = oneShotLoader (fun _ -> failwith "never called")

    let ignoreFile =
        IgnoreFile.current' fs (fs.Path.GetDirectoryName target) loadIgnoreList

    getLoads () |> shouldBeEmpty

    for _ in 1..2 do
        let forced = ignoreFile.Force() |> Option.get
        forced.Location.FullName |> shouldEqual target
        // The second invocation would throw if we were somehow getting the
        // singleton wrong and re-invoking the find-and-load.
        getLoads () |> shouldEqual (Set.ofList [ target ])
