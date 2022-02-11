module Fantomas.Tests.TestIgnoreFile

open System.Collections.Concurrent
open System.Collections.Generic
open NUnit.Framework
open FsUnitTyped
open Fantomas.Extras
open System.IO.Abstractions
open System.IO.Abstractions.TestingHelpers

let private makeFileHierarchy (fs: IFileSystem) (filePaths: string list) : unit =
    for path in filePaths do
        let fileInfo = fs.FileInfo.FromFileName path
        fileInfo.Directory.Create()
        fs.File.WriteAllText(fileInfo.FullName, "some text")

let private snapshot (fs: ConcurrentDictionary<'k, 'v>) : ('k * 'v) list =
    fs
    |> Seq.map (function
        | KeyValue (k, v) -> (k, v))
    |> Seq.toList
    |> List.sort

/// A helper method to create a `parse` function for injection into IgnoreFile;
/// this `parse` function will throw if it sees the same key twice.
/// This allows us to test that we never attempt to read the same .fantomasignore file twice.
let private oneShotParser () : string -> string =
    let seen = HashSet()

    fun s ->
        if seen.Add s then
            s
        else
            failwithf "Seen duplicate key: %s" s

[<TestCase true>]
[<TestCase false>]
let ``findIgnoreFile does not crash at the root`` (fantomasIgnorePresent: bool) =
    let fs = MockFileSystem()
    let root = fs.Path.GetTempPath() |> fs.Path.GetPathRoot

    let fileAtRoot = fs.Path.Combine(root, "SomeFile.fs")

    let fantomasIgnore =
        if fantomasIgnorePresent then
            let target = fs.Path.Combine(root, ".fantomasignore")
            makeFileHierarchy fs [ target ]
            Some target
        else
            None

    let readFile = oneShotParser ()

    let dict = ConcurrentDictionary()

    IgnoreFile.findIgnoreFile fs readFile dict fileAtRoot
    |> shouldEqual fantomasIgnore

    snapshot dict
    |> shouldEqual [ "/", fantomasIgnore ]

[<Test>]
let ``findIgnoreFile preferentially finds the fantomasignore next to the source file`` () =
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

    let readFile = oneShotParser ()

    let dict = ConcurrentDictionary()

    IgnoreFile.findIgnoreFile fs readFile dict source
    |> shouldEqual (Some target)

    snapshot dict
    |> shouldEqual [ fs.Directory.GetParent(target).FullName, Some target ]

[<Test>]
let ``findIgnoreFile can find the fantomasignore one layer up from the source file`` () =
    let fs = MockFileSystem()
    let root = fs.Path.GetTempPath() |> fs.Path.GetPathRoot

    let source = fs.Path.Combine(root, "folder1", "folder2", "SomeSource.fs")
    let target = fs.Path.Combine(root, "folder1", ".fantomasignore")

    [ source
      target
      // Another one, at a higher level of the hierarchy
      fs.Path.Combine(root, ".fantomasignore") ]
    |> makeFileHierarchy fs

    let readFile = oneShotParser ()

    let dict = ConcurrentDictionary()

    IgnoreFile.findIgnoreFile fs readFile dict source
    |> shouldEqual (Some target)

    snapshot dict
    |> shouldEqual [ fs.Directory.GetParent(target).FullName, Some target
                     fs.Directory.GetParent(source).FullName, None ]

[<Test>]
let ``findIgnoreFile uses the store`` () =
    let fs = MockFileSystem()
    let root = fs.Path.GetTempPath() |> fs.Path.GetPathRoot

    let source1 = fs.Path.Combine(root, "folder1", "folder2", "Source1.fs")
    let source2 = fs.Path.Combine(root, "folder1", "folder2", "Source2.fs")
    let target = fs.Path.Combine(root, "folder1", ".fantomasignore")

    [ source1; source2; target ]
    |> makeFileHierarchy fs

    let readFile = oneShotParser ()

    let dict = ConcurrentDictionary()

    IgnoreFile.findIgnoreFile fs readFile dict source1
    |> shouldEqual (Some target)

    snapshot dict
    |> shouldEqual [ fs.Directory.GetParent(target).FullName, Some target
                     fs.Directory.GetParent(source1).FullName, None ]

    IgnoreFile.findIgnoreFile fs readFile dict source2
    |> shouldEqual (Some target)

    snapshot dict
    |> shouldEqual [ fs.Directory.GetParent(target).FullName, Some target
                     fs.Directory.GetParent(source1).FullName, None ]
