module Fantomas.Tests.TestIgnoreFile

open System.Collections.Generic
open System.Collections.Immutable
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

let private snapshot (fs: IReadOnlyDictionary<'k, 'v>) : ('k * 'v) list =
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

    let found, dict =
        IgnoreFileStore.findIgnoreFile fs readFile ImmutableDictionary.Empty fileAtRoot

    found |> shouldEqual fantomasIgnore

    snapshot dict
    |> shouldEqual [ root, fantomasIgnore ]

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

    let found, dict =
        IgnoreFileStore.findIgnoreFile fs readFile ImmutableDictionary.Empty source

    found |> shouldEqual (Some target)

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

    let found, dict =
        IgnoreFileStore.findIgnoreFile fs readFile ImmutableDictionary.Empty source

    found |> shouldEqual (Some target)

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

    let found, dict =
        IgnoreFileStore.findIgnoreFile fs readFile ImmutableDictionary.Empty source1

    found |> shouldEqual (Some target)

    snapshot dict
    |> shouldEqual [ fs.Directory.GetParent(target).FullName, Some target
                     fs.Directory.GetParent(source1).FullName, None ]

    let found, dict = IgnoreFileStore.findIgnoreFile fs readFile dict source2
    found |> shouldEqual (Some target)

    snapshot dict
    |> shouldEqual [ fs.Directory.GetParent(target).FullName, Some target
                     fs.Directory.GetParent(source1).FullName, None ]

[<TestCase true>]
[<TestCase false>]
let ``IgnoreFileStore calls isIgnored`` (source1First: bool) =
    let fs = MockFileSystem()
    let root = fs.Path.GetTempPath() |> fs.Path.GetPathRoot

    let source1 = fs.Path.Combine(root, "folder1", "folder2", "Source1.fs")
    let source2 = fs.Path.Combine(root, "folder1", "folder2", "Source2.fs")
    let target = fs.Path.Combine(root, "folder1", ".fantomasignore")

    [ source1; source2; target ]
    |> makeFileHierarchy fs

    let read = oneShotParser ()

    // Specify that source1 is ignored, but not source2
    let isIgnored ignoreList filePath =
        // The mock "read from filesystem" of oneShotParser will just give us back
        // the path it read.
        ignoreList |> shouldEqual target
        filePath = source1

    use ignoreFileStore = new IgnoreFileStore<_>(fs, read, isIgnored)

    // The cache is hit, so we only make one call to the filesystem in the following.

    let testSource1 () =
        ignoreFileStore.IsIgnoredFile source1
        |> shouldEqual true

    let testSource2 () =
        ignoreFileStore.IsIgnoredFile source2
        |> shouldEqual false

    if source1First then
        testSource1 ()
        testSource2 ()
    else
        testSource2 ()
        testSource1 ()

[<Test>]
let ``Can purge the cache`` () =
    let fs = MockFileSystem()
    let root = fs.Path.GetTempPath() |> fs.Path.GetPathRoot

    let source1 = fs.Path.Combine(root, "folder1", "folder2", "Source1.fs")
    let source2 = fs.Path.Combine(root, "folder1", "folder2", "Source2.fs")
    let target = fs.Path.Combine(root, "folder1", ".fantomasignore")

    [ source1; source2; target ]
    |> makeFileHierarchy fs

    let fileSystemReads, read =
        let reads = ResizeArray()

        reads :> IReadOnlyList<_>,
        fun s ->
            reads.Add s
            "some ignore list contents"

    let isIgnored ignoreList filePath =
        // In this test we only read one .fantomasignore file
        ignoreList
        |> shouldEqual "some ignore list contents"
        // And we only ask about one source file
        filePath |> shouldEqual source1
        false

    use ignoreFileStore = new IgnoreFileStore<_>(fs, read, isIgnored)

    ignoreFileStore.IsIgnoredFile source1
    |> shouldEqual false

    fileSystemReads
    |> Seq.toList
    |> shouldEqual [ target ]

    // The cache is used the second time
    ignoreFileStore.IsIgnoredFile source1
    |> shouldEqual false

    fileSystemReads
    |> Seq.toList
    |> shouldEqual [ target ]

    // Now clear the cache and observe another read
    ignoreFileStore.PurgeCache()

    ignoreFileStore.IsIgnoredFile source1
    |> shouldEqual false

    fileSystemReads
    |> Seq.toList
    |> shouldEqual [ target; target ]
