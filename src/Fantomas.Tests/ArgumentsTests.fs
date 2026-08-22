module Fantomas.Tests.ArgumentsTests

open System.IO.Abstractions
open System.IO.Abstractions.TestingHelpers
open NUnit.Framework
open FsUnitTyped
open Fantomas.Arguments
open Fantomas.Logging
open Fantomas.Tests.TestHelpers

[<Test>]
let ``no input path at all is unspecified`` () =
    classifyInputPath (MockFileSystem()) None |> shouldEqual InputPath.Unspecified

[<Test>]
let ``a path that is not there is reported as not found`` () =
    let fs: IFileSystem = MockFileSystem()
    let missing: string = fs.Path.Combine(mockRoot fs, "nope.fs")

    classifyInputPath fs (Some [ missing ])
    |> shouldEqual (InputPath.NotFound missing)

[<Test>]
let ``an existing folder is a folder`` () =
    let fs: IFileSystem = MockFileSystem()
    let src: string = fs.Path.Combine(mockRoot fs, "src")
    [ fs.Path.Combine(src, "A.fs") ] |> makeFileHierarchy fs

    classifyInputPath fs (Some [ src ]) |> shouldEqual (InputPath.Folder src)

[<Test>]
let ``an existing F# file is a file`` () =
    let fs: IFileSystem = MockFileSystem()
    let file: string = fs.Path.Combine(mockRoot fs, "src", "A.fs")
    [ file ] |> makeFileHierarchy fs

    classifyInputPath fs (Some [ file ]) |> shouldEqual (InputPath.File file)

[<Test>]
let ``an existing file Fantomas does not format is reported as such`` () =
    let fs: IFileSystem = MockFileSystem()
    let file: string = fs.Path.Combine(mockRoot fs, "src", "README.md")
    [ file ] |> makeFileHierarchy fs

    classifyInputPath fs (Some [ file ])
    |> shouldEqual (InputPath.NoFSharpFile file)

[<Test>]
let ``several paths are told apart into files and folders`` () =
    let fs: IFileSystem = MockFileSystem()
    let root: string = mockRoot fs
    let file: string = fs.Path.Combine(root, "A.fs")
    let folder: string = fs.Path.Combine(root, "src")
    [ file; fs.Path.Combine(folder, "B.fs") ] |> makeFileHierarchy fs

    classifyInputPath fs (Some [ file; folder ])
    |> shouldEqual (InputPath.Multiple([ file ], [ folder ]))

[<Test>]
let ``a folder with a dot in its name is still a folder`` () =
    let fs: IFileSystem = MockFileSystem()
    let root: string = mockRoot fs
    let folder: string = fs.Path.Combine(root, "my.stuff")
    let file: string = fs.Path.Combine(root, "A.fs")
    [ file; fs.Path.Combine(folder, "B.fs") ] |> makeFileHierarchy fs

    // Guessing from the extension called this a file, and formatting it then failed.
    classifyInputPath fs (Some [ folder; file ])
    |> shouldEqual (InputPath.Multiple([ file ], [ folder ]))

[<Test>]
let ``a file with no extension is still a file`` () =
    let fs: IFileSystem = MockFileSystem()
    let root: string = mockRoot fs
    let file: string = fs.Path.Combine(root, "Makefile")
    let folder: string = fs.Path.Combine(root, "src")
    [ file; fs.Path.Combine(folder, "B.fs") ] |> makeFileHierarchy fs

    classifyInputPath fs (Some [ file; folder ])
    |> shouldEqual (InputPath.Multiple([ file ], [ folder ]))

[<Test>]
let ``one missing path among several is reported rather than the rest being used`` () =
    let fs: IFileSystem = MockFileSystem()
    let root: string = mockRoot fs
    let present: string = fs.Path.Combine(root, "A.fs")
    let missing: string = fs.Path.Combine(root, "B.fs")
    [ present ] |> makeFileHierarchy fs

    classifyInputPath fs (Some [ present; missing ])
    |> shouldEqual (InputPath.NotFound missing)

[<Test>]
let ``several paths keep the order they were given`` () =
    let fs: IFileSystem = MockFileSystem()
    let root: string = mockRoot fs
    let a: string = fs.Path.Combine(root, "A.fs")
    let b: string = fs.Path.Combine(root, "B.fs")
    [ a; b ] |> makeFileHierarchy fs

    classifyInputPath fs (Some [ a; b ])
    |> shouldEqual (InputPath.Multiple([ a; b ], []))

[<Test>]
[<TestCase(null: string)>]
[<TestCase("n")>]
[<TestCase("normal")>]
[<TestCase("NORMAL")>]
let ``normal verbosity is the default and its spellings`` (value: string) =
    let given: string option = Option.ofObj value
    parseVerbosity given |> shouldEqual (Some VerbosityLevel.Normal)

[<Test>]
[<TestCase("d")>]
[<TestCase("detailed")>]
[<TestCase("Detailed")>]
let ``detailed verbosity and its spellings`` (value: string) =
    parseVerbosity (Some value) |> shouldEqual (Some VerbosityLevel.Detailed)

[<Test>]
[<TestCase("")>]
[<TestCase("verbose")>]
[<TestCase("dd")>]
let ``a verbosity Fantomas does not know is refused`` (value: string) =
    parseVerbosity (Some value) |> shouldEqual None
