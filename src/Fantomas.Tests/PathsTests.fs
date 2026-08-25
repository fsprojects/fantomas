module Fantomas.Tests.PathsTests

open System.IO
open System.IO.Abstractions
open System.IO.Abstractions.TestingHelpers
open NUnit.Framework
open FsUnitTyped
open Fantomas.Paths
open Fantomas.Tests.TestHelpers

let private separator: char = Path.DirectorySeparatorChar

[<Test>]
[<TestCase("A.fs")>]
[<TestCase("A.fsi")>]
[<TestCase("A.fsx")>]
[<TestCase("A.ml")>]
[<TestCase("A.mli")>]
let ``the extensions Fantomas formats are F# files`` (path: string) = isFSharpFile path |> shouldEqual true

[<Test>]
[<TestCase("A.FS")>]
[<TestCase("A.Fsi")>]
[<TestCase("A.ML")>]
let ``the extension is recognised whatever its case`` (path: string) = isFSharpFile path |> shouldEqual true

[<Test>]
[<TestCase("A.cs")>]
[<TestCase("A.txt")>]
[<TestCase("A")>]
[<TestCase("A.fs.txt")>]
let ``anything else is not an F# file`` (path: string) = isFSharpFile path |> shouldEqual false

[<Test>]
[<TestCase("obj")>]
[<TestCase(".fable")>]
[<TestCase("fable_modules")>]
[<TestCase("node_modules")>]
let ``a folder someone else wrote is excluded`` (folder: string) =
    isExcludedDirName folder |> shouldEqual true

[<Test>]
let ``a folder whose name merely starts with an excluded one is not excluded`` () =
    // The walk descends a directory at a time and compares its name, so `objects` no longer has to
    // be told apart from `obj` by looking for separators either side of it inside a path.
    isExcludedDirName "objects" |> shouldEqual false

[<Test>]
let ``every F# file below a folder is found, at any depth`` () =
    let fs: IFileSystem = MockFileSystem()
    let root: string = mockRoot fs
    let src: string = fs.Path.Combine(root, "src")

    [
        fs.Path.Combine(src, "A.fs")
        fs.Path.Combine(src, "nested", "B.fsi")
        fs.Path.Combine(src, "nested", "deeper", "C.fsx")
    ]
    |> makeFileHierarchy fs

    findAllFilesRecursively fs (fun _ -> false) src
    |> Seq.map (fun (file: string) -> fs.Path.GetFileName file)
    |> Seq.sort
    |> List.ofSeq
    |> shouldEqual [ "A.fs"; "B.fsi"; "C.fsx" ]

[<Test>]
let ``files that are not F# are left out of the walk`` () =
    let fs: IFileSystem = MockFileSystem()
    let root: string = mockRoot fs
    let src: string = fs.Path.Combine(root, "src")

    [
        fs.Path.Combine(src, "A.fs")
        fs.Path.Combine(src, "README.md")
        fs.Path.Combine(src, "App.csproj")
    ]
    |> makeFileHierarchy fs

    findAllFilesRecursively fs (fun _ -> false) src
    |> Seq.map (fun (file: string) -> fs.Path.GetFileName file)
    |> List.ofSeq
    |> shouldEqual [ "A.fs" ]

[<Test>]
let ``build output is left out of the walk`` () =
    let fs: IFileSystem = MockFileSystem()
    let root: string = mockRoot fs
    let src: string = fs.Path.Combine(root, "src")

    [
        fs.Path.Combine(src, "A.fs")
        fs.Path.Combine(src, "obj", "Generated.fs")
        fs.Path.Combine(src, "node_modules", "thing", "B.fs")
    ]
    |> makeFileHierarchy fs

    findAllFilesRecursively fs (fun _ -> false) src
    |> Seq.map (fun (file: string) -> fs.Path.GetFileName file)
    |> List.ofSeq
    |> shouldEqual [ "A.fs" ]

[<Test>]
let ``the folders leading up to a file are created`` () =
    let fs: IFileSystem = MockFileSystem()
    let root: string = mockRoot fs
    let target: string = fs.Path.Combine(root, "out", "nested", "A.fs")

    ensureParentFolderExists fs target

    fs.Directory.Exists(fs.Path.Combine(root, "out", "nested")) |> shouldEqual true

[<Test>]
let ``a bare file name has no folder to create`` () =
    let fs: IFileSystem = MockFileSystem()

    // Path.GetDirectoryName yields an empty string here, which must not be created.
    ensureParentFolderExists fs "A.fs"

[<Test>]
let ``two spellings of the same location are the same path`` () =
    let fs: IFileSystem = MockFileSystem()
    let root: string = mockRoot fs
    let direct: string = fs.Path.Combine(root, "src", "A.fs")
    let roundabout: string = fs.Path.Combine(root, "src", ".", "A.fs")

    isSamePath fs direct roundabout |> shouldEqual true

[<Test>]
let ``a trailing separator names the same path`` () =
    let fs: IFileSystem = MockFileSystem()
    let src: string = fs.Path.Combine(mockRoot fs, "src")

    isSamePath fs src (src + string<char> separator) |> shouldEqual true
    isSamePath fs (src + string<char> separator) src |> shouldEqual true

[<Test>]
let ``two different locations are not the same path`` () =
    let fs: IFileSystem = MockFileSystem()
    let root: string = mockRoot fs

    isSamePath fs (fs.Path.Combine(root, "src", "A.fs")) (fs.Path.Combine(root, "src", "B.fs"))
    |> shouldEqual false

[<Test>]
let ``a file below a folder is in it, at any depth`` () =
    let fs: IFileSystem = MockFileSystem()
    let root: string = mockRoot fs
    let src: string = fs.Path.Combine(root, "src")

    isInFolder fs src (fs.Path.Combine(src, "A.fs")) |> shouldEqual true
    isInFolder fs src (fs.Path.Combine(src, "nested", "A.fs")) |> shouldEqual true

[<Test>]
let ``a sibling folder whose name starts with the folder is not inside it`` () =
    let fs: IFileSystem = MockFileSystem()
    let root: string = mockRoot fs

    // `src-generated` starts with `src`, so without the separator the comparison appends it
    // would count as being inside.
    isInFolder fs (fs.Path.Combine(root, "src")) (fs.Path.Combine(root, "src-generated", "A.fs"))
    |> shouldEqual false

[<Test>]
let ``a folder is not inside itself`` () =
    let fs: IFileSystem = MockFileSystem()
    let src: string = fs.Path.Combine(mockRoot fs, "src")

    isInFolder fs src src |> shouldEqual false

[<Test>]
let ``a folder the ignore file names is never opened`` () =
    // Not "every file in it is asked about and the answer discarded": never opened. A run should
    // have no more idea what is in a folder it was told to stay out of than it has about a folder
    // that is not there, which is how a report came to say how many files a vendored checkout has.
    let fs: IFileSystem = MockFileSystem()
    let root: string = mockRoot fs
    let vendored: string = fs.Path.Combine(root, "vendored")

    [
        fs.Path.Combine(root, "A.fs")
        fs.Path.Combine(vendored, "B.fs")
        fs.Path.Combine(vendored, "deep", "C.fs")
    ]
    |> makeFileHierarchy fs

    let opened: ResizeArray<string> = ResizeArray()

    let isIgnoredDirectory (directory: string) : bool =
        opened.Add(fs.Path.GetFileName directory)
        fs.Path.GetFileName directory = "vendored"

    findAllFilesRecursively fs isIgnoredDirectory root
    |> Seq.map (fun (file: string) -> fs.Path.GetFileName file)
    |> List.ofSeq
    |> shouldEqual [ "A.fs" ]

    // Asked about `vendored` and stopped there. What matters is what it was never asked about:
    // `deep` lives inside `vendored`, so reaching it would mean the folder had been opened.
    opened |> shouldContain "vendored"
    opened |> shouldNotContain "deep"
