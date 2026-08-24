module Fantomas.Tests.PlanTests

open System.IO.Abstractions
open System.IO.Abstractions.TestingHelpers
open NUnit.Framework
open FsUnitTyped
open Fantomas
open Fantomas.Arguments
open Fantomas.CommandResult
open Fantomas.Plan
open Fantomas.Tests.TestHelpers

/// Plan against the given file system, honouring no ignore file.
let private planOn
    (fs: IFileSystem)
    (inputPath: InputPath)
    (outputPath: OutputPath)
    : Result<WorkItem list, InputProblem>
    =
    plan fs silentLogger None inputPath outputPath

/// Plan against the given file system, honouring a `.fantomasignore` written at its root.
let private planIgnoring
    (fs: IFileSystem)
    (patterns: string)
    (inputPath: InputPath)
    (outputPath: OutputPath)
    : Result<WorkItem list, InputProblem>
    =
    let root: string = mockRoot fs
    fs.File.WriteAllText(fs.Path.Combine(root, IgnoreFile.IgnoreFileName), patterns)

    let ignoreFile: IgnoreFile option =
        IgnoreFile.findInDirectory fs root (IgnoreFile.loadIgnoreList fs)

    plan fs silentLogger ignoreFile inputPath outputPath

let private shouldPlan (expected: WorkItem list) (actual: Result<WorkItem list, InputProblem>) : unit =
    match actual with
    | Ok items -> items |> List.sort |> shouldEqual (List.sort expected)
    | Error problem -> failwith $"Expected a plan, got %A{problem}"

let private shouldRefuse (expected: InputProblem) (actual: Result<WorkItem list, InputProblem>) : unit =
    match actual with
    | Error problem -> problem |> shouldEqual expected
    | Ok items -> failwith $"Expected %A{expected}, got a plan of %d{items.Length} items"

[<Test>]
let ``an unsupported file type is refused`` () =
    planOn (MockFileSystem()) (InputPath.NoFSharpFile "A.md") OutputPath.NotKnown
    |> shouldRefuse (InputProblem.UnsupportedFileType "A.md")

[<Test>]
let ``a path that is not there is refused`` () =
    planOn (MockFileSystem()) (InputPath.NotFound "A.fs") OutputPath.NotKnown
    |> shouldRefuse (InputProblem.NotFound "A.fs")

[<Test>]
let ``no input path is refused`` () =
    planOn (MockFileSystem()) InputPath.Unspecified OutputPath.NotKnown
    |> shouldRefuse InputProblem.NoPathGiven

[<Test>]
let ``several input paths with an output path is refused`` () =
    planOn (MockFileSystem()) (InputPath.Multiple([ "A.fs" ], [])) (OutputPath.IO "out")
    |> shouldRefuse InputProblem.MultiplePathsWithOut

[<Test>]
let ``a single file with no output path is formatted where it lies`` () =
    let fs: IFileSystem = MockFileSystem()
    let file: string = fs.Path.Combine(mockRoot fs, "A.fs")
    [ file ] |> makeFileHierarchy fs

    planOn fs (InputPath.File file) OutputPath.NotKnown
    |> shouldPlan [ WorkItem.Format(file, file) ]

[<Test>]
let ``a single file with an output path is written there instead`` () =
    let fs: IFileSystem = MockFileSystem()
    let root: string = mockRoot fs
    let file: string = fs.Path.Combine(root, "A.fs")
    let out: string = fs.Path.Combine(root, "out", "A.fs")
    [ file ] |> makeFileHierarchy fs

    planOn fs (InputPath.File file) (OutputPath.IO out)
    |> shouldPlan [ WorkItem.Format(file, out) ]

[<Test>]
let ``a folder with no output path formats every file where it lies`` () =
    let fs: IFileSystem = MockFileSystem()
    let src: string = fs.Path.Combine(mockRoot fs, "src")
    let a: string = fs.Path.Combine(src, "A.fs")
    let b: string = fs.Path.Combine(src, "nested", "B.fs")
    [ a; b ] |> makeFileHierarchy fs

    planOn fs (InputPath.Folder src) OutputPath.NotKnown
    |> shouldPlan [ WorkItem.Format(a, a); WorkItem.Format(b, b) ]

[<Test>]
let ``an output folder mirrors the input tree rather than flattening it`` () =
    let fs: IFileSystem = MockFileSystem()
    let root: string = mockRoot fs
    let src: string = fs.Path.Combine(root, "src")
    let out: string = fs.Path.Combine(root, "out")
    // Two files with the same name would overwrite each other were the tree flattened.
    let a: string = fs.Path.Combine(src, "A.fs")
    let nestedA: string = fs.Path.Combine(src, "nested", "A.fs")
    [ a; nestedA ] |> makeFileHierarchy fs

    planOn fs (InputPath.Folder src) (OutputPath.IO out)
    |> shouldPlan
        [
            WorkItem.Format(a, fs.Path.Combine(out, "A.fs"))
            WorkItem.Format(nestedA, fs.Path.Combine(out, "nested", "A.fs"))
        ]

[<Test>]
let ``an output folder inside the input folder is not formatted into itself again`` () =
    let fs: IFileSystem = MockFileSystem()
    let src: string = fs.Path.Combine(mockRoot fs, "src")
    let out: string = fs.Path.Combine(src, "out")
    let a: string = fs.Path.Combine(src, "A.fs")
    // What a previous run left behind. Formatting it again would nest it one level deeper.
    let previous: string = fs.Path.Combine(out, "A.fs")
    [ a; previous ] |> makeFileHierarchy fs

    planOn fs (InputPath.Folder src) (OutputPath.IO out)
    |> shouldPlan [ WorkItem.Format(a, previous) ]

[<Test>]
let ``an input folder that is also the output folder is formatted in place`` () =
    let fs: IFileSystem = MockFileSystem()
    let src: string = fs.Path.Combine(mockRoot fs, "src")
    let a: string = fs.Path.Combine(src, "A.fs")
    [ a ] |> makeFileHierarchy fs

    // Spelled differently, so only comparing the resolved paths says they are the same place.
    let alsoSrc: string = fs.Path.Combine(src, ".")

    planOn fs (InputPath.Folder src) (OutputPath.IO alsoSrc)
    |> shouldPlan [ WorkItem.Format(a, a) ]

[<Test>]
let ``a folder spelled with a trailing separator is still formatted in place`` () =
    let fs: IFileSystem = MockFileSystem()
    let src: string = fs.Path.Combine(mockRoot fs, "src")
    let a: string = fs.Path.Combine(src, "A.fs")
    [ a ] |> makeFileHierarchy fs

    // `fantomas src/ --out src`. Taken as two different folders, every file below src counts as a
    // previous run's output and the plan comes out empty.
    planOn fs (InputPath.Folder(src + string<char> fs.Path.DirectorySeparatorChar)) (OutputPath.IO src)
    |> shouldPlan [ WorkItem.Format(a, a) ]

[<Test>]
let ``several files and folders are all formatted where they lie`` () =
    let fs: IFileSystem = MockFileSystem()
    let root: string = mockRoot fs
    let loose: string = fs.Path.Combine(root, "Loose.fs")
    let src: string = fs.Path.Combine(root, "src")
    let a: string = fs.Path.Combine(src, "A.fs")
    [ loose; a ] |> makeFileHierarchy fs

    planOn fs (InputPath.Multiple([ loose ], [ src ])) OutputPath.NotKnown
    |> shouldPlan [ WorkItem.Format(loose, loose); WorkItem.Format(a, a) ]

[<Test>]
let ``a file the ignore file matches is skipped rather than formatted`` () =
    let fs: IFileSystem = MockFileSystem()
    let file: string = fs.Path.Combine(mockRoot fs, "A.fs")
    [ file ] |> makeFileHierarchy fs

    planIgnoring fs "A.fs" (InputPath.File file) OutputPath.NotKnown
    |> shouldPlan [ WorkItem.Ignored file ]

[<Test>]
let ``an ignored file is skipped when found by walking a folder too`` () =
    let fs: IFileSystem = MockFileSystem()
    let src: string = fs.Path.Combine(mockRoot fs, "src")
    let kept: string = fs.Path.Combine(src, "Kept.fs")
    let skipped: string = fs.Path.Combine(src, "Skipped.fs")
    [ kept; skipped ] |> makeFileHierarchy fs

    planIgnoring fs "Skipped.fs" (InputPath.Folder src) OutputPath.NotKnown
    |> shouldPlan [ WorkItem.Format(kept, kept); WorkItem.Ignored skipped ]

[<Test>]
let ``an ignore pattern naming a nested path matches only that path`` () =
    let fs: IFileSystem = MockFileSystem()
    let src: string = fs.Path.Combine(mockRoot fs, "src")
    let skipped: string = fs.Path.Combine(src, "generated", "A.fs")
    let kept: string = fs.Path.Combine(src, "handwritten", "A.fs")
    [ skipped; kept ] |> makeFileHierarchy fs

    // Patterns are read relative to the folder holding the ignore file, with forward slashes,
    // whatever separator the platform uses for the paths themselves.
    planIgnoring fs "src/generated/A.fs" (InputPath.Folder src) OutputPath.NotKnown
    |> shouldPlan [ WorkItem.Ignored skipped; WorkItem.Format(kept, kept) ]

[<Test>]
let ``an ignore pattern naming a folder skips everything below it`` () =
    let fs: IFileSystem = MockFileSystem()
    let src: string = fs.Path.Combine(mockRoot fs, "src")
    let skipped: string = fs.Path.Combine(src, "generated", "deep", "A.fs")
    let kept: string = fs.Path.Combine(src, "B.fs")
    [ skipped; kept ] |> makeFileHierarchy fs

    planIgnoring fs "src/generated/" (InputPath.Folder src) OutputPath.NotKnown
    |> shouldPlan [ WorkItem.Ignored skipped; WorkItem.Format(kept, kept) ]

[<Test>]
let ``an ignored file with an output path is skipped, so nothing is written there`` () =
    let fs: IFileSystem = MockFileSystem()
    let root: string = mockRoot fs
    let file: string = fs.Path.Combine(root, "A.fs")
    [ file ] |> makeFileHierarchy fs

    planIgnoring fs "*.fs" (InputPath.File file) (OutputPath.IO(fs.Path.Combine(root, "out", "A.fs")))
    |> shouldPlan [ WorkItem.Ignored file ]
