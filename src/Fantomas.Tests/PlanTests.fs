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
    plan fs silentLogger (fun _ -> None) inputPath outputPath

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

    // The finder production uses, so that what a plan honours is what a run honours.
    let findIgnoreFile: string -> IgnoreFile option =
        IgnoreFile.cachedFinder fs (IgnoreFile.loadIgnoreList fs)

    plan fs silentLogger findIgnoreFile inputPath outputPath

/// Plan against the given file system, honouring a `.fantomasignore` written in a subfolder.
let private planIgnoringUnder
    (fs: IFileSystem)
    (folder: string)
    (patterns: string)
    (inputPath: InputPath)
    (outputPath: OutputPath)
    : Result<WorkItem list, InputProblem>
    =
    fs.Directory.CreateDirectory(folder) |> ignore
    fs.File.WriteAllText(fs.Path.Combine(folder, IgnoreFile.IgnoreFileName), patterns)

    plan fs silentLogger (IgnoreFile.cachedFinder fs (IgnoreFile.loadIgnoreList fs)) inputPath outputPath

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
let ``an ignore pattern naming a folder means the folder is never opened`` () =
    // Not "every file below it is planned as ignored": the folder is not descended into, so the
    // files in it are never found. A run should have no more idea what is in a folder it was told
    // to stay out of than it has about a folder that is not there, and a report that listed them
    // could say how many files a vendored checkout has.
    let fs: IFileSystem = MockFileSystem()
    let src: string = fs.Path.Combine(mockRoot fs, "src")
    let deep: string = fs.Path.Combine(src, "generated", "deep", "A.fs")
    // Directly inside the ignored folder. This is what tells the folder being closed apart from the
    // folder being opened and its contents rejected one by one: the second plans this as ignored.
    let direct: string = fs.Path.Combine(src, "generated", "Direct.fs")
    let kept: string = fs.Path.Combine(src, "B.fs")
    [ deep; direct; kept ] |> makeFileHierarchy fs

    planIgnoring fs "src/generated/" (InputPath.Folder src) OutputPath.NotKnown
    |> shouldPlan [ WorkItem.Format(kept, kept) ]

[<Test>]
let ``a folder is named the same way whether or not the pattern ends in a separator`` () =
    // `.gitignore` spells a folder with a trailing separator, and the question has to be put to the
    // ignore library as one about a directory or the answer comes back no. It used to: `generated/`
    // left the folder open and matched the files directly inside it one at a time, so the same
    // intent skipped the same files by two different routes and reported them two different ways.
    let planWith (pattern: string) : Result<WorkItem list, InputProblem> =
        let fs: IFileSystem = MockFileSystem()
        let src: string = fs.Path.Combine(mockRoot fs, "src")

        [ fs.Path.Combine(src, "generated", "Direct.fs"); fs.Path.Combine(src, "B.fs") ]
        |> makeFileHierarchy fs

        planIgnoring fs pattern (InputPath.Folder src) OutputPath.NotKnown

    let names (plan: Result<WorkItem list, InputProblem>) : string list =
        match plan with
        | Error problem -> failwith $"Expected a plan, got %A{problem}"
        | Ok items ->
            items
            |> List.map (fun item ->
                match item with
                | WorkItem.Ignored file -> $"ignored %s{System.IO.Path.GetFileName file}"
                | WorkItem.Format(input, _) -> $"format %s{System.IO.Path.GetFileName input}"
            )
            |> List.sort

    names (planWith "src/generated/") |> shouldEqual [ "format B.fs" ]
    names (planWith "src/generated") |> shouldEqual [ "format B.fs" ]

[<Test>]
let ``an ignore pattern naming files still reports each one it skipped`` () =
    // The folder is opened, because the pattern is about files in it rather than about the folder,
    // so each file is asked about and each answer is reported. That is `.gitignore`'s own rule.
    let fs: IFileSystem = MockFileSystem()
    let src: string = fs.Path.Combine(mockRoot fs, "src")
    let skipped: string = fs.Path.Combine(src, "generated", "A.fs")
    let kept: string = fs.Path.Combine(src, "B.fs")
    [ skipped; kept ] |> makeFileHierarchy fs

    planIgnoring fs "src/generated/*.fs" (InputPath.Folder src) OutputPath.NotKnown
    |> shouldPlan [ WorkItem.Ignored skipped; WorkItem.Format(kept, kept) ]

[<Test>]
let ``an ignored file with an output path is skipped, so nothing is written there`` () =
    let fs: IFileSystem = MockFileSystem()
    let root: string = mockRoot fs
    let file: string = fs.Path.Combine(root, "A.fs")
    [ file ] |> makeFileHierarchy fs

    planIgnoring fs "*.fs" (InputPath.File file) (OutputPath.IO(fs.Path.Combine(root, "out", "A.fs")))
    |> shouldPlan [ WorkItem.Ignored file ]

[<Test>]
let ``an ignore file in a subfolder governs the files beside it`` () =
    // It used to be invisible to a run started above it. The daemon resolved the nearest ignore
    // file to each file it was asked about while the command line resolved one for the whole run
    // from the directory it started in, so the same file was skipped in an editor and formatted in
    // a pipeline.
    let fs: IFileSystem = MockFileSystem()
    let root: string = mockRoot fs
    let sub: string = fs.Path.Combine(root, "sub")
    let outside: string = fs.Path.Combine(root, "R.fs")
    let inside: string = fs.Path.Combine(sub, "S.fs")
    fs.Directory.CreateDirectory sub |> ignore
    fs.File.WriteAllText(outside, "let r = 1\n")
    fs.File.WriteAllText(inside, "let s = 1\n")

    planIgnoringUnder fs sub "S.fs" (InputPath.Folder root) OutputPath.NotKnown
    |> shouldPlan [ WorkItem.Format(outside, outside); WorkItem.Ignored inside ]

[<Test>]
let ``the nearest ignore file wins rather than every one above it`` () =
    // Where this differs from `.gitignore`, which is cumulative. The daemon has always taken the
    // nearest only, and the command line now answers the same way.
    let fs: IFileSystem = MockFileSystem()
    let root: string = mockRoot fs
    let sub: string = fs.Path.Combine(root, "sub")
    let inside: string = fs.Path.Combine(sub, "S.fs")
    fs.Directory.CreateDirectory sub |> ignore
    fs.File.WriteAllText(inside, "let s = 1\n")
    fs.File.WriteAllText(fs.Path.Combine(root, IgnoreFile.IgnoreFileName), "S.fs")

    // The root would ignore it; the nearer one says nothing about it, and the nearer one is asked.
    planIgnoringUnder fs sub "other.fs" (InputPath.Folder root) OutputPath.NotKnown
    |> shouldPlan [ WorkItem.Format(inside, inside) ]

[<Test>]
let ``a negated pattern takes a folder back out of one an earlier pattern matched, 3447`` () =
    // `sub/*` followed by `!sub/keep` is how `.gitignore` spells "all of it but that one", and it
    // only works if `sub` is opened: closing it decides that `sub/keep` is not there, and the line
    // that would have taken it back out is never reached.
    let fs: IFileSystem = MockFileSystem()
    let root: string = mockRoot fs
    let top: string = fs.Path.Combine(root, "Top.fs")
    let kept: string = fs.Path.Combine(root, "sub", "keep", "A.fs")
    let skipped: string = fs.Path.Combine(root, "sub", "drop", "B.fs")
    [ top; kept; skipped ] |> makeFileHierarchy fs

    planIgnoring fs "sub/*\n!sub/keep\n" (InputPath.Folder root) OutputPath.NotKnown
    |> shouldPlan
        [
            WorkItem.Format(top, top)
            WorkItem.Format(kept, kept)
            WorkItem.Ignored skipped
        ]
