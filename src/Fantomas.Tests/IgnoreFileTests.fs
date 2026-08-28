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

// ---- which line of the ignore file decided ----

/// An ignore file of these lines at the root of a mock file system, and the path to ask about.
let private governing (patterns: string list) (relativePath: string) : IgnoreFile * string =
    let fs: IFileSystem = MockFileSystem()
    let root: string = fs.Path.GetTempPath() |> fs.Path.GetPathRoot

    let file: string =
        fs.Path.Combine(root, relativePath.Replace("/", string<char> fs.Path.DirectorySeparatorChar))

    makeFileHierarchy fs [ file ]
    fs.File.WriteAllText(fs.Path.Combine(root, IgnoreFile.IgnoreFileName), String.concat "\n" patterns)

    match IgnoreFile.findInDirectory fs root (IgnoreFile.loadIgnoreList fs) with
    | None -> failwith $"The ignore file just written at %s{root} was not found again."
    | Some ignoreFile -> ignoreFile, file

[<Test>]
let ``the line whose pattern matched is quoted with its number`` () =
    let ignoreFile, file = governing [ "*.fsx"; "A.fs" ] "A.fs"

    IgnoreFile.matchingLines ignoreFile file
    |> shouldEqual
        [
            {
                LineNumber = 2
                Pattern = "A.fs"
                Negated = false
            }
        ]

[<Test>]
let ``a comment and a blank line match nothing`` () =
    let ignoreFile, file = governing [ "# A.fs"; ""; "   "; "A.fs" ] "A.fs"

    IgnoreFile.matchingLines ignoreFile file
    |> List.map (fun m -> m.LineNumber)
    |> shouldEqual [ 4 ]

[<Test>]
let ``a pattern that takes a path back out is a match and says so`` () =
    let ignoreFile, file = governing [ "*.fs"; "!A.fs" ] "A.fs"

    IgnoreFile.matchingLines ignoreFile file
    |> List.map (fun m -> m.LineNumber, m.Negated)
    |> shouldEqual [ (1, false); (2, true) ]

[<Test>]
let ``a pattern naming a folder matches the files inside it`` () =
    let ignoreFile, file = governing [ "vendor/" ] "vendor/A.fs"

    IgnoreFile.matchingLines ignoreFile file
    |> List.map (fun m -> m.Pattern)
    |> shouldEqual [ "vendor/" ]

[<Test>]
let ``the last line that matches is the one the ignore file itself decided by`` () =
    // The property the whole thing rests on: quoting a line back is only worth doing if that line
    // is the one that settled it. Each case pairs patterns with a path and the answer expected.
    let cases: (string list * string * bool) list =
        [
            [ "A.fs" ], "A.fs", true
            [ "*.fs" ], "A.fs", true
            [ "*.fsx" ], "A.fs", false
            [ "*.fs"; "!A.fs" ], "A.fs", false
            [ "!A.fs"; "*.fs" ], "A.fs", true
            [ "*.fs"; "!A.fs"; "A.fs" ], "A.fs", true
            [ "vendor/" ], "vendor/A.fs", true
            [ "vendor/"; "!vendor/A.fs" ], "vendor/A.fs", false
            [], "A.fs", false
        ]

    for patterns, relativePath, expected in cases do
        let ignoreFile, file = governing patterns relativePath

        let byTheIgnoreFile: bool =
            IgnoreFile.isIgnoredFile Serilog.Log.Logger (Some ignoreFile) file

        let byTheLastMatch: bool =
            match IgnoreFile.matchingLines ignoreFile file |> List.tryLast with
            | None -> false
            | Some matched -> not matched.Negated

        byTheIgnoreFile |> shouldEqual expected
        byTheLastMatch |> shouldEqual expected

[<Test>]
let ``an ignore file is asked whether any line takes a path back out`` () =
    // What a walk consults before it closes a folder whole, so the cases that matter are the ones
    // that decide it: a `!` line is one, and a line that only looks like one is not.
    let cases: (string list * bool) list =
        [
            [ "*.fs" ], false
            [ "*.fs"; "!A.fs" ], true
            [ "# !A.fs" ], false
            [ "" ], false
            // A backslash takes the `!` literally, so this names a file called `!A.fs`.
            [ "\\!A.fs" ], false
        ]

    for patterns, expected in cases do
        let ignoreFile, _ = governing patterns "A.fs"
        IgnoreFile.hasNegatedPattern ignoreFile |> shouldEqual expected

[<Test>]
let ``a pattern the ignore library will not compile fails the whole ignore file`` () =
    // Worth pinning rather than assuming, because it decides what every caller can be told. The
    // rules are compiled as the file is read, so one unclosed bracket takes the file with it and
    // there is no `IgnoreFile` left to ask which line was at fault. Everything that reads an
    // ignore file inherits that, and what a caller can do about it is report the failure.
    let fs: IFileSystem = MockFileSystem()
    let root: string = fs.Path.GetTempPath() |> fs.Path.GetPathRoot
    let ignoreFilePath: string = fs.Path.Combine(root, IgnoreFile.IgnoreFileName)
    makeFileHierarchy fs [ ignoreFilePath ]
    fs.File.WriteAllText(ignoreFilePath, "*.fsx\n[\nA.fs\n")

    // Reading the file is what compiles the rules, so nothing has to be asked of the result for
    // the bad pattern to make itself known.
    let loaded: Result<IsPathIgnored, exn> =
        try
            Ok(IgnoreFile.loadIgnoreList fs ignoreFilePath)
        with error ->
            Error error

    match loaded with
    | Ok _ -> failwith "Expected the ignore file to fail to load."
    | Error error -> error.Message |> shouldContainText "Unterminated"

/// An ignore file at each of the given folders, relative to a mock root, and the one that governs
/// the given file.
let private chainOf (folders: string list) (relativePath: string) : IFileSystem * IgnoreFile * string =
    let fs: IFileSystem = MockFileSystem()
    let root: string = fs.Path.GetTempPath() |> fs.Path.GetPathRoot

    let under (relative: string) : string =
        fs.Path.Combine(root, relative.Replace("/", string<char> fs.Path.DirectorySeparatorChar))

    let file: string = under relativePath
    makeFileHierarchy fs [ file ]

    for folder in folders do
        let ignoreFilePath: string =
            fs.Path.Combine(under folder, IgnoreFile.IgnoreFileName)

        makeFileHierarchy fs [ ignoreFilePath ]
        fs.File.WriteAllText(ignoreFilePath, folder)

    match IgnoreFile.find fs (IgnoreFile.loadIgnoreList fs) file with
    | None -> failwith $"No ignore file was found above %s{file}."
    | Some ignoreFile -> fs, ignoreFile, file

[<Test>]
let ``findAbove returns nothing when the governing ignore file is the only one`` () =
    let fs, ignoreFile, _ = chainOf [ "" ] "src/A.fs"

    IgnoreFile.findAbove fs (IgnoreFile.loadIgnoreList fs) ignoreFile
    |> List.map (fun (found: IgnoreFile) -> found.Location.FullName)
    |> shouldEqual []

[<Test>]
let ``findAbove returns every ignore file above the governing one, nearest first`` () =
    // The layout the command line could not see before 8.0 and the daemon always could. None of
    // these applies; `doctor` is the only thing that asks, and it asks so that a pattern somebody
    // wrote at the root and cannot find the effect of can be pointed at.
    let fs, ignoreFile, _ = chainOf [ ""; "src"; "src/deep" ] "src/deep/A.fs"

    let above: string list =
        IgnoreFile.findAbove fs (IgnoreFile.loadIgnoreList fs) ignoreFile
        |> List.map (fun (found: IgnoreFile) -> found.Location.Directory.Name)

    // The governing file is the one in `src/deep`, so it is not among these.
    above |> shouldEqual [ "src"; fs.Path.GetPathRoot(fs.Path.GetTempPath()) ]

[<Test>]
let ``findAbove does not return the ignore file it was given`` () =
    let fs, ignoreFile, _ = chainOf [ ""; "src" ] "src/A.fs"

    IgnoreFile.findAbove fs (IgnoreFile.loadIgnoreList fs) ignoreFile
    |> List.map (fun (found: IgnoreFile) -> found.Location.FullName)
    |> List.contains ignoreFile.Location.FullName
    |> shouldEqual false
