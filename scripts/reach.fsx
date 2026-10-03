// What each test reaches in Fantomas.Core, one test at a time: every unit test in
// Fantomas.Core.Tests and every snapshot case.
//
//   dotnet fsi build.fsx -- -p CoverageReach      instrument, build, and run this
//
// AltCover's own tracking of which test reached what loses the test at the first async hop, which
// formatting is full of. So this script calls the tests itself, one after the other, and between two
// tests reads and clears what AltCover's recorder keeps. Both test assemblies run against the one
// instrumented Fantomas.Core, so a point is the same point for both.
//
// Produces, in artifacts/coverage/:
//   reach.tsv     every test and case, its suite, whether it passed, and the points it reached

open System
open System.Collections
open System.IO
open System.Reflection
open System.Runtime.Loader

let repository: string = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, ".."))
let artifacts: string = Path.Combine(repository, "artifacts")
let coverageDirectory: string = Path.Combine(artifacts, "coverage")

/// Where `CoverageReach` leaves the instrumented Fantomas.Core, beside the unit tests it ran with.
let instrumented: string =
    Path.Combine(artifacts, "bin", "Fantomas.Core.Tests", "release", "__Instrumented_Fantomas.Core.Tests")

let snapshotBinaries: string =
    Path.Combine(artifacts, "bin", "Fantomas.Core.SnapshotTests", "release")

// The instrumented assemblies first, so that the snapshot tests format with the instrumented
// Fantomas.Core rather than the plain one beside them.
AssemblyLoadContext.Default.add_Resolving (fun (_: AssemblyLoadContext) (name: AssemblyName) ->
    [ instrumented; snapshotBinaries ]
    |> List.map (fun (folder: string) -> Path.Combine(folder, name.Name + ".dll"))
    |> List.tryFind File.Exists
    |> Option.map AssemblyLoadContext.Default.LoadFromAssemblyPath
    |> Option.toObj
)

let load (folder: string) (name: string) : Assembly =
    AssemblyLoadContext.Default.LoadFromAssemblyPath(Path.Combine(folder, name + ".dll"))

let recorder: Assembly = load instrumented "AltCover.Recorder.g"
let unitTests: Assembly = load instrumented "Fantomas.Core.Tests"
let snapshotTests: Assembly = load snapshotBinaries "Fantomas.Core.SnapshotTests"

let everyStatic: BindingFlags =
    BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static

/// The recorder's visits: per instrumented module, the points visited since the table was cleared.
/// Read anew each time, because the recorder can put a new table in its place.
let visitsField: FieldInfo =
    recorder.GetType("AltCover.Recorder.Instance+I").GetField("visits", everyStatic)

/// The points the recorder has seen once. It runs in `Single` mode, where a point seen once is not
/// recorded again, so this is cleared along with the visits for the next test to be seen at all.
let samplesField: FieldInfo =
    recorder.GetType("AltCover.Recorder.Instance+I").GetField("samples", everyStatic)

/// The points visited since the last call, and none from then on.
let take () : int list =
    let visits: IDictionary = visitsField.GetValue(null) :?> IDictionary
    let samples: IDictionary = samplesField.GetValue(null) :?> IDictionary

    lock
        visits
        (fun () ->
            for inner in samples.Values do
                (inner :?> IDictionary).Clear()

            [
                for inner in visits.Values do
                    let inner: IDictionary = inner :?> IDictionary
                    yield! inner.Keys |> Seq.cast<int>
                    inner.Clear()
            ]
        )

/// What a test reached, and whether it passed.
type Reach =
    {
        Suite: string
        /// Unit tests: the file and the test name. Snapshot cases: the path.
        Name: string
        Passed: bool
        Points: Set<int>
    }

let measure (suite: string) (name: string) (run: unit -> unit) : Reach =
    take () |> ignore

    let passed: bool =
        try
            run ()
            true
        with _ ->
            false

    {
        Suite = suite
        Name = name
        Passed = passed
        Points = set (take ())
    }

/// The file of each test module, by the module's name. A module is not always named after its file:
/// `UnionTests.fs` holds `UnionsTests`.
let fileOfModule: Map<string, string> =
    let testsDirectory: string = Path.Combine(repository, "src", "Fantomas.Core.Tests")

    Directory.GetFiles(testsDirectory, "*.fs", SearchOption.AllDirectories)
    |> Array.choose (fun (path: string) ->
        File.ReadLines path
        |> Seq.tryFind (fun (line: string) -> line.StartsWith "module ")
        |> Option.map (fun (line: string) ->
            line.Substring("module ".Length).Trim(), Path.GetRelativePath(testsDirectory, path).Replace('\\', '/')
        )
    )
    |> Map.ofArray

/// Fantomas.Core initialises its top-level values once, on first use, so whatever used a module
/// first would get its initialisation to itself. Running every initialiser, and formatting once,
/// before measuring takes them out of the way: they count for both suites.
let warmUp: Set<int> =
    let core: Assembly = load instrumented "Fantomas.Core"
    let formatter: Type = core.GetType "Fantomas.Core.CodeFormatter"

    take () |> ignore

    for each in core.GetTypes() do
        try
            Runtime.CompilerServices.RuntimeHelpers.RunClassConstructor each.TypeHandle
        with _ ->
            ()

    let format: MethodInfo =
        formatter.GetMethods()
        |> Array.find (fun (methodInfo: MethodInfo) ->
            methodInfo.Name = "FormatDocumentAsync" && methodInfo.GetParameters().Length = 2
        )

    let formatting: obj = format.Invoke(null, [| box false; box "let a = 1" |])

    typeof<Microsoft.FSharp.Control.Async>
        .GetMethod("RunSynchronously")
        .MakeGenericMethod(format.ReturnType.GetGenericArguments())
        .Invoke(null, [| formatting; null; null |])
    |> ignore

    set (take ())

let attributeNamed (name: string) (methodInfo: MethodInfo) : obj list =
    methodInfo.GetCustomAttributes(true)
    |> Array.filter (fun (attribute: obj) -> attribute.GetType().Name = name)
    |> Array.toList

/// Every unit test as NUnit runs it: `[<Test>]`, and `[<TestCase>]` once per case. Ignored tests do
/// not run, so they reach nothing.
let unitReach: Reach list =
    unitTests.GetTypes()
    |> Array.toList
    |> List.collect (fun (testType: Type) ->
        testType.GetMethods(everyStatic ||| BindingFlags.DeclaredOnly)
        |> Array.toList
        |> List.map (fun (methodInfo: MethodInfo) -> testType, methodInfo)
    )
    |> List.filter (fun (_, methodInfo: MethodInfo) -> (attributeNamed "IgnoreAttribute" methodInfo).IsEmpty)
    |> List.collect (fun (testType: Type, methodInfo: MethodInfo) ->
        let file: string =
            fileOfModule
            |> Map.tryFind testType.FullName
            |> Option.defaultValue (testType.FullName + ".fs")

        let name: string = $"%s{file}\t%s{methodInfo.Name}"

        let cases: obj array list =
            attributeNamed "TestCaseAttribute" methodInfo
            |> List.map (fun (attribute: obj) ->
                attribute.GetType().GetProperty("Arguments").GetValue(attribute) :?> obj array
            )

        match attributeNamed "TestAttribute" methodInfo, cases with
        | [], [] -> []
        | _, [] when methodInfo.GetParameters().Length = 0 ->
            [ measure "unit" name (fun () -> methodInfo.Invoke(null, [||]) |> ignore) ]
        | _, cases ->
            cases
            |> List.map (fun (arguments: obj array) ->
                measure "unit" name (fun () -> methodInfo.Invoke(null, arguments) |> ignore)
            )
    )

eprintfn $"%d{unitReach.Length} unit tests measured"

let snapshotReach: Reach list =
    let all: MethodInfo =
        snapshotTests.GetType("Fantomas.Core.SnapshotTests.Case").GetMethod("all", everyStatic)

    let case: MethodInfo =
        snapshotTests.GetType("Fantomas.Core.SnapshotTests.CaseTests").GetMethod("case", everyStatic)

    // An ignored case is skipped, as an ignored unit test is above.
    all.Invoke(null, [||]) :?> string array
    |> Array.toList
    |> List.filter (fun (relativePath: string) ->
        not (Path.GetFileNameWithoutExtension(relativePath).EndsWith(".ignore", StringComparison.Ordinal))
    )
    |> List.map (fun (relativePath: string) ->
        measure "snapshot" relativePath (fun () -> case.Invoke(null, [| relativePath |]) |> ignore)
    )

eprintfn $"%d{snapshotReach.Length} snapshot cases measured"

Directory.CreateDirectory coverageDirectory |> ignore

File.WriteAllLines(
    Path.Combine(coverageDirectory, "reach.tsv"),
    unitReach @ snapshotReach
    |> List.map (fun (reach: Reach) ->
        let points: string = reach.Points |> Seq.map string |> String.concat " "
        $"%s{reach.Suite}\t%s{reach.Name}\t%b{reach.Passed}\t%s{points}"
    )
)

/// A method's visit is its metadata token, 0x06xxxxxx, and says nothing its sequence points do not.
let isMethodToken (point: int) : bool = point >= 0x06000000

let unionOf (reaches: Reach list) : Set<int> =
    reaches
    |> List.map _.Points
    |> Set.unionMany
    |> Set.filter (isMethodToken >> not)

let unitPoints: Set<int> = unionOf unitReach + warmUp
let snapshotPoints: Set<int> = unionOf snapshotReach + warmUp

printfn $"Unit tests: %d{unitReach.Length}, %d{unitPoints.Count} points."
printfn $"Snapshot cases: %d{snapshotReach.Length}, %d{snapshotPoints.Count} points."
printfn $"Reached by unit tests and no snapshot case: %d{(unitPoints - snapshotPoints).Count}."
printfn $"Written to %s{coverageDirectory}"
