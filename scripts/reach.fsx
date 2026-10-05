// What each test reaches in Fantomas.Core, one test at a time: every unit test in
// Fantomas.Core.Tests and every snapshot case.
//
//   dotnet fsi build.fsx -- -p CoverageReach      instrument, build, and run this
//
// AltCover's own tracking of which test reached what loses the test at the first async hop, which
// formatting is full of. So this script calls the tests itself, one after the other, and compares
// what AltCover's recorder has counted before and after each. `CoverageReach` has the recorder count
// every visit, so a point whose count rose during a test is one the test reached. The recorder is
// only read: nothing here changes what it records. Both test assemblies run against the one
// instrumented Fantomas.Core, so a point is the same point for both.
//
// Produces, in artifacts/coverage/:
//   reach.tsv     every test and case, its suite, whether it passed, and the points it reached

open System
open System.Collections
open System.Collections.Generic
open System.IO
open System.Linq.Expressions
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

/// The recorder's visits: per instrumented module, every point visited so far and its `PointVisit`.
/// Read anew each time, because the recorder can put a new table in its place.
let visitsField: FieldInfo =
    recorder.GetType("AltCover.Recorder.Instance+I").GetField("visits", everyStatic)

/// How often a point was visited, read from its `PointVisit`. Compiled once: it is read for every
/// point visited so far, twice per test.
let countOf: Func<obj, int64> =
    let pointVisit: Type =
        recorder.GetTypes() |> Array.find (fun (each: Type) -> each.Name = "PointVisit")

    let visit: ParameterExpression = Expression.Parameter(typeof<obj>, "visit")

    let count: Expression =
        Expression.Convert(Expression.PropertyOrField(Expression.Convert(visit, pointVisit), "Count"), typeof<int64>)

    Expression.Lambda<Func<obj, int64>>(count, visit).Compile()

/// Every point visited so far and how often. A module's table is copied under the lock the recorder
/// takes on it to add a point.
let snapshot () : Dictionary<int, int64> =
    let visits: IDictionary = visitsField.GetValue(null) :?> IDictionary
    let counts: Dictionary<int, int64> = Dictionary<int, int64>()

    for inner in visits.Values do
        let inner: IDictionary = inner :?> IDictionary

        lock
            inner
            (fun () ->
                for point in inner.Keys do
                    counts[point :?> int] <- countOf.Invoke inner[point]
            )

    counts

/// The points whose count rose from one snapshot to the next: the ones visited in between.
let visitedBetween (before: Dictionary<int, int64>) (after: Dictionary<int, int64>) : int list =
    [
        for KeyValue(point: int, count: int64) in after do
            match before.TryGetValue point with
            | true, earlier when earlier = count -> ()
            | _ -> yield point
    ]

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
    let before: Dictionary<int, int64> = snapshot ()

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
        Points = set (visitedBetween before (snapshot ()))
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

    let before: Dictionary<int, int64> = snapshot ()

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

    set (visitedBetween before (snapshot ()))

let attributeNamed (name: string) (methodInfo: MethodInfo) : obj list =
    methodInfo.GetCustomAttributes(true)
    |> Array.filter (fun (attribute: obj) -> attribute.GetType().Name = name)
    |> Array.toList

/// The arguments of every test a `[<TestCaseSource>]` names, as NUnit reads its source. The source
/// is a static field, property or method of the test's own module, or of `SourceType`; a method is
/// called with `MethodParams`. Without a name, `SourceType` is the source itself, created with its
/// constructor. Each item of the source is one test: an `obj array` or a `TestCaseData` holds its
/// arguments, an array as long as the test has parameters is spread over them, unless the test's
/// one parameter takes that array, and anything else, `null` included, is the single argument.
let sourceCases (testType: Type) (test: MethodInfo) (attribute: obj) : obj array list =
    let attributeType: Type = attribute.GetType()

    let property (name: string) : obj =
        attributeType.GetProperty(name).GetValue(attribute)

    let sourceType: Type =
        match property "SourceType" with
        | :? Type as sourceType -> sourceType
        | _ -> testType

    let source: obj =
        match property "SourceName" with
        | :? string as sourceName ->
            let methodParams: obj array =
                match property "MethodParams" with
                | :? (obj array) as methodParams -> methodParams
                | _ -> [||]

            match sourceType.GetMember(sourceName, everyStatic) |> Array.tryHead with
            | Some(:? FieldInfo as field) -> field.GetValue null
            | Some(:? PropertyInfo as property) -> property.GetValue null
            | Some(:? MethodInfo as methodInfo) when methodInfo.GetParameters().Length = methodParams.Length ->
                methodInfo.Invoke(null, methodParams)
            | Some(:? MethodInfo) ->
                failwith
                    $"The test case source %s{sourceName} of %s{test.Name} takes other parameters than the %d{methodParams.Length} it is given."
            | _ ->
                failwith
                    $"The test case source %s{sourceName} of %s{test.Name} is no static member of %s{sourceType.FullName}."
        | _ -> Activator.CreateInstance sourceType

    let parameters: ParameterInfo array = test.GetParameters()

    match source with
    | :? Collections.IEnumerable as items ->
        items
        |> Seq.cast<obj>
        |> Seq.map (fun (item: obj) ->
            match item with
            | null -> [| null |]
            | :? (obj array) as arguments -> arguments
            | item when item.GetType().Name = "TestCaseData" ->
                item.GetType().GetProperty("Arguments").GetValue(item) :?> obj array
            | :? Array as array when
                array.Length = parameters.Length
                && not (parameters.Length = 1 && parameters[0].ParameterType.IsInstanceOfType array)
                ->
                Array.init array.Length (fun (index: int) -> array.GetValue index)
            | item -> [| item |]
        )
        |> Seq.toList
    | _ -> failwith $"The test case source of %s{test.Name} is no sequence of test cases."

/// Every unit test as NUnit runs it: `[<Test>]`, and `[<TestCase>]` and `[<TestCaseSource>]` once
/// per case. Ignored tests do not run, so they reach nothing. A test with parameters and no source
/// to fill them fails the run, rather than being left out of what the tests reach.
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
            (attributeNamed "TestCaseAttribute" methodInfo
             |> List.map (fun (attribute: obj) ->
                 attribute.GetType().GetProperty("Arguments").GetValue(attribute) :?> obj array
             ))
            @ (attributeNamed "TestCaseSourceAttribute" methodInfo
               |> List.collect (sourceCases testType methodInfo))

        let hasSource: bool =
            not (attributeNamed "TestCaseSourceAttribute" methodInfo).IsEmpty

        let isTest: bool =
            hasSource
            || not (attributeNamed "TestAttribute" methodInfo).IsEmpty
            || not (attributeNamed "TestCaseAttribute" methodInfo).IsEmpty

        match isTest, cases with
        | false, _ -> []
        // A source without items is no mistake: NUnit runs no test for it, and neither does this.
        | true, [] when hasSource ->
            eprintfn $"%s{name} has a test case source without test cases."
            []
        | true, [] when methodInfo.GetParameters().Length = 0 ->
            [ measure "unit" name (fun () -> methodInfo.Invoke(null, [||]) |> ignore) ]
        | true, [] -> failwith $"%s{name} has parameters and no test case to fill them with."
        | true, cases ->
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
