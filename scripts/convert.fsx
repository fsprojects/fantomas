// The mechanical port of Fantomas.Core.Tests to snapshot cases. Every test that formats a string and
// compares the result with an expected string becomes a case: its input, its config as front matter,
// and its expected output as the gold. Nothing is judged. A test that does not fit is listed with the
// reason it does not.
//
//   dotnet fsi scripts/convert.fsx                 dry run: what each test would become
//   dotnet fsi scripts/convert.fsx -- --write      write `cases/ported/` and the porting ledger
//   dotnet fsi scripts/convert.fsx -- --check      fail when `cases/ported/` or the ledger differ from that
//
// The tests of one file that format the same source with the same settings become one case. Every
// test is checked against its case before anything is written: its expected output must be what the
// harness formats, for `formatSourceStringWithDefines` the result for its defines merged with itself
// as that helper does. A test whose result equals its input becomes a negative case. An ignored test
// is checked too, and converted once it passes.
//
// Needs a debug build of the snapshot tests (`dotnet build src/Fantomas.Core.SnapshotTests`).

#r "../artifacts/bin/Fantomas.FCS/debug/Fantomas.FCS.dll"
#r "../artifacts/bin/Fantomas.Core/debug/Fantomas.Core.dll"
#r "../artifacts/bin/Fantomas.EditorConfig/debug/Fantomas.EditorConfig.dll"
#r "../artifacts/bin/Fantomas.Core.SnapshotTests/debug/Fantomas.Core.SnapshotTests.dll"

open System
open System.IO
open System.Text.RegularExpressions
open Microsoft.FSharp.Reflection
open Fantomas.FCS.Syntax
open Fantomas.FCS.Text
open Fantomas.Core
open Fantomas.Core.SnapshotTests

let testsDirectory: string =
    Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", "src", "Fantomas.Core.Tests"))

/// The files whose tests are about internals rather than formatted output.
let unitTestFiles: Set<string> =
    set
        [
            "ASTTransformerTests.fs"
            "CodeFormatterTests.fs"
            "CodePrinterHelperFunctionsTests.fs"
            "ContextTests.fs"
            "CursorTests.fs"
            "DefinesTests.fs"
            "FormatAstTests.fs"
            "FormattingSelectionOnlyTests.fs"
            "MultipleDefineCombinationsTests.fs"
            "QueueTests.fs"
            "TriviaAssignmentTests.fs"
            "UtilsTests.fs"
            "ValidationTests.fs"
        ]

/// What a test asks of its format helper.
type Call =
    {
        Helper: string
        Input: string
        IsSignature: bool
        /// The one define combination `formatSourceStringWithDefines` formats.
        Defines: string list option
        Config: FormatConfig
        /// The reason an `[<Ignore>]` gives, for a test that never runs.
        Ignored: string option
    }

/// A test that compares a format result with an expected string.
type Test =
    {
        File: string
        Line: int
        Name: string
        Call: Call
        /// What the helper returned, without the newline `prepend newline` put in front.
        Expected: string
    }

// --- Reading a test -----------------------------------------------------------------------------

let identText (expr: SynExpr) : string option =
    match expr with
    | SynExpr.Ident ident -> Some ident.idText
    | SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)) -> Some(ids |> List.map _.idText |> String.concat ".")
    | _ -> None

/// The stages of a `|>` chain, first to last.
let rec pipeline (expr: SynExpr) : SynExpr list =
    match expr with
    | SynExpr.App(funcExpr = SynExpr.App(isInfix = true; funcExpr = operator; argExpr = left); argExpr = right) when
        identText operator = Some "op_PipeRight"
        ->
        pipeline left @ [ right ]
    | SynExpr.Paren(expr = inner) -> pipeline inner
    | expr -> [ expr ]

/// A function and its arguments, from a chain of applications.
let rec spine (expr: SynExpr) : SynExpr * SynExpr list =
    match expr with
    | SynExpr.App(funcExpr = funcExpr; argExpr = argExpr) ->
        let head, args = spine funcExpr
        head, args @ [ argExpr ]
    | SynExpr.Paren(expr = inner) -> spine inner
    | expr -> expr, []

/// The names a test body can refer to: the `let`s of its file and of the body itself.
type Binding =
    {
        Expr: SynExpr
        /// What the names in `Expr` refer to.
        Scope: Map<string, Binding>
    }

type Scope = Map<string, Binding>

let rec stringOf (scope: Scope) (expr: SynExpr) : Result<string, string> =
    match expr with
    | SynExpr.Const(SynConst.String(text = text), _) -> Ok text
    | SynExpr.Paren(expr = inner) -> stringOf scope inner
    | SynExpr.LongIdent(longDotId = SynLongIdent(id = [ s; e ])) when s.idText = "String" && e.idText = "Empty" -> Ok ""
    | SynExpr.Ident ident when scope.ContainsKey ident.idText ->
        let binding: Binding = scope[ident.idText]
        stringOf binding.Scope binding.Expr
    | _ -> Error "a string that is not a literal"

let configFields: Reflection.PropertyInfo array =
    FSharpType.GetRecordFields typeof<FormatConfig>

/// The value of a field in `{ config with Field = value }`, read from its literal.
let fieldValue (field: Reflection.PropertyInfo) (expr: SynExpr) : Result<obj, string> =
    let rec literal (expr: SynExpr) : Result<obj, string> =
        match expr with
        | SynExpr.Paren(expr = inner) -> literal inner
        | SynExpr.Const(SynConst.Int32 value, _) -> Ok(box value)
        | SynExpr.Const(SynConst.Bool value, _) -> Ok(box value)
        | SynExpr.Const(SynConst.String(text = value), _) -> Ok(box value)
        | expr when FSharpType.IsUnion field.PropertyType ->
            match identText expr with
            | None -> Error $"the value of %s{field.Name} is not a literal"
            | Some name ->

            let caseName: string = name.Split('.') |> Array.last

            match
                FSharpType.GetUnionCases field.PropertyType
                |> Array.tryFind (fun case -> case.Name = caseName && case.GetFields().Length = 0)
            with
            | Some case -> Ok(FSharpValue.MakeUnion(case, [||]))
            | None -> Error $"`%s{name}` is no case of %s{field.PropertyType.Name}"
        | _ -> Error $"the value of %s{field.Name} is not a literal"

    literal expr

let rec configOf (scope: Scope) (expr: SynExpr) : Result<FormatConfig, string> =
    match expr with
    | SynExpr.Paren(expr = inner) -> configOf scope inner
    | SynExpr.Ident ident when ident.idText = "config" ->
        match scope.TryFind "config" with
        | Some binding -> configOf (binding.Scope.Remove "config") binding.Expr
        | None -> Ok FormatConfig.Default
    | SynExpr.LongIdent(longDotId = SynLongIdent(id = [ t; d ])) when t.idText = "FormatConfig" && d.idText = "Default" ->
        Ok FormatConfig.Default
    | SynExpr.Ident ident when scope.ContainsKey ident.idText ->
        let binding: Binding = scope[ident.idText]
        configOf binding.Scope binding.Expr
    | SynExpr.Record(copyInfo = Some(baseExpr, _); recordFields = fields) ->
        configOf scope baseExpr
        |> Result.bind (fun (baseConfig: FormatConfig) ->
            let values: obj array = FSharpValue.GetRecordFields baseConfig

            fields
            |> List.fold
                (fun (state: Result<unit, string>) (field: SynExprRecordFieldOrSpread) ->
                    state
                    |> Result.bind (fun () ->
                        match field with
                        | SynExprRecordFieldOrSpread.Spread _ -> Error "a config with a spread"
                        | SynExprRecordFieldOrSpread.Field(SynExprRecordField(
                                                               fieldName = SynLongIdent(id = ids), _; expr = value),
                                                           _) ->

                        let name: string = (List.last ids).idText

                        let index: int option =
                            configFields
                            |> Array.tryFindIndex (fun (field: Reflection.PropertyInfo) -> field.Name = name)

                        match index, value with
                        | Some index, Some value ->
                            fieldValue configFields[index] value
                            |> Result.map (fun (v: obj) -> values[index] <- v)
                        | _ -> Error $"`%s{name}` is no setting"
                    )
                )
                (Ok())
            |> Result.map (fun () -> FSharpValue.MakeRecord(typeof<FormatConfig>, values) :?> FormatConfig)
        )
    | _ -> Error "a config that is not `config` or a copy of it"

let helpers: Set<string> =
    set
        [
            "formatSourceString"
            "formatSignatureString"
            "formatSourceStringWithDefines"
            "formatAST"
        ]

let rec helperCalls (expr: SynExpr) : int =
    let here: int =
        match spine expr with
        | head, _ :: _ when identText head |> Option.exists helpers.Contains -> 1
        | _ -> 0

    let below: SynExpr list =
        match expr with
        | SynExpr.App(funcExpr = f; argExpr = a) -> [ f; a ]
        | SynExpr.Paren(expr = e)
        | SynExpr.Typed(expr = e)
        | SynExpr.Do(expr = e)
        | SynExpr.Lambda(body = e) -> [ e ]
        | SynExpr.Sequential(expr1 = a; expr2 = b) -> [ a; b ]
        | SynExpr.LetOrUse letOrUse ->
            (letOrUse.Bindings |> List.map (fun (SynBinding(expr = bound)) -> bound))
            @ [ letOrUse.Body ]
        | SynExpr.Tuple(exprs = exprs)
        | SynExpr.ArrayOrList(exprs = exprs) -> exprs
        | _ -> []

    // Counted once per call: a call's own function part is a partial application of it.
    match spine expr with
    | head, args when here = 1 -> 1 + (args |> List.sumBy helperCalls)
    | _ -> below |> List.sumBy helperCalls

let callOf (scope: Scope) (expr: SynExpr) : Result<Call, string> =
    match spine expr with
    | head, args ->

    match identText head, args with
    | Some("formatSourceString" | "formatSignatureString" as helper), [ input; config ] ->
        stringOf scope input
        |> Result.bind (fun input ->
            configOf scope config
            |> Result.map (fun config ->
                {
                    Helper = helper
                    Input = input
                    IsSignature = helper = "formatSignatureString"
                    Defines = None
                    Config = config
                    Ignored = None
                }
            )
        )
    | Some "formatSourceStringWithDefines", [ defines; input; config ] ->
        let definesResult: Result<string list, string> =
            match defines with
            | SynExpr.ArrayOrListComputed(expr = SynExpr.Const(SynConst.String(text = d), _)) -> Ok [ d ]
            | SynExpr.ArrayOrList(exprs = []) -> Ok []
            | SynExpr.ArrayOrList(exprs = exprs) ->
                exprs
                |> List.map (stringOf scope)
                |> List.fold
                    (fun acc r ->
                        match acc, r with
                        | Ok xs, Ok x -> Ok(xs @ [ x ])
                        | Error e, _
                        | _, Error e -> Error e
                    )
                    (Ok [])
            | SynExpr.ArrayOrListComputed(
                expr = SynExpr.Sequential(
                    expr1 = SynExpr.Const(SynConst.String(text = a), _)
                    expr2 = SynExpr.Const(SynConst.String(text = b), _))) -> Ok [ a; b ]
            | _ -> Error "defines that are not a list of literals"

        definesResult
        |> Result.bind (fun defines ->
            stringOf scope input
            |> Result.bind (fun input ->
                configOf scope config
                |> Result.map (fun config ->
                    {
                        Helper = "formatSourceStringWithDefines"
                        Input = input
                        IsSignature = false
                        Defines = Some defines
                        Config = config
                        Ignored = None
                    }
                )
            )
        )
    | Some "formatAST", _ -> Error "formats a syntax tree without its source, so without trivia"
    | Some helper, _ when helpers.Contains helper ->
        Error $"calls %s{helper} with arguments that are not input and config"
    | _ -> Error "does not call a format helper first"

/// A test body as `helper input config |> prepend newline |> should equal expected`.
let testOf (scope: Scope) (body: SynExpr) : Result<Call * string, string> =
    let rec unwrap (scope: Scope) (expr: SynExpr) : Scope * SynExpr =
        match expr with
        | SynExpr.LetOrUse letOrUse when not letOrUse.IsRecursive ->
            let scope: Scope =
                letOrUse.Bindings
                |> List.fold
                    (fun (inner: Scope) (SynBinding(headPat = pat; expr = bound)) ->
                        match pat with
                        | SynPat.Named(ident = SynIdent(ident, _)) ->
                            inner.Add(ident.idText, { Expr = bound; Scope = inner })
                        | _ -> inner
                    )
                    scope

            unwrap scope letOrUse.Body
        | SynExpr.Paren(expr = inner) -> unwrap scope inner
        | expr -> scope, expr

    let scope, body = unwrap scope body

    match helperCalls body with
    | 0 -> Error "does not call a format helper"
    | 1 ->

        match pipeline body with
        | [] -> Error "has no body"
        | call :: stages ->

        let rec stagesOf (prepended: bool) (stages: SynExpr list) : Result<bool * SynExpr, string> =
            match stages with
            | [ last ] ->
                match spine last with
                | should, [ equal; expected ] when identText should = Some "should" && identText equal = Some "equal" ->
                    Ok(prepended, expected)
                | _ -> Error "does not end in `should equal`"
            | stage :: rest ->
                match spine stage with
                | prepend, [ newline ] when identText prepend = Some "prepend" && identText newline = Some "newline" ->
                    stagesOf true rest
                | _ -> Error "pipes the result through something other than `prepend newline`"
            | [] -> Error "does not compare the result"

        stagesOf false stages
        |> Result.bind (fun (prepended, expected) ->
            callOf scope call
            |> Result.bind (fun call ->
                stringOf scope expected
                |> Result.bind (fun expected ->
                    if not prepended then
                        Ok(call, expected)
                    elif expected.StartsWith "\n" then
                        Ok(call, expected.Substring 1)
                    else
                        Error "prepends a newline the expected output does not start with"
                )
            )
        )
    | n -> Error $"calls a format helper %d{n} times"

let testsOf (relativeFile: string) : (string * int * string * Result<Call * string, string>) list =
    let path: string = Path.Combine(testsDirectory, relativeFile)
    let text: string = File.ReadAllText path
    let tree, _ = Fantomas.FCS.Parse.parseFile false (SourceText.ofString text) []

    let rec declarationsOf (decls: SynModuleDecl list) : SynBinding list =
        decls
        |> List.collect (fun (decl: SynModuleDecl) ->
            match decl with
            | SynModuleDecl.Let(bindings = bindings) -> bindings
            | SynModuleDecl.NestedModule(decls = decls) -> declarationsOf decls
            | _ -> []
        )

    let bindings: SynBinding list =
        match tree with
        | ParsedInput.SigFile _ -> []
        | ParsedInput.ImplFile(ParsedImplFileInput(contents = modules)) ->

        modules
        |> List.collect (fun (SynModuleOrNamespace(decls = decls)) -> declarationsOf decls)

    // The values a file defines for its tests, `config` among them, in the order it defines them.
    let fileScope: Scope =
        bindings
        |> List.fold
            (fun (scope: Scope) (SynBinding(headPat = pat; expr = bound)) ->
                match pat with
                | SynPat.Named(ident = SynIdent(ident, _)) -> scope.Add(ident.idText, { Expr = bound; Scope = scope })
                | _ -> scope
            )
            Map.empty

    bindings
    |> List.choose (fun (SynBinding(attributes = attributes; headPat = headPat; expr = body)) ->
        let attributeNames: string list =
            attributes
            |> List.collect _.Attributes
            |> List.map (fun (attribute: SynAttribute) -> (List.last attribute.TypeName.LongIdent).idText)

        let ignoreReason: string option =
            attributes
            |> List.collect _.Attributes
            |> List.tryPick (fun (attribute: SynAttribute) ->
                if (List.last attribute.TypeName.LongIdent).idText <> "Ignore" then
                    None
                else

                match attribute.ArgExpr with
                | SynExpr.Paren(expr = SynExpr.Const(SynConst.String(text = reason), _))
                | SynExpr.Const(SynConst.String(text = reason), _) -> Some reason
                | _ -> Some ""
            )

        match headPat with
        | SynPat.LongIdent(
            longDotId = SynLongIdent(id = [ ident ])
            argPats = SynArgPats.Pats [ SynPat.Paren(pat = SynPat.Const(SynConst.Unit, _)) ]) when
            List.contains "Test" attributeNames || ident.idText.Contains ' '
            ->
            let result: Result<Call * string, string> =
                if not (List.contains "Test" attributeNames) then
                    Error "has no [<Test>] attribute, so never runs"
                else

                testOf fileScope body
                |> Result.map (fun (call: Call, expected: string) -> { call with Ignored = ignoreReason }, expected)

            Some(relativeFile, ident.idRange.StartLine, ident.idText, result)
        | SynPat.LongIdent(longDotId = SynLongIdent(id = [ ident ])) when
            List.exists (fun name -> name = "TestCase" || name = "TestCaseSource") attributeNames
            ->
            Some(relativeFile, ident.idRange.StartLine, ident.idText, Error "is a parameterised test")
        | _ -> None
    )

// --- What a test becomes ------------------------------------------------------------------------

/// The front matter for a config: every setting that differs from what a case formats with by
/// default. `end_of_line` is left out, because the old helpers make every line ending `\n`.
let propertiesOf (config: FormatConfig) : Result<(string * string) list, string> =
    let defaults: Map<string, string> =
        Fantomas.EditorConfig.settingValues Case.defaultConfig |> Map.ofList

    let config: FormatConfig =
        { config with
            EndOfLine = Case.defaultConfig.EndOfLine
        }

    let properties: (string * string) list =
        Fantomas.EditorConfig.settingValues config
        |> List.filter (fun (key: string, value: string) -> defaults.TryFind key <> Some value)

    // The front matter must say exactly the config the test formats with.
    if Case.configOf properties = config then
        Ok properties
    else
        Error "a config the front matter cannot express"

let normalise (text: string) : string = text.Replace("\r\n", "\n")

/// `MultipleDefineCombinations.mergeMultipleFormatResults`, which is internal to Fantomas.Core.
/// `formatSourceStringWithDefines` merges the result for its defines with itself, which puts every
/// directive at the start of its line, and the expected output it is compared with is that.
let mergeWithItself (config: FormatConfig) (defines: string list) (code: string) : string =
    let core: Reflection.Assembly = typeof<FormatConfig>.Assembly
    let combinationType: Type = core.GetType "Fantomas.Core.DefineCombination"

    let combination: obj =
        FSharpValue.MakeUnion(FSharpType.GetUnionCases(combinationType, true)[0], [| box defines |], true)

    let pairType: Type =
        FSharpType.MakeTupleType [| combinationType; typeof<FormatResult> |]

    let pair: obj =
        FSharpValue.MakeTuple([| combination; box { Code = code; Cursor = None } |], pairType)

    let listType: Type = typedefof<list<obj>>.MakeGenericType pairType

    let listCase (name: string) : UnionCaseInfo =
        FSharpType.GetUnionCases listType |> Array.find (fun case -> case.Name = name)

    let empty: obj = FSharpValue.MakeUnion(listCase "Empty", [||])

    let cons (head: obj) (tail: obj) : obj =
        FSharpValue.MakeUnion(listCase "Cons", [| head; tail |])

    let merge: Reflection.MethodInfo =
        core
            .GetType("Fantomas.Core.MultipleDefineCombinations")
            .GetMethod(
                "mergeMultipleFormatResults",
                Reflection.BindingFlags.Static
                ||| Reflection.BindingFlags.Public
                ||| Reflection.BindingFlags.NonPublic
            )

    (merge.Invoke(null, [| box config; cons pair (cons pair empty) |]) :?> FormatResult).Code

/// A test whose expectation the harness reproduces, and the case source that does it.
[<NoComparison; NoEquality>]
type Verified =
    {
        Properties: (string * string) list
        Source: string
        Formatted: Formatting.Formatted
    }

/// Format a source the way its case will be, and check the result against what the test expects.
let verify
    (call: Call)
    (expected: string)
    (properties: (string * string) list)
    (source: string)
    : Result<Verified, string>
    =
    let config: FormatConfig = Case.configOf properties

    let formattedAndChecked: Result<Formatting.Formatted * Problems.Problem list, string> =
        try
            Ok(Formatting.formatAndCheck config call.IsSignature source)
        with ex ->
            Error ex.Message

    match formattedAndChecked with
    | Error message -> Error $"the harness throws: %s{message.Split('\n')[0]}"
    | Ok(formatted, problems) ->

    match problems |> List.filter Problems.breaksResult with
    | problem :: _ -> Error $"the harness finds a problem: %s{(Problems.describe problem).Split('\n')[0]}"
    | [] ->

    let result: Result<string, string> =
        match call.Defines with
        | None -> Ok formatted.Merged
        | Some defines ->

        match
            formatted.Combinations
            |> List.tryFind (fun (each: Formatting.ForDefines) -> List.sort each.Defines = List.sort defines)
        with
        | None -> Error $"""the input has no define combination %s{String.concat "+" defines}"""
        | Some each -> Ok(mergeWithItself config defines each.Code)

    result
    |> Result.bind (fun (code: string) ->
        if normalise code <> normalise expected then
            Error "the harness result differs from the expected one"
        else
            Ok
                {
                    Properties = properties
                    Source = source
                    Formatted = formatted
                }
    )

/// What a test becomes. The newline a triple quoted input starts with is the test's layout, not its
/// input, and is left out when the result stays what the test expects without it.
let outcomeOf (call: Call) (expected: string) : Result<Verified, string> =
    propertiesOf call.Config
    |> Result.bind (fun (properties: (string * string) list) ->
        let input: string = normalise call.Input

        if not (input.StartsWith "\n") then
            verify call expected properties input
        else

        match verify call expected properties (input.Substring 1) with
        | Ok verified -> Ok verified
        | Error _ -> verify call expected properties input
    )

// --- Cases --------------------------------------------------------------------------------------

/// A case name from a test name: lower case words joined by dashes, the issue number first.
let caseNameOf (test: string) : string =
    let issue: Match = Regex.Match(test, @"(?:,\s*|#|issue\s*)(\d{3,5})\b")

    let rest: string =
        if issue.Success then
            test.Remove(issue.Index, issue.Length)
        else
            test

    let words: string =
        Regex.Replace(rest.ToLowerInvariant(), "[^a-z0-9]+", "-").Trim('-')

    // Cut long names at a dash, so that a name is still words.
    let words: string =
        if words.Length <= 60 then
            words
        else

        let cut: int = words.LastIndexOf('-', 60)
        words.Substring(0, (if cut > 0 then cut else 60))

    let name: string =
        match issue.Success, words with
        | true, "" -> issue.Groups[1].Value
        | true, words -> $"%s{issue.Groups[1].Value}-%s{words}"
        | false, "" -> "test"
        | false, words -> words

    name

type Row =
    {
        File: string
        Line: int
        Test: string
        /// `case`, `negative`, `ignored` or `exception`.
        Outcome: string
        /// The case, relative to `cases/`, or why there is none.
        Detail: string
    }

let testFiles: string list =
    Directory.GetFiles(testsDirectory, "*Tests.fs", SearchOption.AllDirectories)
    |> Array.map (fun (path: string) -> Path.GetRelativePath(testsDirectory, path).Replace('\\', '/'))
    |> Array.sort
    |> Array.toList

let tests: (string * int * string * Result<Call * string, string>) array =
    testFiles
    |> List.collect (fun (file: string) ->
        if not (unitTestFiles.Contains(Path.GetFileName file)) then
            testsOf file
        else
            testsOf file
            |> List.map (fun (file, line, name, _) -> file, line, name, Error "is in a file of unit tests")
    )
    |> List.toArray

/// What became of a test.
[<NoComparison; NoEquality>]
type Outcome =
    /// Its expectation holds: a case, shared with the tests of its file that format the same.
    | Converted of Call * Verified
    /// It is ignored and its expectation does not hold: a case of its own, `name.ignore.fs`, whose
    /// gold is what the test expects.
    | Ignored of call: Call * properties: (string * string) list * source: string * expected: string
    | NotConverted of reason: string

/// An ignored test that does not pass. Its expected output becomes the gold of an ignored case, which
/// only works for an output a gold can hold: the merged result, not one define combination's.
let ignoredOutcome (call: Call) (expected: string) (why: string) : Outcome =
    match call.Defines, propertiesOf call.Config with
    | Some _, _ -> NotConverted $"is ignored, %s{why}, and formats one define combination, which no gold holds"
    | None, Error reason -> NotConverted $"is ignored, and %s{reason}"
    | None, Ok properties ->

    let config: FormatConfig = Case.configOf properties
    let input: string = normalise call.Input

    let merged (source: string) : string option =
        try
            Some (Formatting.formatEach config call.IsSignature source).Merged
        with _ ->
            None

    // The newline a triple quoted input starts with goes when formatting gives the same without it.
    let source: string =
        if
            input.StartsWith "\n"
            && Option.isSome (merged input)
            && merged (input.Substring 1) = merged input
        then
            input.Substring 1
        else
            input

    Ignored(call, properties, source, normalise expected)

// Not in parallel: fsi runs a script inside the initialiser of the class it compiles it to, so a
// second thread calling a function of the script waits for that initialiser to finish.
let outcomes: (string * int * string * Outcome) array =
    tests
    |> Array.mapi (fun (index: int) (file, line, name, parsed) ->
        if index % 500 = 0 then
            eprintfn $"%d{index} of %d{tests.Length} tests"

        let outcome: Outcome =
            match parsed with
            | Error reason -> NotConverted reason
            | Ok(call, expected) ->

            match outcomeOf call expected, call.Ignored with
            | Ok verified, _ -> Converted(call, verified)
            | Error why, None -> NotConverted why
            | Error why, Some _ -> ignoredOutcome call expected why

        file, line, name, outcome
    )

/// A case to write, before it has a name of its own in its folder.
[<NoComparison; NoEquality>]
type Planned =
    {
        /// `ported/` and the old test file, without `.fs`.
        Folder: string
        Name: string
        Extension: string
        IsNegative: bool
        /// The reason the old test gave, for an ignored case.
        Ignored: string option
        Properties: (string * string) list
        Source: string
        /// Each gold, by what comes between the case name and the extension: `gold`, `DEBUG.gold`.
        Golds: (string * string) list
        /// The tests it comes from: their line and name.
        Tests: (int * string) list
    }

let folderOf (file: string) : string =
    "ported/" + file.Substring(0, file.Length - ".fs".Length)

let extensionOf (isSignature: bool) : string = if isSignature then ".fsi" else ".fs"

/// The tests of one file that format the same source with the same settings become one case.
let converted: Planned list =
    outcomes
    |> Array.choose (fun (file, line, name, outcome) ->
        match outcome with
        | Converted(call, verified) -> Some(file, line, name, call, verified)
        | Ignored _
        | NotConverted _ -> None
    )
    |> Array.groupBy (fun (file, _, _, call, verified) -> file, call.IsSignature, verified.Source, verified.Properties)
    |> Array.toList
    |> List.map (fun ((file, isSignature, _, _), members) ->
        let _, _, firstName, _, verified = members[0]
        let negative: bool = verified.Formatted.Merged = verified.Source

        {
            Folder = folderOf file
            Name = caseNameOf firstName
            Extension = extensionOf isSignature
            IsNegative = negative
            Ignored = None
            Properties = verified.Properties
            Source = verified.Source
            Golds =
                [
                    if not negative then
                        "gold", verified.Formatted.Merged

                    match verified.Formatted.Combinations with
                    | [ _ ] -> ()
                    | combinations ->
                        for each in combinations do
                            $"%s{Case.combinationName each.Defines}.gold", each.Code
                ]
            Tests = members |> Array.map (fun (_, line, name, _, _) -> line, name) |> Array.toList
        }
    )

let ignoredCases: Planned list =
    outcomes
    |> Array.toList
    |> List.choose (fun (file, line, name, outcome) ->
        match outcome with
        | Ignored(call, properties, source, expected) ->
            let negative: bool = expected = source

            Some
                {
                    Folder = folderOf file
                    Name = caseNameOf name
                    Extension = extensionOf call.IsSignature
                    IsNegative = negative
                    Ignored = call.Ignored
                    Properties = properties
                    Source = source
                    Golds = if negative then [] else [ ("gold", expected) ]
                    Tests = [ line, name ]
                }
        | Converted _
        | NotConverted _ -> None
    )

/// Where each case goes, relative to `cases/`, and the path its golds are named after. Two names
/// that come out the same in a folder are told apart by a number, an ignored case's among them.
let placed: (Planned * string * string) list =
    converted @ ignoredCases
    |> List.groupBy (fun (planned: Planned) -> planned.Folder, planned.IsNegative)
    |> List.collect (fun ((folder, negative), inFolder) ->
        let folder: string = if negative then $"%s{folder}/negative" else folder

        inFolder
        |> List.mapFold
            (fun (taken: Map<string, int>) (planned: Planned) ->
                let key: string = planned.Name + planned.Extension
                let seen: int = Map.tryFind key taken |> Option.defaultValue 0

                let name: string =
                    if seen = 0 then
                        planned.Name
                    else
                        $"%s{planned.Name}-%d{seen + 1}"

                let ignore: string = if planned.Ignored.IsSome then Case.ignoreSuffix else ""

                (planned, $"%s{folder}/%s{name}%s{ignore}%s{planned.Extension}", $"%s{folder}/%s{name}"),
                taken.Add(key, seen + 1)
            )
            Map.empty
        |> fst
    )

/// The files a case is, relative to `cases/`, with what they hold.
let filesOf (planned: Planned, casePath: string, goldStem: string) : (string * string) list =
    let lines: string list =
        [
            match planned.Ignored with
            | None -> ()
            | Some "" -> "# Ignored in Fantomas.Core.Tests, which gave no reason."
            | Some reason -> $"# %s{reason}"

            for key, value in planned.Properties do
                $"%s{key} = %s{value}"
        ]

    let frontMatter: string =
        match lines with
        | [] -> ""
        | lines ->

        let joined: string =
            lines |> List.map (fun (line: string) -> line + "\n") |> String.concat ""

        $"(*---\n%s{joined}---*)\n"

    [
        casePath, frontMatter + planned.Source
        for suffix, content in planned.Golds do
            $"%s{goldStem}.%s{suffix}%s{planned.Extension}", content
    ]

let caseOf: Map<string * int, string * string> =
    placed
    |> List.collect (fun (planned: Planned, casePath: string, _) ->
        let file: string = planned.Folder.Substring("ported/".Length) + ".fs"

        let outcome: string =
            if planned.Ignored.IsSome then "ignored"
            elif planned.IsNegative then "negative"
            else "case"

        planned.Tests |> List.map (fun (line, _) -> (file, line), (casePath, outcome))
    )
    |> Map.ofList

let rows: Row list =
    outcomes
    |> Array.toList
    |> List.map (fun (file, line, name, outcome) ->
        match outcome, caseOf.TryFind(file, line) with
        | NotConverted reason, _ ->
            {
                File = file
                Line = line
                Test = name
                Outcome = "exception"
                Detail = reason
            }
        | _, None -> failwith $"%s{file}:%d{line} was converted and placed nowhere."
        | _, Some(case, kind) ->
            {
                File = file
                Line = line
                Test = name
                Outcome = kind
                Detail = case
            }
    )

let ledgerPath: string = Path.Combine(Case.projectDirectory, "porting-ledger.tsv")

let ledger: string =
    "file\tline\ttest\toutcome\tcase or reason\n"
    + (rows
       |> List.map (fun row -> $"%s{row.File}\t%d{row.Line}\t%s{row.Test}\t%s{row.Outcome}\t%s{row.Detail}\n")
       |> String.concat "")

let files: Map<string, string> = placed |> List.collect filesOf |> Map.ofList

let portedDirectory: string = Path.Combine(Case.casesDirectory, "ported")

let mode: string =
    match fsi.CommandLineArgs |> Array.toList |> List.tail with
    | [ "--write" ] -> "write"
    | [ "--check" ] -> "check"
    | [] -> "dry run"
    | args -> failwith $"""Unknown arguments: %s{String.concat " " args}"""

/// What differs between the cases and ledger on disk and what the old tests make of them.
let differences () : string list =
    let onDisk: Map<string, string> =
        if not (Directory.Exists portedDirectory) then
            Map.empty
        else

        Directory.GetFiles(portedDirectory, "*", SearchOption.AllDirectories)
        // What a failing or ignored case gave last is no part of the port, and git ignores it.
        |> Array.filter (fun (path: string) -> not (Path.GetFileName(path).Contains ".actual."))
        |> Array.map (fun (path: string) ->
            Path.GetRelativePath(Case.casesDirectory, path).Replace('\\', '/'), File.ReadAllText path
        )
        |> Map.ofArray

    let caseDifferences: string list =
        Set.union (set files.Keys) (set onDisk.Keys)
        |> Set.toList
        |> List.choose (fun (path: string) ->
            match files.TryFind path, onDisk.TryFind path with
            | Some _, None -> Some $"missing: %s{path}"
            | None, Some _ -> Some $"not made from any old test: %s{path}"
            | Some expected, Some actual when expected <> normalise actual -> Some $"differs: %s{path}"
            | _ -> None
        )

    let ledgerDifferences: string list =
        if File.Exists ledgerPath && normalise (File.ReadAllText ledgerPath) = ledger then
            []
        else
            [ $"differs: %s{Path.GetFileName ledgerPath}" ]

    caseDifferences @ ledgerDifferences

match mode with
| "write" ->
    if Directory.Exists portedDirectory then
        Directory.Delete(portedDirectory, true)

    for KeyValue(relativePath, content) in files do
        let path: string = Path.Combine(Case.casesDirectory, relativePath)
        Directory.CreateDirectory(Path.GetDirectoryName path) |> ignore
        File.WriteAllText(path, content)

    File.WriteAllText(ledgerPath, ledger)
| _ -> ()

let count (outcome: string) : int =
    rows |> List.filter (fun row -> row.Outcome = outcome) |> List.length

let caseTests, negativeTests, ignoredTests, exceptionTests =
    count "case", count "negative", count "ignored", count "exception"

let negativeCases: int =
    placed
    |> List.filter (fun (planned: Planned, _, _) -> planned.IsNegative && planned.Ignored.IsNone)
    |> List.length

printfn
    $"%d{rows.Length} tests: %d{caseTests} verified against a gold, %d{negativeTests} against their own input, %d{ignoredTests} ignored, %d{exceptionTests} not converted"

printfn $"%d{placed.Length} cases, %d{negativeCases} of them negative and %d{ignoredTests} ignored"

rows
|> List.filter (fun row -> row.Outcome = "exception")
|> List.countBy (fun row -> Regex.Replace(row.Detail, @"`[^`]*`|\d+ times|: .*", "…"))
|> List.sortByDescending snd
|> List.iter (fun (reason, n) -> printfn $"%6d{n}  %s{reason}")

match mode with
| "write" -> printfn $"Wrote %d{files.Count} files to %s{portedDirectory}, and the ledger to %s{ledgerPath}"
| "check" ->
    match differences () with
    | [] -> printfn $"The %d{files.Count} files in %s{portedDirectory} and the ledger are what the old tests make."
    | found ->

    found |> List.truncate 50 |> List.iter (printfn "%s")
    printfn $"%d{found.Length} differences. `-- --write` makes the cases and the ledger again."
    exit 1
| _ -> printfn "Dry run: nothing written. `-- --write` writes the cases and the ledger, `-- --check` compares them."
