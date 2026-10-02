// The porting ledger: one row per test in Fantomas.Core.Tests, saying what became of it in the
// snapshot tests. Generating it keeps what was written by hand in the status, targets and reason
// columns, and refreshes the columns read from the tests themselves.
//
//   dotnet fsi scripts/ledger.fsx                       regenerate the ledger
//   dotnet fsi scripts/ledger.fsx -- --contains A,B     the tests whose input has node A or B
//   dotnet fsi scripts/ledger.fsx -- --resolved         the tests that removing the old suite would delete
//   dotnet fsi scripts/ledger.fsx -- --input F.fs:12    the input of the test on line 12 of F.fs, on stdout
//
// Needs a debug build of Fantomas.Core (`dotnet build src/Fantomas.Core`).

#r "../artifacts/bin/Fantomas.FCS/debug/Fantomas.FCS.dll"
#r "../artifacts/bin/Fantomas.Core/debug/Fantomas.Core.dll"

open System
open System.IO
open System.Text.RegularExpressions
open Fantomas.FCS.Syntax
open Fantomas.FCS.Text
open Fantomas.Core
open Fantomas.Core.SyntaxOak

let testsDirectory: string =
    Path.Combine(__SOURCE_DIRECTORY__, "..", "src", "Fantomas.Core.Tests")

let ledgerPath: string =
    Path.Combine(__SOURCE_DIRECTORY__, "..", "src", "Fantomas.Core.SnapshotTests", "porting-ledger.tsv")

/// The files whose tests are about internals rather than formatted output. Their tests start as `unit`.
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

/// The helpers a test formats with, and which of their arguments is the input.
let formatHelpers: Map<string, int> =
    Map.ofList
        [
            "formatSourceString", 0
            "formatSignatureString", 0
            "formatSourceStringWithDefines", 1
            "formatAST", 1
        ]

type Row =
    {
        File: string
        Line: int
        Test: string
        Issue: string
        Status: string
        Targets: string
        Reason: string
        /// `test` when the binding carries `[<Test>]`, `missing` when it looks like a test and does not.
        Attribute: string
        Helper: string
        Config: string
        Nodes: string
        /// The input the test formats, when it hands its helper a string literal. Not written to
        /// the ledger: `--input` prints it.
        Input: string option
    }

let columns: string list =
    [
        "file"
        "line"
        "test"
        "issue"
        "status"
        "targets"
        "reason"
        "attribute"
        "helper"
        "config"
        "nodes"
    ]

let toLine (row: Row) : string =
    [
        row.File
        string row.Line
        row.Test
        row.Issue
        row.Status
        row.Targets
        row.Reason
        row.Attribute
        row.Helper
        row.Config
        row.Nodes
    ]
    |> String.concat "\t"

/// What was written by hand in an existing ledger, by file and test name.
let handWritten () : Map<string * string, string * string * string> =
    if not (File.Exists ledgerPath) then
        Map.empty
    else

    File.ReadAllLines ledgerPath
    |> Array.skip 1
    |> Array.choose (fun (line: string) ->
        match line.Split '\t' with
        | fields when fields.Length >= 7 -> Some((fields[0], fields[2]), (fields[4], fields[5], fields[6]))
        | _ -> None
    )
    |> Map.ofArray

/// A function and its arguments, from a chain of applications.
let rec spine (expr: SynExpr) : SynExpr * SynExpr list =
    match expr with
    | SynExpr.App(funcExpr = funcExpr; argExpr = argExpr) ->
        let head, args = spine funcExpr
        head, args @ [ argExpr ]
    | expr -> expr, []

/// Every expression in a test body that could hold a call to a format helper.
let rec subExpressions (expr: SynExpr) : SynExpr list =
    let below: SynExpr list =
        match expr with
        | SynExpr.App(funcExpr = funcExpr; argExpr = argExpr) -> [ funcExpr; argExpr ]
        | SynExpr.Paren(expr = inner)
        | SynExpr.Typed(expr = inner)
        | SynExpr.Do(expr = inner) -> [ inner ]
        | SynExpr.Sequential(expr1 = first; expr2 = second) -> [ first; second ]
        | SynExpr.LetOrUse letOrUse ->
            (letOrUse.Bindings |> List.map (fun (SynBinding(expr = bound)) -> bound))
            @ [ letOrUse.Body ]
        | SynExpr.Tuple(exprs = exprs)
        | SynExpr.ArrayOrList(exprs = exprs) -> exprs
        | SynExpr.ArrayOrListComputed(expr = inner)
        | SynExpr.ComputationExpr(expr = inner) -> [ inner ]
        | SynExpr.Lambda(body = body) -> [ body ]
        | SynExpr.IfThenElse(ifExpr = condition; thenExpr = thenExpr; elseExpr = elseExpr) ->
            [ condition; thenExpr ] @ Option.toList elseExpr
        | _ -> []

    expr :: List.collect subExpressions below

let lineRange (source: string array) (range: range) : string =
    if range.StartLine = range.EndLine then
        source[range.StartLine - 1].Substring(range.StartColumn, range.EndColumn - range.StartColumn)
    else

    [
        source[range.StartLine - 1].Substring range.StartColumn
        yield! source[range.StartLine .. range.EndLine - 2]
        source[range.EndLine - 1].Substring(0, range.EndColumn)
    ]
    |> String.concat " "

/// The first format helper a test body calls: its name, the input it formats and the source of the
/// arguments after the input, the config.
let formatCall (source: string array) (body: SynExpr) : (string * string * string) option =
    subExpressions body
    |> List.tryPick (fun (expr: SynExpr) ->
        match spine expr with
        | SynExpr.Ident ident, args when formatHelpers.ContainsKey ident.idText ->
            let inputIndex: int = formatHelpers[ident.idText]

            match List.tryItem inputIndex args with
            | Some(SynExpr.Const(SynConst.String(text = input), _)) ->
                let config: string =
                    args
                    |> List.skip (inputIndex + 1)
                    |> List.map (fun (arg: SynExpr) -> lineRange source arg.Range)
                    |> String.concat " "
                    |> fun (config: string) -> Regex.Replace(config, @"\s+", " ")

                Some(ident.idText, input, config)
            | _ -> None
        | _ -> None
    )

let rec allNodes (node: Node) : Node list =
    node :: (node.Children |> Array.toList |> List.collect allNodes)

/// The node classes the input contains, under every define combination.
let nodesOf (isSignature: bool) (input: string) : string =
    try
        CodeFormatter.ParseOakAsync(isSignature, input)
        |> Async.RunSynchronously
        |> Array.toList
        |> List.collect (fun (oak: Oak, _) -> allNodes oak)
        |> List.map (fun (node: Node) -> node.GetType().Name)
        |> List.distinct
        |> List.sort
        |> String.concat ","
    with _ ->
        "(does not parse)"

let issueOf (test: string) : string =
    Regex.Matches(test, @"(?:,\s*|#|issue\s*)(\d{3,5})\b")
    |> Seq.map (fun (m: Match) -> m.Groups[1].Value)
    |> Seq.distinct
    |> String.concat ","

let rowsOf (relativeFile: string) : Row list =
    let path: string = Path.Combine(testsDirectory, relativeFile)
    let text: string = File.ReadAllText path
    let source: string array = text.Replace("\r\n", "\n").Split('\n')
    let tree, _ = Fantomas.FCS.Parse.parseFile false (SourceText.ofString text) []

    let rec bindingsOf (decls: SynModuleDecl list) : SynBinding list =
        decls
        |> List.collect (fun (decl: SynModuleDecl) ->
            match decl with
            | SynModuleDecl.Let(bindings = bindings) -> bindings
            | SynModuleDecl.NestedModule(decls = decls) -> bindingsOf decls
            | _ -> []
        )

    let bindings: SynBinding list =
        match tree with
        | ParsedInput.SigFile _ -> []
        | ParsedInput.ImplFile(ParsedImplFileInput(contents = modules)) ->

        modules
        |> List.collect (fun (SynModuleOrNamespace(decls = decls)) -> bindingsOf decls)

    // Some files shadow `config` for every test in them, so `config` in a test can mean something
    // other than the default.
    let fileConfig: string option =
        bindings
        |> List.tryPick (fun (SynBinding(headPat = headPat; expr = expr)) ->
            match headPat with
            | SynPat.Named(ident = SynIdent(ident, _)) when ident.idText = "config" ->
                Some(Regex.Replace(lineRange source expr.Range, @"\s+", " "))
            | _ -> None
        )

    bindings
    |> List.choose (fun (SynBinding(attributes = attributes; headPat = headPat; expr = body)) ->
        let attributeNames: string list =
            attributes
            |> List.collect (fun (list: SynAttributeList) -> list.Attributes)
            |> List.map (fun (attribute: SynAttribute) -> (List.last attribute.TypeName.LongIdent).idText)

        let isTestAttribute (name: string) : bool =
            List.contains name [ "Test"; "TestCase"; "TestCaseSource"; "TestAttribute" ]

        match headPat with
        | SynPat.LongIdent(
            longDotId = SynLongIdent(id = [ ident ])
            argPats = SynArgPats.Pats [ SynPat.Paren(pat = SynPat.Const(SynConst.Unit, _)) ]) ->
            let hasAttribute: bool = List.exists isTestAttribute attributeNames

            // A unit function with a name that reads as a sentence and no test attribute is a test
            // that never runs.
            if not hasAttribute && not (ident.idText.Contains ' ') then
                None
            else

            let helper, input, config =
                match formatCall source body with
                | Some(helper, input, config) -> helper, Some input, config
                | None -> "(none)", None, ""

            let isSignature: bool =
                helper = "formatSignatureString"
                || helper = "formatAST" && config.StartsWith("true", StringComparison.Ordinal)

            Some
                {
                    File = relativeFile
                    Line = ident.idRange.StartLine
                    Test = ident.idText
                    Issue = issueOf ident.idText
                    Status =
                        if unitTestFiles.Contains relativeFile then
                            "unit"
                        else
                            "todo"
                    Targets = ""
                    Reason = ""
                    Attribute = if hasAttribute then "test" else "missing"
                    Helper = helper
                    Config =
                        match fileConfig with
                        | Some fileConfig when config = "config" -> $"config, which this file sets to %s{fileConfig}"
                        | _ -> config
                    Nodes = input |> Option.map (nodesOf isSignature) |> Option.defaultValue ""
                    Input = input
                }
        | _ -> None
    )

let generate () : unit =
    let kept: Map<string * string, string * string * string> = handWritten ()

    let rows: Row list =
        Directory.GetFiles(testsDirectory, "*Tests.fs", SearchOption.AllDirectories)
        |> Array.map (fun (path: string) -> Path.GetRelativePath(testsDirectory, path).Replace('\\', '/'))
        |> Array.sort
        |> Array.toList
        |> List.collect rowsOf
        |> List.map (fun (row: Row) ->
            match Map.tryFind (row.File, row.Test) kept with
            | None -> row
            | Some(status, targets, reason) ->

            { row with
                Status = status
                Targets = targets
                Reason = reason
            }
        )

    File.WriteAllLines(ledgerPath, String.concat "\t" columns :: List.map toLine rows)

    let count (status: string) : int =
        rows |> List.filter (fun (row: Row) -> row.Status = status) |> List.length

    let summary: string =
        [ "todo"; "ported"; "merged"; "dropped"; "unit" ]
        |> List.map (fun (status: string) -> $"%d{count status} %s{status}")
        |> String.concat ", "

    printfn $"Wrote %d{rows.Length} tests to %s{ledgerPath}: %s{summary}."

let contains (nodeClasses: string list) : unit =
    File.ReadAllLines ledgerPath
    |> Array.skip 1
    |> Array.map (fun (line: string) -> line.Split '\t')
    |> Array.filter (fun (fields: string array) ->
        let nodes: Set<string> = fields[10].Split ',' |> Set.ofArray
        List.exists nodes.Contains nodeClasses
    )
    |> Array.iter (fun (fields: string array) ->
        printfn $"%s{fields[0]}:%s{fields[1]}\t%s{fields[4]}\t%s{fields[8]}\t%s{fields[9]}\t%s{fields[2]}"
    )

/// The dry run of removing the old tests: every test that is ported, merged or dropped, by file, and
/// whether anything is left in that file afterwards.
let resolved () : unit =
    let rows: string array array =
        File.ReadAllLines ledgerPath
        |> Array.skip 1
        |> Array.map (fun (line: string) -> line.Split '\t')

    let isResolved (fields: string array) : bool =
        List.contains fields[4] [ "ported"; "merged"; "dropped" ]

    for file, inFile in rows |> Array.groupBy (fun (fields: string array) -> fields[0]) do
        let gone: string array array = inFile |> Array.filter isResolved

        if not (Array.isEmpty gone) then
            printfn $"%s{file}: %d{gone.Length} of %d{inFile.Length} would go"

            for fields in gone do
                printfn $"    %s{fields[1]}\t%s{fields[4]}\t%s{fields[2]}"

    let total: int = rows |> Array.filter isResolved |> Array.length
    printfn $"%d{total} of %d{rows.Length} tests would go."

/// The input an old test formats, exactly as it hands it to its helper, ready to save as a case.
/// The test is named by its file and the line of its name, as the ledger and `--contains` print it.
let input (fileAndLine: string) : unit =
    let file, line =
        match fileAndLine.Split ':' with
        | [| file; line |] -> file, int line
        | _ -> failwith $"`%s{fileAndLine}` is not File.fs:line."

    match rowsOf file |> List.tryFind (fun (row: Row) -> row.Line = line) with
    | None -> failwith $"No test starts on line %d{line} of %s{file}."
    | Some row ->

    match row.Input with
    | None -> failwith $"%s{row.Test} does not hand its helper a string literal; read it in %s{file}."
    | Some input ->

    eprintfn $"// %s{row.Test}, %s{row.Helper}, config: %s{row.Config}"
    printf $"%s{input}"

match fsi.CommandLineArgs |> Array.toList |> List.tail with
| [ "--input"; fileAndLine ] -> input fileAndLine
| [ "--contains"; nodeClasses ] -> contains (nodeClasses.Split ',' |> Array.toList)
| [ "--resolved" ] -> resolved ()
| [] -> generate ()
| args -> failwith $"""Unknown arguments: %s{String.concat " " args}"""
