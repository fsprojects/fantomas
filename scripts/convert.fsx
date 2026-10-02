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

/// What a name a test body uses stands for: the `let`s of its file and of the body itself, and the
/// parameters of a parameterised test or of a function of its file.
type Binding =
    {
        Expr: SynExpr
        /// The names of its parameters, for a function: `let checkFormat config source expected = ...`.
        Params: string list
        /// What the names in `Expr` refer to.
        Scope: Map<string, Binding>
    }

type Scope = Map<string, Binding>

let valueBinding (scope: Scope) (expr: SynExpr) : Binding =
    {
        Expr = expr
        Params = []
        Scope = scope
    }

/// The names of parameters, when every one is a plain name, typed or not.
let rec paramNamesOf (pats: SynPat list) : string list option =
    pats
    |> List.map (fun (pat: SynPat) ->
        match pat with
        | SynPat.Named(ident = SynIdent(ident, _)) -> Some [ ident.idText ]
        | SynPat.Const(SynConst.Unit, _) -> Some []
        | SynPat.Paren(pat = inner)
        | SynPat.Typed(pat = inner) -> paramNamesOf [ inner ]
        | SynPat.Tuple(elementPats = elements) -> paramNamesOf elements
        | _ -> None
    )
    |> List.fold (fun (names: string list option) (more: string list option) -> Option.map2 (@) names more) (Some [])

/// A scope with one more `let`: a value, or a function whose parameters are plain names.
let bind (scope: Scope) (SynBinding(headPat = pat; expr = bound): SynBinding) : Scope =
    match pat with
    | SynPat.Named(ident = SynIdent(ident, _)) -> scope.Add(ident.idText, valueBinding scope bound)
    | SynPat.LongIdent(longDotId = SynLongIdent(id = [ ident ]); argPats = SynArgPats.Pats pats) ->
        match paramNamesOf pats with
        | None -> scope
        | Some names ->
            scope.Add(
                ident.idText,
                {
                    Expr = bound
                    Params = names
                    Scope = scope
                }
            )
    | _ -> scope

/// A function of the file applied to its arguments: its body, and the scope its body sees, every
/// parameter bound to its argument.
let apply (callerScope: Scope) (binding: Binding) (args: SynExpr list) : Result<SynExpr * Scope, string> =
    if args.Length <> binding.Params.Length then
        Error "calls a function of its file with a number of arguments it does not take"
    else

    let scope: Scope =
        List.fold2
            (fun (scope: Scope) (name: string) (arg: SynExpr) -> scope.Add(name, valueBinding callerScope arg))
            binding.Scope
            binding.Params
            args

    Ok(binding.Expr, scope)

let functionIn (scope: Scope) (head: SynExpr) : Binding option =
    match head with
    | SynExpr.Ident ident ->
        scope.TryFind ident.idText
        |> Option.filter (fun (binding: Binding) -> not binding.Params.IsEmpty)
    | _ -> None

let configFields: Reflection.PropertyInfo array =
    FSharpType.GetRecordFields typeof<FormatConfig>

/// Every result in order, or the first error.
let sequence (results: Result<'a, string> list) : Result<'a list, string> =
    List.foldBack
        (fun (result: Result<'a, string>) (rest: Result<'a list, string>) ->
            match result, rest with
            | Error e, _ -> Error e
            | Ok _, Error e -> Error e
            | Ok x, Ok xs -> Ok(x :: xs)
        )
        results
        (Ok [])

let helpers: Set<string> =
    set
        [
            "formatSourceString"
            "formatSignatureString"
            "formatSourceStringWithDefines"
            "formatAST"
            "CodeFormatter.FormatDocumentAsync"
        ]

let rec stringOf (scope: Scope) (expr: SynExpr) : Result<string, string> =
    match expr with
    | SynExpr.Const(SynConst.String(text = text), _) -> Ok text
    | SynExpr.Paren(expr = inner) -> stringOf scope inner
    | SynExpr.LongIdent(longDotId = SynLongIdent(id = [ s; e ])) when s.idText = "String" && e.idText = "Empty" -> Ok ""
    | SynExpr.Ident ident when scope.ContainsKey ident.idText && scope[ident.idText].Params.IsEmpty ->
        let binding: Binding = scope[ident.idText]
        stringOf binding.Scope binding.Expr
    | SynExpr.InterpolatedString(contents = parts) ->
        // A fill is taken as it is, under `%s` or with no format; any other format is not followed.
        let rec partsOf (parts: SynInterpolatedStringPart list) : Result<string list, string> =
            match parts with
            | [] -> Ok []
            | SynInterpolatedStringPart.String(value = text) :: rest ->
                partsOf rest |> Result.map (fun (more: string list) -> text :: more)
            | SynInterpolatedStringPart.FillExpr(
                fillExpr = fill; formatting = SynInterpolationFormatting.Printf(specifier = "%s")) :: rest
            | SynInterpolatedStringPart.FillExpr(
                fillExpr = fill; formatting = SynInterpolationFormatting.DotNet(alignment = None; format = None)) :: rest ->
                stringOf scope fill
                |> Result.bind (fun (filled: string) -> partsOf rest |> Result.map (fun more -> filled :: more))
            | SynInterpolatedStringPart.FillExpr _ :: _ -> Error "a string with a formatted fill"

        partsOf parts |> Result.map (String.concat "")
    | expr ->

    // A format helper's result, fed to another test step: formatted as the helper formats it.
    match callOf scope expr with
    | Error _ -> Error "a string that is not a literal"
    | Ok call ->

    match call.Defines with
    | Some _ -> Error "a string formatted for one define combination"
    | None ->

    let config: FormatConfig =
        { call.Config with
            EndOfLine = EndOfLineStyle.LF
        }

    match Formatting.formatAndCheck config call.IsSignature call.Input with
    | formatted, [] -> Ok formatted.Merged
    | _, _ :: _ -> Error "a string formatted with a problem"

/// The value of a field in `{ config with Field = value }`, read from its literal.
and fieldValue (scope: Scope) (field: Reflection.PropertyInfo) (expr: SynExpr) : Result<obj, string> =
    match expr with
    | SynExpr.Paren(expr = inner) -> fieldValue scope field inner
    | SynExpr.Ident ident when scope.ContainsKey ident.idText && scope[ident.idText].Params.IsEmpty ->
        let binding: Binding = scope[ident.idText]
        fieldValue binding.Scope field binding.Expr
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

and configOf (scope: Scope) (expr: SynExpr) : Result<FormatConfig, string> =
    match expr with
    | SynExpr.Paren(expr = inner) -> configOf scope inner
    | SynExpr.Ident ident when ident.idText = "config" && not (scope.ContainsKey "config") -> Ok FormatConfig.Default
    | SynExpr.LongIdent(longDotId = SynLongIdent(id = [ t; d ])) when t.idText = "FormatConfig" && d.idText = "Default" ->
        Ok FormatConfig.Default
    | SynExpr.Ident ident when scope.ContainsKey ident.idText && scope[ident.idText].Params.IsEmpty ->
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
                            fieldValue scope configFields[index] value
                            |> Result.map (fun (v: obj) -> values[index] <- v)
                        | _ -> Error $"`%s{name}` is no setting"
                    )
                )
                (Ok())
            |> Result.map (fun () -> FSharpValue.MakeRecord(typeof<FormatConfig>, values) :?> FormatConfig)
        )
    | expr ->

    // A function of the file that makes a config: `let config x = { config with ... }`.
    match spine expr with
    | head, (_ :: _ as args) when (functionIn scope head).IsSome ->
        apply scope (functionIn scope head).Value args
        |> Result.bind (fun (body: SynExpr, inner: Scope) -> configOf inner body)
    | _ -> Error "a config that is not `config` or a copy of it"

and callOf (scope: Scope) (expr: SynExpr) : Result<Call, string> =
    let head, args = spine expr

    let call
        (helper: string)
        (isSignature: bool)
        (defines: string list option)
        (input: SynExpr)
        (config: SynExpr)
        : Result<Call, string>
        =
        stringOf scope input
        |> Result.bind (fun (input: string) ->
            configOf scope config
            |> Result.map (fun (config: FormatConfig) ->
                {
                    Helper = helper
                    Input = input
                    IsSignature = isSignature
                    Defines = defines
                    Config = config
                    Ignored = None
                }
            )
        )

    match identText head, args with
    | Some("formatSourceString" | "formatSignatureString" as helper), [ input; config ] ->
        call helper (helper = "formatSignatureString") None input config
    | Some "CodeFormatter.FormatDocumentAsync",
      [ SynExpr.Paren(expr = SynExpr.Tuple(exprs = [ SynExpr.Const(SynConst.Bool isSignature, _); input; config ])) ] ->
        call "CodeFormatter.FormatDocumentAsync" isSignature None input config
    | Some "formatSourceStringWithDefines", [ defines; input; config ] ->
        let definesResult: Result<string list, string> =
            match defines with
            | SynExpr.ArrayOrList(exprs = []) -> Ok []
            | SynExpr.ArrayOrList(exprs = exprs) -> exprs |> List.map (stringOf scope) |> sequence
            | SynExpr.ArrayOrListComputed(expr = inner) ->
                let rec elements (expr: SynExpr) : SynExpr list =
                    match expr with
                    | SynExpr.Sequential(expr1 = first; expr2 = rest) -> first :: elements rest
                    | expr -> [ expr ]

                elements inner |> List.map (stringOf scope) |> sequence
            | _ -> Error "defines that are not a list of literals"

        definesResult
        |> Result.bind (fun (defines: string list) ->
            call "formatSourceStringWithDefines" false (Some defines) input config
        )
    | Some "formatAST", _ -> Error "formats a syntax tree without its source, so without trivia"
    | Some helper, _ when helpers.Contains helper ->
        Error $"calls %s{helper} with arguments that are not input and config"
    | _ -> Error "does not call a format helper first"

/// One step of a test: `helper input config |> prepend newline |> should equal expected`. A lambda
/// in between names the result so far and makes its body the result: `|> fun formatted ->
/// formatSourceString formatted config` formats it again. After `CodeFormatter.FormatDocumentAsync`,
/// running the result and taking its code leave it as it is.
let stepOf (scope: Scope) (expr: SynExpr) : Result<Call * string, string> =
    let isDirect (head: SynExpr) : bool =
        identText (fst (spine head)) = Some "CodeFormatter.FormatDocumentAsync"

    let rec stagesOf
        (scope: Scope)
        (head: SynExpr)
        (prepended: bool)
        (stages: SynExpr list)
        : Result<Scope * SynExpr * bool * SynExpr, string>
        =
        match stages with
        | [] -> Error "does not compare the result"
        | [ last ] ->
            match spine last with
            | should, [ equal; expected ] when identText should = Some "should" && identText equal = Some "equal" ->
                Ok(scope, head, prepended, expected)
            | _ -> Error "does not end in `should equal`"
        | stage :: rest ->

        match stage, spine stage with
        | SynExpr.Lambda _, _ when isDirect head -> stagesOf scope head prepended rest
        | SynExpr.Lambda(parsedData = Some([ SynPat.Named(ident = SynIdent(name, _)) ], body)), _ when not prepended ->
            stagesOf (scope.Add(name.idText, valueBinding scope head)) body prepended rest
        | _, (prepend, [ newline ]) when identText prepend = Some "prepend" && identText newline = Some "newline" ->
            stagesOf scope head true rest
        | _, (run, []) when isDirect head && identText run = Some "Async.RunSynchronously" ->
            stagesOf scope head prepended rest
        | _ -> Error "pipes the result through something other than `prepend newline`"

    match pipeline expr with
    | [] -> Error "has no body"
    | head :: stages ->

    stagesOf scope head false stages
    |> Result.bind (fun (scope, head, prepended, expected) ->
        callOf scope head
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

/// Every step of a test body: its `let`s are names for the steps, a sequence is several steps, and
/// a call to a function of its file is that function's steps.
let rec stepsOf (scope: Scope) (body: SynExpr) : Result<(Call * string) list, string> =
    match body with
    | SynExpr.LetOrUse letOrUse when not letOrUse.IsRecursive ->
        stepsOf (List.fold bind scope letOrUse.Bindings) letOrUse.Body
    | SynExpr.Paren(expr = inner) -> stepsOf scope inner
    | SynExpr.Sequential(expr1 = first; expr2 = rest) ->
        stepsOf scope first
        |> Result.bind (fun (steps: (Call * string) list) -> stepsOf scope rest |> Result.map ((@) steps))
    | expr ->

    match spine expr with
    | head, (_ :: _ as args) when (functionIn scope head).IsSome ->
        apply scope (functionIn scope head).Value args
        |> Result.bind (fun (body: SynExpr, inner: Scope) -> stepsOf inner body)
    | _ -> stepOf scope expr |> Result.map List.singleton

/// The arguments of each case of a parameterised test: `[<TestCase "...">]`, or the list a
/// `[<TestCaseSource "name">]` names.
let casesOf (fileScope: Scope) (attributes: SynAttribute list) : Result<SynExpr list list, string> =
    let rec elements (expr: SynExpr) : SynExpr list =
        match expr with
        | SynExpr.Sequential(expr1 = first; expr2 = rest) -> first :: elements rest
        | expr -> [ expr ]

    let argumentsOf (expr: SynExpr) : SynExpr list =
        match expr with
        | SynExpr.Paren(expr = SynExpr.Tuple(exprs = exprs))
        | SynExpr.Tuple(exprs = exprs) -> exprs
        | SynExpr.Paren(expr = inner) -> [ inner ]
        | expr -> [ expr ]

    attributes
    |> List.map (fun (attribute: SynAttribute) ->
        match (List.last attribute.TypeName.LongIdent).idText with
        | "TestCase" -> Ok [ argumentsOf attribute.ArgExpr ]
        | "TestCaseSource" ->
            match stringOf Map.empty attribute.ArgExpr with
            | Error reason -> Error reason
            | Ok name ->

            match fileScope.TryFind name with
            | Some {
                       Expr = SynExpr.ArrayOrListComputed(expr = inner)
                   } -> Ok(elements inner |> List.map argumentsOf)
            | Some {
                       Expr = SynExpr.ArrayOrList(exprs = exprs)
                   } -> Ok(exprs |> List.map argumentsOf)
            | _ -> Error "takes its cases from something other than a list of literals"
        | _ -> Ok []
    )
    |> sequence
    |> Result.map List.concat

let testsOf (relativeFile: string) : (string * int * string * Result<(Call * string) list, string>) list =
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

    // The values and functions a file defines for its tests, `config` among them, in order.
    let fileScope: Scope = List.fold bind Map.empty bindings

    bindings
    |> List.choose (fun (SynBinding(attributes = attributes; headPat = headPat; expr = body)) ->
        let attributes: SynAttribute list = attributes |> List.collect _.Attributes

        let attributeNames: string list =
            attributes
            |> List.map (fun (attribute: SynAttribute) -> (List.last attribute.TypeName.LongIdent).idText)

        let ignoreReason: string option =
            attributes
            |> List.tryPick (fun (attribute: SynAttribute) ->
                if (List.last attribute.TypeName.LongIdent).idText <> "Ignore" then
                    None
                else

                match attribute.ArgExpr with
                | SynExpr.Paren(expr = SynExpr.Const(SynConst.String(text = reason), _))
                | SynExpr.Const(SynConst.String(text = reason), _) -> Some reason
                | _ -> Some ""
            )

        let ignored (steps: (Call * string) list) : (Call * string) list =
            steps
            |> List.map (fun (call: Call, expected: string) -> { call with Ignored = ignoreReason }, expected)

        let isParameterised: bool =
            List.exists (fun name -> name = "TestCase" || name = "TestCaseSource") attributeNames

        match headPat with
        | SynPat.LongIdent(
            longDotId = SynLongIdent(id = [ ident ])
            argPats = SynArgPats.Pats [ SynPat.Paren(pat = SynPat.Const(SynConst.Unit, _)) ]) when
            List.contains "Test" attributeNames || ident.idText.Contains ' '
            ->
            let result: Result<(Call * string) list, string> =
                if not (List.contains "Test" attributeNames) then
                    Error "has no [<Test>] attribute, so never runs"
                else
                    stepsOf fileScope body |> Result.map ignored

            Some(relativeFile, ident.idRange.StartLine, ident.idText, result)
        | SynPat.LongIdent(longDotId = SynLongIdent(id = [ ident ]); argPats = SynArgPats.Pats pats) when
            isParameterised
            ->
            // Every case of a parameterised test is a test of its own, its parameters its arguments.
            let result: Result<(Call * string) list, string> =
                match paramNamesOf pats with
                | None -> Error "is a parameterised test with parameters that are not plain names"
                | Some names ->

                casesOf fileScope attributes
                |> Result.bind (fun (cases: SynExpr list list) ->
                    cases
                    |> List.map (fun (arguments: SynExpr list) ->
                        apply
                            fileScope
                            {
                                Expr = body
                                Params = names
                                Scope = fileScope
                            }
                            arguments
                        |> Result.bind (fun (body: SynExpr, scope: Scope) -> stepsOf scope body)
                    )
                    |> sequence
                    |> Result.map (List.concat >> ignored)
                )

            Some(relativeFile, ident.idRange.StartLine, ident.idText, result)
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
        /// `case`, `negative`, `ignored` or `exception`; for a test of several cases, each kind
        /// among them, separated by commas.
        Outcome: string
        /// The cases, relative to `cases/` and separated by `; `, or why there is none.
        Detail: string
    }

let testFiles: string list =
    Directory.GetFiles(testsDirectory, "*Tests.fs", SearchOption.AllDirectories)
    |> Array.map (fun (path: string) -> Path.GetRelativePath(testsDirectory, path).Replace('\\', '/'))
    |> Array.sort
    |> Array.toList

let tests: (string * int * string * Result<(Call * string) list, string>) array =
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
/// What became of each test: a case for each of its steps, or why it has none. A test converts as a
/// whole or not at all, so a step that does not convert keeps every step of its test where it is.
let outcomes: (string * int * string * Result<Outcome list, string>) array =
    tests
    |> Array.mapi (fun (index: int) (file, line, name, parsed) ->
        if index % 500 = 0 then
            eprintfn $"%d{index} of %d{tests.Length} tests"

        let outcomeOfStep (call: Call, expected: string) : Outcome =
            match outcomeOf call expected, call.Ignored with
            | Ok verified, _ -> Converted(call, verified)
            | Error why, None -> NotConverted why
            | Error why, Some _ -> ignoredOutcome call expected why

        let outcome: Result<Outcome list, string> =
            parsed
            |> Result.bind (fun (steps: (Call * string) list) ->
                let outcomes: Outcome list = List.map outcomeOfStep steps

                match
                    outcomes
                    |> List.tryPick (fun (outcome: Outcome) ->
                        match outcome with
                        | NotConverted reason -> Some reason
                        | Converted _
                        | Ignored _ -> None
                    )
                with
                | Some reason -> Error reason
                | None -> Ok outcomes
            )

        file, line, name, outcome
    )

/// Every step that became a case, with the test it comes from.
let steps: (string * int * string * Outcome) list =
    outcomes
    |> Array.toList
    |> List.collect (fun (file, line, name, outcome) ->
        match outcome with
        | Ok steps -> steps |> List.map (fun (step: Outcome) -> file, line, name, step)
        | Error _ -> []
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
    steps
    |> List.choose (fun (file, line, name, outcome) ->
        match outcome with
        | Converted(call, verified) -> Some(file, line, name, call, verified)
        | Ignored _
        | NotConverted _ -> None
    )
    |> List.groupBy (fun (file, _, _, call, verified) -> file, call.IsSignature, verified.Source, verified.Properties)
    |> List.map (fun ((file, isSignature, _, _), members) ->
        let _, _, firstName, _, verified = members.Head
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
            Tests = members |> List.map (fun (_, line, name, _, _) -> line, name) |> List.distinct
        }
    )

let ignoredCases: Planned list =
    steps
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
                    Tests = List.singleton (line, name)
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

/// The cases each test became, with what kind of case each is.
let caseOf: Map<string * int, (string * string) list> =
    placed
    |> List.collect (fun (planned: Planned, casePath: string, _) ->
        let file: string = planned.Folder.Substring("ported/".Length) + ".fs"

        let kind: string =
            if planned.Ignored.IsSome then "ignored"
            elif planned.IsNegative then "negative"
            else "case"

        planned.Tests |> List.map (fun (line, _) -> (file, line), (casePath, kind))
    )
    |> List.groupBy fst
    |> List.map (fun (test, cases) -> test, cases |> List.map snd |> List.sortBy fst)
    |> Map.ofList

let rows: Row list =
    outcomes
    |> Array.toList
    |> List.map (fun (file, line, name, outcome) ->
        match outcome, caseOf.TryFind(file, line) with
        | Error reason, _ ->
            {
                File = file
                Line = line
                Test = name
                Outcome = "exception"
                Detail = reason
            }
        | Ok _, None -> failwith $"%s{file}:%d{line} was converted and placed nowhere."
        | Ok _, Some cases ->
            {
                File = file
                Line = line
                Test = name
                Outcome = cases |> List.map snd |> List.distinct |> String.concat ","
                Detail = cases |> List.map fst |> String.concat "; "
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
