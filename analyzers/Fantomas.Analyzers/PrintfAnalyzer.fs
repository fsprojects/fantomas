module Fantomas.Analyzers.PrintfAnalyzer

open System
open System.Text.RegularExpressions
open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.ASTCollecting
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

[<Literal>]
let Code: string = "FANTOMAS-PRINTF-001"

[<Literal>]
let Name: string = "PrintfAnalyzer"

[<Literal>]
let ShortDescription: string =
    "Detects printf in code the command line tool runs, which fails at runtime in a Native AOT build."

[<Literal>]
let HelpUri: string =
    "https://github.com/fsprojects/fantomas/blob/main/analyzers/AGENTS.md#fantomas-printf-001"

// The projects whose code the tool runs. The tests and Fantomas.Client only ever run on the JIT,
// where printf works. The editor hands an analyzer the file and not its project, so the file's
// folder is what decides, for the command line and the editor alike.
let isToolCode (fileName: string) : bool =
    let path: string = fileName.Replace('\\', '/')

    path.Contains("/src/Fantomas/", StringComparison.Ordinal)
    || path.Contains("/src/Fantomas.Core/", StringComparison.Ordinal)

// The functions that take a printf format. `kprintf` and its kin are here too: a function that
// forwards a format to one of them makes its callers' interpolated strings printf formats, which is
// not something the syntax at those calls can show.
let printfFunctions: Set<string> =
    set
        [
            "sprintf"
            "printf"
            "printfn"
            "eprintf"
            "eprintfn"
            "fprintf"
            "fprintfn"
            "bprintf"
            "failwithf"
            "kprintf"
            "ksprintf"
            "kfprintf"
            "kbprintf"
        ]

// A printf specifier at the very end of the text before a hole, which is where the untyped tree
// keeps it: `$"took %.0f{x}"` is the text `took %.0f` and then the hole. The run of percent signs
// before it has to be odd, since `%%` is an escaped percent sign and `%%d{x}` a bare hole.
let trailingSpecifier: Regex =
    Regex(@"(?<percents>%+)(?<spec>[-+ 0#]*(\d+|\*)?(\.\d+)?[a-zA-Z])$", RegexOptions.Compiled)

// The specifiers an interpolated string that produces a string turns into `String.Concat` rather
// than printf, since dotnet/fsharp#19971: a bare `%s`, `%c`, `%d`, `%i` or `%M`, with no flags,
// width or precision. A hole without a specifier, or with a .NET format such as `{x:N2}`, needs no
// printf either. Every other specifier becomes a `sprintf` call.
let loweredSpecifiers: Set<string> = set [ "s"; "c"; "d"; "i"; "M" ]

let needsPrintf (textBeforeHole: string) : bool =
    let found: Match = trailingSpecifier.Match textBeforeHole

    found.Success
    && found.Groups.["percents"].Length % 2 = 1
    && not (loweredSpecifiers.Contains found.Groups.["spec"].Value)

let rec interpolatedStringNeedsPrintf (parts: SynInterpolatedStringPart list) : bool =
    match parts with
    | SynInterpolatedStringPart.String(text, _) :: (SynInterpolatedStringPart.FillExpr _ :: _ as rest) ->
        needsPrintf text || interpolatedStringNeedsPrintf rest
    | _ :: rest -> interpolatedStringNeedsPrintf rest
    | [] -> false

// Reported on the format, or on the function that takes one, which is the part to rewrite.
let analyze (fileName: string) (parsedInput: ParsedInput) : Message list =
    if not (isToolCode fileName) then
        []
    else

    let found: ResizeArray<range> = ResizeArray<range>()

    let walker: SyntaxCollectorBase =
        { new SyntaxCollectorBase() with
            override _.WalkExpr(_path: SyntaxVisitorPath, expr: SynExpr) : unit =
                match expr with
                | SynExpr.InterpolatedString(contents = parts; range = range) when interpolatedStringNeedsPrintf parts ->
                    found.Add range
                | SynExpr.Ident ident when printfFunctions.Contains ident.idText -> found.Add ident.idRange
                | SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)) ->
                    match List.tryLast ids with
                    | Some ident when printfFunctions.Contains ident.idText -> found.Add ident.idRange
                    | _ -> ()
                | _ -> ()
        }

    walkAst walker parsedInput

    found
    |> Seq.distinct
    |> Seq.map (fun (range: range) ->
        {
            Type = Name
            Message =
                "This goes through printf, and printf needs runtime code generation, which a Native AOT build of the tool does not have. Use concatenation, `ToString`, or an interpolated string whose holes are bare or use only `%s`, `%c`, `%d`, `%i` or `%M`."
            Code = Code
            Severity = Severity.Error
            Range = range
            Fixes = []
        }
    )
    |> Seq.toList

let cliAnalyzer (ctx: CliContext) : Async<Message list> =
    async { return analyze ctx.FileName ctx.ParseFileResults.ParseTree }

let editorAnalyzer (ctx: EditorContext) : Async<Message list> =
    async { return analyze ctx.FileName ctx.ParseFileResults.ParseTree }
