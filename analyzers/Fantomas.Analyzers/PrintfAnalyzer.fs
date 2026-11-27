module Fantomas.Analyzers.PrintfAnalyzer

open System
open FSharp.Analyzers.SDK
open FSharp.Compiler.Symbols
open FSharp.Compiler.Symbols.FSharpExprPatterns
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

// A printf format, under whichever abbreviation it was written: `Printf.StringFormat`,
// `Printf.TextWriterFormat` and `Format` all stand for one of the two `PrintfFormat` classes.
let isPrintfFormat (t: FSharpType) : bool =
    let stripped: FSharpType = t.StripAbbreviations()

    stripped.HasTypeDefinition
    && stripped.TypeDefinition.TryFullName
       |> Option.exists (fun (name: string) ->
           name.StartsWith("Microsoft.FSharp.Core.PrintfFormat`", StringComparison.Ordinal)
       )

// Reported on the format being constructed: the string after `sprintf`, or the interpolated
// string itself, which the compiler has already lowered to `String.Concat` where it can, so what
// is left is printf. And on a call into FSharp.Core that is handed a format as a value rather than
// built in place, which is a function of your own forwarding its format to `kprintf` or one of its
// kin: the construction is at every call of that function, and is reported there.
let analyze (fileName: string) (typedTree: FSharpImplementationFileContents option) : Message list =
    match typedTree with
    | None -> []
    | Some _ when not (isToolCode fileName) -> []
    | Some typedTree ->

    let found: ResizeArray<range> = ResizeArray<range>()

    let rec walk (expr: FSharpExpr) : unit =
        match expr with
        | NewObject _ when isPrintfFormat expr.Type -> found.Add expr.Range
        | Call(_, called, _, _, arguments) when
            called.FullName.StartsWith("Microsoft.FSharp.Core.", StringComparison.Ordinal)
            && arguments
               |> List.exists (fun (argument: FSharpExpr) ->
                   match argument with
                   | Value _ -> isPrintfFormat argument.Type
                   | _ -> false
               )
            ->
            found.Add expr.Range
        | _ -> ()

        List.iter walk expr.ImmediateSubExpressions

    let rec walkDeclaration (declaration: FSharpImplementationFileDeclaration) : unit =
        match declaration with
        | FSharpImplementationFileDeclaration.Entity(_, declarations) -> List.iter walkDeclaration declarations
        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(_, _, body) -> walk body
        | FSharpImplementationFileDeclaration.InitAction action -> walk action

    List.iter walkDeclaration typedTree.Declarations

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
    async { return analyze ctx.FileName ctx.TypedTree }

// The editor has a typed tree only once the file is checked, and says nothing until then.
let editorAnalyzer (ctx: EditorContext) : Async<Message list> =
    async { return analyze ctx.FileName ctx.TypedTree }
