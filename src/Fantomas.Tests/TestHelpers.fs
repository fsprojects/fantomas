module Fantomas.Tests.TestHelper

open FsUnit
open System
open Fantomas.FormatConfig
open Fantomas
open Fantomas.Extras
open FSharp.Compiler.Text
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.SyntaxTree
open FSharp.Compiler.XmlDoc
open NUnit.Framework
open FsCheck
open System.IO

[<assembly: Parallelizable(ParallelScope.All)>]
do ()

let config = FormatConfig.Default
let newline = "\n"

let sharedChecker = lazy (FSharpChecker.Create())

let private safeToIgnoreWarnings =
    [ "This construct is deprecated: it is only for use in the F# library"
      "Identifiers containing '@' are reserved for use in F# code generation" ]

let private isValidAndHasNoWarnings fileName source parsingOptions =
    let allDefineOptions, _ = TokenParser.getDefines source

    allDefineOptions
    |> List.map
        (fun conditionalCompilationDefines ->
            async {
                let parsingOptionsWithDefines =
                    { parsingOptions with
                          ConditionalCompilationDefines = conditionalCompilationDefines }
                // Run the first phase (untyped parsing) of the compiler
                let sourceText = SourceText.ofString source

                let! untypedRes = sharedChecker.Value.ParseFile(fileName, sourceText, parsingOptionsWithDefines)

                let errors =
                    untypedRes.Errors
                    |> Array.filter (fun e -> not (List.contains e.Message safeToIgnoreWarnings))
                // FSharpErrorInfo contains both Errors and Warnings
                // https://fsharp.github.io/FSharp.Compiler.Service/reference/fsharp-compiler-sourcecodeservices-fsharperrorinfo.html
                return Array.isEmpty errors
            })
    |> Async.Parallel
    |> Async.map (Seq.fold (&&) true)

let formatSourceString isFsiFile (s: string) config =
    // On Linux/Mac this will exercise different line endings
    let s = s.Replace("\r\n", Environment.NewLine)

    let fileName =
        if isFsiFile then
            "/src.fsi"
        else
            "/src.fsx"

    let parsingOptions =
        FakeHelpers.createParsingOptionsFromFile fileName

    async {
        let! formatted =
            CodeFormatter.FormatDocumentAsync(
                fileName,
                SourceOrigin.SourceString s,
                config,
                parsingOptions,
                sharedChecker.Value
            )

        let! isValid = isValidAndHasNoWarnings fileName formatted parsingOptions

        if not isValid then
            failwithf "The formatted result is not valid F# code or contains warnings\n%s" formatted

        return formatted.Replace("\r\n", "\n")
    }

    |> Async.RunSynchronously

let formatSourceStringWithDefines defines (s: string) config =
    // On Linux/Mac this will exercise different line endings
    let s = s.Replace("\r\n", Environment.NewLine)
    let fileName = "/src.fsx"

    let formatContext =
        CodeFormatterImpl.createFormatContext fileName (SourceOrigin.SourceString s)

    let parsingOptions =
        FakeHelpers.createParsingOptionsFromFile fileName
        |> fun p ->
            { p with
                  ConditionalCompilationDefines = defines }

    let result =
        async {
            let! asts = CodeFormatterImpl.parse sharedChecker.Value parsingOptions formatContext

            let ast, hashTokens =
                Array.filter (fun (_, d, _) -> d = defines) asts
                |> Array.head
                |> fun (ast, _, hashTokens) -> ast, hashTokens

            return CodeFormatterImpl.formatWith ast defines hashTokens formatContext config
        }
        |> Async.RunSynchronously

    // merge with itself to make #if go on beginning of line
    String.merge result result
    |> String.normalizeNewLine

let formatSelectionOnly isFsiFile r (s: string) config =
    let s = s.Replace("\r\n", Environment.NewLine)

    let fileName =
        if isFsiFile then
            "/tmp.fsi"
        else
            "/tmp.fsx"

    CodeFormatter.FormatSelectionAsync(
        fileName,
        r,
        SourceOrigin.SourceString s,
        config,
        FakeHelpers.createParsingOptionsFromFile fileName,
        sharedChecker.Value
    )
    |> Async.RunSynchronously
    |> fun s -> s.Replace("\r\n", "\n")

let isValidFSharpCode isFsiFile s =
    let fileName =
        if isFsiFile then
            "/tmp.fsi"
        else
            "/tmp.fsx"

    CodeFormatter.IsValidFSharpCodeAsync(
        fileName,
        SourceOrigin.SourceString s,
        FakeHelpers.createParsingOptionsFromFile fileName,
        sharedChecker.Value
    )
    |> Async.RunSynchronously

let parse isFsiFile s =
    let fileName =
        if isFsiFile then
            "/tmp.fsi"
        else
            "/tmp.fsx"

    CodeFormatterImpl.createFormatContext fileName (SourceOrigin.SourceString s)
    |> CodeFormatterImpl.parse sharedChecker.Value (FakeHelpers.createParsingOptionsFromFile fileName)
    |> Async.RunSynchronously

let formatAST a s c =
    CodeFormatter.FormatASTAsync(a, "/tmp.fsx", [], s, c)
    |> Async.RunSynchronously

let makeRange l1 c1 l2 c2 =
    CodeFormatter.MakeRange("/tmp.fsx", l1, c1, l2, c2)

let makePos l1 c1 = CodeFormatter.MakePos(l1, c1)

let equal x =
    let x =
        match box x with
        | :? String as s -> s.Replace("\r\n", "\n") |> box
        | x -> x

    equal x

let inline prepend s content = s + content
let inline append s content = content + s

let printAST isFsiFile sourceCode =
    let ast = parse isFsiFile sourceCode
    printfn "AST:"
    printfn "%A" ast

let zero = range.Zero

type Input = Input of string

let toSynExprs (Input s) =
    let ast =
        try
            parse false s
            |> Array.map (fun (ast, _, _) -> ast)
            |> Some
        with _ -> None

    match ast with
    | Some [| (ParsedInput.ImplFile (ParsedImplFileInput ("/tmp.fsx",
                                                          _,
                                                          QualifiedNameOfFile _,
                                                          [],
                                                          [],
                                                          [ SynModuleOrNamespace (_,
                                                                                  false,
                                                                                  AnonModule,
                                                                                  exprs,
                                                                                  _,
                                                                                  _,
                                                                                  _,
                                                                                  _) ],
                                                          _))) |] ->
        List.choose
            (function
            | (SynModuleDecl.DoExpr (_, expr, _)) -> Some expr
            | _ -> None)
            exprs
    | _ -> []

let tryFormatAST ast sourceCode config =
    try
        formatAST ast sourceCode config
    with _ -> ""

let formatConfig =
    { FormatConfig.Default with
          StrictMode = true }

// Regenerate inputs from expression ASTs
// Might suffer from bugs in formatting phase
let fromSynExpr expr =
    let ast =
        let ident = Ident("Tmp", zero)

        ParsedInput.ImplFile(
            ParsedImplFileInput(
                "/tmp.fsx",
                true,
                QualifiedNameOfFile ident,
                [],
                [],
                [ SynModuleOrNamespace(
                      [ ident ],
                      false,
                      AnonModule,
                      [ SynModuleDecl.DoExpr(NoDebugPointAtDoBinding, expr, zero) ],
                      PreXmlDoc.Empty,
                      [],
                      None,
                      zero
                  ) ],
                (true, true)
            )
        )

    Input(tryFormatAST ast None formatConfig)

let shouldNotChangeAfterFormat source =
    formatSourceString false source config
    |> prepend newline
    |> should equal source

let (==) actual expected = Assert.AreEqual(expected, actual)
let fail () = Assert.Fail()
let pass () = Assert.Pass()


/// An FsCheck runner which reports FsCheck test results to NUnit.
type NUnitRunner() =
    interface IRunner with
        member __.OnStartFixture _ = ()

        member __.OnArguments(_ntest, _args, _every) =
            //stdout.Write(every ntest args)
            ()

        member __.OnShrink(_args, _everyShrink) =
            //stdout.Write(everyShrink args)
            ()

        member __.OnFinished(name, result) =
            match result with
            | TestResult.True (_data, _) ->
                // TODO : Log the result data.
                Runner.onFinishedToString name result
                |> stdout.WriteLine

            | TestResult.Exhausted _data ->
                // TODO : Log the result data.
                Runner.onFinishedToString name result
                |> Assert.Inconclusive

            | TestResult.False _ ->
                // TODO : Log more information about the test failure.
                Runner.onFinishedToString name result
                |> Assert.Fail

let private getTempFolder () = Path.GetTempPath()

let private mkConfigPath fileName folder =
    match folder with
    | Some folder ->
        let folderPath = Path.Combine(getTempFolder (), folder)
        Directory.CreateDirectory(folderPath) |> ignore
        Path.Combine(folderPath, fileName)
    | None -> Path.Combine(getTempFolder (), fileName)

let mkConfigFromContent fileName folder content =
    let file = mkConfigPath fileName folder
    File.WriteAllText(file, content)
    file

type TemporaryFileCodeSample internal (codeSnippet: string) =
    let filename =
        Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString() + ".fs")

    do File.WriteAllText(filename, codeSnippet)

    member __.Filename : string = filename

    interface IDisposable with
        member this.Dispose() : unit = File.Delete(filename)
