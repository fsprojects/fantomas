module Fantomas.Tests.TestHelper

open System
open System.IO
open NUnit.Framework
open FsCheck
open FsUnit
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open FSharp.Compiler.Xml
open Fantomas.FormatConfig
open Fantomas

[<assembly: Parallelizable(ParallelScope.All)>]
do ()

let config = FormatConfig.Default
let newline = "\n"

let private safeToIgnoreWarnings =
    [ "This construct is deprecated: it is only for use in the F# library"
      "Identifiers containing '@' are reserved for use in F# code generation" ]

let private isValidAndHasNoWarnings fileName source =
    let allDefineOptions, _ = [], [] //TokenParser.getDefines source

    allDefineOptions
    |> List.map (fun conditionalCompilationDefines ->
        async {

            // Run the first phase (untyped parsing) of the compiler
            let sourceText = SourceText.ofString source

            let! untypedRes = CodeFormatter.ParseAsync(fileName, source)
            //sharedChecker.Value.ParseFile(fileName, sourceText, parsingOptionsWithDefines)

            //            let errors =
//                untypedRes.Diagnostics
//                |> Array.filter (fun e -> not (List.contains e.Message safeToIgnoreWarnings))
            // FSharpErrorInfo contains both Errors and Warnings
            // https://fsharp.github.io/FSharp.Compiler.Service/reference/fsharp-compiler-sourcecodeservices-fsharperrorinfo.html
            return Array.isEmpty [||] // errors
        })
    |> Async.Parallel
    |> Async.map (Seq.fold (&&) true)

let formatSourceString isFsiFile (s: string) config =
    async {
        let! formatted = CodeFormatter.FormatDocumentAsync(isFsiFile, s, config)

        let! isValid = isValidAndHasNoWarnings isFsiFile formatted

        if not isValid then
            failwithf "The formatted result is not valid F# code or contains warnings\n%s" formatted

        return formatted.Replace("\r\n", "\n")
    }

    |> Async.RunSynchronously

let formatSourceStringWithDefines defines (s: string) config =
    // On Linux/Mac this will exercise different line endings
    let s = s.Replace("\r\n", Environment.NewLine)
    let fileName = "/src.fsx"

    let result =
        async {
            let source = CodeFormatterImpl.getSourceText s
            let! asts = CodeFormatterImpl.parse false source

            let ast, hashTokens =
                Array.filter (fun (_, d, _) -> d = defines) asts
                |> Array.head
                |> fun (ast, _, hashTokens) -> ast, hashTokens

            return CodeFormatterImpl.formatWith ast defines hashTokens (Some source) config
        }
        |> Async.RunSynchronously

    // merge with itself to make #if go on beginning of line
    let _, fragments =
        String.splitInFragments config.EndOfLine.NewLineString [ (defines, result) ]
        |> List.head

    String.merge fragments fragments
    |> String.concat config.EndOfLine.NewLineString
    |> String.normalizeNewLine

let formatSelectionOnly isFsiFile r (s: string) config = failwith "no longer supported"
//    let s = s.Replace("\r\n", Environment.NewLine)
//
//    let fileName =
//        if isFsiFile then
//            "/tmp.fsi"
//        else
//            "/tmp.fsx"
//
//    CodeFormatter.FormatSelectionAsync(
//        fileName,
//        r,
//        SourceOrigin.SourceString s,
//        config,
//        CodeFormatterImpl.createParsingOptionsFromFile fileName,
//        sharedChecker.Value
//    )
//    |> Async.RunSynchronously
//    |> fun s -> s.Replace("\r\n", "\n")

let isValidFSharpCode isFsiFile s =
    CodeFormatter.IsValidFSharpCodeAsync(isFsiFile, s)
    |> Async.RunSynchronously

let parse isFsiFile s =
    CodeFormatterImpl.getSourceText s
    |> CodeFormatterImpl.parse isFsiFile
    |> Async.RunSynchronously

let formatAST a s c =
    CodeFormatter.FormatASTAsync(a, [], s, c)
    |> Async.RunSynchronously

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
        with
        | _ -> None

    match ast with
    | Some [| (ParsedInput.ImplFile (ParsedImplFileInput ("/tmp.fsx",
                                                          _,
                                                          QualifiedNameOfFile _,
                                                          [],
                                                          [],
                                                          [ SynModuleOrNamespace (_,
                                                                                  false,
                                                                                  SynModuleOrNamespaceKind.AnonModule,
                                                                                  exprs,
                                                                                  _,
                                                                                  _,
                                                                                  _,
                                                                                  _) ],
                                                          _))) |] ->
        List.choose
            (function
            | SynModuleDecl.DoExpr (_, expr, _) -> Some expr
            | _ -> None)
            exprs
    | _ -> []

let tryFormatAST ast sourceCode config =
    try
        formatAST ast sourceCode config
    with
    | _ -> ""

let formatConfig = { FormatConfig.Default with StrictMode = true }

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
                      SynModuleOrNamespaceKind.AnonModule,
                      [ SynModuleDecl.DoExpr(DebugPointAtBinding.NoneAtDo, expr, zero) ],
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
    let filename = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString() + ".fs")

    do File.WriteAllText(filename, codeSnippet)

    member __.Filename: string = filename

    interface IDisposable with
        member this.Dispose() : unit = File.Delete(filename)
