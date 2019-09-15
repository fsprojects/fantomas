module Fantomas.Tests.TestHelper

open FsUnit
open System
open Fantomas.FormatConfig
open Fantomas
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Ast
open FSharp.Compiler.Range
open NUnit.Framework
open FsCheck

let config = FormatConfig.Default
let newline = "\n"

let sharedChecker = lazy(FSharpChecker.Create())

let formatSourceString isFsiFile (s : string) config = 
    // On Linux/Mac this will exercise different line endings
    let s = s.Replace("\r\n", Environment.NewLine)
    let fileName = if isFsiFile then "/src.fsi" else "/src.fsx"

    CodeFormatter.FormatDocumentAsync(fileName, SourceOrigin.SourceString s, config, sharedChecker.Value)
    |> Async.RunSynchronously
    |> fun s -> s.Replace("\r\n", "\n")

let formatSourceStringWithDefines defines (s : string) config =
    // On Linux/Mac this will exercise different line endings
    let s = s.Replace("\r\n", Environment.NewLine)
    let fileName = "/src.fsx"
    let formatContext = CodeFormatterImpl.createFormatContext fileName (SourceOrigin.SourceString s) sharedChecker.Value
    let formatContextWithDefines =
        { formatContext with ParsingOptions = { formatContext.ParsingOptions with ConditionalCompilationDefines = defines } }

    let result =
        async {
            let! asts = CodeFormatterImpl.parse formatContextWithDefines
            let ast =
                Array.filter (fun (_,d) -> d = defines) asts
                |> Array.head
                |> fst
            return CodeFormatterImpl.formatWith ast formatContextWithDefines config
        }
        |> Async.RunSynchronously
        |> CodeFormatterImpl.addNewlineIfNeeded

    // merge with itself to make #if go on beginning of line
    String.merge result result
    |> String.normalizeNewLine

let formatSelectionFromString isFsiFile r (s : string) config = 
    let s = s.Replace("\r\n", Environment.NewLine)
    let fileName = if isFsiFile then "/tmp.fsi" else "/tmp.fsx"
    CodeFormatter.FormatSelectionInDocumentAsync(fileName, r, SourceOrigin.SourceString s, config, sharedChecker.Value)
    |> Async.RunSynchronously
    |> fun s -> s.Replace("\r\n", "\n")

let formatSelectionOnly isFsiFile r (s : string) config = 
    let s = s.Replace("\r\n", Environment.NewLine)
    let fileName = if isFsiFile then "/tmp.fsi" else "/tmp.fsx"
    CodeFormatter.FormatSelectionAsync(fileName, r, SourceOrigin.SourceString s, config, sharedChecker.Value)
    |> Async.RunSynchronously
    |> fun s -> s.Replace("\r\n", "\n")

let formatAroundCursor isFsiFile p (s : string) config = 
    let s = s.Replace("\r\n", Environment.NewLine)
    let fileName = if isFsiFile then "/tmp.fsi" else "/tmp.fsx"
    CodeFormatter.FormatAroundCursorAsync(fileName, p, SourceOrigin.SourceString s, config, sharedChecker.Value)
    |> Async.RunSynchronously
    |> fun s -> s.Replace("\r\n", "\n")

let isValidFSharpCode isFsiFile s =
    let fileName = if isFsiFile then "/tmp.fsi" else "/tmp.fsx"
    CodeFormatter.IsValidFSharpCodeAsync(fileName, SourceOrigin.SourceString s, sharedChecker.Value)
    |> Async.RunSynchronously

let parse isFsiFile s =
    let fileName = if isFsiFile then "/tmp.fsi" else "/tmp.fsx"
    CodeFormatter.ParseAsync(fileName, SourceOrigin.SourceString s, sharedChecker.Value)
    |> Async.RunSynchronously

let formatAST a s c =
    CodeFormatter.FormatASTAsync(a, "/tmp.fsx",s, sharedChecker.Value, c)
    |> Async.RunSynchronously

let makeRange l1 c1 l2 c2 = 
    CodeFormatter.MakeRange("/tmp.fsx", l1, c1, l2, c2)

let makePos l1 c1 = 
    CodeFormatter.MakePos(l1, c1)

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
    
let printContext sourceCode =
    let normalizedSourceCode = Fantomas.String.normalizeNewLine sourceCode
    let defines = Fantomas.TokenParser.getDefines sourceCode
    let context = Fantomas.Context.Context.create config defines normalizedSourceCode None
    printfn "context: %A" context

let zero = range.Zero   
 
type Input = Input of string
  
let toSynExprs (Input s) =
    match (try Some (parse false s) with _ -> None) with
    | Some 
      [|(ParsedInput.ImplFile
        (ParsedImplFileInput
            ("/tmp.fsx", _,
            QualifiedNameOfFile _, [], [],
            [SynModuleOrNamespace
                (_, false, AnonModule, exprs, _, _, _, _)], _))), _|] -> 
                List.choose (function (SynModuleDecl.DoExpr(_, expr, _)) -> Some expr | _ -> None) exprs
    | _ -> 
        //stdout.WriteLine("Can't convert {0}.", sprintf "%A" ast)
        []

let tryFormatAST ast sourceCode config =
    try
        formatAST ast sourceCode config
    with _ ->
        ""
        
let formatConfig = { FormatConfig.Default with StrictMode = true }   
             
// Regenerate inputs from expression ASTs
// Might suffer from bugs in formatting phase
let fromSynExpr expr =
    let ast =
        let ident = Ident("Tmp", zero)
        ParsedInput.ImplFile
            (ParsedImplFileInput
               ("/tmp.fsx", true,
                QualifiedNameOfFile ident, [], [],
                [SynModuleOrNamespace
                   ([ident], false, AnonModule,
                    [SynModuleDecl.DoExpr(NoSequencePointAtDoBinding, expr, zero)], PreXmlDocEmpty, [], None,
                    zero)], (true, true)))
    Input (tryFormatAST ast None formatConfig)

let shouldNotChangeAfterFormat source =
    formatSourceString false source config
    |> prepend newline
    |> should equal source
    
let (==) actual expected = Assert.AreEqual(expected, actual)
let fail() = Assert.Fail()
let pass() = Assert.Pass()


/// An FsCheck runner which reports FsCheck test results to NUnit.
type NUnitRunner () =
    interface IRunner with
        member __.OnStartFixture _ = ()
        member __.OnArguments (_ntest, _args, _every) = 
            //stdout.Write(every ntest args)
            ()

        member __.OnShrink(_args, _everyShrink) = 
            //stdout.Write(everyShrink args)
            ()

        member __.OnFinished (name, result) =
            match result with
            | TestResult.True(_data, _) ->
                // TODO : Log the result data.
                Runner.onFinishedToString name result
                |> stdout.WriteLine

            | TestResult.Exhausted _data ->
                // TODO : Log the result data.
                Runner.onFinishedToString name result
                |> Assert.Inconclusive

            | TestResult.False (_,_,_,_,_) ->
                // TODO : Log more information about the test failure.
                Runner.onFinishedToString name result
                |> Assert.Fail
