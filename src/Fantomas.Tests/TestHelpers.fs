module Fantomas.Tests.TestHelper

open FsUnit

open System
open Fantomas.FormatConfig
open Fantomas
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.ErrorLogger
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range

let config = FormatConfig.Default
let newline = "\n"

let parsingOptions fileName = 
    { FSharpParsingOptions.Default with 
        SourceFiles = [| fileName |]
        ConditionalCompilationDefines = ["DEBUG";"TRACE";"SILVERLIGHT"]
        IsInteractive = true }

let sharedChecker = lazy(FSharpChecker.Create())

let formatSourceString isFsiFile (s : string) config = 
    // On Linux/Mac this will exercise different line endings
    let s = s.Replace("\r\n", Environment.NewLine)
    let fileName = if isFsiFile then "/src.fsi" else "/src.fsx"
    CodeFormatter.FormatDocumentAsync(fileName, s, config, parsingOptions fileName, sharedChecker.Value)
    |> Async.RunSynchronously
    |> fun s -> s.Replace("\r\n", "\n")

let formatSelectionFromString isFsiFile r (s : string) config = 
    let s = s.Replace("\r\n", Environment.NewLine)
    let fileName = if isFsiFile then "/tmp.fsi" else "/tmp.fsx"
    CodeFormatter.FormatSelectionInDocumentAsync(fileName, r, s, config, parsingOptions fileName, sharedChecker.Value)
    |> Async.RunSynchronously
    |> fun s -> s.Replace("\r\n", "\n")

let formatSelectionOnly isFsiFile r (s : string) config = 
    let s = s.Replace("\r\n", Environment.NewLine)
    let fileName = if isFsiFile then "/tmp.fsi" else "/tmp.fsx"
    CodeFormatter.FormatSelectionAsync(fileName, r, s, config, parsingOptions fileName, sharedChecker.Value)
    |> Async.RunSynchronously
    |> fun s -> s.Replace("\r\n", "\n")

let formatAroundCursor isFsiFile p (s : string) config = 
    let s = s.Replace("\r\n", Environment.NewLine)
    let fileName = if isFsiFile then "/tmp.fsi" else "/tmp.fsx"
    CodeFormatter.FormatAroundCursorAsync(fileName, p, s, config, parsingOptions fileName, sharedChecker.Value)
    |> Async.RunSynchronously
    |> fun s -> s.Replace("\r\n", "\n")

let isValidFSharpCode isFsiFile s =
    let fileName = if isFsiFile then "/tmp.fsi" else "/tmp.fsx"
    CodeFormatter.IsValidFSharpCodeAsync(fileName, s, parsingOptions fileName, sharedChecker.Value)
    |> Async.RunSynchronously

let parse isFsiFile s =
    let fileName = if isFsiFile then "/tmp.fsi" else "/tmp.fsx"
    CodeFormatter.ParseAsync(fileName, s, parsingOptions fileName, sharedChecker.Value)
    |> Async.RunSynchronously

let formatAST a s c =
    CodeFormatter.FormatAST(a, "/tmp.fsx",s, c)

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
    let context = Fantomas.FormatConfig.Context.create config normalizedSourceCode
    printfn "directives:"
    context.Directives
    |> Seq.iter (fun kv -> printfn "%A %s" kv.Key kv.Value)
    printfn "context: %A" context

let zero = range.Zero   
 
type Input = Input of string
  
let toSynExprs (Input s) =
    match (try Some (parse false s) with _ -> None) with
    | Some 
      (ParsedInput.ImplFile
        (ParsedImplFileInput
            ("/tmp.fsx", _,
            QualifiedNameOfFile _, [], [],
            [SynModuleOrNamespace
                (_, false, true, exprs, _, _, _, _)], _))) -> 
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
                   ([ident], false, true,
                    [SynModuleDecl.DoExpr(NoSequencePointAtDoBinding, expr, zero)], PreXmlDocEmpty, [], None,
                    zero)], (true, true)))
    Input (tryFormatAST ast None formatConfig)
