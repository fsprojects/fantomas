module Fantomas.Tests.TestHelper

open FsUnit
open System
open Fantomas.FormatConfig
open Fantomas
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Ast
open FSharp.Compiler.Range
open NUnit.Framework
open System.Text.RegularExpressions

let config = FormatConfig.Default
let newline = "\n"

let parsingOptions fileName = 
    { FSharpParsingOptions.Default with 
        SourceFiles = [| fileName |]
        ConditionalCompilationDefines = []
        IsInteractive = true }

let sharedChecker = lazy(FSharpChecker.Create())

let formatSourceString isFsiFile (s : string) config = 
    // On Linux/Mac this will exercise different line endings
    let s = s.Replace("\r\n", Environment.NewLine)
    let fileName = if isFsiFile then "/src.fsi" else "/src.fsx"
    let defines = TokenParser.getDefines s
    let parsingOptions = { (parsingOptions fileName) with ConditionalCompilationDefines = defines }
    
    CodeFormatter.FormatDocumentAsync(fileName, s, config, parsingOptions, sharedChecker.Value)
    |> Async.RunSynchronously
    |> fun s -> s.Replace("\r\n", "\n")
    
let hashRegex = @"^\s*#.*"
    
let formatSourceStringX (s : string) config define = 
    // On Linux/Mac this will exercise different line endings
    let s = s.Replace("\r\n", Environment.NewLine)
    let fileName = "/src.fsx"
    let conditionalCompilationDefines = match define with | Some d -> [d] | None -> []
    let parsingOptions = { (parsingOptions fileName) with ConditionalCompilationDefines = conditionalCompilationDefines }
    CodeFormatter.FormatDocumentAsync(fileName, s, config, parsingOptions, sharedChecker.Value)
    |> Async.RunSynchronously
    |> fun s -> s.Replace("\r\n", "\n")
    
let private splitWhenHash (source: string) = 
    source.Split([| Environment.NewLine; "\r\n"; "\n" |], options = StringSplitOptions.None)
    |> Array.fold (fun acc line ->
        if Regex.IsMatch(line, hashRegex) then
            [line]::acc
        else
            acc
            |> List.mapi (fun idx l -> if idx = 0 then (line::l) else l)
    ) [[]]
    |> List.map (List.rev >> String.concat Environment.NewLine)
    |> List.rev

let private merge a b =
    let aChunks = splitWhenHash a
    let bChunks = splitWhenHash b
    List.zip aChunks bChunks
    |> List.map (fun (a', b') ->
        if String.length a' > String.length b' then a' else b'
    )
    |> String.concat Environment.NewLine

let formatSourceStringMultipleTimes (s: string) config =
    let defines = TokenParser.getDefines s
    let results =
        defines
        |> List.map (Some)
        |> List.append [None]
        |> List.map (formatSourceStringX s config)
    match results with
    | [] -> failwith "not possible"
    | [x] -> x
    | all -> List.reduce merge all
    |> fun formatted -> formatted.Replace("\r\n", "\n")

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
    let defines = TokenParser.getDefines s
    let parsingOptions = { (parsingOptions fileName) with ConditionalCompilationDefines = defines }
    CodeFormatter.ParseAsync(fileName, s, parsingOptions, sharedChecker.Value)
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
