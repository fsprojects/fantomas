module Fantomas.Core.Parse

open System
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

let private checker =
    FSharpChecker.Create(captureIdentifiersWhenParsing = false, useSyntaxTreeCache = false)

let parseFile
    (isSignature: bool)
    (sourceText: ISourceText)
    (defines: string list)
    : ParsedInput * FSharpDiagnostic list =
    let fileName =
        sprintf "%s.%s" (Guid.NewGuid().ToString("N")) (if isSignature then "fsi" else "fs")

    let parsingOptions =
        { FSharpParsingOptions.Default with
            SourceFiles = [| fileName |]
            IndentationAwareSyntax = Some true
            ConditionalDefines = defines
            LangVersionText = "preview"
            IsExe = true
            OnlyUseSpecifiedDefines = true }

    let parseResult =
        checker.ParseFile(fileName, sourceText, parsingOptions, false)
        |> Async.RunSynchronously

    parseResult.ParseTree, List.ofArray parseResult.Diagnostics
