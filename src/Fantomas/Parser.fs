module Fantomas.Parser

// Open the namespace with InteractiveChecker type
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices

open System
open System.IO

open Fantomas.Converter

/// Parse an fs, fsi or fsx file using compiler services
let parse fileName = 
    let content = File.ReadAllText(fileName)
    // Create an interactive checker instance (ignore notifications)
    let checker = InteractiveChecker.Create(NotifyFileTypeCheckStateIsDirty ignore)
    // Get compiler options for a single script file
    let checkOptions = checker.GetCheckOptionsFromScriptRoot(fileName, content, DateTime.Now, [||])
    // Run the first phase (untyped parsing) of the compiler
    let untypedRes = checker.UntypedParse(fileName, content, checkOptions)
    match untypedRes.ParseTree with
    // Not support sig files at the moment
    | Some (ParsedInput.ImplFile tree) -> 
        let (ParsedImplFileInput(_, _, _, _, _, m, _)) = tree
        // Not support multiple modules now
        let (SynModuleOrNamespace(_, _,decls, _, _, _, _)) = List.head m
        foldDecls decls
    | Some tree -> failwithf "Currently not supported tree %A" tree
    | None -> failwith "parse: Wrong input"
