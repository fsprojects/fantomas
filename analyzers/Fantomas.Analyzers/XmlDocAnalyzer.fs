module Fantomas.Analyzers.XmlDocAnalyzer

open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.ASTCollecting
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Compiler.Xml
open Fantomas.Analyzers.Common

[<Literal>]
let Code: string = "FANTOMAS-XMLDOC-001"

[<Literal>]
let Name: string = "XmlDocAnalyzer"

[<Literal>]
let ShortDescription: string =
    "Detects a documentation comment in an implementation file that has a signature file, where the signature is the copy readers and tooling see."

[<Literal>]
let HelpUri: string =
    "https://github.com/fsprojects/fantomas/blob/main/analyzers/AGENTS.md#fantomas-xmldoc-001"

// Documentation comments in a file whose signature file is the one readers and tooling see.
//
// The untyped tree hangs a `PreXmlDoc` off every declaration that can carry one, so this needs no
// trivia and no second parse. What it cannot tell on its own is whether the signature documents
// the same binding, so a private helper that appears in neither is reported too. That is the
// looser of the two readings of the rule, and the one to revisit if it proves noisy.
let analyze (fileName: string) (sourceFiles: string list) (parsedInput: ParsedInput) : Message list =
    if not (hasSignatureFile fileName sourceFiles) then
        []
    else

        let ranges: ResizeArray<range> = ResizeArray<range>()

        let collect (doc: PreXmlDoc) : unit =
            if not doc.IsEmpty then
                ranges.Add doc.Range

        let walker: SyntaxCollectorBase =
            { new SyntaxCollectorBase() with
                override _.WalkBinding(_path: SyntaxVisitorPath, binding: SynBinding) : unit =
                    match binding with
                    | SynBinding(xmlDoc = doc) -> collect doc

                override _.WalkComponentInfo(_path: SyntaxVisitorPath, info: SynComponentInfo) : unit =
                    match info with
                    | SynComponentInfo(xmlDoc = doc) -> collect doc

                override _.WalkUnionCase(_path: SyntaxVisitorPath, unionCase: SynUnionCase) : unit =
                    match unionCase with
                    | SynUnionCase(xmlDoc = doc) -> collect doc

                override _.WalkEnumCase(_path: SyntaxVisitorPath, enumCase: SynEnumCase) : unit =
                    match enumCase with
                    | SynEnumCase(xmlDoc = doc) -> collect doc

                override _.WalkField(_path: SyntaxVisitorPath, field: SynField) : unit =
                    match field with
                    | SynField(xmlDoc = doc) -> collect doc }

        walkAst walker parsedInput

        ranges
        |> Seq.map (fun (doc: range) ->
            { Type = Name
              Message =
                "Move this documentation comment to the signature file. Keeping a copy in both is a second one to keep in step, and the signature is the one readers and tooling see."
              Code = Code
              Severity = Severity.Warning
              Range = doc
              Fixes = [] })
        |> Seq.toList

let cliAnalyzer (ctx: CliContext) : Async<Message list> =
    async { return analyze ctx.FileName ctx.ProjectOptions.SourceFiles ctx.ParseFileResults.ParseTree }

let editorAnalyzer (ctx: EditorContext) : Async<Message list> =
    async { return analyze ctx.FileName ctx.ProjectOptions.SourceFiles ctx.ParseFileResults.ParseTree }
