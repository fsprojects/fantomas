module Fantomas.Analyzers.Common

open System
open System.IO
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Text

let triviaOf (parsedInput: ParsedInput) : range list * range list =
    let comments, directives =
        match parsedInput with
        | ParsedInput.SigFile(ParsedSigFileInput(trivia = trivia)) -> trivia.CodeComments, trivia.ConditionalDirectives
        | ParsedInput.ImplFile(ParsedImplFileInput(trivia = trivia)) ->
            trivia.CodeComments, trivia.ConditionalDirectives

    let commentRanges: range list =
        comments
        |> List.map (fun (comment: CommentTrivia) ->
            match comment with
            | CommentTrivia.LineComment range -> range
            | CommentTrivia.BlockComment range -> range
        )

    let directiveRanges: range list =
        directives
        |> List.map (fun (directive: ConditionalDirectiveTrivia) ->
            match directive with
            | ConditionalDirectiveTrivia.Else range -> range
            | ConditionalDirectiveTrivia.EndIf range -> range
            | ConditionalDirectiveTrivia.If(range = range) -> range
        )

    commentRanges, directiveRanges

let hasSignatureFile (fileName: string) (sourceFiles: string list) : bool =
    let normalize (path: string) : string =
        Path.GetFullPath(path).Replace('\\', '/')

    if not (fileName.EndsWith(".fs", StringComparison.OrdinalIgnoreCase)) then
        false
    else
        let signatureFile: string = normalize (fileName + "i")

        sourceFiles
        |> List.exists (fun (source: string) ->
            String.Equals(normalize source, signatureFile, StringComparison.OrdinalIgnoreCase)
        )

let testAttributes: Set<string> =
    set
        [
            "Test"
            "TestCase"
            "TestCaseSource"
            "Theory"
            "Property"
            "SetUp"
            "TearDown"
            "OneTimeSetUp"
            "OneTimeTearDown"
            "Explicit"
            "Ignore"
        ]

let isTest (attributes: SynAttributes) : bool =
    attributes
    |> List.exists (fun (attributeList: SynAttributeList) ->
        attributeList.Attributes
        |> List.exists (fun (attribute: SynAttribute) ->
            match List.tryLast attribute.TypeName.LongIdent with
            | None -> false
            | Some name ->
                let bare: string =
                    if name.idText.EndsWith("Attribute", StringComparison.Ordinal) then
                        name.idText.Substring(0, name.idText.Length - "Attribute".Length)
                    else
                        name.idText

                testAttributes.Contains bare
        )
    )
