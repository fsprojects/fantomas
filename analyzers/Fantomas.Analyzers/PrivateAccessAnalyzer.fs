module Fantomas.Analyzers.PrivateAccessAnalyzer

open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.ASTCollecting
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Text
open Fantomas.Analyzers.Common

[<Literal>]
let Code: string = "FANTOMAS-PRIVATE-001"

[<Literal>]
let Name: string = "PrivateAccessAnalyzer"

[<Literal>]
let ShortDescription: string =
    "Detects a private let binding in an implementation file that has a signature file, where the signature file is already the visibility boundary."

[<Literal>]
let HelpUri: string =
    "https://github.com/fsprojects/fantomas/blob/main/analyzers/AGENTS.md#fantomas-private-001"

// Where the accessibility of a binding lives. It is not on the `SynBinding` itself but on the
// pattern that heads it, in one of two places depending on whether the binding takes arguments.
let accessibilityOf (headPattern: SynPat) : SynAccess option =
    match headPattern with
    | SynPat.Named(accessibility = accessibility) -> accessibility
    | SynPat.LongIdent(accessibility = accessibility) -> accessibility
    | _ -> None

// Whether a binding is a `let`, as opposed to a member, a `val` or anything else that also parses
// to a `SynBinding`. The leading keyword is what separates them, and the rule is about `let`.
let isLetBinding (keyword: SynLeadingKeyword) : bool =
    match keyword with
    | SynLeadingKeyword.Let _
    | SynLeadingKeyword.LetRec _
    | SynLeadingKeyword.And _
    | SynLeadingKeyword.StaticLet _
    | SynLeadingKeyword.StaticLetRec _ -> true
    | _ -> false

// Every `let private` in a file whose signature file already hides it, reported on the `private`
// keyword, which is the thing to delete.
let analyze (fileName: string) (sourceFiles: string list) (parsedInput: ParsedInput) : Message list =
    if not (hasSignatureFile fileName sourceFiles) then
        []
    else

        let ranges: ResizeArray<range> = ResizeArray<range>()

        let walker: SyntaxCollectorBase =
            { new SyntaxCollectorBase() with
                override _.WalkBinding(_path: SyntaxVisitorPath, binding: SynBinding) : unit =
                    match binding with
                    | SynBinding(headPat = headPattern; trivia = { LeadingKeyword = keyword }) when isLetBinding keyword ->
                        match accessibilityOf headPattern with
                        | Some(SynAccess.Private range) -> ranges.Add range
                        | _ -> ()
                    | _ -> ()
            }

        walkAst walker parsedInput

        ranges
        |> Seq.map (fun (keyword: range) ->
            {
                Type = Name
                Message =
                    "Remove `private`. The signature file is the visibility boundary, so anything it does not list is already hidden."
                Code = Code
                Severity = Severity.Error
                Range = keyword
                Fixes = []
            })
        |> Seq.toList

let cliAnalyzer (ctx: CliContext) : Async<Message list> =
    async { return analyze ctx.FileName ctx.ProjectOptions.SourceFiles ctx.ParseFileResults.ParseTree }

let editorAnalyzer (ctx: EditorContext) : Async<Message list> =
    async { return analyze ctx.FileName ctx.ProjectOptions.SourceFiles ctx.ParseFileResults.ParseTree }
