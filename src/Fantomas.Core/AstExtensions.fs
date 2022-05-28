module Fantomas.Core.AstExtensions

open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Text
open FSharp.Compiler.Text.Range
open FSharp.Compiler.Syntax

// LongIdent is a bit of an artificial AST node
// meant to be used as namespace or module identifier
let longIdentFullRange (li: LongIdent) : Range =
    match li with
    | [] -> range.Zero
    | h :: _ -> unionRanges h.idRange (List.last li).idRange

type SynIdent with
    member this.FullRange: range =
        match this with
        | SynIdent (ident, None) -> ident.idRange
        | SynIdent (ident, Some trivia) ->
            match trivia with
            | IdentTrivia.OriginalNotationWithParen (leftParenRange, _, rightParenRange)
            | IdentTrivia.HasParenthesis (leftParenRange, rightParenRange) -> unionRanges leftParenRange rightParenRange
            | IdentTrivia.OriginalNotation _ -> ident.idRange

type SynLongIdent with
    member this.FullRange: range =
        match this.IdentsWithTrivia with
        | [] -> Range.Zero
        | [ single ] -> single.FullRange
        | head :: tail ->
            List.fold (fun acc (synIdent: SynIdent) -> unionRanges acc synIdent.FullRange) head.FullRange tail

type SynExprRecordField with
    member this.FullRange: range =
        match this with
        | SynExprRecordField ((fieldName, _), Some equalsRange, _, _) -> unionRanges fieldName.FullRange equalsRange
        | SynExprRecordField ((fieldName, _), None, _, _) -> fieldName.FullRange

// TODO: construct actual range of  file, from first to last content
type ParsedInput with
    member this.FullRange: range =
        match this with
        | ParsedInput.ImplFile (ParsedImplFileInput _)
        | ParsedInput.SigFile (ParsedSigFileInput _) -> Range.Zero
