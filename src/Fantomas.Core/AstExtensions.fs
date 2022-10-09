module internal Fantomas.Core.AstExtensions

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
        | SynExprRecordField ((fieldName, _), _, Some expr, _) -> unionRanges fieldName.FullRange expr.Range
        | SynExprRecordField ((fieldName, _), Some equalsRange, _, _) -> unionRanges fieldName.FullRange equalsRange
        | SynExprRecordField ((fieldName, _), None, _, _) -> fieldName.FullRange

type SynModuleOrNamespace with

    member this.FullRange: range =
        match this with
        | SynModuleOrNamespace (kind = SynModuleOrNamespaceKind.AnonModule; decls = decls) ->
            match List.tryHead decls, List.tryLast decls with
            | None, None -> Range.Zero
            | Some d, None
            | None, Some d -> d.Range
            | Some s, Some e -> unionRanges s.Range e.Range

        | _ -> this.Range

type SynModuleOrNamespaceSig with

    member this.FullRange: range =
        match this with
        | SynModuleOrNamespaceSig (kind = SynModuleOrNamespaceKind.AnonModule; decls = decls) ->
            match List.tryHead decls, List.tryLast decls with
            | None, None -> Range.Zero
            | Some d, None
            | None, Some d -> d.Range
            | Some s, Some e -> unionRanges s.Range e.Range

        | _ -> this.Range

type CommentTrivia with

    member this.Range =
        match this with
        | CommentTrivia.LineComment (range = r)
        | CommentTrivia.BlockComment (range = r) -> r

type ConditionalDirectiveTrivia with

    member this.Range =
        match this with
        | ConditionalDirectiveTrivia.If (range = range)
        | ConditionalDirectiveTrivia.Else (range = range)
        | ConditionalDirectiveTrivia.EndIf (range = range) -> range

let includeTrivia
    (baseRange: range)
    (comments: CommentTrivia list)
    (conditionDirectives: ConditionalDirectiveTrivia list)
    : range =
    let ranges =
        [ yield! List.map (fun (c: CommentTrivia) -> c.Range) comments
          yield! List.map (fun (c: ConditionalDirectiveTrivia) -> c.Range) conditionDirectives ]

    (baseRange, ranges)
    ||> List.fold (fun acc triviaRange ->
        if acc.StartLine < triviaRange.StartLine && acc.EndLine > triviaRange.EndLine then
            acc
        elif triviaRange.EndLine > acc.EndLine then
            unionRanges acc triviaRange
        else
            unionRanges triviaRange acc)

type ParsedInput with

    member this.FullRange: range =
        match this with
        | ParsedInput.ImplFile (ParsedImplFileInput (hashDirectives = directives; modules = modules; trivia = trivia)) ->
            let startPos =
                match directives with
                | ParsedHashDirective (range = r) :: _ -> r.Start
                | [] ->
                    match modules with
                    | m :: _ -> m.FullRange.Start
                    | _ -> Range.Zero.Start

            let endPos =
                match List.tryLast modules with
                | None ->
                    match List.tryLast directives with
                    | None -> Range.Zero.End
                    | Some (ParsedHashDirective (range = r)) -> r.End
                | Some lastModule -> lastModule.FullRange.End

            let astRange = mkRange this.Range.FileName startPos endPos
            includeTrivia astRange trivia.CodeComments trivia.ConditionalDirectives

        | ParsedInput.SigFile (ParsedSigFileInput (hashDirectives = directives; modules = modules; trivia = trivia)) ->
            let startPos =
                match directives with
                | ParsedHashDirective (range = r) :: _ -> r.Start
                | [] ->
                    match modules with
                    | m :: _ -> m.FullRange.Start
                    | _ -> Range.Zero.Start

            let endPos =
                match List.tryLast modules with
                | None ->
                    match List.tryLast directives with
                    | None -> Range.Zero.End
                    | Some (ParsedHashDirective (range = r)) -> r.End
                | Some lastModule -> lastModule.FullRange.End

            let astRange = mkRange this.Range.FileName startPos endPos
            includeTrivia astRange trivia.CodeComments trivia.ConditionalDirectives

type SynMemberFlags with

    member memberFlags.FullRange: range option =
        RangeHelpers.mergeRanges
            [ yield! Option.toList memberFlags.Trivia.AbstractRange
              yield! Option.toList memberFlags.Trivia.DefaultRange
              yield! Option.toList memberFlags.Trivia.MemberRange
              yield! Option.toList memberFlags.Trivia.OverrideRange
              yield! Option.toList memberFlags.Trivia.StaticRange ]

type SynValInfo with

    member synValInfo.FullRange: range option =
        match synValInfo with
        | SynValInfo (returnInfo = ri) ->
            match List.tryHead ri.Attributes, ri.Ident with
            | None, None -> None
            | Some a, None -> Some a.Range
            | None, Some i -> Some i.idRange
            | Some a, Some i -> Some(unionRanges a.Range i.idRange)

type SynValData with

    member synValData.FullRange: range option =
        match synValData with
        | SynValData (mf, valInfo, thisIdOpt) ->
            let mfRange = Option.bind (fun (mf: SynMemberFlags) -> mf.FullRange) mf
            let valInfo = valInfo.FullRange
            let thisRange = Option.map (fun (id: Ident) -> id.idRange) thisIdOpt

            RangeHelpers.mergeRanges
                [ yield! Option.toList mfRange
                  yield! Option.toList valInfo
                  yield! Option.toList thisRange ]

let synTypeDefnKindDelegateFullRange (signature: SynType) (signatureInfo: SynValInfo) =
    let startRange = signature.Range

    let endRange =
        match signatureInfo.FullRange with
        | Some r -> r
        | None -> signature.Range

    unionRanges startRange endRange

type SynArgInfo with

    member this.FullRange: range option =
        let (SynArgInfo (attrs, _, ident)) = this
        let attrRange = attrs |> List.map (fun a -> a.Range) |> RangeHelpers.mergeRanges
        let identRange = Option.map (fun (i: Ident) -> i.idRange) ident

        match attrRange, identRange with
        | None, None -> None
        | Some a, Some i -> Some(unionRanges a i)
        | None, Some r
        | Some r, None -> Some r

type SynInterpolatedStringPart with

    member this.FullRange =
        match this with
        | SynInterpolatedStringPart.String (_, r) -> r
        | SynInterpolatedStringPart.FillExpr (expr, ident) ->
            match ident with
            | None -> expr.Range
            | Some i -> unionRanges expr.Range i.idRange

type SynTyparDecl with

    member std.FullRange: range =
        let (SynTyparDecl (attrs, synTypar)) = std
        let attrRange = List.map (fun (a: SynAttributeList) -> a.Range) attrs

        match RangeHelpers.mergeRanges attrRange with
        | None -> synTypar.Range
        | Some ar -> unionRanges ar synTypar.Range

// TODO: made fix this one over at the compiler side
type SynField with

    member sf.FullRange: range =
        let (SynField (attributes = attrs; range = r)) = sf

        match attrs with
        | [] -> r
        | head :: _ -> unionRanges head.Range r

type SynAttribute with

    member this.FullRange: range = unionRanges this.Range this.ArgExpr.Range
