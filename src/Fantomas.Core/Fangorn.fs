module rec Fantomas.Core.Fangorn

open System.Collections.Generic
open System.Text.RegularExpressions
open FSharp.Compiler.Text
open FSharp.Compiler.Text.Range
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Xml
open Fantomas.Core.FormatConfig
open Fantomas.Core.ISourceTextExtensions
open Fantomas.Core.RangePatterns
open Fantomas.Core.SyntaxOak
open Microsoft.FSharp.Core

type CreationAide =
    { SourceText: ISourceText option
      Config: FormatConfig }

    member x.TextFromSource fallback range =
        match x.SourceText with
        | None -> fallback
        | Some sourceText -> sourceText.GetContentAt range

let stn text range = SingleTextNode(text, range)

let mkIdent (ident: Ident) =
    let width = ident.idRange.EndColumn - ident.idRange.StartColumn

    let text =
        if ident.idText.Length + 4 = width then
            // add backticks
            $"``{ident.idText}``"
        else
            ident.idText

    stn text ident.idRange

let mkSynIdent (SynIdent (ident, trivia)) =
    match trivia with
    | None -> stn ident.idText ident.idRange
    | Some (IdentTrivia.OriginalNotation text) -> stn text ident.idRange
    | Some (IdentTrivia.OriginalNotationWithParen (_, text, _)) -> stn $"({text})" ident.idRange
    | Some (IdentTrivia.HasParenthesis _) -> stn $"({ident.idText})" ident.idRange

let mkSynLongIdent (sli: SynLongIdent) =
    match sli.IdentsWithTrivia with
    | [] -> IdentListNode.Empty
    | [ single ] -> IdentListNode([ IdentifierOrDot.Ident(mkSynIdent single) ], sli.Range)
    | head :: tail ->
        assert (tail.Length = sli.Dots.Length)

        let rest =
            (sli.Dots, tail)
            ||> List.zip
            |> List.collect (fun (dot, ident) ->
                [ IdentifierOrDot.KnownDot(DotNode(dot))
                  IdentifierOrDot.Ident(mkSynIdent ident) ])

        IdentListNode(IdentifierOrDot.Ident(mkSynIdent head) :: rest, sli.Range)

let mkLongIdent (longIdent: LongIdent) : IdentListNode =
    match longIdent with
    | [] -> IdentListNode.Empty
    | [ single ] -> IdentListNode([ IdentifierOrDot.Ident(stn single.idText single.idRange) ], single.idRange)
    | head :: tail ->
        let rest =
            tail
            |> List.collect (fun ident ->
                [ IdentifierOrDot.UnknownDot
                  IdentifierOrDot.Ident(stn ident.idText ident.idRange) ])

        let range =
            longIdent |> List.map (fun ident -> ident.idRange) |> List.reduce unionRanges

        IdentListNode(IdentifierOrDot.Ident(stn head.idText head.idRange) :: rest, range)

let mkSynAccess (vis: SynAccess option) =
    match vis with
    | None -> None
    | Some (SynAccess.Internal range) -> Some(stn "internal" range)
    | Some (SynAccess.Private range) -> Some(stn "private" range)
    | Some (SynAccess.Public range) -> Some(stn "public" range)

let parseExpressionInSynBinding returnInfo expr =
    match returnInfo, expr with
    | Some (SynBindingReturnInfo (typeName = t1)), SynExpr.Typed (e, t2, _) when RangeHelpers.rangeEq t1.Range t2.Range ->
        e
    | _ -> expr

let mkConstString (creationAide: CreationAide) (stringKind: SynStringKind) (value: string) (range: range) =
    let escaped = Regex.Replace(value, "\"{1}", "\\\"")

    let fallback =
        match stringKind with
        | SynStringKind.Regular -> sprintf "\"%s\"" escaped
        | SynStringKind.Verbatim -> sprintf "@\"%s\"" escaped
        | SynStringKind.TripleQuote -> sprintf "\"\"\"%s\"\"\"" escaped

    stn (creationAide.TextFromSource fallback range) range

let mkParsedHashDirective (creationAide: CreationAide) (ParsedHashDirective (ident, args, range)) =
    let args =
        args
        |> List.map (function
            | ParsedHashDirectiveArgument.String (value, stringKind, range) ->
                mkConstString creationAide stringKind value range
            | ParsedHashDirectiveArgument.SourceIdentifier (identifier, _, range) -> stn identifier range)

    ParsedHashDirectiveNode(ident, args, range)

let mkConstant (creationAide: CreationAide) c r : Constant =
    let orElse fallback =
        stn (creationAide.TextFromSource fallback r) r |> Constant.FromText

    match c with
    | SynConst.Unit ->
        match r with
        | StartEndRange 1 (lpr, _, rpr) -> UnitNode(stn "(" lpr, stn ")" rpr, r) |> Constant.Unit
    | SynConst.Bool b -> stn (if b then "true" else "false") r |> Constant.FromText
    | SynConst.Byte v -> orElse $"%A{v}"
    | SynConst.SByte v -> orElse $"%A{v}"
    | SynConst.Int16 v -> orElse $"%A{v}"
    | SynConst.Int32 v -> orElse $"%A{v}"
    | SynConst.Int64 v -> orElse $"%A{v}"
    | SynConst.UInt16 v -> orElse $"%A{v}"
    | SynConst.UInt16s v -> orElse $"%A{v}"
    | SynConst.UInt32 v -> orElse $"%A{v}"
    | SynConst.UInt64 v -> orElse $"%A{v}"
    | SynConst.Double v -> orElse $"%A{v}"
    | SynConst.Single v -> orElse $"%A{v}"
    | SynConst.Decimal v -> orElse $"%A{v}"
    | SynConst.IntPtr v -> orElse $"%A{v}"
    | SynConst.UIntPtr v -> orElse $"%A{v}"
    | SynConst.UserNum _ -> failwith "todo, 90D57090-9123-4344-9B4F-9B51BB50DA31"
    | SynConst.String (value, stringKind, r) -> mkConstString creationAide stringKind value r |> Constant.FromText
    | SynConst.Char c -> failwith "todo, 9AD2DFA7-80E2-43C7-A573-777987EA941B"
    | SynConst.Bytes (bytes, _, r) -> failwith "todo, ED679198-BED9-42FD-BE24-7E7AD959CE93"
    | SynConst.Measure (c, numberRange, m) -> failwith "todo, 1BF1C723-1931-40BE-8C02-3A4BAC1D8BAD"
    | SynConst.SourceIdentifier (c, _, r) -> stn c r |> Constant.FromText

let mkAttribute (creationAide: CreationAide) (a: SynAttribute) =
    let expr =
        match a.ArgExpr with
        | SynExpr.Const (SynConst.Unit, _) -> None
        | e -> mkExpr creationAide e |> Some

    AttributeNode(mkSynLongIdent a.TypeName, expr, Option.map mkIdent a.Target, a.Range)

let mkAttributeList (creationAide: CreationAide) (al: SynAttributeList) : AttributeListNode =
    let attributes = List.map (mkAttribute creationAide) al.Attributes

    let opening, closing =
        match al.Range with
        | StartEndRange 2 (s, _, e) -> stn "[<" s, stn ">]" e

    AttributeListNode(opening, attributes, closing, al.Range)

let mkAttributes (creationAide: CreationAide) (al: SynAttributeList list) : MultipleAttributeListNode =
    let attributeLists = List.map (mkAttributeList creationAide) al
    let range = List.map (fun al -> (al :> Node).Range) attributeLists |> combineRanges
    MultipleAttributeListNode(attributeLists, range)

let (|Sequentials|_|) e =
    let rec visit (e: SynExpr) (finalContinuation: SynExpr list -> SynExpr list) : SynExpr list =
        match e with
        | SynExpr.Sequential (_, _, e1, e2, _) -> visit e2 (fun xs -> e1 :: xs |> finalContinuation)
        | e -> finalContinuation [ e ]

    match e with
    | SynExpr.Sequential (_, _, e1, e2, _) ->
        let xs = visit e2 id
        Some(e1 :: xs)
    | _ -> None

let (|EndsWithSingleListAppExpr|_|) (isStroustrup: bool) (e: SynExpr) =
    if not isStroustrup then
        None
    else
        match e with
        | SynExpr.App (ExprAtomicFlag.NonAtomic,
                       false,
                       (SynExpr.App _ as funcExpr),
                       (SynExpr.ArrayOrList _ | SynExpr.ArrayOrListComputed _ as lastArg),
                       _) ->
            let rec collectApplicationArgument (e: SynExpr) (continuation: SynExpr seq -> SynExpr seq) =
                match e with
                | SynExpr.App (ExprAtomicFlag.NonAtomic, false, (SynExpr.App _ as funcExpr), argExpr, _) ->
                    collectApplicationArgument funcExpr (fun es ->
                        seq {
                            yield! es
                            yield argExpr
                        }
                        |> continuation)
                | SynExpr.App (ExprAtomicFlag.NonAtomic, false, funcNameExpr, ae, _) ->
                    let args = Seq.toList (continuation (Seq.singleton ae))

                    Some(funcNameExpr, args, lastArg)
                | _ -> None

            collectApplicationArgument funcExpr id
        | SynExpr.App (ExprAtomicFlag.NonAtomic,
                       false,
                       funcExpr,
                       (SynExpr.ArrayOrList _ | SynExpr.ArrayOrListComputed _ as lastArg),
                       _) -> Some(funcExpr, [], lastArg)
        | _ -> None

let (|EndsWithDualListAppExpr|_|) (isStroustrup: bool) (e: SynExpr) =
    if not isStroustrup then
        None
    else
        match e with
        | SynExpr.App (ExprAtomicFlag.NonAtomic,
                       false,
                       EndsWithSingleListAppExpr isStroustrup (e, es, lastButOneArg),
                       (SynExpr.ArrayOrList _ | SynExpr.ArrayOrListComputed _ as lastArg),
                       _) -> Some(e, es, lastButOneArg, lastArg)
        | _ -> None

let mkOpenAndCloseForArrayOrList isArray range =
    if isArray then
        let (StartEndRange 2 (mO, _, mC)) = range
        stn "[|" mO, stn "|]" mC
    else
        let (StartEndRange 1 (mO, _, mC)) = range
        stn "[" mO, stn "]" mC

let mkInheritConstructor (creationAide: CreationAide) (t: SynType) (e: SynExpr) (mInherit: range) (m: range) =
    let inheritNode = stn "inherit" mInherit

    match e with
    | SynExpr.Const (constant = SynConst.Unit; range = StartEndRange 1 (mOpen, unitRange, mClose)) ->
        // The unit expression could have been added artificially.
        if unitRange.StartColumn + 2 = unitRange.EndColumn then
            InheritConstructorUnitNode(inheritNode, mkType creationAide t, stn "(" mOpen, stn ")" mClose, m)
            |> InheritConstructor.Unit
        else
            InheritConstructorTypeOnlyNode(inheritNode, mkType creationAide t, m)
            |> InheritConstructor.TypeOnly
    | SynExpr.Paren _ as px ->
        InheritConstructorParenNode(inheritNode, mkType creationAide t, mkExpr creationAide px, m)
        |> InheritConstructor.Paren
    | _ ->
        InheritConstructorOtherNode(inheritNode, mkType creationAide t, mkExpr creationAide e, m)
        |> InheritConstructor.Other

let mkTuple (creationAide: CreationAide) (exprs: SynExpr list) (commas: range list) (m: range) =
    match exprs with
    | [] -> failwith "SynExpr.Tuple with no elements"
    | head :: tail ->
        let rest =
            assert (tail.Length = commas.Length)

            List.zip commas tail
            |> List.collect (fun (c, e) -> [ yield Choice2Of2(stn "," c); yield Choice1Of2(mkExpr creationAide e) ])

        ExprTupleNode([ yield Choice1Of2(mkExpr creationAide head); yield! rest ], m)

/// Unfold a list of let bindings
/// Recursive and use properties have to be determined at this point
let rec (|LetOrUses|_|) =
    function
    | SynExpr.LetOrUse (_, _, xs, LetOrUses (ys, e), _, trivia) ->
        let xs' = List.mapWithLast (fun b -> b, None) (fun b -> b, trivia.InKeyword) xs
        Some(xs' @ ys, e)
    | SynExpr.LetOrUse (_, _, xs, e, _, trivia) ->
        let xs' = List.mapWithLast (fun b -> b, None) (fun b -> b, trivia.InKeyword) xs
        Some(xs', e)
    | _ -> None

let rec collectComputationExpressionStatements
    (creationAide: CreationAide)
    (e: SynExpr)
    (finalContinuation: ComputationExpressionStatement list -> ComputationExpressionStatement list)
    : ComputationExpressionStatement list =
    match e with
    | LetOrUses (bindings, body) ->
        let bindings =
            bindings
            |> List.map (fun (b, inNode) ->
                let b = mkBinding creationAide b

                let inNode, m =
                    match inNode with
                    | None -> None, (b :> Node).Range
                    | Some mIn -> Some(stn "in" mIn), unionRanges (b :> Node).Range mIn

                ExprLetOrUseNode(b, inNode, m)
                |> ComputationExpressionStatement.LetOrUseStatement)

        collectComputationExpressionStatements creationAide body (fun bodyStatements ->
            [ yield! bindings; yield! bodyStatements ] |> finalContinuation)
    | SynExpr.LetOrUseBang (_,
                            isUse,
                            _,
                            pat,
                            expr,
                            andBangs,
                            body,
                            StartRange 4 (mLeading, m),
                            { EqualsRange = Some mEq }) ->
        let letOrUseBang =
            ExprLetOrUseBangNode(
                stn (if isUse then "use!" else "let!") mLeading,
                mkPat creationAide pat,
                stn "=" mEq,
                mkExpr creationAide expr,
                m
            )
            |> ComputationExpressionStatement.LetOrUseBangStatement

        let andBangs =
            andBangs
            |> List.map (fun (SynExprAndBang (_, _, _, ap, ae, StartRange 4 (mAnd, m), trivia)) ->
                ExprAndBang(
                    stn "and!" mAnd,
                    mkPat creationAide ap,
                    stn "=" trivia.EqualsRange,
                    mkExpr creationAide ae,
                    m
                )
                |> ComputationExpressionStatement.AndBangStatement)

        collectComputationExpressionStatements creationAide body (fun bodyStatements ->
            [ letOrUseBang; yield! andBangs; yield! bodyStatements ] |> finalContinuation)
    | SynExpr.Sequential (_, _, e1, e2, _) ->
        let continuations: ((ComputationExpressionStatement list -> ComputationExpressionStatement list) -> ComputationExpressionStatement list) list =
            [ collectComputationExpressionStatements creationAide e1
              collectComputationExpressionStatements creationAide e2 ]

        let finalContinuation (nodes: ComputationExpressionStatement list list) : ComputationExpressionStatement list =
            List.collect id nodes |> finalContinuation

        Continuation.sequence continuations finalContinuation
    | expr -> finalContinuation [ ComputationExpressionStatement.OtherStatement(mkExpr creationAide expr) ]

/// Process compiler-generated matches in an appropriate way
let rec private skipGeneratedLambdas expr =
    match expr with
    | SynExpr.Lambda (inLambdaSeq = true; body = bodyExpr) -> skipGeneratedLambdas bodyExpr
    | _ -> expr

and skipGeneratedMatch expr =
    match expr with
    | SynExpr.Match (_, _, [ SynMatchClause.SynMatchClause (resultExpr = innerExpr) as clause ], matchRange, _) when
        matchRange.Start = clause.Range.Start
        ->
        skipGeneratedMatch innerExpr
    | _ -> expr

let inline private getLambdaBodyExpr expr =
    let skippedLambdas = skipGeneratedLambdas expr
    skipGeneratedMatch skippedLambdas

let mkLambda creationAide pats body (StartRange 3 (mFun, m)) (trivia: SynExprLambdaTrivia) : ExprLambdaNode =
    ExprLambdaNode(
        stn "fun" mFun,
        List.map (mkPat creationAide) pats,
        stn "->" trivia.ArrowRange.Value,
        mkExpr creationAide body,
        m
    )

let mkSynMatchClause creationAide (SynMatchClause (p, eo, e, range, _, trivia)) : MatchClauseNode =
    let fullRange =
        match trivia.BarRange with
        | None -> range
        | Some barRange -> unionRanges barRange range

    MatchClauseNode(
        Option.map (stn "|") trivia.BarRange,
        mkPat creationAide p,
        Option.map (mkExpr creationAide) eo,
        stn "->" trivia.ArrowRange.Value,
        mkExpr creationAide e,
        fullRange
    )

let (|InfixApp|_|) synExpr =
    match synExpr with
    | SynExpr.App (isInfix = true
                   funcExpr = SynExpr.LongIdent(longDotId = SynLongIdent ([ operatorIdent ],
                                                                          [],
                                                                          [ Some (IdentTrivia.OriginalNotation "::") ]))
                   argExpr = SynExpr.Tuple(exprs = [ e1; e2 ])) -> Some(e1, stn "::" operatorIdent.idRange, e2)
    | SynExpr.App (funcExpr = SynExpr.App (isInfix = true
                                           funcExpr = SynExpr.LongIdent(longDotId = SynLongIdent ([ operatorIdent ],
                                                                                                  [],
                                                                                                  [ Some (IdentTrivia.OriginalNotation operator) ]))
                                           argExpr = e1)
                   argExpr = e2) -> Some(e1, stn operator operatorIdent.idRange, e2)
    | _ -> None

let (|SameInfixApps|_|) expr =
    let rec visit expr continuation =
        match expr with
        | InfixApp (lhs, operator, rhs) ->
            visit lhs (fun (head, xs: Queue<SingleTextNode * SynExpr>) ->
                xs.Enqueue(operator, rhs)
                continuation (head, xs))
        | e -> continuation (e, Queue())

    let head, xs = visit expr id
    if xs.Count < 2 then None else Some(head, Seq.toList xs)

let rec (|ElIf|_|) =
    function
    | SynExpr.IfThenElse (e1, e2, Some (ElIf ((elifNode, eshE1, eshThenKw, eshE2) :: es, elseInfo)), _, _, _, trivia) ->
        let ifNode = MultipleTextsNode([ stn "if" trivia.IfKeyword ], trivia.IfKeyword)

        let elifNode =
            match trivia.ElseKeyword with
            | None -> elifNode
            | Some mElse ->
                let m = unionRanges mElse (elifNode :> Node).Range
                MultipleTextsNode([ yield stn "else" mElse; yield! elifNode.Content ], m)

        Some(
            (ifNode, e1, stn "then" trivia.ThenKeyword, e2)
            :: (elifNode, eshE1, eshThenKw, eshE2) :: es,
            elseInfo
        )

    | SynExpr.IfThenElse (e1, e2, e3, _, _, _, trivia) ->
        let elseInfo =
            match trivia.ElseKeyword, e3 with
            | Some elseKw, Some elseExpr -> Some(stn "else" elseKw, elseExpr)
            | _ -> None

        let ifNode = MultipleTextsNode([ stn "if" trivia.IfKeyword ], trivia.IfKeyword)
        Some([ (ifNode, e1, stn "then" trivia.ThenKeyword, e2) ], elseInfo)
    | _ -> None

let mkExpr (creationAide: CreationAide) (e: SynExpr) : Expr =
    let exprRange = e.Range

    match e with
    | SynExpr.Lazy (e, StartRange 4 (lazyKeyword, _range)) ->
        ExprLazyNode(stn "lazy" lazyKeyword, mkExpr creationAide e, exprRange)
        |> Expr.Lazy
    | SynExpr.InferredDowncast (e, StartRange 8 (downcastKeyword, _range)) ->
        ExprSingleNode(stn "downcast" downcastKeyword, false, mkExpr creationAide e, exprRange)
        |> Expr.Single
    | SynExpr.InferredUpcast (e, StartRange 6 (upcastKeyword, _range)) ->
        ExprSingleNode(stn "upcast" upcastKeyword, false, mkExpr creationAide e, exprRange)
        |> Expr.Single
    | SynExpr.Assert (e, StartRange 6 (assertKeyword, _range)) ->
        ExprSingleNode(stn "assert" assertKeyword, false, mkExpr creationAide e, exprRange)
        |> Expr.Single
    | SynExpr.AddressOf (true, e, _, StartRange 1 (ampersandToken, _range)) ->
        ExprSingleNode(stn "&" ampersandToken, false, mkExpr creationAide e, exprRange)
        |> Expr.Single
    | SynExpr.AddressOf (false, e, _, StartRange 2 (ampersandToken, _range)) ->
        ExprSingleNode(stn "&&" ampersandToken, false, mkExpr creationAide e, exprRange)
        |> Expr.Single
    | SynExpr.YieldOrReturn ((true, _), e, StartRange 5 (yieldKeyword, _range)) ->
        ExprSingleNode(stn "yield" yieldKeyword, true, mkExpr creationAide e, exprRange)
        |> Expr.Single
    | SynExpr.YieldOrReturn ((false, _), e, StartRange 6 (returnKeyword, _range)) ->
        ExprSingleNode(stn "return" returnKeyword, true, mkExpr creationAide e, exprRange)
        |> Expr.Single
    | SynExpr.YieldOrReturnFrom ((true, _), e, StartRange 6 (yieldBangKeyword, _range)) ->
        ExprSingleNode(stn "yield!" yieldBangKeyword, true, mkExpr creationAide e, exprRange)
        |> Expr.Single
    | SynExpr.YieldOrReturnFrom ((false, _), e, StartRange 7 (returnBangKeyword, _range)) ->
        ExprSingleNode(stn "return!" returnBangKeyword, true, mkExpr creationAide e, exprRange)
        |> Expr.Single
    | SynExpr.Do (e, StartRange 2 (doKeyword, _range)) ->
        ExprSingleNode(stn "do" doKeyword, true, mkExpr creationAide e, exprRange)
        |> Expr.Single
    | SynExpr.DoBang (e, StartRange 3 (doBangKeyword, _range)) ->
        ExprSingleNode(stn "do!" doBangKeyword, true, mkExpr creationAide e, exprRange)
        |> Expr.Single
    | SynExpr.Fixed (e, StartRange 5 (fixedKeyword, _range)) ->
        ExprSingleNode(stn "fixed" fixedKeyword, false, mkExpr creationAide e, exprRange)
        |> Expr.Single
    | SynExpr.Const (c, r) -> mkConstant creationAide c r |> Expr.Constant
    | SynExpr.Null _ -> stn "null" exprRange |> Expr.Null
    | SynExpr.Quote (_, isRaw, e, _, range) -> mkExprQuote creationAide isRaw e range |> Expr.Quote
    | SynExpr.TypeTest (e, t, _) ->
        ExprTypedNode(mkExpr creationAide e, ":?", mkType creationAide t, exprRange)
        |> Expr.Typed
    | SynExpr.Downcast (e, t, _) ->
        ExprTypedNode(mkExpr creationAide e, ":?>", mkType creationAide t, exprRange)
        |> Expr.Typed
    | SynExpr.Upcast (e, t, _) ->
        ExprTypedNode(mkExpr creationAide e, ":>", mkType creationAide t, exprRange)
        |> Expr.Typed
    | SynExpr.Typed (e, t, _) ->
        ExprTypedNode(mkExpr creationAide e, ":", mkType creationAide t, exprRange)
        |> Expr.Typed
    | SynExpr.New (_, t, e, StartRange 3 (newRange, _)) ->
        ExprNewNode(stn "new" newRange, mkType creationAide t, mkExpr creationAide e, exprRange)
        |> Expr.New
    | SynExpr.Tuple (false, exprs, commas, _) -> mkTuple creationAide exprs commas exprRange |> Expr.Tuple
    | SynExpr.Tuple (true, exprs, commas, StartRange 6 (mStruct, _) & EndRange 1 (mClosing, _)) ->
        let mTuple =
            match List.tryHead exprs, List.tryLast exprs with
            | Some e1, Some e2 -> unionRanges e1.Range e2.Range
            | _ -> failwith "SynExpr.Tuple with no elements"

        ExprStructTupleNode(stn "struct" mStruct, mkTuple creationAide exprs commas mTuple, stn ")" mClosing, exprRange)
        |> Expr.StructTuple
    | SynExpr.ArrayOrListComputed (isArray, Sequentials xs, range)
    | SynExpr.ArrayOrList (isArray, xs, range) ->
        let o, c = mkOpenAndCloseForArrayOrList isArray range

        ExprArrayOrListNode(o, List.map (mkExpr creationAide) xs, c, exprRange)
        |> Expr.ArrayOrList
    | SynExpr.ArrayOrListComputed (isArray, singleExpr, range) ->
        let o, c = mkOpenAndCloseForArrayOrList isArray range

        ExprArrayOrListNode(o, [ mkExpr creationAide singleExpr ], c, exprRange)
        |> Expr.ArrayOrList
    | SynExpr.Record (baseInfo, copyInfo, recordFields, StartEndRange 1 (mOpen, _, mClose)) ->
        let extra =
            match baseInfo, copyInfo with
            | Some _, Some _ -> failwith "Unexpected that both baseInfo and copyInfo are present in SynExpr.Record"
            | Some (t, e, mInherit, _, m), None ->
                mkInheritConstructor creationAide t e mInherit m |> RecordNodeExtra.Inherit
            | None, Some (copyExpr, _) -> mkExpr creationAide copyExpr |> RecordNodeExtra.With
            | None, None -> RecordNodeExtra.None

        let fieldNodes =
            recordFields
            |> List.choose (function
                | SynExprRecordField ((fieldName, _), Some mEq, Some expr, _) ->
                    let m = unionRanges fieldName.Range expr.Range
                    Some(RecordFieldNode(mkSynLongIdent fieldName, stn "=" mEq, mkExpr creationAide expr, m))
                | _ -> None)

        ExprRecordNode(stn "{" mOpen, extra, fieldNodes, stn "}" mClose, exprRange)
        |> Expr.Record
    | SynExpr.AnonRecd (isStruct, copyInfo, recordFields, StartEndRange 2 (mOpen, _, mClose)) ->
        let fields =
            recordFields
            |> List.choose (function
                | ident, Some mEq, e ->
                    let m = unionRanges ident.idRange e.Range
                    Some(AnonRecordFieldNode(mkIdent ident, stn "=" mEq, mkExpr creationAide e, m))
                | _ -> None)

        ExprAnonRecordNode(
            isStruct,
            stn "{|" mOpen,
            Option.map (fst >> mkExpr creationAide) copyInfo,
            fields,
            stn "|}" mClose,
            exprRange
        )
        |> Expr.AnonRecord
    | SynExpr.ObjExpr (t, eio, withKeyword, bd, members, ims, StartRange 3 (mNew, _), StartEndRange 1 (mOpen, _, mClose)) ->
        let interfaceNodes =
            ims
            |> List.map (fun (SynInterfaceImpl (t, mWith, bs, members, StartRange 9 (mInterface, m))) ->
                InterfaceImplNode(
                    stn "interface" mInterface,
                    mkType creationAide t,
                    Option.map (stn "with") mWith,
                    List.map (mkBinding creationAide) bd,
                    List.map (mkMemberDefn creationAide) members,
                    m
                ))

        ExprObjExprNode(
            stn "{" mOpen,
            stn "new" mNew,
            mkType creationAide t,
            Option.map (fun (e, _) -> mkExpr creationAide e) eio,
            Option.map (stn "with") withKeyword,
            List.map (mkBinding creationAide) bd,
            List.map (mkMemberDefn creationAide) members,
            interfaceNodes,
            stn "}" mClose,
            exprRange
        )
        |> Expr.ObjExpr
    | SynExpr.While (_, ew, ed, StartRange 5 (mWhile, _)) ->
        ExprWhileNode(stn "while" mWhile, mkExpr creationAide ew, mkExpr creationAide ed, exprRange)
        |> Expr.While
    | SynExpr.For (_, _, ident, Some equalsRange, e1, isUp, e2, e3, StartRange 3 (mFor, _)) ->
        ExprForNode(
            stn "for" mFor,
            mkIdent ident,
            stn "=" equalsRange,
            mkExpr creationAide e1,
            isUp,
            mkExpr creationAide e2,
            mkExpr creationAide e3,
            exprRange
        )
        |> Expr.For
    | SynExpr.ForEach (_,
                       _,
                       SeqExprOnly true,
                       _,
                       pat,
                       e1,
                       SynExpr.YieldOrReturn ((true, _), e2, _),
                       StartRange 3 (mFor, _)) ->
        ExprForEachNode(
            stn "for" mFor,
            mkPat creationAide pat,
            mkExpr creationAide e1,
            true,
            mkExpr creationAide e2,
            exprRange
        )
        |> Expr.ForEach
    | SynExpr.ForEach (_, _, SeqExprOnly isArrow, _, pat, e1, e2, StartRange 3 (mFor, _)) ->
        ExprForEachNode(
            stn "for" mFor,
            mkPat creationAide pat,
            mkExpr creationAide e1,
            isArrow,
            mkExpr creationAide e2,
            exprRange
        )
        |> Expr.ForEach
    | SynExpr.App (ExprAtomicFlag.NonAtomic,
                   false,
                   (SynExpr.App _ | SynExpr.TypeApp _ | SynExpr.Ident _ | SynExpr.LongIdent _ as nameExpr),
                   (SynExpr.ComputationExpr (_, expr, StartEndRange 1 (openingBrace, _range, closingBrace))),
                   _) ->
        ExprNamedComputationNode(
            mkExpr creationAide nameExpr,
            stn "{" openingBrace,
            mkExpr creationAide expr,
            stn "}" closingBrace,
            exprRange
        )
        |> Expr.NamedComputation
    | SynExpr.ComputationExpr (_, expr, StartEndRange 1 (openingBrace, _range, closingBrace)) ->
        ExprComputationNode(stn "{" openingBrace, mkExpr creationAide expr, stn "}" closingBrace, exprRange)
        |> Expr.Computation

    | SynExpr.LetOrUse _
    | SynExpr.LetOrUseBang _
    | SynExpr.Sequential _ ->
        ExprCompExprBodyNode(collectComputationExpressionStatements creationAide e id, exprRange)
        |> Expr.CompExprBody

    | SynExpr.JoinIn (e1, mIn, e2, _) ->
        ExprJoinInNode(mkExpr creationAide e1, stn "in" mIn, mkExpr creationAide e2, exprRange)
        |> Expr.JoinIn

    | SynExpr.Paren (SynExpr.Lambda (_, _, _, _, Some (pats, body), range, trivia), lpr, Some rpr, pr) ->
        let body = getLambdaBodyExpr body
        let lambdaNode = mkLambda creationAide pats body range trivia

        ExprParenLambdaNode(stn "(" lpr, lambdaNode, stn ")" rpr, exprRange)
        |> Expr.ParenLambda

    | SynExpr.Lambda (_, _, _, _, Some (pats, body), range, trivia) ->
        let body = getLambdaBodyExpr body
        mkLambda creationAide pats body range trivia |> Expr.Lambda

    | SynExpr.MatchLambda (_, mFun, cs, _, _) ->
        ExprMatchLambdaNode(stn "function" mFun, List.map (mkSynMatchClause creationAide) cs, exprRange)
        |> Expr.MatchLambda

    | SynExpr.Match (_, e, cs, _, trivia) ->
        ExprMatchNode(
            stn "match" trivia.MatchKeyword,
            mkExpr creationAide e,
            stn "with" trivia.WithKeyword,
            List.map (mkSynMatchClause creationAide) cs,
            exprRange
        )
        |> Expr.Match
    | SynExpr.MatchBang (_, e, cs, _, trivia) ->
        ExprMatchNode(
            stn "match!" trivia.MatchBangKeyword,
            mkExpr creationAide e,
            stn "with" trivia.WithKeyword,
            List.map (mkSynMatchClause creationAide) cs,
            exprRange
        )
        |> Expr.Match
    | SynExpr.TraitCall (tps, msg, expr, _) ->
        ExprTraitCallNode(mkType creationAide tps, mkMemberSig creationAide msg, mkExpr creationAide expr, exprRange)
        |> Expr.TraitCall

    | SynExpr.Paren(expr = SynExpr.LibraryOnlyILAssembly (range = m)) ->
        stn (creationAide.TextFromSource "" m) m |> Expr.ParenILEmbedded
    | SynExpr.LongIdent(longDotId = SynLongIdent ([ ident ],
                                                  [],
                                                  [ Some (ParenStarSynIdent (lpr, originalNotation, rpr)) ])) ->
        ExprParenFunctionNameWithStarNode(stn "(" lpr, stn originalNotation ident.idRange, stn ")" rpr, exprRange)
        |> Expr.ParenFunctionNameWithStar
    | SynExpr.Paren (e, lpr, Some rpr, _) ->
        ExprParenNode(stn "(" lpr, mkExpr creationAide e, stn ")" rpr, exprRange)
        |> Expr.Paren

    | SynExpr.Dynamic (funcExpr, _, argExpr, _) ->
        ExprDynamicNode(mkExpr creationAide funcExpr, mkExpr creationAide argExpr, exprRange)
        |> Expr.Dynamic
    | SynExpr.App (_,
                   false,
                   SynExpr.LongIdent (_,
                                      SynLongIdent ([ ident ], [], [ Some (IdentTrivia.OriginalNotation operatorName) ]),
                                      _,
                                      _),
                   e2,
                   _) when
        PrettyNaming.IsValidPrefixOperatorDefinitionName
            (PrettyNaming.ConvertValLogicalNameToDisplayNameCore ident.idText)
        ->
        ExprPrefixAppNode(stn operatorName ident.idRange, mkExpr creationAide e2, exprRange)
        |> Expr.PrefixApp

    // | Expr.NewlineInfixApps _ -> failwith "Not Implemented"
    | SameInfixApps (head, xs) ->
        let rest = xs |> List.map (fun (operator, e) -> operator, mkExpr creationAide e)

        ExprSameInfixAppsNode(mkExpr creationAide head, rest, exprRange)
        |> Expr.SameInfixApps

    | InfixApp (e1, operator, e2) ->
        ExprInfixAppNode(mkExpr creationAide e1, operator, mkExpr creationAide e2, exprRange)
        |> Expr.InfixApp

    | SynExpr.App (ExprAtomicFlag.Atomic, false, identifierExpr, SynExpr.ArrayOrListComputed (false, indexExpr, _), _) ->
        ExprIndexWithoutDotNode(mkExpr creationAide identifierExpr, mkExpr creationAide indexExpr, exprRange)
        |> Expr.IndexWithoutDot
    | SynExpr.App (ExprAtomicFlag.NonAtomic,
                   false,
                   identifierExpr,
                   (SynExpr.ArrayOrListComputed (isArray = false; expr = indexExpr) as argExpr),
                   _) when (RangeHelpers.isAdjacentTo identifierExpr.Range argExpr.Range) ->
        ExprIndexWithoutDotNode(mkExpr creationAide identifierExpr, mkExpr creationAide indexExpr, exprRange)
        |> Expr.IndexWithoutDot

    // | Expr.AppDotGetTypeApp _ -> failwith "Not Implemented"
    // | Expr.DotGetAppDotGetAppParenLambda _ -> failwith "Not Implemented"
    // | Expr.DotGetAppParen _ -> failwith "Not Implemented"
    // | Expr.DotGetAppWithParenLambda _ -> failwith "Not Implemented"
    // | Expr.DotGetApp _ -> failwith "Not Implemented"
    // | Expr.AppLongIdentAndSingleParenArg _ -> failwith "Not Implemented"
    // | Expr.AppSingleParenArg _ -> failwith "Not Implemented"
    // | Expr.DotGetAppWithLambda _ -> failwith "Not Implemented"
    // | Expr.AppWithLambda _ -> failwith "Not Implemented"
    | SynExpr.App (ExprAtomicFlag.NonAtomic,
                   false,
                   SynExpr.App (ExprAtomicFlag.Atomic,
                                false,
                                identifierExpr,
                                SynExpr.ArrayOrListComputed (false, indexExpr, _),
                                _),
                   argExpr,
                   _) ->
        ExprNestedIndexWithoutDotNode(
            mkExpr creationAide identifierExpr,
            mkExpr creationAide indexExpr,
            mkExpr creationAide argExpr,
            exprRange
        )
        |> Expr.NestedIndexWithoutDot
    | SynExpr.App (ExprAtomicFlag.NonAtomic,
                   false,
                   SynExpr.App (ExprAtomicFlag.NonAtomic,
                                false,
                                identifierExpr,
                                (SynExpr.ArrayOrListComputed (isArray = false; expr = indexExpr) as indexArgExpr),
                                _),
                   argExpr,
                   _) when (RangeHelpers.isAdjacentTo identifierExpr.Range indexArgExpr.Range) ->
        ExprNestedIndexWithoutDotNode(
            mkExpr creationAide identifierExpr,
            mkExpr creationAide indexExpr,
            mkExpr creationAide argExpr,
            exprRange
        )
        |> Expr.NestedIndexWithoutDot

    | EndsWithDualListAppExpr creationAide.Config.ExperimentalStroustrupStyle (e, es, firstList, lastList) ->
        ExprEndsWithDualListAppNode(
            mkExpr creationAide e,
            List.map (mkExpr creationAide) es,
            mkExpr creationAide firstList,
            mkExpr creationAide lastList,
            exprRange
        )
        |> Expr.EndsWithDualListApp
    | EndsWithSingleListAppExpr creationAide.Config.ExperimentalStroustrupStyle (e, es, aol) ->
        ExprEndsWithSingleListAppNode(
            mkExpr creationAide e,
            List.map (mkExpr creationAide) es,
            mkExpr creationAide aol,
            exprRange
        )
        |> Expr.EndsWithSingleListApp

    // | Expr.App _ -> failwith "Not Implemented"
    // | Expr.TypeApp _ -> failwith "Not Implemented"
    | SynExpr.TryWith (e, [ SynMatchClause (pat = pat) as c ], _, _, _, trivia) ->
        match pat with
        | SynPat.Or _
        | SynPat.As (SynPat.Or _, _, _) ->
            ExprTryWithNode(
                stn "try" trivia.TryKeyword,
                mkExpr creationAide e,
                stn "with" trivia.WithKeyword,
                [ mkSynMatchClause creationAide c ],
                exprRange
            )
            |> Expr.TryWith
        | _ ->
            ExprTryWithSingleClauseNode(
                stn "try" trivia.TryKeyword,
                mkExpr creationAide e,
                stn "with" trivia.WithKeyword,
                mkSynMatchClause creationAide c,
                exprRange
            )
            |> Expr.TryWithSingleClause
    | SynExpr.TryWith (e, clauses, _, _, _, trivia) ->
        ExprTryWithNode(
            stn "try" trivia.TryKeyword,
            mkExpr creationAide e,
            stn "with" trivia.WithKeyword,
            List.map (mkSynMatchClause creationAide) clauses,
            exprRange
        )
        |> Expr.TryWith

    | SynExpr.TryFinally (e1, e2, _, _, _, trivia) ->
        ExprTryFinallyNode(
            stn "try" trivia.TryKeyword,
            mkExpr creationAide e1,
            stn "finally" trivia.FinallyKeyword,
            mkExpr creationAide e2,
            exprRange
        )
        |> Expr.TryFinally

    | SynExpr.IfThenElse (ifExpr = ifExpr; thenExpr = thenExpr; elseExpr = None; trivia = trivia) ->
        ExprIfThenNode(
            MultipleTextsNode([ stn "if" trivia.IfKeyword ], trivia.IfKeyword),
            mkExpr creationAide ifExpr,
            stn "then" trivia.ThenKeyword,
            mkExpr creationAide thenExpr,
            exprRange
        )
        |> Expr.IfThen

    | ElIf ([ elifKw, ifExpr, thenKw, thenExpr ], Some (elseKw, elseExpr)) ->
        ExprIfThenElseNode(
            elifKw,
            mkExpr creationAide ifExpr,
            thenKw,
            mkExpr creationAide thenExpr,
            elseKw,
            mkExpr creationAide elseExpr,
            exprRange
        )
        |> Expr.IfThenElse

    | ElIf (elifs, elseOpt) ->
        let elifs =
            elifs
            |> List.map (fun (elifNode, ifExpr, thenNode, thenExpr) ->
                let m = unionRanges (elifNode :> Node).Range thenExpr.Range
                ExprIfThenNode(elifNode, mkExpr creationAide ifExpr, thenNode, mkExpr creationAide thenExpr, m))

        let optElse =
            match elseOpt with
            | None -> None
            | Some (elseNode, e) -> Some(elseNode, mkExpr creationAide e)

        ExprIfThenElifNode(elifs, optElse, exprRange) |> Expr.IfThenElif

    | SynExpr.Ident ident -> mkIdent ident |> Expr.Ident
    | SynExpr.LongIdent (isOpt, synLongIdent, _, m) ->
        ExprOptVarNode(isOpt, mkSynLongIdent synLongIdent, m) |> Expr.OptVar
    | SynExpr.LongIdentSet (synLongIdent, e, _) ->
        ExprLongIdentSetNode(mkSynLongIdent synLongIdent, mkExpr creationAide e, exprRange)
        |> Expr.LongIdentSet
    | SynExpr.DotIndexedGet (objectExpr, indexArgs, _, _) ->
        ExprDotIndexedGetNode(mkExpr creationAide objectExpr, mkExpr creationAide indexArgs, exprRange)
        |> Expr.DotIndexedGet
    // | Expr.DotIndexedGet _ -> failwith "Not Implemented"
    // | Expr.DotIndexedSet _ -> failwith "Not Implemented"
    // | Expr.NamedIndexedPropertySet _ -> failwith "Not Implemented"
    // | Expr.DotNamedIndexedPropertySet _ -> failwith "Not Implemented"
    // | Expr.DotGet _ -> failwith "Not Implemented"
    // | Expr.DotSet _ -> failwith "Not Implemented"
    // | Expr.Set _ -> failwith "Not Implemented"
    // | Expr.LibraryOnlyStaticOptimization _ -> failwith "Not Implemented"
    // | Expr.InterpolatedStringExpr _ -> failwith "Not Implemented"
    // | Expr.IndexRangeWildcard _ -> failwith "Not Implemented"
    // | Expr.IndexRange _ -> failwith "Not Implemented"
    // | Expr.IndexFromEnd _ -> failwith "Not Implemented"
    // | Expr.Typar _ -> failwith "Not Implemented"
    | _ -> failwithf "todo for %A" e

let mkExprQuote creationAide isRaw e range : ExprQuoteNode =
    let startToken, endToken =
        let sText, length, eText = if isRaw then "<@@", 3, "@@>" else "<@", 2, "@>"

        match range with
        | StartEndRange length (startRange, _, endRange) -> stn sText startRange, stn eText endRange

    ExprQuoteNode(startToken, mkExpr creationAide e, endToken, range)

let (|ParenStarSynIdent|_|) =
    function
    | IdentTrivia.OriginalNotationWithParen (lpr, originalNotation, rpr) ->
        if originalNotation.Length > 1 && originalNotation.StartsWith("*") then
            Some(lpr, originalNotation, rpr)
        else
            None
    | _ -> None

let mkPat (creationAide: CreationAide) (p: SynPat) =
    let patternRange = p.Range

    match p with
    | SynPat.OptionalVal (ident, _) -> stn $"?{ident.idText}" patternRange |> Pattern.OptionalVal
    | SynPat.Attrib (p, ats, _) ->
        PatAttribNode(mkAttributes creationAide ats, mkPat creationAide p, patternRange)
        |> Pattern.Attrib
    | SynPat.Or (p1, p2, _, trivia) ->
        PatLeftMiddleRight(
            mkPat creationAide p1,
            Choice1Of2(stn "|" trivia.BarRange),
            mkPat creationAide p2,
            patternRange
        )
        |> Pattern.Or
    | SynPat.Ands (ps, _) -> PatAndsNode(List.map (mkPat creationAide) ps, patternRange) |> Pattern.Ands
    | SynPat.Null _ -> stn "null" patternRange |> Pattern.Null
    | SynPat.Wild _ -> stn "_" patternRange |> Pattern.Wild
    | SynPat.Typed (p, t, _) ->
        PatTypedNode(mkPat creationAide p, mkType creationAide t, patternRange)
        |> Pattern.Typed
    | SynPat.Named (accessibility = ao; ident = SynIdent (ident, Some (ParenStarSynIdent (lpr, op, rpr)))) ->
        PatNamedParenStarIdentNode(mkSynAccess ao, stn "(" lpr, stn op ident.idRange, stn ")" rpr, patternRange)
        |> Pattern.NamedParenStarIdent
    | SynPat.Named (accessibility = ao; ident = ident) ->
        PatNamedNode(mkSynAccess ao, mkSynIdent ident, patternRange) |> Pattern.Named
    | SynPat.As (p1, p2, r) ->
        PatLeftMiddleRight(mkPat creationAide p1, Choice2Of2 "as", mkPat creationAide p2, patternRange)
        |> Pattern.As
    | SynPat.ListCons (p1, p2, _, trivia) ->
        PatLeftMiddleRight(
            mkPat creationAide p1,
            Choice1Of2(stn "::" trivia.ColonColonRange),
            mkPat creationAide p2,
            patternRange
        )
        |> Pattern.ListCons
    | SynPat.LongIdent (synLongIdent,
                        _,
                        vtdo,
                        SynArgPats.NamePatPairs (nps, _, { ParenRange = StartEndRange 1 (lpr, range, rpr) }),
                        _,
                        _) ->
        let typarDecls = mkSynValTyparDecls creationAide vtdo

        let pairs =
            nps
            |> List.map (fun (ident, eq, pat) ->
                NamePatPair(mkIdent ident, stn "=" eq, mkPat creationAide pat, unionRanges ident.idRange pat.Range))

        PatNamePatPairsNode(mkSynLongIdent synLongIdent, typarDecls, stn "(" lpr, pairs, stn ")" rpr, patternRange)
        |> Pattern.NamePatPairs
    | SynPat.LongIdent (synLongIdent, _, vtdo, SynArgPats.Pats pats, ao, _) ->
        let typarDecls = mkSynValTyparDecls creationAide vtdo

        PatLongIdentNode(
            mkSynAccess ao,
            mkSynLongIdent synLongIdent,
            typarDecls,
            List.map (mkPat creationAide) pats,
            patternRange
        )
        |> Pattern.LongIdent
    | SynPat.Paren (SynPat.Const (SynConst.Unit, _), StartEndRange 1 (lpr, _, rpr)) ->
        UnitNode(stn "(" lpr, stn ")" rpr, patternRange) |> Pattern.Unit
    | SynPat.Paren (p, StartEndRange 1 (lpr, _, rpr)) ->
        PatParenNode(stn "(" lpr, mkPat creationAide p, stn ")" rpr, patternRange)
        |> Pattern.Paren
    | SynPat.Tuple (false, ps, _) -> PatTupleNode(List.map (mkPat creationAide) ps, patternRange) |> Pattern.Tuple
    | SynPat.Tuple (true, ps, _) ->
        PatStructTupleNode(List.map (mkPat creationAide) ps, patternRange)
        |> Pattern.StructTuple
    | SynPat.ArrayOrList (isArray, ps, range) ->
        let openToken, closeToken = mkOpenAndCloseForArrayOrList isArray range

        PatArrayOrListNode(openToken, List.map (mkPat creationAide) ps, closeToken, patternRange)
        |> Pattern.ArrayOrList
    | SynPat.Record (fields, StartEndRange 1 (o, _, c)) ->
        let fields =
            fields
            |> List.map (fun ((lid, ident), eq, pat) ->
                let prefix = if lid.IsEmpty then None else Some(mkLongIdent lid)

                let range =
                    match prefix with
                    | None -> unionRanges ident.idRange pat.Range
                    | Some prefix -> unionRanges (prefix :> Node).Range pat.Range

                PatRecordField(prefix, mkIdent ident, stn "=" eq, mkPat creationAide pat, range))

        PatRecordNode(stn "{" o, fields, stn "}" c, patternRange) |> Pattern.Record
    | SynPat.Const (c, r) -> mkConstant creationAide c r |> Pattern.Const
    | SynPat.IsInst (t, StartRange 2 (tokenRange, _)) ->
        PatIsInstNode(stn ":?" tokenRange, mkType creationAide t, patternRange)
        |> Pattern.IsInst
    | SynPat.QuoteExpr (SynExpr.Quote (_, isRaw, e, _, _), _) ->
        mkExprQuote creationAide isRaw e patternRange |> Pattern.QuoteExpr
    | pat -> failwith $"unexpected pattern: {pat}"

let mkBindingReturnInfo creationAide (returnInfo: SynBindingReturnInfo option) =
    Option.bind
        (fun (SynBindingReturnInfo (typeName = t; trivia = trivia)) ->
            trivia.ColonRange
            |> Option.map (fun mColon ->
                let m = unionRanges mColon t.Range
                BindingReturnInfoNode(stn ":" mColon, mkType creationAide t, m)))
        returnInfo

let mkBinding
    (creationAide: CreationAide)
    (SynBinding (_ao, _, _isInline, _isMutable, _attrs, _px, _, pat, returnInfo, expr, _, _, trivia))
    =
    let functionName, parameters =
        match pat with
        | SynPat.LongIdent (longDotId = SynLongIdent ([ _ ], _, _) as lid; argPats = SynArgPats.Pats ps) ->
            Choice1Of2(mkSynIdent lid.IdentsWithTrivia.[0]), List.map (mkPat creationAide) ps
        | _ -> Choice2Of2(mkPat creationAide pat), []

    let equals = stn "=" trivia.EqualsRange.Value

    let e = parseExpressionInSynBinding returnInfo expr

    let returnTypeNode = mkBindingReturnInfo creationAide returnInfo

    let range =
        let start =
            // if not xmlDoc.IsEmpty then
            //     xmlDoc.Range
            // elif not attributes.IsEmpty then
            //     attributes.Head.Range
            // else
            match trivia.LeadingKeyword, pat with
            | SynLeadingKeyword.Member _, SynPat.LongIdent(extraId = Some _) -> pat.Range
            | _ -> trivia.LeadingKeyword.Range

        unionRanges start e.Range

    BindingNode(
        mkSynLeadingKeyword trivia.LeadingKeyword,
        functionName,
        parameters,
        returnTypeNode,
        equals,
        (mkExpr creationAide e),
        range
    )

let mkXmlDoc (px: PreXmlDoc) =
    if px.IsEmpty then
        None
    else
        let xmlDoc = px.ToXmlDoc(false, None)
        let lines = Array.map (sprintf "///%s") xmlDoc.UnprocessedLines
        Some(stn (String.concat "\n" lines) xmlDoc.Range)

let mkModuleDecl (creationAide: CreationAide) (decl: SynModuleDecl) =
    let declRange = decl.Range

    match decl with
    | SynModuleDecl.Expr (e, _) -> mkExpr creationAide e |> ModuleDecl.DeclExpr
    | SynModuleDecl.Exception (SynExceptionDefn (SynExceptionDefnRepr (attrs, caseName, _, xmlDoc, vis, _),
                                                 withKeyword,
                                                 ms,
                                                 _),
                               _) ->
        ExceptionDefnNode(
            mkXmlDoc xmlDoc,
            mkAttributes creationAide attrs,
            mkSynAccess vis,
            mkSynUnionCase creationAide caseName,
            Option.map (stn "with") withKeyword,
            List.map (mkMemberDefn creationAide) ms,
            declRange
        )
        |> ModuleDecl.Exception
    | SynModuleDecl.Let (_, [ SynBinding(trivia = { LeadingKeyword = SynLeadingKeyword.Extern _ }) ], _) ->
        failwith "todo: extern"
    // | ExternBinding of ExternBindingNode
    | SynModuleDecl.Let(bindings = [ singleBinding ]) ->
        mkBinding creationAide singleBinding |> ModuleDecl.TopLevelBinding
    | SynModuleDecl.ModuleAbbrev (ident, lid, StartRange 6 (mModule, _)) ->
        ModuleAbbrevNode(stn "module" mModule, mkIdent ident, mkLongIdent lid, declRange)
        |> ModuleDecl.ModuleAbbrev
    | SynModuleDecl.NestedModule (SynComponentInfo (ats, _, _, lid, px, _, ao, _),
                                  isRecursive,
                                  decls,
                                  _,
                                  _,
                                  { ModuleKeyword = Some mModule
                                    EqualsRange = Some mEq }) ->
        NestedModuleNode(
            mkXmlDoc px,
            mkAttributes creationAide ats,
            stn "module" mModule,
            mkSynAccess ao,
            isRecursive,
            mkLongIdent lid,
            stn "=" mEq,
            List.map (mkModuleDecl creationAide) decls,
            declRange
        )
        |> ModuleDecl.NestedModule
    | decl -> failwithf $"Failed to create ModuleDecl for %A{decl}"

let mkSynTyparDecls (creationAide: CreationAide) (tds: SynTyparDecls) : TyparDecls =
    match tds with
    | SynTyparDecls.PostfixList _
    | SynTyparDecls.PrefixList _
    | SynTyparDecls.SinglePrefix _ -> failwith "todo"

let mkSynValTyparDecls (creationAide: CreationAide) (vt: SynValTyparDecls option) : TyparDecls option =
    match vt with
    | None -> None
    | Some (SynValTyparDecls (tds, _)) -> Option.map (mkSynTyparDecls creationAide) tds

let mkSynRationalConst rc =
    let rec visit rc =
        match rc with
        | SynRationalConst.Integer i -> string i
        | SynRationalConst.Rational (numerator, denominator, _) -> $"(%i{numerator}/%i{denominator})"
        | SynRationalConst.Negate innerRc -> $"-{visit innerRc}"

    visit rc

let mkSynTypar (SynTypar (ident, req, _)) =
    let range =
        mkRange
            ident.idRange.FileName
            (Position.mkPos ident.idRange.StartLine (ident.idRange.StartColumn - 1))
            ident.idRange.End

    match req with
    | TyparStaticReq.None -> stn $"'{ident}" range
    | TyparStaticReq.HeadType -> stn $"^{ident.idText}" range

let mkSynTypeConstraint (creationAide: CreationAide) (tc: SynTypeConstraint) : TypeConstraint =
    match tc with
    | SynTypeConstraint.WhereTyparIsValueType (tp, EndRange 6 (mKeyword, m)) ->
        TypeConstraintSingleNode(mkSynTypar tp, stn "struct" mKeyword, m)
        |> TypeConstraint.Single
    | SynTypeConstraint.WhereTyparIsReferenceType (tp, EndRange 10 (mKeyword, m)) ->
        TypeConstraintSingleNode(mkSynTypar tp, stn "not struct" mKeyword, m)
        |> TypeConstraint.Single
    | SynTypeConstraint.WhereTyparIsUnmanaged (tp, EndRange 9 (mKeyword, m)) ->
        TypeConstraintSingleNode(mkSynTypar tp, stn "unmanaged" mKeyword, m)
        |> TypeConstraint.Single
    | SynTypeConstraint.WhereTyparSupportsNull (tp, EndRange 4 (mKeyword, m)) ->
        TypeConstraintSingleNode(mkSynTypar tp, stn "null" mKeyword, m)
        |> TypeConstraint.Single
    | SynTypeConstraint.WhereTyparIsComparable (tp, EndRange 10 (mKeyword, m)) ->
        TypeConstraintSingleNode(mkSynTypar tp, stn "comparison" mKeyword, m)
        |> TypeConstraint.Single
    | SynTypeConstraint.WhereTyparIsEquatable (tp, EndRange 8 (mKeyword, m)) ->
        TypeConstraintSingleNode(mkSynTypar tp, stn "equality" mKeyword, m)
        |> TypeConstraint.Single
    | SynTypeConstraint.WhereTyparDefaultsToType (tp, t, StartRange 7 (mDefaults, m)) ->
        TypeConstraintDefaultsToTypeNode(stn "default" mDefaults, mkSynTypar tp, mkType creationAide t, m)
        |> TypeConstraint.DefaultsToType
    | SynTypeConstraint.WhereTyparSubtypeOfType (tp, t, m) ->
        TypeConstraintSubtypeOfTypeNode(mkSynTypar tp, mkType creationAide t, m)
        |> TypeConstraint.SubtypeOfType
    | SynTypeConstraint.WhereTyparSupportsMember (tps, msg, m) ->
        TypeConstraintSupportsMemberNode(mkType creationAide tps, box msg, m)
        |> TypeConstraint.SupportsMember
    | SynTypeConstraint.WhereTyparIsEnum (tp, ts, m) ->
        TypeConstraintEnumOrDelegateNode(mkSynTypar tp, "enum", List.map (mkType creationAide) ts, m)
        |> TypeConstraint.EnumOrDelegate
    | SynTypeConstraint.WhereTyparIsDelegate (tp, ts, m) ->
        TypeConstraintEnumOrDelegateNode(mkSynTypar tp, "delegate", List.map (mkType creationAide) ts, m)
        |> TypeConstraint.EnumOrDelegate
    | SynTypeConstraint.WhereSelfConstrained (t, _) -> mkType creationAide t |> TypeConstraint.WhereSelfConstrained

// Arrow type is right-associative
let rec (|TFuns|_|) =
    function
    | SynType.Fun (t1, TFuns (ts, ret), _, trivia) -> Some((t1, trivia.ArrowRange) :: ts, ret)
    | SynType.Fun (t1, t2, _, trivia) -> Some([ t1, trivia.ArrowRange ], t2)
    | _ -> None

let mkTypeList creationAide ts rt m =
    let parameters =
        ts |> List.map (fun (t, mArrow) -> mkType creationAide t, stn "->" mArrow)

    TypeFunsNode(parameters, mkType creationAide rt, m)

let mkType (creationAide: CreationAide) (t: SynType) : Type =
    let typeRange = t.Range

    match t with
    | TFuns (ts, rt) -> mkTypeList creationAide ts rt typeRange |> Type.Funs
    | SynType.Tuple (false, ts, _) ->
        let path =
            ts
            |> List.map (function
                | SynTupleTypeSegment.Type t -> Choice1Of2(mkType creationAide t)
                | SynTupleTypeSegment.Slash m -> Choice2Of2(stn "/" m)
                | SynTupleTypeSegment.Star m -> Choice2Of2(stn "*" m))

        TypeTupleNode(path, typeRange) |> Type.Tuple
    | SynType.Tuple (true, ts, (StartRange 6 (mStruct, _) & StartEndRange 1 (_, _, closingParen))) ->
        let path =
            ts
            |> List.map (function
                | SynTupleTypeSegment.Type t -> Choice1Of2(mkType creationAide t)
                | SynTupleTypeSegment.Slash m -> Choice2Of2(stn "/" m)
                | SynTupleTypeSegment.Star m -> Choice2Of2(stn "*" m))

        TypeStructTupleNode(stn "struct" mStruct, path, stn ")" closingParen, typeRange)
        |> Type.StructTuple
    | SynType.HashConstraint (t, StartRange 1 (mHash, _)) ->
        TypeHashConstraintNode(stn "#" mHash, mkType creationAide t, typeRange)
        |> Type.HashConstraint
    | SynType.MeasurePower (t, rc, _) ->
        TypeMeasurePowerNode(mkType creationAide t, mkSynRationalConst rc, typeRange)
        |> Type.MeasurePower
    | SynType.StaticConstant (c, r) -> mkConstant creationAide c r |> Type.StaticConstant
    | SynType.StaticConstantExpr (e, StartRange 5 (mConst, _)) ->
        TypeStaticConstantExprNode(stn "const" mConst, mkExpr creationAide e, typeRange)
        |> Type.StaticConstantExpr
    | SynType.StaticConstantNamed (t1, t2, _) ->
        TypeStaticConstantNamedNode(mkType creationAide t1, mkType creationAide t2, typeRange)
        |> Type.StaticConstantNamed
    | SynType.Array (rank, t, _) -> TypeArrayNode(mkType creationAide t, rank, typeRange) |> Type.Array
    | SynType.Anon _ -> stn "_" typeRange |> Type.Anon
    | SynType.Var (tp, _) -> mkSynTypar tp |> Type.Var
    | SynType.App (t1, None, [ t2 ], _commaRanges, None, true, _) ->
        TypeAppPostFixNode(mkType creationAide t2, mkType creationAide t1, typeRange)
        |> Type.AppPostfix
    | SynType.App (t, Some mLt, args, _commaRanges, Some mGt, false, _) ->
        TypeAppPrefixNode(
            mkType creationAide t,
            None,
            stn "<" mLt,
            List.map (mkType creationAide) args,
            stn ">" mGt,
            typeRange
        )
        |> Type.AppPrefix
    | SynType.LongIdentApp (t, lid, Some mLt, args, _, Some mGt, _) ->
        TypeAppPrefixNode(
            mkType creationAide t,
            Some(mkSynLongIdent lid),
            stn "<" mLt,
            List.map (mkType creationAide) args,
            stn ">" mGt,
            typeRange
        )
        |> Type.AppPrefix
    | SynType.WithGlobalConstraints (SynType.Var _, [ SynTypeConstraint.WhereTyparSubtypeOfType _ as tc ], _) ->
        mkSynTypeConstraint creationAide tc |> Type.WithSubTypeConstraint
    | SynType.WithGlobalConstraints (t, tcs, _) ->
        TypeWithGlobalConstraintsNode(mkType creationAide t, List.map (mkSynTypeConstraint creationAide) tcs, typeRange)
        |> Type.WithGlobalConstraints
    | SynType.LongIdent lid -> Type.LongIdent(mkSynLongIdent lid)
    | SynType.AnonRecd (isStruct, fields, StartEndRange 2 (_, r, mClosing)) ->
        let structNode, openingNode =
            if isStruct then
                match r with
                | StartRange 6 (mStruct, _) -> Some(stn "struct" mStruct), None
            else
                match r with
                | StartRange 2 (mOpening, _) -> None, Some(stn "{|" mOpening)

        let fields = fields |> List.map (fun (i, t) -> mkIdent i, mkType creationAide t)

        TypeAnonRecordNode(structNode, openingNode, fields, stn "|}" mClosing, typeRange)
        |> Type.AnonRecord
    | SynType.Paren (innerType, StartEndRange 1 (lpr, _, rpr)) ->
        TypeParenNode(stn "(" lpr, mkType creationAide innerType, stn ")" rpr, typeRange)
        |> Type.Paren
    | SynType.SignatureParameter (attrs, isOptional, identOpt, t, _) ->
        let identNode =
            identOpt
            |> Option.map (fun ident ->
                if isOptional then
                    stn $"?{ident.idText}" ident.idRange
                else
                    mkIdent ident)

        TypeSignatureParameterNode(mkAttributes creationAide attrs, identNode, mkType creationAide t, typeRange)
        |> Type.SignatureParameter
    | SynType.Or (lhs, rhs, _, trivia) ->
        TypeOrNode(mkType creationAide lhs, stn "or" trivia.OrKeyword, mkType creationAide rhs, typeRange)
        |> Type.Or
    | t -> failwith $"unexpected type: {t}"

let rec (|OpenL|_|) =
    function
    | SynModuleDecl.Open (target, range) :: OpenL (xs, ys) -> Some((target, range) :: xs, ys)
    | SynModuleDecl.Open (target, range) :: ys -> Some([ target, range ], ys)
    | _ -> None

let mkOpenNodeForImpl (creationAide: CreationAide) (target, range) : Open =
    match target with
    | SynOpenDeclTarget.ModuleOrNamespace (longId, _) ->
        OpenModuleOrNamespaceNode(mkSynLongIdent longId, range)
        |> Open.ModuleOrNamespace
    | SynOpenDeclTarget.Type (typeName, range) -> OpenTargetNode(mkType creationAide typeName, range) |> Open.Target

let rec (|HashDirectiveL|_|) =
    function
    | SynModuleDecl.HashDirective (p, _) :: HashDirectiveL (xs, ys) -> Some(p :: xs, ys)
    | SynModuleDecl.HashDirective (p, _) :: ys -> Some([ p ], ys)
    | _ -> None

let mkSynLeadingKeyword (lk: SynLeadingKeyword) =
    let mtn v =
        v
        |> List.map (fun (t, r) -> stn t r)
        |> fun nodes -> MultipleTextsNode(nodes, lk.Range)

    match lk with
    | SynLeadingKeyword.Let letRange -> mtn [ "let", letRange ]
    | SynLeadingKeyword.LetRec (letRange, recRange) -> mtn [ "let", letRange; "rec", recRange ]
    | SynLeadingKeyword.And andRange -> mtn [ "and", andRange ]
    | SynLeadingKeyword.Use useRange -> mtn [ "use", useRange ]
    | SynLeadingKeyword.UseRec (useRange, recRange) -> mtn [ "use", useRange; "rec", recRange ]
    | SynLeadingKeyword.Extern externRange -> mtn [ "extern", externRange ]
    | SynLeadingKeyword.Member memberRange -> mtn [ "member", memberRange ]
    | SynLeadingKeyword.MemberVal (memberRange, valRange) -> mtn [ "member", memberRange; "val", valRange ]
    | SynLeadingKeyword.Override overrideRange -> mtn [ "override", overrideRange ]
    | SynLeadingKeyword.OverrideVal (overrideRange, valRange) -> mtn [ "override", overrideRange; "val", valRange ]
    | SynLeadingKeyword.Abstract abstractRange -> mtn [ "abstract", abstractRange ]
    | SynLeadingKeyword.AbstractMember (abstractRange, memberRange) ->
        mtn [ "abstract", abstractRange; "member", memberRange ]
    | SynLeadingKeyword.StaticMember (staticRange, memberRange) -> mtn [ "static", staticRange; "member", memberRange ]
    | SynLeadingKeyword.StaticMemberVal (staticRange, memberRange, valRange) ->
        mtn [ "static", staticRange; "member", memberRange; "val", valRange ]
    | SynLeadingKeyword.StaticAbstract (staticRange, abstractRange) ->
        mtn [ "static", staticRange; "abstract", abstractRange ]
    | SynLeadingKeyword.StaticAbstractMember (staticRange, abstractMember, memberRange) ->
        mtn [ "static", staticRange; "abstract", abstractMember; "member", memberRange ]
    | SynLeadingKeyword.StaticVal (staticRange, valRange) -> mtn [ "static", staticRange; "val", valRange ]
    | SynLeadingKeyword.StaticLet (staticRange, letRange) -> mtn [ "static", staticRange; "let", letRange ]
    | SynLeadingKeyword.StaticLetRec (staticRange, letRange, recRange) ->
        mtn [ "static", staticRange; "let", letRange; "rec", recRange ]
    | SynLeadingKeyword.StaticDo (staticRange, doRange) -> mtn [ "static", staticRange; "do", doRange ]
    | SynLeadingKeyword.Default defaultRange -> mtn [ "default", defaultRange ]
    | SynLeadingKeyword.DefaultVal (defaultRange, valRange) -> mtn [ "default", defaultRange; "val", valRange ]
    | SynLeadingKeyword.Val valRange -> mtn [ "val", valRange ]
    | SynLeadingKeyword.New newRange -> mtn [ "new", newRange ]
    | SynLeadingKeyword.Do doRange -> mtn [ "do", doRange ]
    | SynLeadingKeyword.Synthetic -> failwith "Unexpected SynLeadingKeyword.Synthetic"

let mkSynField
    (creationAide: CreationAide)
    (SynField (ats, _isStatic, ido, t, isMutable, px, ao, range, { LeadingKeyword = lk }))
    =
    FieldNode(
        mkXmlDoc px,
        mkAttributes creationAide ats,
        Option.map mkSynLeadingKeyword lk,
        isMutable,
        mkSynAccess ao,
        Option.map mkIdent ido,
        mkType creationAide t,
        range
    )

let mkSynUnionCase
    (creationAide: CreationAide)
    (SynUnionCase (attributes, ident, caseType, xmlDoc, vis, m, trivia))
    : UnionCaseNode =
    let fullRange =
        if not xmlDoc.IsEmpty then
            m
        else
            match trivia.BarRange with
            | None -> m
            | Some barRange -> unionRanges barRange m

    let fields =
        match caseType with
        | SynUnionCaseKind.FullType _ -> []
        | SynUnionCaseKind.Fields cases -> List.map (mkSynField creationAide) cases

    UnionCaseNode(
        mkXmlDoc xmlDoc,
        mkAttributes creationAide attributes,
        Option.map (stn "|") trivia.BarRange,
        mkSynIdent ident,
        fields,
        fullRange
    )

let mkImplicitCtor creationAide vis (attrs: SynAttributeList list) pats (self: Ident option) (xmlDoc: PreXmlDoc) m =
    let openNode, closeNode =
        match pats with
        | SynSimplePats.SimplePats(range = StartEndRange 1 (mOpen, _, mClose))
        | SynSimplePats.Typed(range = StartEndRange 1 (mOpen, _, mClose)) -> stn "(" mOpen, stn ")" mClose

    let pats =
        match pats with
        | SynSimplePats.SimplePats (pats = pats) -> pats
        | SynSimplePats.Typed _ -> []
        |> List.choose (function
            | SynSimplePat.Attrib (SynSimplePat.Typed (SynSimplePat.Id (ident = ident; isOptional = isOptional), t, _),
                                   attributes,
                                   m) ->
                Some(
                    SimplePatNode(
                        mkAttributes creationAide attributes,
                        isOptional,
                        mkIdent ident,
                        Some(mkType creationAide t),
                        m
                    )
                )
            | SynSimplePat.Typed (SynSimplePat.Id (ident = ident; isOptional = isOptional), t, _) ->
                Some(
                    SimplePatNode(
                        mkAttributes creationAide [],
                        isOptional,
                        mkIdent ident,
                        Some(mkType creationAide t),
                        m
                    )
                )
            | SynSimplePat.Id (ident = ident; isOptional = isOptional) ->
                Some(SimplePatNode(mkAttributes creationAide [], isOptional, mkIdent ident, None, m))
            | _ -> None)

    let range =
        let startRange =
            if not xmlDoc.IsEmpty then xmlDoc.Range
            else if not attrs.IsEmpty then attrs.[0].Range
            else (openNode :> Node).Range

        let endRange =
            match self with
            | Some self -> self.idRange
            | None -> (closeNode :> Node).Range

        unionRanges startRange endRange

    ImplicitConstructorNode(
        mkXmlDoc xmlDoc,
        mkAttributes creationAide attrs,
        mkSynAccess vis,
        openNode,
        pats,
        closeNode,
        Option.map mkIdent self,
        range
    )

let mkTypeDefn
    (creationAide: CreationAide)
    (SynTypeDefn (typeInfo, typeRepr, members, implicitConstructor, range, trivia))
    : TypeDefn =
    let typeDefnRange = range

    let typeNameNode =
        match typeInfo with
        | SynComponentInfo (ats, tds, tcs, lid, px, _preferPostfix, ao, _) ->
            let identifierNode = mkLongIdent lid

            let leadingKeyword =
                match trivia.LeadingKeyword with
                | SynTypeDefnLeadingKeyword.Type mType -> stn "type" mType
                | SynTypeDefnLeadingKeyword.And mAnd -> stn "and" mAnd
                | SynTypeDefnLeadingKeyword.StaticType _
                | SynTypeDefnLeadingKeyword.Synthetic _ -> failwithf "unexpected %A" trivia.LeadingKeyword

            TypeNameNode(
                mkXmlDoc px,
                mkAttributes creationAide ats,
                leadingKeyword,
                mkSynAccess ao,
                identifierNode,
                Option.map (mkSynTyparDecls creationAide) tds,
                List.map (mkSynTypeConstraint creationAide) tcs,
                Option.map (stn "=") trivia.EqualsRange,
                Option.map (stn "with") trivia.WithKeyword,
                unionRanges (leadingKeyword :> Node).Range (identifierNode :> Node).Range
            )

    let members = List.map (mkMemberDefn creationAide) members

    match typeRepr with
    | SynTypeDefnRepr.Simple(simpleRepr = SynTypeDefnSimpleRepr.Enum (ecs, _)) ->
        let enumCases =
            ecs
            |> List.map (fun (SynEnumCase (attributes, ident, value, valueRange, xmlDoc, range, trivia)) ->
                EnumCaseNode(
                    mkXmlDoc xmlDoc,
                    Option.map (stn "|") trivia.BarRange,
                    mkAttributes creationAide attributes,
                    mkSynIdent ident,
                    stn "=" trivia.EqualsRange,
                    mkConstant creationAide value valueRange,
                    range
                ))

        TypeDefnEnumNode(typeNameNode, enumCases, members, typeDefnRange)
        |> TypeDefn.Enum

    | SynTypeDefnRepr.Simple(simpleRepr = SynTypeDefnSimpleRepr.Union (ao, cases, _)) ->
        let unionCases = cases |> List.map (mkSynUnionCase creationAide)

        TypeDefnUnionNode(typeNameNode, mkSynAccess ao, unionCases, members, typeDefnRange)
        |> TypeDefn.Union

    | SynTypeDefnRepr.Simple(simpleRepr = SynTypeDefnSimpleRepr.Record (ao,
                                                                        fs,
                                                                        StartEndRange 1 (openingBrace, _, closingBrace))) ->
        let fields = List.map (mkSynField creationAide) fs

        TypeDefnRecordNode(
            typeNameNode,
            mkSynAccess ao,
            stn "{" openingBrace,
            fields,
            stn "}" closingBrace,
            members,
            typeDefnRange
        )
        |> TypeDefn.Record

    | SynTypeDefnRepr.Simple(simpleRepr = SynTypeDefnSimpleRepr.TypeAbbrev (rhsType = t)) ->
        TypeDefn.Abbrev(TypeDefnAbbrevNode(typeNameNode, mkType creationAide t, range))

    | SynTypeDefnRepr.Simple(simpleRepr = SynTypeDefnSimpleRepr.None _) -> TypeDefn.None typeNameNode

    | SynTypeDefnRepr.ObjectModel (kind = SynTypeDefnKind.Class | SynTypeDefnKind.Interface | SynTypeDefnKind.Struct as tdk
                                   members = objectMembers
                                   range = range) ->
        let implicitConstructorNode =
            match implicitConstructor with
            | Some (SynMemberDefn.ImplicitCtor (vis, attrs, pats, self, xmlDoc, m)) ->
                mkImplicitCtor creationAide vis attrs pats self xmlDoc m |> Some
            | _ -> None

        let kindNode =
            match tdk, range with
            | SynTypeDefnKind.Class, StartRange 5 (mClass, _) -> stn "class" mClass
            | SynTypeDefnKind.Interface, StartRange 9 (mInterface, _) -> stn "interface" mInterface
            | SynTypeDefnKind.Struct, StartRange 6 (mStruct, _) -> stn "struct" mStruct
            | _ -> failwith "unexpected kind"

        let objectMembers =
            objectMembers
            |> List.filter (function
                | SynMemberDefn.ImplicitCtor _ -> false
                | _ -> true)
            |> List.map (mkMemberDefn creationAide)

        let endNode =
            match range with
            | EndRange 3 (mEnd, _) -> stn "end" mEnd

        let body = TypeDefnExplicitBodyNode(kindNode, objectMembers, endNode, range)

        TypeDefnExplicitNode(typeNameNode, implicitConstructorNode, body, members, typeDefnRange)
        |> TypeDefn.Explicit

    | SynTypeDefnRepr.ObjectModel(kind = SynTypeDefnKind.Augmentation mWith) ->
        let typeNameNode =
            TypeNameNode(
                typeNameNode.XmlDoc,
                typeNameNode.Attributes,
                typeNameNode.LeadingKeyword,
                typeNameNode.Accessibility,
                typeNameNode.Identifier,
                typeNameNode.TypeParameters,
                typeNameNode.Constraints,
                None,
                Some(stn "with" mWith),
                (typeNameNode :> Node).Range
            )

        TypeDefnAugmentationNode(typeNameNode, members, typeDefnRange)
        |> TypeDefn.Augmentation

    | SynTypeDefnRepr.ObjectModel (kind = SynTypeDefnKind.Delegate(signature = (TFuns (ts, rt)) as st)
                                   range = StartRange 8 (mDelegate, _)) ->
        let typeList = mkTypeList creationAide ts rt st.Range

        TypeDefnDelegateNode(typeNameNode, stn "delegate" mDelegate, typeList, typeDefnRange)
        |> TypeDefn.Delegate

    | SynTypeDefnRepr.ObjectModel (members = objectMembers) ->
        let implicitConstructorNode =
            match implicitConstructor with
            | Some (SynMemberDefn.ImplicitCtor (vis, attrs, pats, self, xmlDoc, m)) ->
                mkImplicitCtor creationAide vis attrs pats self xmlDoc m |> Some
            | _ -> None

        let allMembers =
            let objectMembers =
                objectMembers
                |> List.filter (function
                    | SynMemberDefn.ImplicitCtor _ -> false
                    | _ -> true)
                |> List.map (mkMemberDefn creationAide)

            [ yield! objectMembers; yield! members ]

        TypeDefnRegularNode(typeNameNode, implicitConstructorNode, allMembers, typeDefnRange)
        |> TypeDefn.Regular
    | _ -> failwithf "Could not create a TypeDefn for %A" typeRepr

let mkWithGetSet (t: SynType option) (withKeyword: range option) (getSet: range option) (memberKind: SynMemberKind) =
    let isFunctionProperty =
        match t with
        | Some (SynType.Fun _) -> true
        | _ -> false

    match withKeyword, getSet with
    | Some mWith, Some mGS ->
        let withNode = stn "with" mWith
        let m = unionRanges mWith mGS

        match memberKind with
        | SynMemberKind.PropertyGet ->
            if not isFunctionProperty then
                None
            else
                Some(MultipleTextsNode([ withNode; stn "get" mGS ], m))
        | SynMemberKind.PropertySet -> Some(MultipleTextsNode([ withNode; stn "set" mGS ], m))
        | SynMemberKind.PropertyGetSet -> Some(MultipleTextsNode([ withNode; stn "get, set" mGS ], m))
        | _ -> None
    | _ -> None

let mkPropertyGetSetBinding
    (creationAide: CreationAide)
    (leadingKeyword: SingleTextNode)
    (binding: SynBinding)
    : PropertyGetSetBindingNode =
    match binding with
    | SynBinding (headPat = SynPat.LongIdent (accessibility = ao; argPats = SynArgPats.Pats ps)
                  returnInfo = returnInfo
                  expr = expr
                  trivia = { EqualsRange = Some mEq }
                  range = range) ->
        let e = parseExpressionInSynBinding returnInfo expr
        let returnTypeNode = mkBindingReturnInfo creationAide returnInfo

        let pats =
            match ps with
            | [ SynPat.Tuple (false, [ p1; p2 ], _) ] -> [ mkPat creationAide p1; mkPat creationAide p2 ]
            | ps -> List.map (mkPat creationAide) ps

        PropertyGetSetBindingNode(
            mkSynAccess ao,
            leadingKeyword,
            pats,
            returnTypeNode,
            stn "=" mEq,
            mkExpr creationAide e,
            range
        )
    | _ -> failwith "SynBinding does not expected information for PropertyGetSetBinding"

let mkMemberDefn (creationAide: CreationAide) (md: SynMemberDefn) =
    let memberDefinitionRange = md.Range

    match md with
    | SynMemberDefn.ImplicitInherit (t, e, _, StartRange 7 (mInherit, _)) ->
        mkInheritConstructor creationAide t e mInherit memberDefinitionRange
        |> MemberDefn.ImplicitInherit
    | SynMemberDefn.GetSetMember (Some (SynBinding (ao,
                                                    kind,
                                                    isInline,
                                                    isMutable,
                                                    ats,
                                                    px,
                                                    valData,
                                                    SynPat.LongIdent (lid,
                                                                      extraId,
                                                                      typarDecls,
                                                                      SynArgPats.Pats [ SynPat.Paren (SynPat.Const (SynConst.Unit,
                                                                                                                    _),
                                                                                                      _) ],
                                                                      None,
                                                                      mPat),
                                                    ri,
                                                    e,
                                                    bindingRange,
                                                    dp,
                                                    trivia)),
                                  None,
                                  _,
                                  { GetKeyword = Some _ }) ->

        let pat =
            SynPat.LongIdent(lid, extraId, typarDecls, SynArgPats.Pats([]), None, mPat)

        mkBinding
            creationAide
            (SynBinding(ao, kind, isInline, isMutable, ats, px, valData, pat, ri, e, bindingRange, dp, trivia))
        |> MemberDefn.Member
    | SynMemberDefn.Member(memberDefn = SynBinding (attributes = ats
                                                    xmlDoc = px
                                                    valData = SynValData (Some { MemberKind = SynMemberKind.Constructor },
                                                                          _,
                                                                          ido)
                                                    headPat = SynPat.LongIdent (longDotId = SynLongIdent(id = [ newIdent ])
                                                                                argPats = SynArgPats.Pats [ SynPat.Paren _ as pat ]
                                                                                accessibility = ao)
                                                    expr = expr
                                                    trivia = { EqualsRange = Some mEq })) when (newIdent.idText = "new") ->
        let exprNode, thenExprNode =
            match expr with
            | SynExpr.Sequential (_, false, e1, e2, _) -> mkExpr creationAide e1, Some(mkExpr creationAide e2)
            | e -> mkExpr creationAide e, None

        MemberDefnExplicitCtorNode(
            mkXmlDoc px,
            mkAttributes creationAide ats,
            mkSynAccess ao,
            mkIdent newIdent,
            mkPat creationAide pat,
            Option.map mkIdent ido,
            stn "=" mEq,
            exprNode,
            thenExprNode,
            memberDefinitionRange
        )
        |> MemberDefn.ExplicitCtor
    | SynMemberDefn.Member (memberDefn, _) -> mkBinding creationAide memberDefn |> MemberDefn.Member
    | SynMemberDefn.Inherit (baseType, _, StartRange 7 (mInherit, _)) ->
        MemberDefnInheritNode(stn "inherit" mInherit, mkType creationAide baseType, memberDefinitionRange)
        |> MemberDefn.Inherit
    | SynMemberDefn.ValField (f, _) -> mkSynField creationAide f |> MemberDefn.ValField
    | SynMemberDefn.LetBindings(bindings = [ SynBinding (kind = SynBindingKind.Do; expr = expr; trivia = trivia) ]) ->
        ExprSingleNode(stn "do" trivia.LeadingKeyword.Range, false, mkExpr creationAide expr, memberDefinitionRange)
        |> MemberDefn.DoExpr
    | SynMemberDefn.LetBindings (bindings = bindings) ->
        BindingListNode(List.map (mkBinding creationAide) bindings, memberDefinitionRange)
        |> MemberDefn.LetBinding
    | SynMemberDefn.Interface (t, mWith, mdsOpt, _) ->
        let interfaceNode =
            match memberDefinitionRange with
            | StartRange 9 (mInterface, _) -> stn "interface" mInterface

        let members =
            match mdsOpt with
            | None -> []
            | Some mds -> List.map (mkMemberDefn creationAide) mds

        MemberDefnInterfaceNode(
            interfaceNode,
            mkType creationAide t,
            Option.map (stn "with") mWith,
            members,
            memberDefinitionRange
        )
        |> MemberDefn.Interface
    | SynMemberDefn.AutoProperty (ats,
                                  _isStatic,
                                  ident,
                                  typeOpt,
                                  mk,
                                  _,
                                  _,
                                  px,
                                  ao,
                                  e,
                                  _,
                                  { LeadingKeyword = lk
                                    EqualsRange = Some mEq
                                    WithKeyword = mWith
                                    GetSetKeyword = mGS }) ->
        MemberDefnAutoPropertyNode(
            mkXmlDoc px,
            mkAttributes creationAide ats,
            mkSynLeadingKeyword lk,
            mkSynAccess ao,
            mkIdent ident,
            Option.map (mkType creationAide) typeOpt,
            stn "=" mEq,
            mkExpr creationAide e,
            mkWithGetSet typeOpt mWith mGS mk,
            memberDefinitionRange
        )
        |> MemberDefn.AutoProperty
    | SynMemberDefn.AbstractSlot (SynValSig (ats, ident, tds, t, _, _, _, px, _ao, _, _, trivia), mf, _) ->
        MemberDefnAbstractSlotNode(
            mkXmlDoc px,
            mkAttributes creationAide ats,
            mkSynLeadingKeyword trivia.LeadingKeyword,
            mkSynIdent ident,
            mkSynValTyparDecls creationAide (Some tds),
            mkType creationAide t,
            // TODO: add to parser
            mkWithGetSet (Some t) trivia.WithKeyword None mf.MemberKind,
            memberDefinitionRange
        )
        |> MemberDefn.AbstractSlot
    | SynMemberDefn.GetSetMember (Some (SynBinding (accessibility = ao
                                                    isInline = isInline
                                                    attributes = ats
                                                    xmlDoc = px
                                                    headPat = SynPat.LongIdent (longDotId = memberName)
                                                    trivia = { LeadingKeyword = lk }) as getBinding),
                                  Some setBinding,
                                  _,
                                  { GetKeyword = Some getKeyword
                                    SetKeyword = Some setKeyword
                                    WithKeyword = withKeyword
                                    AndKeyword = andKeyword }) ->
        let firstBinding, lastBinding =
            if Position.posLt getKeyword.Start setKeyword.Start then
                mkPropertyGetSetBinding creationAide (stn "get" getKeyword) getBinding,
                Some(mkPropertyGetSetBinding creationAide (stn "set" setKeyword) setBinding)
            else
                mkPropertyGetSetBinding creationAide (stn "set" setKeyword) setBinding,
                Some(mkPropertyGetSetBinding creationAide (stn "get" getKeyword) getBinding)

        MemberDefnPropertyGetSetNode(
            mkXmlDoc px,
            mkAttributes creationAide ats,
            mkSynLeadingKeyword lk,
            isInline,
            mkSynAccess ao,
            mkSynLongIdent memberName,
            stn "with" withKeyword,
            firstBinding,
            Option.map (stn "and") andKeyword,
            lastBinding,
            memberDefinitionRange
        )
        |> MemberDefn.PropertyGetSet
    | SynMemberDefn.GetSetMember (None,
                                  Some (SynBinding (accessibility = ao
                                                    isInline = isInline
                                                    attributes = ats
                                                    xmlDoc = px
                                                    headPat = SynPat.LongIdent (longDotId = memberName)
                                                    trivia = { LeadingKeyword = lk }) as binding),
                                  _,
                                  { WithKeyword = withKeyword
                                    GetKeyword = getKeyword
                                    SetKeyword = setKeyword })
    | SynMemberDefn.GetSetMember (Some (SynBinding (accessibility = ao
                                                    isInline = isInline
                                                    attributes = ats
                                                    xmlDoc = px
                                                    headPat = SynPat.LongIdent (longDotId = memberName)
                                                    trivia = { LeadingKeyword = lk }) as binding),
                                  None,
                                  _,
                                  { WithKeyword = withKeyword
                                    GetKeyword = getKeyword
                                    SetKeyword = setKeyword }) ->

        match getKeyword, setKeyword with
        | Some getKeyword, None ->
            let bindingNode =
                mkPropertyGetSetBinding creationAide (stn "get" getKeyword) binding

            MemberDefnPropertyGetSetNode(
                mkXmlDoc px,
                mkAttributes creationAide ats,
                mkSynLeadingKeyword lk,
                isInline,
                mkSynAccess ao,
                mkSynLongIdent memberName,
                stn "with" withKeyword,
                bindingNode,
                None,
                None,
                memberDefinitionRange
            )
            |> MemberDefn.PropertyGetSet
        | None, Some setKeyword ->
            let bindingNode =
                mkPropertyGetSetBinding creationAide (stn "set" setKeyword) binding

            MemberDefnPropertyGetSetNode(
                mkXmlDoc px,
                mkAttributes creationAide ats,
                mkSynLeadingKeyword lk,
                isInline,
                mkSynAccess ao,
                mkSynLongIdent memberName,
                stn "with" withKeyword,
                bindingNode,
                None,
                None,
                memberDefinitionRange
            )
            |> MemberDefn.PropertyGetSet
        | _ -> failwith "SynMemberDefn.GetSetMember cannot exist with get and without set"
    | _ -> failwithf "Unexpected SynMemberDefn: %A" md

let mkVal
    (creationAide: CreationAide)
    (SynValSig (ats, synIdent, vtd, t, _vi, isInline, isMutable, px, ao, eo, range, trivia))
    : ValNode =
    let lk =
        match trivia.LeadingKeyword with
        | SynLeadingKeyword.New _ -> None
        | lk -> Some(mkSynLeadingKeyword lk)

    ValNode(
        mkXmlDoc px,
        mkAttributes creationAide ats,
        lk,
        isInline,
        isMutable,
        mkSynAccess ao,
        mkSynIdent synIdent,
        mkSynValTyparDecls creationAide (Some vtd),
        mkType creationAide t,
        Option.map (stn "=") trivia.EqualsRange,
        Option.map (mkExpr creationAide) eo,
        range
    )

let mkMemberSig (creationAide: CreationAide) (ms: SynMemberSig) =
    let memberSigRange = ms.Range

    match ms with
    | SynMemberSig.Member (vs, mf, _) ->
        let (SynValSig (synType = t; trivia = trivia)) = vs

        MemberDefnSigMemberNode(
            mkVal creationAide vs,
            // TODO: add getSet to trivia
            mkWithGetSet (Some t) trivia.WithKeyword None mf.MemberKind,
            memberSigRange
        )
        |> MemberDefn.SigMember
    | SynMemberSig.Interface (t, StartRange 9 (mInterface, _)) ->
        MemberDefnInterfaceNode(stn "interface" mInterface, mkType creationAide t, None, [], memberSigRange)
        |> MemberDefn.Interface

    | SynMemberSig.Inherit (t, StartRange 7 (mInherit, _)) ->
        MemberDefnInheritNode(stn "inherit" mInherit, mkType creationAide t, memberSigRange)
        |> MemberDefn.Inherit
    | SynMemberSig.ValField (f, _) -> mkSynField creationAide f |> MemberDefn.ValField
    | _ -> failwithf "Cannot construct node for %A" ms

let rec mkModuleDecls
    (creationAide: CreationAide)
    (decls: SynModuleDecl list)
    (finalContinuation: ModuleDecl list -> ModuleDecl list)
    =
    match decls with
    | [] -> finalContinuation []
    | OpenL (xs, ys) ->
        let openListNode =
            List.map (mkOpenNodeForImpl creationAide) xs
            |> OpenListNode
            |> ModuleDecl.OpenList

        mkModuleDecls creationAide ys (fun nodes -> openListNode :: nodes |> finalContinuation)

    | HashDirectiveL (xs, ys) ->
        let listNode =
            List.map (mkParsedHashDirective creationAide) xs
            |> HashDirectiveListNode
            |> ModuleDecl.HashDirectiveList

        mkModuleDecls creationAide ys (fun nodes -> listNode :: nodes |> finalContinuation)

    | SynModuleDecl.Types (typeDefns = typeDefns) :: rest ->
        let typeNodes =
            List.map (fun tdn -> mkTypeDefn creationAide tdn |> ModuleDecl.TypeDefn) typeDefns

        mkModuleDecls creationAide rest (fun nodes -> [ yield! typeNodes; yield! nodes ] |> finalContinuation)

    | SynModuleDecl.Attributes (a, _) :: SynModuleDecl.Expr (e, _) :: rest ->
        let attributes = mkAttributes creationAide a
        let expr = mkExpr creationAide e
        let range = unionRanges (attributes :> Node).Range (Expr.Node expr).Range
        let node = ModuleDeclAttributesNode(attributes, expr, range)
        mkModuleDecls creationAide rest (fun nodes -> ModuleDecl.Attributes node :: nodes |> finalContinuation)
    | head :: tail ->
        mkModuleDecls creationAide tail (fun nodes -> mkModuleDecl creationAide head :: nodes |> finalContinuation)

let mkModuleOrNamespace
    (creationAide: CreationAide)
    (SynModuleOrNamespace (longId = longId; kind = kind; decls = decls; range = range; trivia = trivia))
    =
    let leadingKeyword =
        match trivia.LeadingKeyword with
        | SynModuleOrNamespaceLeadingKeyword.Module mModule -> Some(stn "module" mModule)
        | SynModuleOrNamespaceLeadingKeyword.Namespace mNamespace -> Some(stn "namespace" mNamespace)
        | SynModuleOrNamespaceLeadingKeyword.None -> None

    let name =
        match kind with
        | SynModuleOrNamespaceKind.AnonModule -> IdentListNode.Empty
        | _ -> mkLongIdent longId

    let decls = mkModuleDecls creationAide decls id

    ModuleOrNamespaceNode(leadingKeyword, name, decls, range)

let mkImplFile
    (creationAide: CreationAide)
    (ParsedImplFileInput (hashDirectives = hashDirectives; contents = contents): ParsedImplFileInput)
    =
    let phds = List.map (mkParsedHashDirective creationAide) hashDirectives
    let mds = List.map (mkModuleOrNamespace creationAide) contents
    Oak(phds, mds)

let mkOak (config: FormatConfig) (sourceText: ISourceText option) (ast: ParsedInput) =
    let creationAide =
        { SourceText = sourceText
          Config = config }

    match ast with
    | ParsedInput.ImplFile parsedImplFileInput -> mkImplFile creationAide parsedImplFileInput
    | ParsedInput.SigFile _ -> failwith "todo 75E74A3A-C84D-4150-8D49-F111F0916839"
