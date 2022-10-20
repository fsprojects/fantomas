module rec Fantomas.Core.Fangorn

open FSharp.Compiler.Text
open FSharp.Compiler.Text.Range
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open Fantomas.Core.FormatConfig
open Fantomas.Core.ISourceTextExtensions
open Fantomas.Core.RangePatterns
open Fantomas.Core.SyntaxOak

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

let mkSynIdent (SynIdent (ident, _trivia)) = stn ident.idText ident.idRange

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

let mkSynAccess (vis: SynAccess) =
    match vis with
    | SynAccess.Internal range -> stn "internal" range
    | SynAccess.Private range -> stn "private" range
    | SynAccess.Public range -> stn "public" range

let parseExpressionInSynBinding returnInfo expr =
    match returnInfo, expr with
    | Some (SynBindingReturnInfo (typeName = t1)), SynExpr.Typed (e, t2, _) when RangeHelpers.rangeEq t1.Range t2.Range ->
        e
    | _ -> expr

let mkParsedHashDirective (creationAide: CreationAide) (ParsedHashDirective (ident, args, range)) =
    let args =
        args
        |> List.map (function
            | ParsedHashDirectiveArgument.String (value, stringKind, range) ->
                let fallback =
                    match stringKind with
                    | SynStringKind.Regular -> sprintf "\"%s\"" value
                    | SynStringKind.Verbatim -> sprintf "@\"%s\"" value
                    | SynStringKind.TripleQuote -> sprintf "\"\"\"%s\"\"\"" value

                stn (creationAide.TextFromSource fallback range) range
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
    | SynConst.String (s, kind, r) -> failwith "todo, C1CDBFC9-1B5D-471C-8189-CEC3A676D386"
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

let mkAttributes (creationAide: CreationAide) (al: SynAttributeList) =
    AttributesNode(List.map (mkAttribute creationAide) al.Attributes, al.Range)

let mkAttributeList (creationAide: CreationAide) (ats: SynAttributes) =
    let attributes = List.map (mkAttributes creationAide) ats
    let range = attributes |> List.map (fun a -> (a :> Node).Range) |> combineRanges
    AttributesListNode(attributes, range)

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
    | SynExpr.New (_, t, (SynExpr.Paren _ as px), StartRange 3 (newRange, _))
    | SynExpr.New (_, t, (SynExpr.Const (SynConst.Unit, _) as px), StartRange 3 (newRange, _)) ->
        ExprNewParenNode(stn "new" newRange, mkType creationAide t, mkExpr creationAide px, exprRange)
        |> Expr.NewParen
    // | Expr.New _ -> failwith "Not Implemented"
    // | Expr.Tuple _ -> failwith "Not Implemented"
    // | Expr.StructTuple _ -> failwith "Not Implemented"
    // | Expr.ArrayOrList _ -> failwith "Not Implemented"
    // | Expr.Record _ -> failwith "Not Implemented"
    // | Expr.AnonRecord _ -> failwith "Not Implemented"
    // | Expr.ObjExpr _ -> failwith "Not Implemented"
    // | Expr.While _ -> failwith "Not Implemented"
    // | Expr.For _ -> failwith "Not Implemented"
    // | Expr.ForEach _ -> failwith "Not Implemented"
    // | Expr.NamedComputation _ -> failwith "Not Implemented"
    // | Expr.Computation _ -> failwith "Not Implemented"
    // | Expr.CompExprBody _ -> failwith "Not Implemented"
    // | Expr.JoinIn _ -> failwith "Not Implemented"
    // | Expr.ParenLambda _ -> failwith "Not Implemented"
    // | Expr.Lambda _ -> failwith "Not Implemented"
    // | Expr.MatchLambda _ -> failwith "Not Implemented"
    // | Expr.Match _ -> failwith "Not Implemented"
    // | Expr.TraitCall _ -> failwith "Not Implemented"
    // | Expr.ParenILEmbedded _ -> failwith "Not Implemented"
    // | Expr.ParenFunctionNameWithStar _ -> failwith "Not Implemented"
    | SynExpr.Paren (e, lpr, Some rpr, _) ->
        ExprParenNode(stn "(" lpr, mkExpr creationAide e, stn ")" rpr, exprRange)
        |> Expr.Paren
    // | Expr.Paren _ -> failwith "Not Implemented"
    // | Expr.Dynamic _ -> failwith "Not Implemented"
    // | Expr.PrefixApp _ -> failwith "Not Implemented"
    // | Expr.NewlineInfixAppAlwaysMultiline _ -> failwith "Not Implemented"
    // | Expr.NewlineInfixApps _ -> failwith "Not Implemented"
    // | Expr.SameInfixApps _ -> failwith "Not Implemented"
    // | Expr.TernaryApp _ -> failwith "Not Implemented"
    // | Expr.IndexWithoutDot _ -> failwith "Not Implemented"
    // | Expr.AppDotGetTypeApp _ -> failwith "Not Implemented"
    // | Expr.DotGetAppDotGetAppParenLambda _ -> failwith "Not Implemented"
    // | Expr.DotGetAppParen _ -> failwith "Not Implemented"
    // | Expr.DotGetAppWithParenLambda _ -> failwith "Not Implemented"
    // | Expr.DotGetApp _ -> failwith "Not Implemented"
    // | Expr.AppLongIdentAndSingleParenArg _ -> failwith "Not Implemented"
    // | Expr.AppSingleParenArg _ -> failwith "Not Implemented"
    // | Expr.DotGetAppWithLambda _ -> failwith "Not Implemented"
    // | Expr.AppWithLambda _ -> failwith "Not Implemented"
    // | Expr.NestedIndexWithoutDot _ -> failwith "Not Implemented"
    // | Expr.EndsWithDualListApp _ -> failwith "Not Implemented"
    // | Expr.EndsWithSingleListApp _ -> failwith "Not Implemented"
    // | Expr.App _ -> failwith "Not Implemented"
    // | Expr.TypeApp _ -> failwith "Not Implemented"
    // | Expr.LetOrUses _ -> failwith "Not Implemented"
    // | Expr.TryWithSingleClause _ -> failwith "Not Implemented"
    // | Expr.TryWith _ -> failwith "Not Implemented"
    // | Expr.TryFinally _ -> failwith "Not Implemented"
    // | Expr.Sequentials _ -> failwith "Not Implemented"
    // | Expr.IfThen _ -> failwith "Not Implemented"
    // | Expr.IfThenElse _ -> failwith "Not Implemented"
    // | Expr.IfThenElif _ -> failwith "Not Implemented"
    | SynExpr.Ident ident -> mkIdent ident |> Expr.Ident
    // | Expr.OptVar _ -> failwith "Not Implemented"
    // | Expr.LongIdentSet _ -> failwith "Not Implemented"
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
    | _ -> failwith "todo, 693F570D-5A08-4E44-8937-FF98CE0AD8FC"

let mkExprQuote creationAide isRaw e range : ExprQuoteNode =
    let startToken, endToken =
        let sText, length, eText = if isRaw then "<@@", 3, "@@>" else "<@", 2, "@>"

        match range with
        | StartEndRange length (startRange, _, endRange) -> stn sText startRange, stn eText endRange

    ExprQuoteNode(startToken, mkExpr creationAide e, endToken, range)

let mkPat (creationAide: CreationAide) (p: SynPat) =
    let patternRange = p.Range

    match p with
    | SynPat.OptionalVal (ident, _) -> stn $"?{ident.idText}" patternRange |> Pattern.OptionalVal
    | SynPat.Attrib (p, ats, _) ->
        PatAttribNode(mkAttributeList creationAide ats, mkPat creationAide p, patternRange)
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
    | SynPat.Named (ident = ident) -> PatNamedNode(mkSynIdent ident, patternRange) |> Pattern.Named
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
        let typarDecls =
            Option.bind (fun (SynValTyparDecls (tds, _)) -> Option.bind (mkTyparDecls creationAide) tds) vtdo

        let pairs =
            nps
            |> List.map (fun (ident, eq, pat) ->
                NamePatPair(mkIdent ident, stn "=" eq, mkPat creationAide pat, unionRanges ident.idRange pat.Range))

        PatNamePatPairsNode(mkSynLongIdent synLongIdent, typarDecls, stn "(" lpr, pairs, stn ")" rpr, patternRange)
        |> Pattern.NamePatPairs
    | SynPat.LongIdent (synLongIdent, _, vtdo, SynArgPats.Pats pats, ao, _) ->
        let typarDecls =
            Option.bind (fun (SynValTyparDecls (tds, _)) -> Option.bind (mkTyparDecls creationAide) tds) vtdo

        PatLongIdentNode(
            Option.map mkSynAccess ao,
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
        let openToken, closeToken =
            let size = if isArray then 2 else 1

            match range with
            | StartEndRange size (o, _, c) ->
                let openText = if isArray then "[|" else "["
                let closeText = if isArray then "|]" else "]"
                stn openText o, stn closeText c

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

let mkBinding
    (creationAide: CreationAide)
    (SynBinding (_ao, _, _isInline, _isMutable, _attrs, _px, _, pat, returnInfo, expr, _, _, trivia))
    =
    let leadingKeyword =
        match trivia.LeadingKeyword with
        | SynLeadingKeyword.Let m -> stn "let" m
        | _ -> failwith "todo, FF881966-836F-4425-A600-8C928DE4CDE1"

    let functionName, parameters =
        match pat with
        | SynPat.LongIdent (longDotId = SynLongIdent ([ _ ], _, _) as lid; argPats = SynArgPats.Pats ps) ->
            Choice1Of2(mkSynIdent lid.IdentsWithTrivia.[0]), List.map (mkPat creationAide) ps
        | _ -> Choice2Of2(mkPat creationAide pat), []

    let equals = stn "=" trivia.EqualsRange.Value

    let e = parseExpressionInSynBinding returnInfo expr

    let returnTypeNodes =
        Option.bind
            (fun (SynBindingReturnInfo (typeName = t; trivia = trivia)) ->
                trivia.ColonRange
                |> Option.map (fun mColon -> stn ":" mColon, mkType creationAide t))
            returnInfo

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

    BindingNode(leadingKeyword, functionName, parameters, returnTypeNodes, equals, (mkExpr creationAide e), range)

let mkModuleDecl (creationAide: CreationAide) (decl: SynModuleDecl) =
    match decl with
    // | OpenList of OpenListNode
    // | HashDirectiveList of HashDirectiveListNode
    // | AttributesList of AttributesListNode
    | SynModuleDecl.Expr (e, _) -> mkExpr creationAide e |> ModuleDecl.DeclExpr
    // | ExternBinding of ExternBindingNode
    | SynModuleDecl.Let(bindings = [ singleBinding ]) ->
        mkBinding creationAide singleBinding |> ModuleDecl.TopLevelBinding
    // | ModuleAbbrev of ModuleAbbrevNode
    // | NestedModule of ModuleOrNamespaceNode
    // | TypeDefn of TypeDefn
    | _ -> failwith "todo, 068F312B-A840-4E14-AF82-A000652532E8"

let mkTyparDecls (creationAide: CreationAide) (tds: SynTyparDecls) : TyparDecls option =
    match tds with
    | SynTyparDecls.PostfixList _
    | SynTyparDecls.PrefixList _
    | SynTyparDecls.SinglePrefix _ -> None

let mkSynRationalConst rc =
    let rec visit rc =
        match rc with
        | SynRationalConst.Integer i -> string i
        | SynRationalConst.Rational (numerator, denominator, _) -> $"(%i{numerator}/%i{denominator})"
        | SynRationalConst.Negate innerRc -> $"-{visit innerRc}"

    visit rc

let mkSynTypar (SynTypar (ident, req, _)) range =
    match req with
    | TyparStaticReq.None -> stn $"'{ident}" range
    | TyparStaticReq.HeadType -> stn $"^{ident.idText}" range

// Arrow type is right-associative
let rec (|TFuns|_|) =
    function
    | SynType.Fun (t1, TFuns (ts, ret), _, trivia) -> Some((t1, trivia.ArrowRange) :: ts, ret)
    | SynType.Fun (t1, t2, _, trivia) -> Some([ t1, trivia.ArrowRange ], t2)
    | _ -> None

let mkType (creationAide: CreationAide) (t: SynType) : Type =
    let typeRange = t.Range

    match t with
    | TFuns (ts, rt) ->
        let parameters =
            ts |> List.map (fun (t, mArrow) -> mkType creationAide t, stn "->" mArrow)

        TypeFunsNode(parameters, mkType creationAide rt, typeRange) |> Type.Funs
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
    // | StaticConstantNamed of TypeStaticConstantNamedNode
    | SynType.Array (rank, t, _) -> TypeArrayNode(mkType creationAide t, rank, typeRange) |> Type.Array
    | SynType.Anon _ -> stn "_" typeRange |> Type.Anon
    | SynType.Var (tp, r) -> mkSynTypar tp r |> Type.Var
    // | Var of TypeVarNode
    // | App of TypeAppNode
    // | LongIdentApp of TypeLongIdentAppNode
    // | WithGlobalConstraints of TypeWithGlobalConstraintsNode
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
    // | SignatureParameter of TypeSignatureParameterNode
    // | Or of TypeOrNode
    | _ -> failwith "todo, F28E0FA1-7C39-4BFF-AFBF-0E9FD3D1D4E4"

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

let mkTypeDefn
    (creationAide: CreationAide)
    (isFirst: bool)
    (SynTypeDefn (typeInfo, typeRepr, members, implicitConstructor, range, trivia))
    : TypeDefn =

    let typeNameNode =
        match typeInfo with
        | SynComponentInfo (ats, tds, tcs, lid, px, preferPostfix, ao, _) ->
            let identifierNode = mkLongIdent lid

            let leadingKeyword =
                match trivia.LeadingKeyword with
                | SynTypeDefnLeadingKeyword.Type mType -> stn "type" mType
                | SynTypeDefnLeadingKeyword.And mAnd -> stn "and" mAnd
                | SynTypeDefnLeadingKeyword.StaticType _
                | SynTypeDefnLeadingKeyword.Synthetic _ -> failwithf "unexpected %A" trivia.LeadingKeyword

            TypeNameNode(
                AttributesListNode.Empty,
                leadingKeyword,
                isFirst,
                None,
                identifierNode,
                None,
                Option.map (stn "=") trivia.EqualsRange,
                None,
                unionRanges (leadingKeyword :> Node).Range (identifierNode :> Node).Range
            )

    match typeRepr with
    // | Simple (TDSREnum ecs) ->
    // | Simple (TDSRUnion (ao', xs)) ->
    // | Simple (TDSRRecord (openingBrace, ao', fs, closingBrace)) ->
    // | Simple TDSRNone -> typeName
    | SynTypeDefnRepr.Simple(simpleRepr = SynTypeDefnSimpleRepr.TypeAbbrev (rhsType = t)) ->
        TypeDefn.Abbrev(TypeDefnAbbrevNode(typeNameNode, mkType creationAide t, range))
    // | Simple (TDSRException (ExceptionDefRepr (ats, px, ao, uc)))
    // | ObjectModel (TCSimple (TCInterface | TCClass) as tdk, MemberDefnList (impCtor, others), range) ->
    // | ObjectModel (TCSimple TCStruct as tdk, MemberDefnList (impCtor, others), _) ->
    // | ObjectModel (TCSimple (TCAugmentation withKeywordAug), _, _) ->
    // | ObjectModel (TCDelegate (FunType ts), _, _) ->
    // | ObjectModel (TCSimple TCUnspecified, MemberDefnList (impCtor, others), _) when not (List.isEmpty ms) ->
    // | ObjectModel (_, MemberDefnList (impCtor, others), _) ->
    // | ExceptionRepr (ExceptionDefRepr (ats, px, ao, uc)) -> genExceptionBody ats px ao uc
    | _ -> failwith "not implemented, C8C6C667-6A67-46A6-9EE3-A0DF663A3A91"

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

        mkModuleDecls creationAide ys (fun nodes -> openListNode :: nodes)

    | SynModuleDecl.Types (typeDefns = typeDefns) :: rest ->
        let typeNodes =
            List.mapi (fun idx tdn -> mkTypeDefn creationAide (idx = 0) tdn |> ModuleDecl.TypeDefn) typeDefns

        mkModuleDecls creationAide rest (fun nodes -> [ yield! typeNodes; yield! nodes ] |> finalContinuation)
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
    (ParsedImplFileInput (hashDirectives = hashDirectives; contents = contents))
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
