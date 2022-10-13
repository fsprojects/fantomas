module internal Fantomas.Core.SourceParser

open System
open FSharp.Compiler.Syntax
open FSharp.Compiler.Syntax.PrettyNaming
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Text
open FSharp.Compiler.Xml
open Fantomas.Core
open Fantomas.Core.AstExtensions
open Fantomas.Core.TriviaTypes
open Fantomas.Core.RangePatterns

/// Always break into newlines on these operators
let internal newLineInfixOps = set [ "|>"; "||>"; "|||>"; ">>"; ">>=" ]

/// Never break into newlines on these operators
let internal noBreakInfixOps = set [ "="; ">"; "<"; "%" ]

// Type params

let inline (|Typar|) (SynTypar.SynTypar (ident, req, _)) =
    match req with
    | TyparStaticReq.None -> (ident, false)
    | TyparStaticReq.HeadType -> (ident, true)

let inline (|ValTyparDecls|) (SynValTyparDecls (tds, b)) = (tds, b)

// Literals

let rec (|RationalConst|) =
    function
    | SynRationalConst.Integer i -> string i
    | SynRationalConst.Rational (numerator, denominator, _) -> sprintf "(%i/%i)" numerator denominator
    | SynRationalConst.Negate (RationalConst s) -> sprintf "-%s" s

let (|Unit|_|) =
    function
    | SynConst.Unit _ -> Some()
    | _ -> None

// File level patterns

let (|ImplFile|SigFile|) =
    function
    | ParsedInput.ImplFile im -> ImplFile im
    | ParsedInput.SigFile si -> SigFile si

let (|ParsedImplFileInput|)
    (ParsedImplFileInput.ParsedImplFileInput (hashDirectives = hs
                                              contents = mns
                                              trivia = { ConditionalDirectives = directives
                                                         CodeComments = codeComments }))
    =
    (hs, mns, directives, codeComments)

let (|ParsedSigFileInput|)
    (ParsedSigFileInput.ParsedSigFileInput (hashDirectives = hs
                                            contents = mns
                                            trivia = { ConditionalDirectives = directives
                                                       CodeComments = codeComments }))
    =
    (hs, mns, directives, codeComments)

let (|ModuleOrNamespace|)
    (SynModuleOrNamespace.SynModuleOrNamespace (lids, isRecursive, kind, mds, px, ats, ao, _range, trivia) as m)
    =
    (ats, px, trivia.ModuleKeyword, trivia.NamespaceKeyword, ao, lids, mds, isRecursive, kind, m.FullRange)

let (|SigModuleOrNamespace|)
    (SynModuleOrNamespaceSig.SynModuleOrNamespaceSig (lids, isRecursive, kind, mds, px, ats, ao, _range, trivia) as m)
    =
    (ats, px, trivia.ModuleKeyword, trivia.NamespaceKeyword, ao, lids, mds, isRecursive, kind, m.FullRange)

let (|Attribute|) (a: SynAttribute) = (a.TypeName, a.ArgExpr, a.Target)

let (|PreXmlDoc|) (px: PreXmlDoc) =
    let xmlDoc = px.ToXmlDoc(false, None)
    xmlDoc.UnprocessedLines, xmlDoc.Range

// Module declarations (11 cases)
let (|Open|_|) =
    function
    | SynModuleDecl.Open (SynOpenDeclTarget.ModuleOrNamespace (lid, _m), _) -> Some lid
    | _ -> None

let (|OpenType|_|) =
    function
    | SynModuleDecl.Open (SynOpenDeclTarget.Type (SynType.LongIdent synLid, _m), _) -> Some synLid
    | _ -> None

let (|ModuleAbbrev|_|) =
    function
    | SynModuleDecl.ModuleAbbrev (ident, lid, _) -> Some(ident, lid)
    | _ -> None

let (|HashDirective|_|) =
    function
    | SynModuleDecl.HashDirective (p, _) -> Some p
    | _ -> None

let (|NamespaceFragment|_|) =
    function
    | SynModuleDecl.NamespaceFragment m -> Some m
    | _ -> None

let (|Attributes|_|) =
    function
    | SynModuleDecl.Attributes (ats, _) -> Some(ats)
    | _ -> None

let (|Let|_|) =
    function
    | SynModuleDecl.Let (false, [ x ], _) -> Some x
    | _ -> None

let (|LetRec|_|) =
    function
    | SynModuleDecl.Let (true, xs, _) -> Some xs
    | _ -> None

let (|DeclExpr|_|) =
    function
    | SynModuleDecl.Expr (x, _) -> Some x
    | _ -> None

let (|Types|_|) =
    function
    | SynModuleDecl.Types (xs, _) -> Some xs
    | _ -> None

let (|NestedModule|_|) =
    function
    | SynModuleDecl.NestedModule (SynComponentInfo (ats, _, _, lid, px, _, ao, _), isRecursive, xs, _, _, trivia) ->
        Some(ats, px, trivia.ModuleKeyword, ao, lid, isRecursive, trivia.EqualsRange, xs)
    | _ -> None

let (|Exception|_|) =
    function
    | SynModuleDecl.Exception (ed, _) -> Some ed
    | _ -> None

// Module declaration signatures (9 cases)

let (|SigOpen|_|) =
    function
    | SynModuleSigDecl.Open (SynOpenDeclTarget.ModuleOrNamespace (lid, _), _) -> Some lid
    | _ -> None

let (|SigOpenType|_|) =
    function
    | SynModuleSigDecl.Open (SynOpenDeclTarget.Type (SynType.LongIdent synLongIdent, _), _) -> Some synLongIdent
    | _ -> None

let (|SigModuleAbbrev|_|) =
    function
    | SynModuleSigDecl.ModuleAbbrev (ident, lid, _) -> Some(ident, lid)
    | _ -> None

let (|SigHashDirective|_|) =
    function
    | SynModuleSigDecl.HashDirective (p, _) -> Some p
    | _ -> None

let (|SigNamespaceFragment|_|) =
    function
    | SynModuleSigDecl.NamespaceFragment m -> Some m
    | _ -> None

let (|SigVal|_|) =
    function
    | SynModuleSigDecl.Val (v, _) -> Some v
    | _ -> None

let (|SigTypes|_|) =
    function
    | SynModuleSigDecl.Types (tds, _) -> Some tds
    | _ -> None

let (|SigNestedModule|_|) =
    function
    | SynModuleSigDecl.NestedModule (SynComponentInfo (ats, _, _, lid, px, _, ao, _), _, xs, _, trivia) ->
        Some(ats, px, trivia.ModuleKeyword, ao, lid, trivia.EqualsRange, xs)
    | _ -> None

let (|SigException|_|) =
    function
    | SynModuleSigDecl.Exception (es, _) -> Some es
    | _ -> None

// Exception definitions

let (|ExceptionDefRepr|) (SynExceptionDefnRepr.SynExceptionDefnRepr (ats, uc, _, px, ao, _)) = (ats, px, ao, uc)

let (|SigExceptionDefRepr|) (SynExceptionDefnRepr.SynExceptionDefnRepr (ats, uc, _, px, ao, _)) = (ats, px, ao, uc)

let (|ExceptionDef|)
    (SynExceptionDefn.SynExceptionDefn (SynExceptionDefnRepr.SynExceptionDefnRepr (ats, uc, _, px, ao, _),
                                        withKeyword,
                                        ms,
                                        _))
    =
    (ats, px, ao, uc, withKeyword, ms)

let (|SigExceptionDef|)
    (SynExceptionSig.SynExceptionSig (SynExceptionDefnRepr.SynExceptionDefnRepr (ats, uc, _, px, ao, _),
                                      withKeyword,
                                      ms,
                                      _))
    =
    (ats, px, ao, uc, withKeyword, ms)

let (|UnionCase|) (SynUnionCase (ats, ident, uct, px, ao, r, trivia)) =
    let fullRange =
        if not px.IsEmpty then
            r
        else
            match trivia.BarRange with
            | None -> r
            | Some barRange -> Range.unionRanges barRange r

    (ats, px, trivia.BarRange, ao, ident, uct, fullRange)

let (|UnionCaseType|) =
    function
    | SynUnionCaseKind.Fields fs -> fs
    | SynUnionCaseKind.FullType _ -> failwith "UnionCaseFullType should be used internally only."

let (|Field|) (SynField (ats, _isStatic, ido, t, isMutable, px, ao, range, { LeadingKeyword = lk })) =
    let innerRange = ido |> Option.map (fun i -> Range.unionRanges i.idRange t.Range)
    (ats, px, lk, ao, isMutable, t, ido, innerRange, range)

let (|EnumCase|) (SynEnumCase (ats, ident, c, cr, px, r, trivia)) =
    let fullRange =
        if not px.IsEmpty then
            r
        else
            match trivia.BarRange with
            | None -> r
            | Some barRange -> Range.unionRanges barRange r

    (ats, trivia.BarRange, px, ident, trivia.EqualsRange, c, cr, fullRange)

// Member definitions (11 cases)

let (|MDNestedType|_|) =
    function
    | SynMemberDefn.NestedType (td, ao, _) -> Some(td, ao)
    | _ -> None

let (|MDOpen|_|) =
    function
    | SynMemberDefn.Open (SynOpenDeclTarget.ModuleOrNamespace (lid, _), _) -> Some lid
    | _ -> None

let (|MDOpenType|_|) =
    function
    | SynMemberDefn.Open (SynOpenDeclTarget.Type (SynType.LongIdent synLongIdent, _), _) -> Some synLongIdent
    | _ -> None

let (|MDImplicitInherit|_|) =
    function
    | SynMemberDefn.ImplicitInherit (t, e, ido, _) -> Some(t, e, ido)
    | _ -> None

let (|MDInherit|_|) =
    function
    | SynMemberDefn.Inherit (t, ido, _) -> Some(t, ido)
    | _ -> None

let (|MDValField|_|) =
    function
    | SynMemberDefn.ValField (f, _) -> Some f
    | _ -> None

let (|MDImplicitCtor|_|) =
    function
    | SynMemberDefn.ImplicitCtor (ao, ats, ps, ido, docs, _) -> Some(docs, ats, ao, ps, ido)
    | _ -> None

let (|MDMember|_|) =
    function
    | SynMemberDefn.Member (b, _) -> Some b
    | _ -> None

let (|MDLetBindings|_|) =
    function
    | SynMemberDefn.LetBindings (bindings = es) -> Some es
    | _ -> None

let mkWithGetSet t withKeyword memberKind =
    let isFunctionProperty =
        match t with
        | Some (SynType.Fun _) -> true
        | _ -> false

    withKeyword
    |> Option.map (fun _ ->
        match memberKind with
        | SynMemberKind.PropertyGet -> if not isFunctionProperty then "" else " with get"
        | SynMemberKind.PropertySet -> " with set"
        | SynMemberKind.PropertyGetSet -> " with get, set"
        | _ -> "")

let (|MDAbstractSlot|_|) =
    function
    | SynMemberDefn.AbstractSlot (SynValSig (ats, ident, tds, t, _, _, _, px, ao, _, _, trivia), mf, _) ->
        Some(ats, px, ao, trivia.LeadingKeyword, ident, t, tds, mkWithGetSet (Some t) trivia.WithKeyword mf.MemberKind)
    | _ -> None

let (|MDInterface|_|) =
    function
    | SynMemberDefn.Interface (t, withKeyword, mdo, range) -> Some(t, withKeyword, mdo, range)
    | _ -> None

let (|MDAutoProperty|_|) =
    function
    | SynMemberDefn.AutoProperty (ats, isStatic, ident, typeOpt, mk, _, _, px, ao, e, _, trivia) ->
        Some(
            ats,
            px,
            ao,
            trivia.LeadingKeyword,
            trivia.EqualsRange,
            e,
            mkWithGetSet typeOpt trivia.WithKeyword mk,
            ident,
            isStatic,
            typeOpt
        )
    | _ -> None

let (|MDPropertyGetSet|_|) =
    function
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
        let firstBinding, secondBinding =
            if Position.posLt getKeyword.Start setKeyword.Start then
                (SynMemberDefn_GetSetMember_Get, getKeyword, getBinding),
                Some(SynMemberDefn_GetSetMember_Set, setKeyword, setBinding)
            else
                (SynMemberDefn_GetSetMember_Set, setKeyword, setBinding),
                Some(SynMemberDefn_GetSetMember_Get, getKeyword, getBinding)

        Some(px, ats, lk, isInline, ao, memberName, withKeyword, firstBinding, andKeyword, secondBinding)
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
            Some(
                px,
                ats,
                lk,
                isInline,
                ao,
                memberName,
                withKeyword,
                (SynMemberDefn_GetSetMember_Get, getKeyword, binding),
                None,
                None
            )
        | None, Some setKeyword ->
            Some(
                px,
                ats,
                lk,
                isInline,
                ao,
                memberName,
                withKeyword,
                (SynMemberDefn_GetSetMember_Set, setKeyword, binding),
                None,
                None
            )
        | _ -> None

    | _ -> None

// Interface impl

let (|InterfaceImpl|) (SynInterfaceImpl (t, withKeywordRange, bs, members, range)) =
    (t, withKeywordRange, bs, members, range)

// Bindings

let (|PropertyGet|_|) =
    function
    | SynMemberKind.PropertyGet -> Some()
    | _ -> None

let (|PropertySet|_|) =
    function
    | SynMemberKind.PropertySet -> Some()
    | _ -> None

let (|PropertyGetSet|_|) =
    function
    | SynMemberKind.PropertyGetSet -> Some()
    | _ -> None

let (|MFProperty|_|) (mf: SynMemberFlags) =
    match mf.MemberKind with
    | SynMemberKind.PropertyGet
    | SynMemberKind.PropertySet
    | SynMemberKind.PropertyGetSet as mk -> Some mk
    | _ -> None

/// This pattern finds out which keyword to use
let (|MFMember|MFStaticMember|MFConstructor|MFOverride|) (mf: SynMemberFlags) =
    match mf.MemberKind with
    | SynMemberKind.ClassConstructor
    | SynMemberKind.Constructor -> MFConstructor()
    | SynMemberKind.Member
    | SynMemberKind.PropertyGet
    | SynMemberKind.PropertySet
    | SynMemberKind.PropertyGetSet as mk ->
        if mf.IsInstance && mf.IsOverrideOrExplicitImpl then
            MFOverride mk
        elif mf.IsInstance then
            MFMember mk
        else
            MFStaticMember mk

let parseExpressionInSynBinding returnInfo expr =
    match returnInfo, expr with
    | Some (SynBindingReturnInfo (typeName = t1)), SynExpr.Typed (e, t2, _) when RangeHelpers.rangeEq t1.Range t2.Range ->
        e
    | _ -> expr

let (|Binding|) (SynBinding (ao, _, isInline, isMutable, attrs, px, _, pat, returnInfo, expr, _, _, trivia) as b) =
    let e = parseExpressionInSynBinding returnInfo expr
    let rt = Option.map (fun (SynBindingReturnInfo (typeName = t)) -> t) returnInfo
    attrs, px, trivia.LeadingKeyword, ao, isInline, isMutable, pat, rt, trivia.EqualsRange, e, b.FullRange

let (|ExplicitCtor|_|) =
    function
    | SynBinding (ao, _, _, _, ats, px, SynValData (Some MFConstructor, _, ido), pat, _, expr, _, _, trivia) ->
        Some(ats, px, ao, pat, trivia.EqualsRange, expr, ido)
    | _ -> None

let (|DoBinding|_|) =
    function
    | SynBinding (_, SynBindingKind.Do, _, _, ats, px, _, _, _, expr, _, _, trivia) ->
        Some(ats, px, trivia.LeadingKeyword, expr)
    | _ -> None

let (|ExternBinding|_|) =
    function
    | SynBinding (accessibility = ao
                  attributes = attrs
                  xmlDoc = px
                  headPat = pat
                  returnInfo = returnInfo
                  trivia = { LeadingKeyword = SynLeadingKeyword.Extern _ as lk }) ->
        let rt =
            Option.map (fun (SynBindingReturnInfo (typeName = t; attributes = a)) -> a, t) returnInfo

        Some(lk, ao, attrs, px, pat, rt)
    | _ -> None

// Expressions (55 cases, lacking to handle 11 cases)

let (|TraitCall|_|) =
    function
    | SynExpr.TraitCall (tps, msg, expr, _) -> Some(tps, msg, expr)
    | _ -> None

/// isRaw = true with <@@ and @@>
let (|Quote|_|) =
    function
    | SynExpr.Quote (e1, isRaw, e2, _, _) -> Some(e1, e2, isRaw)
    | _ -> None

let (|Paren|_|) =
    function
    | SynExpr.Paren (e, lpr, rpr, r) -> Some(lpr, e, rpr, r)
    | _ -> None

let (|LazyExpr|_|) (e: SynExpr) =
    match e with
    | SynExpr.Lazy (e, StartRange 4 (lazyKeyword, _range)) -> Some(lazyKeyword, e)
    | _ -> None

type ExprKind =
    | InferredDowncast of keyword: range
    | InferredUpcast of keyword: range
    | Assert of keyword: range
    | AddressOfSingle of token: range
    | AddressOfDouble of token: range
    | Yield of keyword: range
    | Return of keyword: range
    | YieldFrom of keyword: range
    | ReturnFrom of keyword: range
    | Do of keyword: range
    | DoBang of Keyword: range
    | Fixed of keyword: range

let (|SingleExpr|_|) =
    function
    | SynExpr.InferredDowncast (e, StartRange 8 (downcastKeyword, _range)) -> Some(InferredDowncast downcastKeyword, e)
    | SynExpr.InferredUpcast (e, StartRange 6 (upcastKeyword, _range)) -> Some(InferredUpcast upcastKeyword, e)
    | SynExpr.Assert (e, StartRange 6 (assertKeyword, _range)) -> Some(Assert assertKeyword, e)
    | SynExpr.AddressOf (true, e, _, StartRange 1 (ampersandToken, _range)) -> Some(AddressOfSingle ampersandToken, e)
    | SynExpr.AddressOf (false, e, _, StartRange 2 (ampersandToken, _range)) -> Some(AddressOfDouble ampersandToken, e)
    | SynExpr.YieldOrReturn ((true, _), e, StartRange 5 (yieldKeyword, _range)) -> Some(Yield yieldKeyword, e)
    | SynExpr.YieldOrReturn ((false, _), e, StartRange 6 (returnKeyword, _range)) -> Some(Return returnKeyword, e)
    | SynExpr.YieldOrReturnFrom ((true, _), e, StartRange 6 (yieldBangKeyword, _range)) ->
        Some(YieldFrom yieldBangKeyword, e)
    | SynExpr.YieldOrReturnFrom ((false, _), e, StartRange 7 (returnBangKeyword, _range)) ->
        Some(ReturnFrom returnBangKeyword, e)
    | SynExpr.Do (e, StartRange 2 (doKeyword, _range)) -> Some(Do doKeyword, e)
    | SynExpr.DoBang (e, StartRange 3 (doBangKeyword, _range)) -> Some(DoBang doBangKeyword, e)
    | SynExpr.Fixed (e, StartRange 5 (fixedKeyword, _range)) -> Some(Fixed fixedKeyword, e)
    | _ -> None

type TypedExprKind =
    | TypeTest
    | Downcast
    | Upcast
    | Typed

let (|TypedExpr|_|) =
    function
    | SynExpr.TypeTest (e, t, _) -> Some(TypeTest, e, t)
    | SynExpr.Downcast (e, t, _) -> Some(Downcast, e, t)
    | SynExpr.Upcast (e, t, _) -> Some(Upcast, e, t)
    | SynExpr.Typed (e, t, _) -> Some(Typed, e, t)
    | _ -> None

let (|While|_|) =
    function
    | SynExpr.While (_, e1, e2, _) -> Some(e1, e2)
    | _ -> None

let (|For|_|) =
    function
    | SynExpr.For (_, _, ident, equalsRange, e1, isUp, e2, e3, _) -> Some(ident, equalsRange, e1, e2, e3, isUp)
    | _ -> None

let (|NullExpr|_|) =
    function
    | SynExpr.Null _ -> Some()
    | _ -> None

let (|ConstExpr|_|) =
    function
    | SynExpr.Const (x, r) -> Some(x, r)
    | _ -> None

let (|ConstUnitExpr|_|) =
    function
    | ConstExpr (Unit, _) -> Some()
    | _ -> None

let (|TypeApp|_|) =
    function
    | SynExpr.TypeApp (e, lessRange, ts, _, Some greaterRange, _, _range) -> Some(e, lessRange, ts, greaterRange)
    | _ -> None

let (|Match|_|) =
    function
    | SynExpr.Match (_, e, cs, _, trivia) -> Some(trivia.MatchKeyword, e, trivia.WithKeyword, cs)
    | _ -> None

let (|MatchBang|_|) =
    function
    | SynExpr.MatchBang (_, e, cs, _, trivia) -> Some(trivia.MatchBangKeyword, e, trivia.WithKeyword, cs)
    | _ -> None

let (|Sequential|_|) =
    function
    | SynExpr.Sequential (_, isSeq, e1, e2, _) -> Some(e1, e2, isSeq)
    | _ -> None

let (|Sequentials|_|) e =
    let rec visit (e: SynExpr) (finalContinuation: SynExpr list -> SynExpr list) : SynExpr list =
        match e with
        | Sequential (e1, e2, _) -> visit e2 (fun xs -> e1 :: xs |> finalContinuation)
        | e -> finalContinuation [ e ]

    match e with
    | Sequential (e1, e2, _) ->
        let xs = visit e2 id
        Some(e1 :: xs)
    | _ -> None

let (|SimpleExpr|_|) =
    function
    | SynExpr.Null _
    | SynExpr.Ident _
    | SynExpr.LongIdent _
    | SynExpr.Const _ as e -> Some e
    | _ -> None

let (|ComputationExpr|_|) =
    function
    | SynExpr.ComputationExpr (_, expr, StartEndRange 1 (openingBrace, _range, closingBrace)) ->
        Some(openingBrace, expr, closingBrace)
    | _ -> None

// seq { expr }
let (|NamedComputationExpr|_|) =
    function
    | SynExpr.App (ExprAtomicFlag.NonAtomic,
                   false,
                   (SynExpr.App _ | SynExpr.TypeApp _ as nameExpr | SimpleExpr nameExpr),
                   (ComputationExpr (openingBrace, bodyExpr, closingBrace) as compExpr),
                   _) -> Some(nameExpr, openingBrace, bodyExpr, closingBrace, compExpr.Range)
    | _ -> None

/// Combines all ArrayOrList patterns
let (|ArrayOrList|_|) =
    let (|Size|) isArray = if isArray then 2 else 1

    function
    | SynExpr.ArrayOrListComputed (Size size as isArray, Sequentials xs, range) ->
        let sr, er = RangeHelpers.mkStartEndRange size range
        Some(sr, isArray, xs, er, range)
    | SynExpr.ArrayOrListComputed (Size size as isArray, singleExpr, range) ->
        let sr, er = RangeHelpers.mkStartEndRange size range
        Some(sr, isArray, [ singleExpr ], er, range)
    | SynExpr.ArrayOrList (Size size as isArray, xs, range) ->
        let sr, er = RangeHelpers.mkStartEndRange size range
        Some(sr, isArray, xs, er, range)
    | _ -> None

let (|Tuple|_|) =
    function
    | SynExpr.Tuple (false, exprs, _, tupleRange) -> Some(exprs, tupleRange)
    | _ -> None

let (|StructTuple|_|) =
    function
    | SynExpr.Tuple (true, exprs, _, _) -> Some exprs
    | _ -> None

let (|InterpolatedStringExpr|_|) =
    function
    | SynExpr.InterpolatedString (parts, stringKind, _) -> Some(parts, stringKind)
    | _ -> None

let (|IndexRangeExpr|_|) =
    function
    | SynExpr.IndexRange (expr1, _, expr2, _, _, _) -> Some(expr1, expr2)
    | _ -> None

let (|IndexFromEndExpr|_|) =
    function
    | SynExpr.IndexFromEnd (e, _r) -> Some e
    | _ -> None

let (|TyparExpr|_|) =
    function
    | SynExpr.Typar (typar, _) -> Some typar
    | _ -> None

let (|ConstNumberExpr|_|) =
    function
    | ConstExpr (SynConst.Double _, _) as e -> Some e
    | ConstExpr (SynConst.Decimal _, _) as e -> Some e
    | ConstExpr (SynConst.Single _, _) as e -> Some e
    | ConstExpr (SynConst.Int16 _, _) as e -> Some e
    | ConstExpr (SynConst.Int32 _, _) as e -> Some e
    | ConstExpr (SynConst.Int64 _, _) as e -> Some e
    | _ -> None

let (|NegativeNumber|_|) =
    function
    | ConstExpr (SynConst.Double v, _) as e when v < 0. -> Some e
    | ConstExpr (SynConst.Decimal v, _) as e when v < 0M -> Some e
    | ConstExpr (SynConst.Single v, _) as e when v < 0f -> Some e
    | ConstExpr (SynConst.Int16 v, _) as e when v < 0s -> Some e
    | ConstExpr (SynConst.Int32 v, _) as e when v < 0 -> Some e
    | ConstExpr (SynConst.Int64 v, _) as e when v < 0L -> Some e
    | _ -> None

let (|IdentExpr|_|) =
    function
    | SynExpr.Ident ident -> Some ident
    | _ -> None

let (|OptVar|_|) =
    function
    | SynExpr.LongIdent (isOpt, synLongIdent, _, m) -> Some(isOpt, synLongIdent, m)
    | _ -> None

let (|LongIdentExpr|_|) =
    function
    | SynExpr.LongIdent (longDotId = synLongIdent) -> Some synLongIdent
    | _ -> None

let (|LongIdentExprWithMoreThanOneIdent|_|) =
    function
    | SynExpr.LongIdent(longDotId = SynLongIdent (id = lids) as synLongIdent) when (List.moreThanOne lids) ->
        Some synLongIdent
    | _ -> None

let (|DynamicExpr|_|) =
    function
    | SynExpr.Dynamic (funcExpr, _, argExpr, _) -> Some(funcExpr, argExpr)
    | _ -> None

let (|ParenStarSynIdent|_|) =
    function
    | IdentTrivia.OriginalNotationWithParen (lpr, originalNotation, rpr) ->
        if originalNotation.Length > 1 && originalNotation.StartsWith("*") then
            Some(lpr, originalNotation, rpr)
        else
            None
    | _ -> None

// Example: ( *** ) a b
// (*) a b is ok though
let (|ParenFunctionNameWithStar|_|) =
    function
    | LongIdentExpr (SynLongIdent ([ _ ], [], [ Some (ParenStarSynIdent (lpr, originalNotation, rpr)) ])) ->
        Some(lpr, originalNotation, rpr)
    | _ -> None

/// Get all application params at once
let (|App|_|) e =
    let rec loop =
        function
        // function application is left-recursive
        | SynExpr.App (_, _, e, e2, _) ->
            let e1, es = loop e
            (e1, e2 :: es)
        | e -> (e, [])

    match loop e with
    | _, [] -> None
    | e, es -> Some(e, List.rev es)

// captures application with single parenthesis argument
let (|AppSingleParenArg|_|) =
    function
    | App (SynExpr.DotGet _, [ (Paren (_, Tuple _, _, _)) ]) -> None
    | App (e, [ Paren (_, singleExpr, _, _) as px ]) ->
        match singleExpr with
        | SynExpr.Lambda _
        | SynExpr.MatchLambda _ -> None
        | _ -> Some(e, px)
    | _ -> None

let (|AppOrTypeApp|_|) e =
    match e with
    | App (TypeApp (e, lt, ts, gt), es) -> Some(e, Some(lt, ts, gt), es)
    | App (e, es) -> Some(e, None, es)
    | _ -> None

let (|NewTuple|_|) =
    function
    | SynExpr.New (_, t, (Paren _ as px), _) -> Some(t, px)
    | SynExpr.New (_, t, (ConstExpr (SynConst.Unit, _) as px), _) -> Some(t, px)
    | _ -> None

/// Only process prefix operators here
let (|PrefixApp|_|) =
    function
    // Var pattern causes a few prefix operators appear as infix operators
    | SynExpr.App (_,
                   false,
                   SynExpr.LongIdent (_,
                                      SynLongIdent ([ ident ], [], [ Some (IdentTrivia.OriginalNotation operatorName) ]),
                                      _,
                                      _),
                   e2,
                   _) when IsValidPrefixOperatorDefinitionName(ConvertValLogicalNameToDisplayNameCore ident.idText) ->
        Some(operatorName, e2)
    | _ -> None

let (|InfixApp|_|) synExpr =
    match synExpr with
    | SynExpr.App (_,
                   true,
                   OptVar (_, (SynLongIdent ([ _ident ], [], [ Some (IdentTrivia.OriginalNotation "::") ]) as sli), _),
                   Tuple ([ e1; e2 ], _),
                   range) -> Some("::", sli, e1, e2, range)
    | SynExpr.App (_,
                   _,
                   SynExpr.App (_,
                                true,
                                (OptVar (_,
                                         (SynLongIdent ([ _ident ], [], [ Some (IdentTrivia.OriginalNotation operator) ]) as sli),
                                         _)),
                                e1,
                                _),
                   e2,
                   range) -> Some(operator, sli, e1, e2, range)
    | _ -> None

let (|NewlineInfixApp|_|) =
    function
    | InfixApp (text, operatorExpr, e1, e2, _range) when newLineInfixOps.Contains(text) ->
        Some(text, operatorExpr, e1, e2)
    | _ -> None

let (|NewlineInfixApps|_|) e =
    let rec loop synExpr =
        match synExpr with
        | NewlineInfixApp (s, opE, e, e2) ->
            let e1, es = loop e
            (e1, (s, opE, e2) :: es)
        | e -> (e, [])

    match loop e with
    | e, es when (List.length es > 1) -> Some(e, List.rev es)
    | _ -> None

let (|SameInfixApps|_|) e =
    let rec loop operator synExpr =
        match synExpr with
        | InfixApp (s, opE, e, e2, _range) when (s = operator) ->
            let e1, es = loop operator e
            (e1, (s, opE, e2) :: es)
        | e -> (e, [])

    match e with
    | InfixApp (operatorText, _, _, _, _) ->
        match loop operatorText e with
        | e, es when (List.length es > 1) -> Some(e, List.rev es)
        | _ -> None
    | _ -> None

let (|TernaryApp|_|) =
    function
    | SynExpr.App (_,
                   _,
                   SynExpr.App (_,
                                _,
                                SynExpr.App (_,
                                             true,
                                             OptVar (_,
                                                     SynLongIdent ([ _ident ],
                                                                   [],
                                                                   [ Some (IdentTrivia.OriginalNotation "?<-") ]),
                                                     _),
                                             e1,
                                             _),
                                e2,
                                _),
                   e3,
                   _) -> Some(e1, e2, e3)
    | _ -> None

let (|MatchLambda|_|) =
    function
    | SynExpr.MatchLambda (_, keywordRange, pats, _, _) -> Some(keywordRange, pats)
    | _ -> None

let (|JoinIn|_|) =
    function
    | SynExpr.JoinIn (e1, _, e2, _) -> Some(e1, e2)
    | _ -> None

/// Unfold a list of let bindings
/// Recursive and use properties have to be determined at this point
let rec (|LetOrUses|_|) =
    function
    | SynExpr.LetOrUse (_, _, xs, LetOrUses (ys, e), _, trivia) ->
        let xs' =
            let lastIndex = xs.Length - 1
            List.mapi (fun i x -> if i = lastIndex then x, trivia.InKeyword else x, None) xs

        Some(xs' @ ys, e)
    | SynExpr.LetOrUse (_, _, xs, e, _, trivia) ->
        let xs' =
            let lastIndex = xs.Length - 1
            List.mapi (fun i x -> if i = lastIndex then x, trivia.InKeyword else x, None) xs

        Some(xs', e)
    | _ -> None

type ComputationExpressionStatement =
    | LetOrUseStatement of binding: SynBinding * inKeyword: range option
    | LetOrUseBangStatement of isUse: bool * SynPat * equalsRange: range option * SynExpr * range: range
    | AndBangStatement of SynPat * equalsRange: range * SynExpr * range: range
    | OtherStatement of SynExpr

let rec collectComputationExpressionStatements
    (e: SynExpr)
    (finalContinuation: ComputationExpressionStatement list -> ComputationExpressionStatement list)
    : ComputationExpressionStatement list =
    match e with
    | LetOrUses (bindings, body) ->
        let letBindings = bindings |> List.map LetOrUseStatement

        collectComputationExpressionStatements body (fun bodyStatements ->
            [ yield! letBindings; yield! bodyStatements ] |> finalContinuation)
    | SynExpr.LetOrUseBang (_, isUse, _, pat, expr, andBangs, body, r, trivia) ->
        let letOrUseBang = LetOrUseBangStatement(isUse, pat, trivia.EqualsRange, expr, r)

        let andBangs =
            andBangs
            |> List.map (fun (SynExprAndBang (_, _, _, ap, ae, range, trivia)) ->
                AndBangStatement(ap, trivia.EqualsRange, ae, range))

        collectComputationExpressionStatements body (fun bodyStatements ->
            [ letOrUseBang; yield! andBangs; yield! bodyStatements ] |> finalContinuation)
    | SynExpr.Sequential (_, _, e1, e2, _) ->
        let continuations: ((ComputationExpressionStatement list -> ComputationExpressionStatement list) -> ComputationExpressionStatement list) list =
            [ collectComputationExpressionStatements e1
              collectComputationExpressionStatements e2 ]

        let finalContinuation (nodes: ComputationExpressionStatement list list) : ComputationExpressionStatement list =
            List.collect id nodes |> finalContinuation

        Continuation.sequence continuations finalContinuation
    | expr -> finalContinuation [ OtherStatement expr ]

/// Matches if the SynExpr has some or of computation expression member call inside.
let rec (|CompExprBody|_|) expr =
    match expr with
    | SynExpr.LetOrUse(body = CompExprBody _)
    | SynExpr.LetOrUseBang _
    | SynExpr.Sequential _ -> Some(collectComputationExpressionStatements expr id)
    | _ -> None

let (|ForEach|_|) =
    function
    | SynExpr.ForEach (_, _, SeqExprOnly true, _, pat, e1, SingleExpr (Yield _, e2), _) -> Some(pat, e1, e2, true)
    | SynExpr.ForEach (_, _, SeqExprOnly isArrow, _, pat, e1, e2, _) -> Some(pat, e1, e2, isArrow)
    | _ -> None

let (|DotIndexedSet|_|) =
    function
    | SynExpr.DotIndexedSet (objectExpr, indexArgs, valueExpr, _, _, _) -> Some(objectExpr, indexArgs, valueExpr)
    | _ -> None

let (|NamedIndexedPropertySet|_|) =
    function
    | SynExpr.NamedIndexedPropertySet (synLongIdent, e1, e2, _) -> Some(synLongIdent, e1, e2)
    | _ -> None

let (|DotNamedIndexedPropertySet|_|) =
    function
    | SynExpr.DotNamedIndexedPropertySet (e, synLongIdent, e1, e2, _) -> Some(e, synLongIdent, e1, e2)
    | _ -> None

let (|DotIndexedGet|_|) =
    function
    | SynExpr.DotIndexedGet (objectExpr, indexArgs, _, _) -> Some(objectExpr, indexArgs)
    | _ -> None

let (|DotGet|_|) =
    function
    | SynExpr.DotGet (e, _, synLongIdent, _) -> Some(e, synLongIdent)
    | _ -> None

/// Match function call followed by Property
let (|DotGetAppParen|_|) e =
    match e with
    //| App(e, [DotGet (Paren _ as p, (s,r))]) -> Some (e, p, s, r)
    | DotGet (App (e, [ Paren (_, Tuple _, _, _) as px ]), lids) -> Some(e, px, lids)
    | DotGet (App (e, [ Paren (_, singleExpr, _, _) as px ]), lids) ->
        match singleExpr with
        | SynExpr.Lambda _
        | SynExpr.MatchLambda _ -> None
        | _ -> Some(e, px, lids)
    | DotGet (App (e, [ ConstExpr (SynConst.Unit, _) as px ]), lids) -> Some(e, px, lids)
    | _ -> None

let (|DotGetAppDotGetAppParenLambda|_|) (e: SynExpr) =
    match e with
    | DotGet (App (DotGet (App (e, [ Paren (_, SynExpr.Lambda _, _, _) as px ]), appLids), es), lids) ->
        Some(e, px, appLids, es, lids)
    | _ -> None

/// Gather series of application for line breaking
let rec (|DotGetApp|_|) =
    function
    | SynExpr.App (_, _, DotGet (DotGetApp (e, es), s), e', _) -> Some(e, [ yield! es; yield (s, e', None) ])
    | SynExpr.App (_, _, DotGet (e, s), e', _) -> Some(e, [ (s, e', None) ])
    | SynExpr.App (_, _, TypeApp (DotGet (DotGetApp (e, es), s), lt, ts, gt), e', _) ->
        Some(e, [ yield! es; yield (s, e', Some(lt, ts, gt)) ])
    | SynExpr.App (_, _, TypeApp (DotGet (e, s), lt, ts, gt), e', _) -> Some(e, [ (s, e', Some(lt, ts, gt)) ])
    | _ -> None

let (|DotSet|_|) =
    function
    | SynExpr.DotSet (e1, synLongIdent, e2, _) -> Some(e1, synLongIdent, e2)
    | _ -> None

let (|IfThenElse|_|) =
    function
    | SynExpr.IfThenElse _ as e -> Some e
    | _ -> None

let rec (|ElIf|_|) =
    function
    | SynExpr.IfThenElse (e1,
                          e2,
                          Some (ElIf ((_, eshIfKw, eshIsElif, eshE1, eshThenKw, eshE2) :: es, elseInfo, _)),
                          _,
                          _,
                          range,
                          trivia) ->
        Some(
            ((None, trivia.IfKeyword, trivia.IsElif, e1, trivia.ThenKeyword, e2)
             :: (trivia.ElseKeyword, eshIfKw, eshIsElif, eshE1, eshThenKw, eshE2) :: es),
            elseInfo,
            range
        )

    | SynExpr.IfThenElse (e1, e2, e3, _, _, range, trivia) ->
        let elseInfo =
            match trivia.ElseKeyword, e3 with
            | Some elseKw, Some elseExpr -> Some(elseKw, elseExpr)
            | _ -> None

        Some([ (None, trivia.IfKeyword, trivia.IsElif, e1, trivia.ThenKeyword, e2) ], elseInfo, range)
    | _ -> None

let (|TypeOnlyInheritConstructor|UnitInheritConstructor|ParenInheritConstructor|OtherInheritConstructor|)
    (
        t: SynType,
        e: SynExpr
    ) =
    match e with
    | SynExpr.Const (constant = SynConst.Unit; range = unitRange) ->
        // The unit expression could have been added artificially.
        if unitRange.StartColumn + 2 = unitRange.EndColumn then
            UnitInheritConstructor(t)
        else
            TypeOnlyInheritConstructor t
    | SynExpr.Paren _ as px -> ParenInheritConstructor(t, px)
    | _ -> OtherInheritConstructor(t, e)

let (|Record|_|) =
    function
    | SynExpr.Record (inheritOpt, eo, xs, StartEndRange 1 (openingBrace, _, closingBrace)) ->
        let inheritOpt = inheritOpt |> Option.map (fun (t, e, _, _, _) -> t, e)

        Some(openingBrace, inheritOpt, xs, Option.map fst eo, closingBrace)
    | _ -> None

let (|AnonRecord|_|) =
    function
    | SynExpr.AnonRecd (isStruct, copyInfo, fields, _) -> Some(isStruct, fields, Option.map fst copyInfo)
    | _ -> None

let (|ObjExpr|_|) =
    function
    | SynExpr.ObjExpr (t, eio, withKeyword, bd, members, ims, _, range) ->
        Some(t, eio, withKeyword, bd, members, ims, range)
    | _ -> None

let (|LongIdentSet|_|) =
    function
    | SynExpr.LongIdentSet (synLongIdent, e, r) -> Some(synLongIdent, e, r)
    | _ -> None

let (|TryWith|_|) =
    function
    | SynExpr.TryWith (e, cs, _, _, _, trivia) -> Some(trivia.TryKeyword, e, trivia.WithKeyword, cs)
    | _ -> None

let (|TryFinally|_|) =
    function
    | SynExpr.TryFinally (e1, e2, _, _, _, trivia) -> Some(trivia.TryKeyword, e1, trivia.FinallyKeyword, e2)
    | _ -> None

let (|ParsingError|_|) =
    function
    | SynExpr.ArbitraryAfterError (_, r)
    | SynExpr.FromParseError (_, r)
    | SynExpr.DiscardAfterMissingQualificationAfterDot (_, r) -> Some r
    | _ -> None

let (|ILEmbedded|_|) =
    function
    | SynExpr.LibraryOnlyILAssembly (_, _, _, _, r) -> Some(r)
    | _ -> None

let (|LibraryOnlyStaticOptimization|_|) (e: SynExpr) =
    match e with
    | SynExpr.LibraryOnlyStaticOptimization (constraints, e, optExpr, _) -> Some(optExpr, constraints, e)
    | _ -> None

let (|UnsupportedExpr|_|) =
    function
    // Temporarily ignore these cases not often used outside FSharp.Core
    | SynExpr.LibraryOnlyUnionCaseFieldGet (_, _, _, r)
    | SynExpr.LibraryOnlyUnionCaseFieldSet (_, _, _, _, r) -> Some r
    | _ -> None

// Patterns (18 cases, lacking to handle 2 cases)

let (|PatOptionalVal|_|) =
    function
    | SynPat.OptionalVal (ident, _) -> Some ident
    | _ -> None

let (|PatAttrib|_|) =
    function
    | SynPat.Attrib (p, ats, _) -> Some(p, ats)
    | _ -> None

let (|PatOr|_|) =
    function
    | SynPat.Or (p1, p2, _, trivia) -> Some(p1, trivia.BarRange, p2)
    | _ -> None

let rec (|PatOrs|_|) =
    function
    | PatOr (PatOrs (p1, pats), barRange, p2) as p -> Some(p1, [ yield! pats; yield (barRange, p2, p.Range) ])
    | PatOr (p1, barRange, p2) as p -> Some(p1, [ barRange, p2, p.Range ])
    | _ -> None

let (|PatAnds|_|) =
    function
    | SynPat.Ands (ps, _) -> Some ps
    | _ -> None

type PatNullaryKind =
    | PatNull
    | PatWild

let (|PatNullary|_|) =
    function
    | SynPat.Null _ -> Some PatNull
    | SynPat.Wild _ -> Some PatWild
    | _ -> None

let (|PatTuple|_|) =
    function
    | SynPat.Tuple (false, ps, _) -> Some ps
    | _ -> None

let (|PatStructTuple|_|) =
    function
    | SynPat.Tuple (true, ps, _) -> Some ps
    | _ -> None

type SeqPatKind =
    | PatArray
    | PatList

let (|PatSeq|_|) =
    function
    | SynPat.ArrayOrList (true, ps, _) -> Some(PatArray, ps)
    | SynPat.ArrayOrList (false, ps, _) -> Some(PatList, ps)
    | _ -> None

let (|PatTyped|_|) =
    function
    | SynPat.Typed (p, t, _) -> Some(p, t)
    | _ -> None

let (|PatNamed|_|) pat =
    match pat with
    | SynPat.Named (si, _, ao, _) -> Some(ao, si)
    | _ -> None

let (|PatAs|_|) =
    function
    | SynPat.As (p1, p2, r) -> Some(p1, p2, r)
    | _ -> None

let (|PatNamePatPairs|_|) =
    function
    | SynPat.LongIdent (synLongIdent,
                        _,
                        vtdo,
                        SynArgPats.NamePatPairs (nps, _, { ParenRange = StartEndRange 1 (lpr, range, rpr) }),
                        _,
                        _) -> Some(synLongIdent, vtdo, lpr, nps, rpr, range)
    | _ -> None

let (|PatLongIdent|_|) =
    function
    | SynPat.LongIdent (synLongIdent, _, tpso, SynArgPats.Pats ps, ao, _) -> Some(ao, synLongIdent, ps, tpso)
    | _ -> None

let (|PatListCons|_|) =
    function
    | SynPat.ListCons (p1, p2, _, trivia) -> Some(p1, trivia.ColonColonRange, p2)
    | _ -> None

let (|OperatorNameWithStar|PrefixedOperatorNameWithStar|NotAnOperatorNameWithStar|) (synLongIdent: SynLongIdent) =
    match synLongIdent.IdentsWithTrivia with
    | [ SynIdent(trivia = Some (ParenStarSynIdent (lpr, originalNotation, rpr))) as synIdent ] ->
        OperatorNameWithStar(lpr, originalNotation, rpr, synIdent.FullRange, synLongIdent.FullRange)
    | [ prefix; SynIdent(trivia = Some (ParenStarSynIdent (lpr, originalNotation, rpr))) as synIdent ] ->
        PrefixedOperatorNameWithStar(prefix, lpr, originalNotation, rpr, synIdent.FullRange, synLongIdent.FullRange)
    | _ -> NotAnOperatorNameWithStar

let (|PatParen|_|) =
    function
    | SynPat.Paren (p, StartEndRange 1 (lpr, _, rpr)) -> Some(lpr, p, rpr)
    | _ -> None

let (|PatRecord|_|) =
    function
    | SynPat.Record (xs, _) -> Some xs
    | _ -> None

let (|PatConst|_|) =
    function
    | SynPat.Const (c, r) -> Some(c, r)
    | _ -> None

let (|PatUnitConst|_|) =
    function
    | SynPat.Const (Unit, _) -> Some()
    | _ -> None

let (|PatIsInst|_|) =
    function
    | SynPat.IsInst (t, _) -> Some t
    | _ -> None

let (|PatQuoteExpr|_|) =
    function
    | SynPat.QuoteExpr (e, _) -> Some e
    | _ -> None

let (|PatExplicitCtor|_|) =
    function
    | SynPat.LongIdent (SynLongIdent(id = [ newIdent ]), _, _, SynArgPats.Pats [ PatParen _ as pat ], ao, _) when
        (newIdent.idText = "new")
        ->
        Some(ao, pat)
    | _ -> None

// Members
type SynSimplePats with

    member pat.Range =
        match pat with
        | SynSimplePats.SimplePats (_, r)
        | SynSimplePats.Typed (_, _, r) -> r

let (|SPAttrib|SPId|SPTyped|) =
    function
    | SynSimplePat.Attrib (sp, ats, _) -> SPAttrib(ats, sp)
    // Not sure compiler generated SPIds are used elsewhere.
    | SynSimplePat.Id (ident, _, isGen, _, isOptArg, _) -> SPId(ident, isOptArg, isGen)
    | SynSimplePat.Typed (sp, t, _) -> SPTyped(sp, t)

let (|SimplePats|SPSTyped|) =
    function
    | SynSimplePats.SimplePats (ps, _) -> SimplePats ps
    | SynSimplePats.Typed (ps, t, _) -> SPSTyped(ps, t)

let (|RecordField|) =
    function
    | SynField (ats, _, ido, _, _, px, ao, _, { LeadingKeyword = lk }) -> (ats, px, lk, ao, ido)

let (|Clause|) (SynMatchClause (p, eo, e, range, _, trivia)) =
    let fullRange =
        match trivia.BarRange with
        | None -> range
        | Some barRange -> Range.unionRanges barRange range

    (trivia.BarRange, p, eo, trivia.ArrowRange, e, fullRange)

let (|TryWithSingleClause|_|) =
    function
    | TryWith (tryKeyword, e, withKeyword, [ (Clause (barRange, p, eo, arrowRange, catchExpr, clauseRange)) ]) ->
        match p with
        | SynPat.Or _
        | SynPat.As (SynPat.Or _, _, _) -> None
        | _ -> Some(tryKeyword, e, withKeyword, barRange, p, eo, arrowRange, catchExpr, clauseRange)
    | _ -> None

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

let (|Lambda|_|) =
    function
    | SynExpr.Lambda (_, _, _, _, Some (pats, body), range, trivia) ->
        let inline getLambdaBodyExpr expr =
            let skippedLambdas = skipGeneratedLambdas expr
            skipGeneratedMatch skippedLambdas

        Some(pats, trivia.ArrowRange, getLambdaBodyExpr body, range)
    | _ -> None

let (|AppWithLambda|_|) (e: SynExpr) =
    match e with
    | App (e, es) ->
        let rec visit (es: SynExpr list) (finalContinuation: SynExpr list -> SynExpr list) =
            match es with
            | [] -> None
            | [ Paren (lpr, Lambda (pats, arrowRange, body, range), rpr, pr) ] ->
                Some(e, finalContinuation [], lpr, Choice1Of2(pats, arrowRange, body, range), rpr, pr)
            | [ Paren (lpr, (MatchLambda (keywordRange, pats) as me), rpr, pr) ] ->
                Some(e, finalContinuation [], lpr, Choice2Of2(keywordRange, pats, me.Range), rpr, pr)
            | h :: tail ->
                match h with
                | Paren (_, Lambda _, _, _)
                | Paren (_, MatchLambda _, _, _) -> None
                | _ -> visit tail (fun leadingArguments -> h :: leadingArguments |> finalContinuation)

        visit es id
    | _ -> None

// Foo(fun x -> y).Bar
let (|DotGetAppWithLambda|_|) =
    function
    | DotGet (AppWithLambda (e, [], lpr, c, rpr, pr), lids) -> Some((e, [], lpr, c, rpr, pr), lids)
    | _ -> None

// Type definitions

let (|TDSREnum|TDSRUnion|TDSRRecord|TDSRNone|TDSRTypeAbbrev|TDSRException|) =
    function
    | SynTypeDefnSimpleRepr.Enum (ecs, _) -> TDSREnum ecs
    | SynTypeDefnSimpleRepr.Union (ao, xs, _) -> TDSRUnion(ao, xs)
    | SynTypeDefnSimpleRepr.Record (ao, fs, StartEndRange 1 (openingBrace, _, closingBrace)) ->
        TDSRRecord(openingBrace, ao, fs, closingBrace)
    | SynTypeDefnSimpleRepr.None _ -> TDSRNone()
    | SynTypeDefnSimpleRepr.TypeAbbrev (_, t, _) -> TDSRTypeAbbrev t
    | SynTypeDefnSimpleRepr.General _ -> failwith "General should not appear in the parse tree"
    | SynTypeDefnSimpleRepr.LibraryOnlyILAssembly _ -> failwith "LibraryOnlyILAssembly is not supported yet"
    | SynTypeDefnSimpleRepr.Exception repr -> TDSRException repr

let (|Simple|ObjectModel|ExceptionRepr|) =
    function
    | SynTypeDefnRepr.Simple (tdsr, _) -> Simple tdsr
    | SynTypeDefnRepr.ObjectModel (tdk, mds, range) -> ObjectModel(tdk, mds, range)
    | SynTypeDefnRepr.Exception repr -> ExceptionRepr repr

let (|MemberDefnList|) mds =
    // Assume that there is at most one implicit constructor
    let impCtor =
        List.tryFind
            (function
            | MDImplicitCtor _ -> true
            | _ -> false)
            mds
    // Might need to sort so that let and do bindings come first
    let others =
        List.filter
            (function
            | MDImplicitCtor _ -> false
            | _ -> true)
            mds

    (impCtor, others)

let (|SigSimple|SigObjectModel|SigExceptionRepr|) =
    function
    | SynTypeDefnSigRepr.Simple (tdsr, _) -> SigSimple tdsr
    | SynTypeDefnSigRepr.ObjectModel (tdk, mds, _) -> SigObjectModel(tdk, mds)
    | SynTypeDefnSigRepr.Exception repr -> SigExceptionRepr repr

type TypeDefnKindSingle =
    | TCUnspecified
    | TCClass
    | TCInterface
    | TCStruct
    | TCRecord
    | TCUnion
    | TCAbbrev
    | TCOpaque
    | TCAugmentation of withKeyword: range
    | TCIL

let (|TCSimple|TCDelegate|) =
    function
    | SynTypeDefnKind.Unspecified -> TCSimple TCUnspecified
    | SynTypeDefnKind.Class -> TCSimple TCClass
    | SynTypeDefnKind.Interface -> TCSimple TCInterface
    | SynTypeDefnKind.Struct -> TCSimple TCStruct
    | SynTypeDefnKind.Record -> TCSimple TCRecord
    | SynTypeDefnKind.Union -> TCSimple TCUnion
    | SynTypeDefnKind.Abbrev -> TCSimple TCAbbrev
    | SynTypeDefnKind.Opaque -> TCSimple TCOpaque
    | SynTypeDefnKind.Augmentation withKeyword -> TCSimple(TCAugmentation withKeyword)
    | SynTypeDefnKind.IL -> TCSimple TCIL
    | SynTypeDefnKind.Delegate (t, _) -> TCDelegate t

let (|TypeDef|) (SynTypeDefn (SynComponentInfo (ats, tds, tcs, lid, px, preferPostfix, ao, _), tdr, ms, _, _, trivia)) =
    (ats, px, trivia.TypeKeyword, ao, tds, tcs, trivia.EqualsRange, tdr, trivia.WithKeyword, ms, lid, preferPostfix)

let (|SigTypeDef|)
    (SynTypeDefnSig (SynComponentInfo (ats, tds, tcs, lid, px, preferPostfix, ao, _), tdr, ms, range, trivia))
    =
    (ats,
     px,
     trivia.TypeKeyword,
     ao,
     tds,
     tcs,
     trivia.EqualsRange,
     tdr,
     trivia.WithKeyword,
     ms,
     lid,
     preferPostfix,
     range)

let (|TyparDecl|) (SynTyparDecl (ats, tp) as std) = (ats, tp, std.FullRange)

let (|PostfixList|_|) =
    function
    | SynTyparDecls.PostfixList (tds, tcs, StartEndRange 1 (gt, r, lt)) -> Some(gt, tds, tcs, lt, r)
    | _ -> None

// Types (15 cases)

let (|THashConstraint|_|) =
    function
    | SynType.HashConstraint (t, _) -> Some t
    | _ -> None

let (|TMeasurePower|_|) =
    function
    | SynType.MeasurePower (t, RationalConst n, _) -> Some(t, n)
    | _ -> None

let (|TMeasureDivide|_|) =
    function
    | SynType.MeasureDivide (t1, t2, _) -> Some(t1, t2)
    | _ -> None

let (|TStaticConstant|_|) =
    function
    | SynType.StaticConstant (c, r) -> Some(c, r)
    | _ -> None

let (|TStaticConstantExpr|_|) =
    function
    | SynType.StaticConstantExpr (c, _) -> Some c
    | _ -> None

let (|TStaticConstantNamed|_|) =
    function
    | SynType.StaticConstantNamed (t1, t2, _) -> Some(t1, t2)
    | _ -> None

let (|TArray|_|) =
    function
    | SynType.Array (n, t, r) -> Some(t, n, r)
    | _ -> None

let (|TAnon|_|) =
    function
    | SynType.Anon _ -> Some()
    | _ -> None

let (|TVar|_|) =
    function
    | SynType.Var (tp, r) -> Some(tp, r)
    | _ -> None

let (|TFun|_|) =
    function
    | SynType.Fun (t1, t2, _, { ArrowRange = arrow }) -> Some(t1, arrow, t2)
    | _ -> None

// Arrow type is right-associative
let rec (|TFuns|_|) =
    function
    | TFun (t1, arrow, TFuns (ts, ret)) -> Some((t1, arrow) :: ts, ret)
    | TFun (t1, arrow, t2) -> Some([ t1, arrow ], t2)
    | _ -> None

let (|TApp|_|) =
    function
    | SynType.App (t, lessRange, ts, _, greaterRange, isPostfix, range) ->
        Some(t, lessRange, ts, greaterRange, isPostfix, range)
    | _ -> None

let (|TLongIdentApp|_|) =
    function
    | SynType.LongIdentApp (t, synLongIdent, lessRange, ts, _, greaterRange, _) ->
        Some(t, synLongIdent, lessRange, ts, greaterRange)
    | _ -> None

let (|TTuple|_|) =
    function
    | SynType.Tuple (false, ts, _) -> Some ts
    | _ -> None

let (|TStructTuple|_|) =
    function
    | SynType.Tuple (true, ts, _) -> Some ts
    | _ -> None

let (|TWithGlobalConstraints|_|) =
    function
    | SynType.WithGlobalConstraints (t, tcs, _) -> Some(t, tcs)
    | _ -> None

let (|TLongIdent|_|) =
    function
    | SynType.LongIdent synLongIdent -> Some synLongIdent
    | _ -> None

let (|TAnonRecord|_|) =
    function
    | SynType.AnonRecd (isStruct, fields, _) -> Some(isStruct, fields)
    | _ -> None

let (|TParen|_|) =
    function
    | SynType.Paren (innerType, StartEndRange 1 (lpr, pr, rpr)) -> Some(lpr, innerType, rpr, pr)
    | _ -> None

let (|TSignatureParameter|_|) =
    function
    | SynType.SignatureParameter (attrs, isOptional, identOpt, t, _) -> Some(attrs, isOptional, identOpt, t)
    | _ -> None

let (|TOr|_|) =
    function
    | SynType.Or (lhs, rhs, _, trivia) -> Some(lhs, trivia.OrKeyword, rhs)
    | _ -> None

let (|TArrayInExtern|_|) t =
    match t with
    | TLongIdent (SynLongIdent(id = [ lid ])) when (lid.idText = "[]") -> Some "[]"
    | _ -> None

// Type parameter

type SingleTyparConstraintKind =
    | TyparIsValueType
    | TyparIsReferenceType
    | TyparIsUnmanaged
    | TyparSupportsNull
    | TyparIsComparable
    | TyparIsEquatable

    override x.ToString() =
        match x with
        | TyparIsValueType -> "struct"
        | TyparIsReferenceType -> "not struct"
        | TyparIsUnmanaged -> "unmanaged"
        | TyparSupportsNull -> "null"
        | TyparIsComparable -> "comparison"
        | TyparIsEquatable -> "equality"

let (|TyparSingle|TyparDefaultsToType|TyparSubtypeOfType|TyparSupportsMember|TyparIsEnum|TyparIsDelegate|TyparWhereSelfConstrained|) =
    function
    | SynTypeConstraint.WhereTyparIsValueType (tp, _) -> TyparSingle(TyparIsValueType, tp)
    | SynTypeConstraint.WhereTyparIsReferenceType (tp, _) -> TyparSingle(TyparIsReferenceType, tp)
    | SynTypeConstraint.WhereTyparIsUnmanaged (tp, _) -> TyparSingle(TyparIsUnmanaged, tp)
    | SynTypeConstraint.WhereTyparSupportsNull (tp, _) -> TyparSingle(TyparSupportsNull, tp)
    | SynTypeConstraint.WhereTyparIsComparable (tp, _) -> TyparSingle(TyparIsComparable, tp)
    | SynTypeConstraint.WhereTyparIsEquatable (tp, _) -> TyparSingle(TyparIsEquatable, tp)
    | SynTypeConstraint.WhereTyparDefaultsToType (tp, t, _) -> TyparDefaultsToType(tp, t)
    | SynTypeConstraint.WhereTyparSubtypeOfType (tp, t, _) -> TyparSubtypeOfType(tp, t)
    | SynTypeConstraint.WhereTyparSupportsMember (tps, msg, _) -> TyparSupportsMember(tps, msg)
    | SynTypeConstraint.WhereTyparIsEnum (tp, ts, _) -> TyparIsEnum(tp, ts)
    | SynTypeConstraint.WhereTyparIsDelegate (tp, ts, _) -> TyparIsDelegate(tp, ts)
    | SynTypeConstraint.WhereSelfConstrained (t, _) -> TyparWhereSelfConstrained t

let (|MSMember|MSInterface|MSInherit|MSValField|MSNestedType|) =
    function
    | SynMemberSig.Member (vs, mf, _) ->
        let (SynValSig (synType = t; trivia = trivia)) = vs
        MSMember(vs, mkWithGetSet (Some t) trivia.WithKeyword mf.MemberKind)
    | SynMemberSig.Interface (t, _) -> MSInterface t
    | SynMemberSig.Inherit (t, _) -> MSInherit t
    | SynMemberSig.ValField (f, _) -> MSValField f
    | SynMemberSig.NestedType (tds, _) -> MSNestedType tds

let (|Val|)
    (SynValSig (ats, synIdent, SynValTyparDecls (typars, _), t, vi, isInline, isMutable, px, ao, eo, range, trivia))
    =
    let synIdent =
        match trivia.LeadingKeyword with
        | SynLeadingKeyword.New _ -> None
        | _ -> Some synIdent

    (ats, px, trivia.LeadingKeyword, ao, synIdent, t, vi, isInline, isMutable, typars, eo, range)

// Misc

let (|RecordFieldName|) ((synLongIdent, _): RecordFieldName, eo: SynExpr option, _) = (synLongIdent, eo)

let (|AnonRecordFieldName|) (ident: Ident, eq: range option, e: SynExpr) =
    let range = Range.unionRanges ident.idRange e.Range
    (ident, eq, e, range)

let (|AnonRecordFieldType|) (ident, t: SynType) = (ident, t)

/// Extract function arguments with their associated info
let (|FunType|) t =
    // Parse arg info by attach them into relevant types.
    // The number of arg info will determine semantics of argument types.
    let rec loop =
        function
        | TFun (t1, arrow, t2) -> (t1, Some arrow) :: loop t2
        | t -> [ t, None ]

    loop t

let private getLastPartOfSynLongIdent (synLongIdent: SynLongIdent) : string option =
    List.tryLast synLongIdent.IdentsWithTrivia
    |> Option.map (fun (SynIdent (lp, trivia)) ->
        match trivia with
        | None
        | Some (IdentTrivia.HasParenthesis _) -> lp.idText
        | Some (IdentTrivia.OriginalNotation (text = text))
        | Some (IdentTrivia.OriginalNotationWithParen (text = text)) -> text)

let rec (|UppercaseSynExpr|LowercaseSynExpr|) (synExpr: SynExpr) =
    let upperOrLower (v: string) =
        let isUpper = Seq.tryHead v |> Option.map Char.IsUpper |> Option.defaultValue false
        if isUpper then UppercaseSynExpr else LowercaseSynExpr

    match synExpr with
    | SynExpr.Ident ident -> upperOrLower ident.idText

    | SynExpr.LongIdent (_, synLongIdent, _, _)
    | SynExpr.DotGet (_, _, synLongIdent, _) ->
        match getLastPartOfSynLongIdent synLongIdent with
        | None -> LowercaseSynExpr
        | Some text -> upperOrLower text

    | SynExpr.DotIndexedGet (expr, _, _, _)
    | SynExpr.TypeApp (expr, _, _, _, _, _, _)
    | SynExpr.Dynamic (funcExpr = expr) -> (|UppercaseSynExpr|LowercaseSynExpr|) expr
    | _ -> failwithf "cannot determine if synExpr %A is uppercase or lowercase" synExpr

let rec (|UppercaseSynType|LowercaseSynType|) (synType: SynType) =
    let upperOrLower (v: string) =
        let isUpper = Seq.tryHead v |> Option.map Char.IsUpper |> Option.defaultValue false
        if isUpper then UppercaseSynType else LowercaseSynType

    match synType with
    | SynType.LongIdent synLongIdent ->
        match getLastPartOfSynLongIdent synLongIdent with
        | None -> LowercaseSynType
        | Some text -> upperOrLower text
    | SynType.Var (Typar (ident, _), _) -> upperOrLower ident.idText
    | SynType.App (st, _, _, _, _, _, _) -> (|UppercaseSynType|LowercaseSynType|) st
    | _ -> failwithf "cannot determine if synType %A is uppercase or lowercase" synType

let private (|IdentExprOrLongIdentExpr|_|) e =
    match e with
    | SynExpr.Ident _
    | SynExpr.LongIdent _ -> Some e
    | _ -> None

let rec (|IndexWithoutDotExpr|NestedIndexWithoutDotExpr|NonAppExpr|) e =
    match e with
    | SynExpr.App (ExprAtomicFlag.Atomic, false, identifierExpr, SynExpr.ArrayOrListComputed (false, indexExpr, _), _) ->
        IndexWithoutDotExpr(identifierExpr, indexExpr)
    | SynExpr.App (ExprAtomicFlag.NonAtomic,
                   false,
                   identifierExpr,
                   (SynExpr.ArrayOrListComputed (isArray = false; expr = indexExpr) as argExpr),
                   _) when (RangeHelpers.isAdjacentTo identifierExpr.Range argExpr.Range) ->
        IndexWithoutDotExpr(identifierExpr, indexExpr)
    | SynExpr.App (ExprAtomicFlag.NonAtomic, false, IndexWithoutDotExpr (identifier, indexExpr), argExpr, _) ->
        NestedIndexWithoutDotExpr(identifier, indexExpr, argExpr)
    | _ -> NonAppExpr

let rec (|EndsWithSingleListAppExpr|_|) (isStroustrup: bool) (e: SynExpr) =
    if not isStroustrup then
        None
    else
        match e with
        | SynExpr.App (ExprAtomicFlag.NonAtomic, false, (SynExpr.App _ as funcExpr), (ArrayOrList _ as lastArg), _) ->
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
        | SynExpr.App (ExprAtomicFlag.NonAtomic, false, funcExpr, (ArrayOrList _ as lastArg), _) ->
            Some(funcExpr, [], lastArg)
        | _ -> None

let (|EndsWithDualListAppExpr|_|) (isStroustrup: bool) (e: SynExpr) =
    if not isStroustrup then
        None
    else
        match e with
        | SynExpr.App (ExprAtomicFlag.NonAtomic,
                       false,
                       EndsWithSingleListAppExpr isStroustrup (e, es, lastButOneArg),
                       (ArrayOrList _ as lastArg),
                       _) -> Some(e, es, lastButOneArg, lastArg)
        | _ -> None

let isIfThenElseWithYieldReturn e =
    match e with
    | SynExpr.IfThenElse (thenExpr = SynExpr.YieldOrReturn _; elseExpr = None)
    | SynExpr.IfThenElse (thenExpr = SynExpr.YieldOrReturn _; elseExpr = Some (SynExpr.YieldOrReturn _))
    | SynExpr.IfThenElse (thenExpr = SynExpr.YieldOrReturnFrom _; elseExpr = None)
    | SynExpr.IfThenElse (thenExpr = SynExpr.YieldOrReturn _; elseExpr = Some (SynExpr.YieldOrReturnFrom _)) -> true
    | _ -> false

let isSynExprLambdaOrIfThenElse =
    function
    | SynExpr.Lambda _
    | SynExpr.IfThenElse _ -> true
    | _ -> false

let (|StroustrupStyleExpr|_|) (isStroustrupStyleEnabled: bool) (e: SynExpr) =
    if not isStroustrupStyleEnabled then
        None
    else
        match e with
        // { foo with Bar = bar }
        | SynExpr.AnonRecd(copyInfo = Some _)
        | SynExpr.Record(copyInfo = Some _) -> None
        | SynExpr.Record _
        | SynExpr.AnonRecd _
        // task { ... }
        | SynExpr.App (ExprAtomicFlag.NonAtomic, false, SynExpr.Ident _, SynExpr.ComputationExpr _, _)
        | ArrayOrList _ -> Some e
        | _ -> None

let hasMultipleClausesWhereOneHasStroustrup isStroustrupStyleEnabled (cs: SynMatchClause list) : bool =
    isStroustrupStyleEnabled
    && List.moreThanOne cs
    && List.exists
        (fun (SynMatchClause (resultExpr = e)) ->
            match e with
            | StroustrupStyleExpr isStroustrupStyleEnabled _ -> true
            | _ -> false)
        cs
