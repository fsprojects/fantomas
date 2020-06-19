module internal Fantomas.SourceCounter

open FSharp.Compiler.SyntaxTree
open Fantomas
open Fantomas.SourceParser

/// General idea is to be able to make some decisions based on the length of Idents
/// This would be faster than short expression checks

type IdentCounter =
    | UnderThreshold of current: int * max: int
    | OverThreshold

let private lift max = UnderThreshold(0, max)

let private map value current =
    match current with
    | UnderThreshold (current, max) when (current + value > max) -> OverThreshold
    | UnderThreshold (current, max) -> UnderThreshold(current + value, max)
    | OverThreshold -> OverThreshold

let private bind f current =
    match current with
    | UnderThreshold (_, _) as ut -> f ut
    | OverThreshold -> OverThreshold

let private bindItems f items v =
    List.fold (fun acc a -> bind (f a) acc) v items

let private bindOption f o =
    match o with
    | Some s -> f s
    | None -> id

let private (>>=) = bind

let private (>=>) f g = fun v -> bind f v |> bind g

let rec private countIdent (ident: Ident) = map ident.idText.Length

and countLongIdent li = bindItems countIdent li

and countLongIdentWithDots lid =
    match lid with
    | LongIdentWithDots.LongIdentWithDots (li, _) -> countLongIdent li

and countSynType synType =
    match synType with
    | _ -> map 0

and countSynExpr expr =
    match expr with
    | _ -> map 0

and countSynAttribute (attribute: SynAttribute) =
    countLongIdentWithDots attribute.TypeName
    >=> countSynExpr attribute.ArgExpr
    >=> bindOption countIdent attribute.Target

and countSynAttributeList (attributeList: SynAttributeList) =
    bindItems countSynAttribute attributeList.Attributes

and countSynAttributes (attributes: SynAttributes): IdentCounter -> IdentCounter =
    bindItems countSynAttributeList attributes

and countSynField synField =
    match synField with
    | SynField.Field (attributes, _, ident, t, _, _, _, _) ->
        countSynAttributes attributes
        >=> bindOption countIdent ident
        >=> countSynType t

and countSynTypar synTypar =
    match synTypar with
    | SynTypar.Typar (ident, _, _) -> countIdent ident

and countSynTypeConstraint synTypeConstraint =
    match synTypeConstraint with
    | WhereTyparIsValueType (tpar, _)
    | WhereTyparIsReferenceType (tpar, _)
    | WhereTyparIsUnmanaged (tpar, _)
    | WhereTyparSupportsNull (tpar, _)
    | WhereTyparIsComparable (tpar, _)
    | WhereTyparIsEquatable (tpar, _) -> countSynTypar tpar
    | WhereTyparDefaultsToType (tpar, t, _)
    | WhereTyparSubtypeOfType (tpar, t, _) -> countSynTypar tpar >=> countSynType t
    | WhereTyparSupportsMember (tps, sms, _) ->
        bindItems countSynType tps
        >=> countSynMemberSig sms
    | WhereTyparIsEnum (typar, types, _)
    | WhereTyparIsDelegate (typar, types, _) ->
        countSynTypar typar
        >=> bindItems countSynType types

and countSynTyparDecl synTyparDecl =
    match synTyparDecl with
    | SynTyparDecl.TyparDecl (attributes, typar) ->
        countSynAttributes attributes
        >=> countSynTypar typar

and countSynTyparDecls synValTyparDecls =
    match synValTyparDecls with
    | SynValTyparDecls.SynValTyparDecls (typars, _, tc) ->
        bindItems countSynTyparDecl typars
        >=> bindItems countSynTypeConstraint tc

and countSynArgInfo synArgInfo =
    match synArgInfo with
    | SynArgInfo.SynArgInfo (attributes, _, ident) ->
        countSynAttributes attributes
        >=> bindOption countIdent ident

and countSynValInfo synValInfo =
    match synValInfo with
    | SynValInfo.SynValInfo (sais, sai) ->
        List.collect id sais
        |> bindItems countSynArgInfo
        >=> countSynArgInfo sai

and countSynValSig synValSig =
    match synValSig with
    | SynValSig.ValSpfn (attributes, ident, typarDecls, synType, synValInfo, _, _, _, _, expr, _) ->
        countSynAttributes attributes
        >=> countIdent ident
        >=> countSynTyparDecls typarDecls
        >=> countSynType synType
        >=> countSynValInfo synValInfo
        >=> bindOption countSynExpr expr

and countSynComponentInfo synComponentInfo =
    match synComponentInfo with
    | SynComponentInfo.ComponentInfo (attributes, typarDecls, synTypeConstraints, lid, _, _, _, _) ->
        countSynAttributes attributes
        >=> bindItems countSynTyparDecl typarDecls
        >=> bindItems countSynTypeConstraint synTypeConstraints
        >=> countLongIdent lid

and countSynMemberSig synMemberSig =
    match synMemberSig with
    | SynMemberSig.Inherit (t, _)
    | SynMemberSig.Interface (t, _) -> countSynType t
    | SynMemberSig.ValField (f, _) -> countSynField f
    | SynMemberSig.Member (svs, _, _) -> countSynValSig svs
    | SynMemberSig.NestedType (sg, _) -> countSynTypeDefnSig sg

and countSynUnionCaseType synUnionCaseType =
    match synUnionCaseType with
    | SynUnionCaseType.UnionCaseFields fields -> bindItems countSynField fields
    | SynUnionCaseType.UnionCaseFullType (t, valInfo) -> countSynType t >=> countSynValInfo valInfo

and countSynUnionCase synUnionCase =
    match synUnionCase with
    | SynUnionCase.UnionCase (attributes, ident, unionCaseType, _, _, _) ->
        countSynAttributes attributes
        >=> countIdent ident
        >=> countSynUnionCaseType unionCaseType

and countSynEnumCase synEnumCase =
    match synEnumCase with
    | SynEnumCase.EnumCase (attributes, ident, _, _, _) -> countSynAttributes attributes >=> countIdent ident

and countSynTypeDefnKind synTypeDefnKind =
    match synTypeDefnKind with
    | TyconUnspecified
    | TyconClass
    | TyconInterface
    | TyconStruct
    | TyconRecord
    | TyconUnion
    | TyconAbbrev
    | TyconHiddenRepr
    | TyconAugmentation
    | TyconILAssemblyCode -> id
    | TyconDelegate (t, valInfo) -> countSynType t >=> countSynValInfo valInfo

and countSynExceptionDefnRepr synExceptionDefnRepr =
    match synExceptionDefnRepr with
    | SynExceptionDefnRepr.SynExceptionDefnRepr (attributes, unionCase, lid, _, _, _) ->
        countSynAttributes attributes
        >=> countSynUnionCase unionCase
        >=> bindOption countLongIdent lid

and countSynTypeDefnSimpleRepr synTypeDefnSimpleRepr =
    match synTypeDefnSimpleRepr with
    | SynTypeDefnSimpleRepr.Union (_, suc, _) -> bindItems countSynUnionCase suc
    | SynTypeDefnSimpleRepr.Enum (cases, _) -> bindItems countSynEnumCase cases
    | SynTypeDefnSimpleRepr.Record (_, fields, _) -> bindItems countSynField fields
    | SynTypeDefnSimpleRepr.General (stDefnKind, types, sigs, fields, _, _, pats, _) ->
        countSynTypeDefnKind stDefnKind
        >=> bindItems (fun (t, _, io) -> countSynType t >=> bindOption countIdent io) types
        >=> bindItems (fst >> countSynValSig) sigs
        >=> bindItems countSynField fields
        >=> bindOption countSynSimplePats pats
    | SynTypeDefnSimpleRepr.LibraryOnlyILAssembly _ -> id
    | SynTypeDefnSimpleRepr.TypeAbbrev (_, t, _) -> countSynType t
    | SynTypeDefnSimpleRepr.None _ -> id
    | SynTypeDefnSimpleRepr.Exception exdr -> countSynExceptionDefnRepr exdr

and countSynTypeDefnSigRepr synTypeDefnSigRepr =
    match synTypeDefnSigRepr with
    | SynTypeDefnSigRepr.ObjectModel (_, sms, _) -> bindItems countSynMemberSig sms
    | SynTypeDefnSigRepr.Simple (stdsr, _) -> countSynTypeDefnSimpleRepr stdsr
    | SynTypeDefnSigRepr.Exception exdr -> countSynExceptionDefnRepr exdr

and countSynTypeDefnSig synTypeDefnSig =
    match synTypeDefnSig with
    | SynTypeDefnSig.TypeDefnSig (sci, stdsr, sms, _) ->
        countSynComponentInfo sci
        >=> countSynTypeDefnSigRepr stdsr
        >=> bindItems countSynMemberSig sms

and countSynSimplePat p =
    match p with
    | SynSimplePat.Id (id, _, _, isCompilerGenerated, _, _) when (not isCompilerGenerated) -> countIdent id
    | SynSimplePat.Id _ -> id
    | SynSimplePat.Typed (pat, t, _) -> countSynSimplePat pat >=> countSynType t
    | SynSimplePat.Attrib (pat, attributes, _) ->
        countSynSimplePat pat
        >=> countSynAttributes attributes

and countSynSimplePats pats =
    match pats with
    | SynSimplePats.SimplePats (pats, _) -> bindItems countSynSimplePat pats
    | SynSimplePats.Typed (pats, t, _) -> countSynSimplePats pats >=> countSynType t

and countSynValTyparDecls synValTyparDecls =
    match synValTyparDecls with
    | SynValTyparDecls.SynValTyparDecls (decls, _, constraints) ->
        bindItems countSynTyparDecl decls
        >=> bindItems countSynTypeConstraint constraints

and countSynPat synPat =
    match synPat with
    | SynPat.Named (pat, ident, _, _, _) -> countSynPat pat >=> countIdent ident
    | SynPat.Typed (pat, t, _) -> countSynPat pat >=> countSynType t
    | SynPat.Attrib (pat, attributes, _) -> countSynPat pat >=> countSynAttributes attributes
    | SynPat.Or (p1, p2, _) -> countSynPat p1 >=> countSynPat p2
    | SynPat.Ands (pats, _) -> bindItems countSynPat pats
    | SynPat.LongIdent (lid, ident, vtDecls, argPats, _, _) ->
        countLongIdentWithDots lid
        >=> bindOption countIdent ident
        >=> bindOption countSynValTyparDecls vtDecls
        >=> countSynArgPats argPats
    | SynPat.Wild _
    | SynPat.Const _ -> id
    | SynPat.Tuple (_, pats, _) -> bindItems countSynPat pats
    | SynPat.Paren (pat, _) -> countSynPat pat
    | SynPat.ArrayOrList (_, pats, _) -> bindItems countSynPat pats
    | SynPat.Record (fields, _) ->
        bindItems (fun ((lid, id), pat) ->
            countLongIdent lid
            >=> countIdent id
            >=> countSynPat pat) fields
    | SynPat.Null _ -> id
    | SynPat.OptionalVal (id, _) -> countIdent id
    | SynPat.IsInst (t, _) -> countSynType t
    | SynPat.QuoteExpr (expr, _) -> countSynExpr expr
    | SynPat.DeprecatedCharRange _ -> id
    | SynPat.InstanceMember (i1, i2, i3, _, _) ->
        countIdent i1
        >=> countIdent i2
        >=> bindOption countIdent i3
    | SynPat.FromParseError (pat, _) -> countSynPat pat

and countSynArgPats synArgPats =
    match synArgPats with
    | SynArgPats.Pats (pats) -> bindItems countSynPat pats
    | SynArgPats.NamePatPairs (npats, _) -> bindItems (fun (i, s) -> countIdent i >=> countSynPat s) npats

and countComplexPat cp =
    match cp with
    | CPAttrib (attributes, cp) ->
        countSynAttributes attributes
        >=> countComplexPat cp
    | CPId pat -> countSynPat pat
    | CPSimpleId (s, _, _) -> map s.Length
    | CPTyped (cp, t) -> countComplexPat cp >=> countSynType t

and countComplexPats cp =
    match cp with
    | ComplexPats cps -> bindItems countComplexPat cps
    | ComplexTyped (cps, t) -> countComplexPats cps >=> countSynType t

type CountAstNode = ComplexPatsList of ComplexPats list

let isASTLongerThan threshold node =
    lift threshold
    |> match node with
       | ComplexPatsList pats -> bindItems countComplexPats pats
    |> Dbg.tee (printfn "%A")
    |> function
    | UnderThreshold _ -> false
    | OverThreshold -> true
