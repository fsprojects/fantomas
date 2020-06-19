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
    | UnderThreshold _ as ut -> f ut
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
    | SynType.LongIdent lid -> countLongIdentWithDots lid
    | SynType.App (synType, _, types, _, _, _, _) ->
        countSynType synType
        >=> bindItems countSynType types
    | SynType.LongIdentApp (t, lids, _, types, _, _, _) ->
        countSynType t
        >=> countLongIdentWithDots lids
        >=> bindItems countSynType types
    | SynType.Tuple (_, types, _) -> bindItems (snd >> countSynType) types
    | SynType.AnonRecd (_, types, _) -> bindItems (fun (i, t) -> countIdent i >=> countSynType t) types
    | SynType.Array (_, t, _) -> countSynType t
    | SynType.Fun (t1, t2, _) -> countSynType t1 >=> countSynType t2
    | SynType.Var (typar, _) -> countSynTypar typar
    | SynType.Anon _ -> id
    | SynType.WithGlobalConstraints (t, constraints, _) ->
        countSynType t
        >=> bindItems countSynTypeConstraint constraints
    | SynType.HashConstraint (t, _) -> countSynType t
    | SynType.MeasureDivide (t1, t2, _) -> countSynType t1 >=> countSynType t2
    | SynType.MeasurePower (t, _, _) -> countSynType t
    | SynType.StaticConstant _ -> id
    | SynType.StaticConstantExpr (expr, _) -> countSynExpr expr
    | SynType.StaticConstantNamed (t1, t2, _) -> countSynType t1 >=> countSynType t2
    | SynType.Paren (t, _) -> countSynType t

and countSynExpr expr =
    match expr with
    | SynExpr.Paren (expr, _, _, _) -> countSynExpr expr
    | SynExpr.Quote (e1, _, e2, _, _) -> countSynExpr e1 >=> countSynExpr e2
    | SynExpr.Const _ -> id
    | SynExpr.Typed (e, t, _) -> countSynExpr e >=> countSynType t
    | SynExpr.Tuple (_, es, _, _) -> countSynExprs es
    | SynExpr.AnonRecd (_, eo, fields, _) ->
        bindOption (fst >> countSynExpr) eo
        >=> bindItems (fun (i, e) -> countIdent i >=> countSynExpr e) fields
    | SynExpr.ArrayOrList (_, es, _) -> countSynExprs es
    | SynExpr.Record (updateExpr, inheritExpr, fields, _) ->
        bindOption (fun (t, e, _, _, _) -> countSynType t >=> countSynExpr e) updateExpr
        >=> bindOption (fun (e, _) -> countSynExpr e) inheritExpr
        >=> bindItems (fun ((lid, _), eo, _) ->
                countLongIdentWithDots lid
                >=> countSynExprOption eo) fields
    | SynExpr.New (_, t, e, _) -> countSynType t >=> countSynExpr e
    | SynExpr.ObjExpr (t, eio, bindings, interfaces, _, _) ->
        countSynType t
        >=> bindOption (fun (e, io) -> countSynExpr e >=> bindOption countIdent io) eio
        >=> bindItems countSynBinding bindings
        >=> bindItems countSynInterfaceImpl interfaces
    | SynExpr.While (_, e1, e2, _) -> countSynExpr e1 >=> countSynExpr e2
    | SynExpr.For (_, ident, e1, _, e2, e3, _) ->
        countIdent ident
        >=> countSynExpr e1
        >=> countSynExpr e2
        >=> countSynExpr e3
    | SynExpr.ForEach (_, _, _, pat, e1, e2, _) ->
        countSynPat pat
        >=> countSynExpr e1
        >=> countSynExpr e2
    | SynExpr.ArrayOrListOfSeqExpr (_, e, _) -> countSynExpr e
    | SynExpr.CompExpr (_, _, e, _) -> countSynExpr e
    | SynExpr.Lambda (_, _, pats, e, _) -> countSynSimplePats pats >=> countSynExpr e
    | SynExpr.MatchLambda (_, _, smcs, _, _) -> bindItems countSynMatchClause smcs
    | SynExpr.Match (_, e, smcs, _) ->
        countSynExpr e
        >=> bindItems countSynMatchClause smcs
    | SynExpr.Do (e, _)
    | SynExpr.Assert (e, _) -> countSynExpr e
    | SynExpr.App (_, _, e1, e2, _) -> countSynExpr e1 >=> countSynExpr e2
    | SynExpr.TypeApp (e, _, types, _, _, _, _) -> countSynExpr e >=> bindItems countSynType types
    | SynExpr.LetOrUse (_, _, bindings, e, _) ->
        bindItems countSynBinding bindings
        >=> countSynExpr e
    | SynExpr.TryWith (e, _, smcs, _, _, _, _) ->
        countSynExpr e
        >=> bindItems countSynMatchClause smcs
    | SynExpr.TryFinally (e1, e2, _, _, _) -> countSynExpr e1 >=> countSynExpr e2
    | SynExpr.Lazy (e, _) -> countSynExpr e
    | SynExpr.Sequential (_, _, e1, e2, _) -> countSynExpr e1 >=> countSynExpr e2
    | SynExpr.IfThenElse (e1, e2, eo, _, _, _, _) ->
        countSynExpr e1
        >=> countSynExpr e2
        >=> countSynExprOption eo
    | SynExpr.Ident ident -> countIdent ident
    | SynExpr.LongIdent (_, lid, _, _) -> countLongIdentWithDots lid
    | SynExpr.LongIdentSet (lid, e, _) -> countLongIdentWithDots lid >=> countSynExpr e
    | SynExpr.DotGet (e, _, lid, _) -> countSynExpr e >=> countLongIdentWithDots lid
    | SynExpr.DotSet (e1, lid, e2, _) ->
        countSynExpr e1
        >=> countLongIdentWithDots lid
        >=> countSynExpr e2
    | SynExpr.Set (e1, e2, _) -> countSynExpr e1 >=> countSynExpr e2
    | SynExpr.DotIndexedGet (e, args, _, _) ->
        countSynExpr e
        >=> bindItems countSynIndexerArg args
    | SynExpr.DotIndexedSet (e1, args, e2, _, _, _) ->
        countSynExpr e1
        >=> bindItems countSynIndexerArg args
        >=> countSynExpr e2
    | SynExpr.NamedIndexedPropertySet (lid, e1, e2, _) ->
        countLongIdentWithDots lid
        >=> countSynExpr e1
        >=> countSynExpr e2
    | SynExpr.DotNamedIndexedPropertySet (e1, lid, e2, e3, _) ->
        countSynExpr e1
        >=> countLongIdentWithDots lid
        >=> countSynExpr e2
        >=> countSynExpr e3
    | SynExpr.TypeTest (e, t, _)
    | SynExpr.Upcast (e, t, _)
    | SynExpr.Downcast (e, t, _) -> countSynExpr e >=> countSynType t
    | SynExpr.InferredUpcast (e, _)
    | SynExpr.InferredDowncast (e, _) -> countSynExpr e
    | SynExpr.Null _ -> id
    | SynExpr.AddressOf (_, e, _, _) -> countSynExpr e
    | SynExpr.TraitCall (typars, sms, e, _) ->
        bindItems countSynTypar typars
        >=> countSynMemberSig sms
        >=> countSynExpr e
    | SynExpr.JoinIn (e1, _, e2, _) -> countSynExpr e1 >=> countSynExpr e2
    | SynExpr.ImplicitZero _ -> id
    | SynExpr.SequentialOrImplicitYield (_, e1, e2, e3, _) ->
        countSynExpr e1
        >=> countSynExpr e2
        >=> countSynExpr e3
    | SynExpr.YieldOrReturn (_, e, _)
    | SynExpr.YieldOrReturnFrom (_, e, _) -> countSynExpr e
    | SynExpr.LetOrUseBang (_, _, _, pat, e1, andBangs, e2, _) ->
        countSynPat pat
        >=> countSynExpr e1
        >=> bindItems (fun (_, _, _, pat, e, _) -> countSynPat pat >=> countSynExpr e) andBangs
        >=> countSynExpr e2
    | SynExpr.MatchBang (_, e, mcs, _) ->
        countSynExpr e
        >=> bindItems countSynMatchClause mcs
    | SynExpr.DoBang (e, _) -> countSynExpr e
    | SynExpr.LibraryOnlyILAssembly _
    | SynExpr.LibraryOnlyStaticOptimization _
    | SynExpr.LibraryOnlyUnionCaseFieldGet _
    | SynExpr.LibraryOnlyUnionCaseFieldSet _ -> id
    | SynExpr.ArbitraryAfterError (s, _) -> map s.Length
    | SynExpr.FromParseError (e, _) -> countSynExpr e
    | SynExpr.DiscardAfterMissingQualificationAfterDot (e, _)
    | SynExpr.Fixed (e, _) -> countSynExpr e

and countSynIndexerArg synIndexerArg =
    match synIndexerArg with
    | SynIndexerArg.One (e, _, _) -> countSynExpr e
    | SynIndexerArg.Two (e1, _, e2, _, _, _) -> countSynExpr e1 >=> countSynExpr e2

and countSynMatchClause synMatchClause =
    match synMatchClause with
    | SynMatchClause.Clause (pat, eo, e, _, _) ->
        countSynPat pat
        >=> countSynExprOption eo
        >=> countSynExpr e

and countSynValData synValData =
    match synValData with
    | SynValData (_, valInfo, io) ->
        countSynValInfo valInfo
        >=> bindOption countIdent io

and countSynBindingReturnInfo synBindingReturnInfo =
    match synBindingReturnInfo with
    | SynBindingReturnInfo.SynBindingReturnInfo (t, _, attributes) -> countSynType t >=> countSynAttributes attributes

and countSynBinding synBinding =
    match synBinding with
    | SynBinding.Binding (_, _, _, _, attributes, _, synValData, synPat, retInfo, e, _, _) ->
        countSynAttributes attributes
        >=> countSynValData synValData
        >=> countSynPat synPat
        >=> bindOption countSynBindingReturnInfo retInfo
        >=> countSynExpr e

and countSynInterfaceImpl synInterfaceImpl =
    match synInterfaceImpl with
    | SynInterfaceImpl.InterfaceImpl (t, bindings, _) ->
        countSynType t
        >=> bindItems countSynBinding bindings

and countSynExprs es = bindItems countSynExpr es

and countSynExprOption eo = bindOption countSynExpr eo

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
        >=> countSynExprOption expr

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

type CountAstNode =
    | ComplexPatsList of ComplexPats list
    | FunctionSignature of functionName: string * (string option * SynPat) list * SynType option
    | RecordInstance of (SynType * SynExpr) option * (RecordFieldName * SynExpr option * BlockSeparator option) list * SynExpr option

let isASTLongerThan threshold node =
    lift threshold
    |> match node with
       | ComplexPatsList pats -> bindItems countComplexPats pats
       | FunctionSignature (fn, pats, retType) ->
           map fn.Length
           >=> bindItems (fun (s, pat) ->
                   bindOption (String.length >> map) s
                   >=> countSynPat pat) pats
           >=> bindOption countSynType retType
        | RecordInstance(inheritOpt, fields, e) ->
            bindOption (fun (t,e) -> countSynType t >=> countSynExpr e) inheritOpt
            >=> bindItems (fun ((lid,_), eo, _) ->
                                countLongIdentWithDots lid
                                >=> countSynExprOption eo) fields
            >=> countSynExprOption e
    |> function
    | UnderThreshold _ -> false
    | OverThreshold -> true
