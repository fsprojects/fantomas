module internal Fantomas.Core.SourceTransformer

open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open Fantomas.Core.SourceParser
open Fantomas.Core.TriviaTypes

[<RequireQualifiedAccess>]
module List =
    let inline atMostOne xs =
        match xs with
        | []
        | [ _ ] -> true
        | _ -> false

/// Check if the expression already has surrounding parentheses
let hasParenthesis =
    function
    | Paren _
    | ConstUnitExpr _
    | Tuple _ -> true
    | _ -> false

let isArrayOrList =
    function
    | ArrayOrList _ -> true
    | _ -> false

let hasParenInPat =
    function
    | PatParen _ -> true
    | _ -> false

// A few active patterns for printing purpose

let rec (|DeclExprAttributesL|_|) =
    function
    | DeclExpr _ | Attributes _ as x :: DeclExprAttributesL (xs, ys) -> Some(x :: xs, ys)
    | DeclExpr _ | Attributes _ as x :: ys -> Some([ x ], ys)
    | _ -> None

let rec (|HashDirectiveL|_|) =
    function
    | HashDirective _ as x :: HashDirectiveL (xs, ys) -> Some(x :: xs, ys)
    | HashDirective _ as x :: ys -> Some([ x ], ys)
    | _ -> None

let rec (|SigHashDirectiveL|_|) =
    function
    | SigHashDirective _ as x :: SigHashDirectiveL (xs, ys) -> Some(x :: xs, ys)
    | SigHashDirective _ as x :: ys -> Some([ x ], ys)
    | _ -> None

let rec (|ModuleAbbrevL|_|) =
    function
    | ModuleAbbrev _ as x :: ModuleAbbrevL (xs, ys) -> Some(x :: xs, ys)
    | ModuleAbbrev _ as x :: ys -> Some([ x ], ys)
    | _ -> None

let rec (|SigModuleAbbrevL|_|) =
    function
    | SigModuleAbbrev _ as x :: SigModuleAbbrevL (xs, ys) -> Some(x :: xs, ys)
    | SigModuleAbbrev _ as x :: ys -> Some([ x ], ys)
    | _ -> None

let rec (|OpenL|_|) =
    function
    | Open _ as x :: OpenL (xs, ys) -> Some(x :: xs, ys)
    | Open _ as x :: ys -> Some([ x ], ys)
    | _ -> None

let rec (|AttributesL|_|) =
    function
    | Attributes _ as x :: AttributesL (xs, ys) -> Some(x :: xs, ys)
    | Attributes _ as x :: ys -> Some([ x ], ys)
    | _ -> None

let rec (|SigOpenL|_|) =
    function
    | SigOpen _ as x :: SigOpenL (xs, ys) -> Some(x :: xs, ys)
    | SigOpen _ as x :: ys -> Some([ x ], ys)
    | _ -> None

let rec (|MDOpenL|_|) =
    function
    | MDOpen _ as x :: MDOpenL (xs, ys) -> Some(x :: xs, ys)
    | MDOpen _ as x :: ys -> Some([ x ], ys)
    | _ -> None

let rec (|SigValL|_|) =
    function
    | SigVal _ as x :: SigValL (xs, ys) -> Some(x :: xs, ys)
    | SigVal _ as x :: ys -> Some([ x ], ys)
    | _ -> None

let private (|SynLongIdentAsString|) (synLongIdent: SynLongIdent) =
    synLongIdent.LongIdent |> List.map (fun i -> i.idText) |> String.concat "."

// Provide short-hand notation `x.Member = ...` for `x.Member with get()` getters
let (|LongGetMember|_|) =
    function
    | SynMemberDefn.GetSetMember (Some (SynBinding (ao,
                                                    kind,
                                                    isInline,
                                                    isMutable,
                                                    ats,
                                                    px,
                                                    valData,
                                                    (PatLongIdent (None, _, [ _, PatParen (_, PatUnitConst, _) ], _) as p),
                                                    ri,
                                                    e,
                                                    bindingRange,
                                                    dp,
                                                    trivia)),
                                  None,
                                  _,
                                  { GetKeyword = Some _ }) ->
        let pat =
            match p with
            | SynPat.LongIdent (lid, extraId, typarDecls, _, accessibility, range) ->
                SynPat.LongIdent(lid, extraId, typarDecls, SynArgPats.Pats([]), accessibility, range)
            | _ -> p

        Some(SynBinding(ao, kind, isInline, isMutable, ats, px, valData, pat, ri, e, bindingRange, dp, trivia))
    | _ -> None

let synModuleDeclToFsAstType =
    function
    | SynModuleDecl.Expr _ -> SynModuleDecl_Expr
    | SynModuleDecl.Types _ -> SynModuleDecl_Types
    | SynModuleDecl.NestedModule _ -> SynModuleDecl_NestedModule
    | SynModuleDecl.Let _ -> SynModuleDecl_Let
    | SynModuleDecl.Open (SynOpenDeclTarget.ModuleOrNamespace _, _) -> SynModuleDecl_Open
    | SynModuleDecl.Open (SynOpenDeclTarget.Type _, _) -> SynModuleDecl_OpenType
    | SynModuleDecl.ModuleAbbrev _ -> SynModuleDecl_ModuleAbbrev
    | SynModuleDecl.Exception _ -> SynModuleDecl_Exception
    | SynModuleDecl.Attributes _ -> SynModuleDecl_Attributes
    | SynModuleDecl.HashDirective _ -> SynModuleDecl_HashDirective
    | SynModuleDecl.NamespaceFragment _ -> SynModuleDecl_NamespaceFragment

let synMemberDefnToFsAstType =
    function
    | SynMemberDefn.Member _ -> SynMemberDefn_Member
    | SynMemberDefn.Open _ -> SynMemberDefn_Open
    | SynMemberDefn.ImplicitCtor _ -> SynMemberDefn_ImplicitCtor
    | SynMemberDefn.ImplicitInherit _ -> SynMemberDefn_ImplicitInherit
    | SynMemberDefn.LetBindings _ -> SynMemberDefn_LetBindings
    | SynMemberDefn.AbstractSlot _ -> SynMemberDefn_AbstractSlot
    | SynMemberDefn.Interface _ -> SynMemberDefn_Interface
    | SynMemberDefn.Inherit _ -> SynMemberDefn_Inherit
    | SynMemberDefn.ValField _ -> SynMemberDefn_ValField
    | SynMemberDefn.NestedType _ -> SynMemberDefn_NestedType
    | SynMemberDefn.AutoProperty _ -> SynMemberDefn_AutoProperty
    | SynMemberDefn.GetSetMember _ -> SynMemberDefn_GetSetMember

let synMemberSigToFsAstType =
    function
    | SynMemberSig.Interface _ -> SynMemberSig_Interface
    | SynMemberSig.Inherit _ -> SynMemberSig_Inherit
    | SynMemberSig.Member _ -> SynMemberSig_Member
    | SynMemberSig.NestedType _ -> SynMemberSig_NestedType
    | SynMemberSig.ValField _ -> SynMemberSig_ValField

let synConstToFsAstType =
    function
    | SynConst.Bool _ -> SynConst_Bool
    | SynConst.Unit _ -> SynConst_Unit
    | SynConst.SByte _ -> SynConst_SByte
    | SynConst.Byte _ -> SynConst_Byte
    | SynConst.Int16 _ -> SynConst_Int16
    | SynConst.UInt16 _ -> SynConst_UInt16
    | SynConst.Int32 _ -> SynConst_Int32
    | SynConst.UInt32 _ -> SynConst_UInt32
    | SynConst.Int64 _ -> SynConst_Int64
    | SynConst.UInt64 _ -> SynConst_UInt64
    | SynConst.IntPtr _ -> SynConst_IntPtr
    | SynConst.UIntPtr _ -> SynConst_UIntPtr
    | SynConst.Single _ -> SynConst_Single
    | SynConst.Double _ -> SynConst_Double
    | SynConst.Char _ -> SynConst_Char
    | SynConst.Decimal _ -> SynConst_Decimal
    | SynConst.UserNum _ -> SynConst_UserNum
    | SynConst.String _ -> SynConst_String
    | SynConst.Bytes _ -> SynConst_Bytes
    | SynConst.UInt16s _ -> SynConst_UInt16s
    | SynConst.Measure _ -> SynConst_Measure
    | SynConst.SourceIdentifier _ -> SynConst_SourceIdentifier

let rec synExprToFsAstType (expr: SynExpr) : FsAstType * Range =
    match expr with
    | SynExpr.YieldOrReturn _ -> SynExpr_YieldOrReturn, expr.Range
    | SynExpr.IfThenElse _ -> SynExpr_IfThenElse, expr.Range
    | SynExpr.LetOrUseBang _ -> SynExpr_LetOrUseBang, expr.Range
    | SynExpr.Const _ -> SynExpr_Const, expr.Range
    | SynExpr.Lambda _ -> SynExpr_Lambda, expr.Range
    | SynExpr.Ident _ -> SynExpr_Ident, expr.Range
    | SynExpr.App _ -> SynExpr_App, expr.Range
    | SynExpr.Match _ -> SynExpr_Match, expr.Range
    | SynExpr.Record _ -> SynExpr_Record, expr.Range
    | SynExpr.Tuple _ -> SynExpr_Tuple, expr.Range
    | SynExpr.DoBang _ -> SynExpr_DoBang, expr.Range
    | SynExpr.Paren _ -> SynExpr_Paren, expr.Range
    | SynExpr.AnonRecd _ -> SynExpr_AnonRecd, expr.Range
    | SynExpr.ArrayOrList _ -> SynExpr_ArrayOrList, expr.Range
    | SynExpr.ArrayOrListComputed _ -> SynExpr_ArrayOrList, expr.Range
    | SynExpr.LongIdentSet _ -> SynExpr_LongIdentSet, expr.Range
    | SynExpr.New _ -> SynExpr_New, expr.Range
    | SynExpr.Quote _ -> SynExpr_Quote, expr.Range
    | SynExpr.DotIndexedSet _ -> SynExpr_DotIndexedSet, expr.Range
    | SynExpr.LetOrUse (bindings = bs; body = e) ->
        match bs with
        | [] -> synExprToFsAstType e
        | SynBinding (kind = kind) as b :: _ ->
            match kind with
            | SynBindingKind.StandaloneExpression -> SynBindingKind_StandaloneExpression, b.RangeOfBindingWithRhs
            | SynBindingKind.Normal -> SynBindingKind_Normal, b.RangeOfBindingWithRhs
            | SynBindingKind.Do -> SynBindingKind_Do, b.RangeOfBindingWithRhs
    | SynExpr.TryWith _ -> SynExpr_TryWith, expr.Range
    | SynExpr.YieldOrReturnFrom _ -> SynExpr_YieldOrReturnFrom, expr.Range
    | SynExpr.While _ -> SynExpr_While, expr.Range
    | SynExpr.TryFinally _ -> SynExpr_TryFinally, expr.Range
    | SynExpr.Do _ -> SynExpr_Do, expr.Range
    | SynExpr.AddressOf _ -> SynExpr_AddressOf, expr.Range
    | SynExpr.Typed (e, _, _) -> synExprToFsAstType e
    | SynExpr.ObjExpr _ -> SynExpr_ObjExpr, expr.Range
    | SynExpr.For _ -> SynExpr_For, expr.Range
    | SynExpr.ForEach _ -> SynExpr_ForEach, expr.Range
    | SynExpr.ComputationExpr (_, e, _) -> synExprToFsAstType e
    | SynExpr.MatchLambda _ -> SynExpr_MatchLambda, expr.Range
    | SynExpr.Assert _ -> SynExpr_Assert, expr.Range
    | SynExpr.TypeApp _ -> SynExpr_TypeApp, expr.Range
    | SynExpr.Lazy _ -> SynExpr_Lazy, expr.Range
    | SynExpr.LongIdent _ -> SynExpr_LongIdent, expr.Range
    | SynExpr.DotGet _ -> SynExpr_DotGet, expr.Range
    | SynExpr.DotSet _ -> SynExpr_DotSet, expr.Range
    | SynExpr.Set _ -> SynExpr_Set, expr.Range
    | SynExpr.DotIndexedGet _ -> SynExpr_DotIndexedGet, expr.Range
    | SynExpr.NamedIndexedPropertySet _ -> SynExpr_NamedIndexedPropertySet, expr.Range
    | SynExpr.DotNamedIndexedPropertySet _ -> SynExpr_DotNamedIndexedPropertySet, expr.Range
    | SynExpr.TypeTest _ -> SynExpr_TypeTest, expr.Range
    | SynExpr.Upcast _ -> SynExpr_Upcast, expr.Range
    | SynExpr.Downcast _ -> SynExpr_Downcast, expr.Range
    | SynExpr.InferredUpcast _ -> SynExpr_InferredUpcast, expr.Range
    | SynExpr.InferredDowncast _ -> SynExpr_InferredDowncast, expr.Range
    | SynExpr.Null _ -> SynExpr_Null, expr.Range
    | SynExpr.TraitCall _ -> SynExpr_TraitCall, expr.Range
    | SynExpr.JoinIn _ -> SynExpr_JoinIn, expr.Range
    | SynExpr.ImplicitZero _ -> SynExpr_ImplicitZero, expr.Range
    | SynExpr.SequentialOrImplicitYield _ -> SynExpr_SequentialOrImplicitYield, expr.Range
    | SynExpr.MatchBang _ -> SynExpr_MatchBang, expr.Range
    | SynExpr.LibraryOnlyILAssembly _ -> SynExpr_LibraryOnlyILAssembly, expr.Range
    | SynExpr.LibraryOnlyStaticOptimization _ -> SynExpr_LibraryOnlyStaticOptimization, expr.Range
    | SynExpr.LibraryOnlyUnionCaseFieldGet _ -> SynExpr_LibraryOnlyUnionCaseFieldGet, expr.Range
    | SynExpr.LibraryOnlyUnionCaseFieldSet _ -> SynExpr_LibraryOnlyUnionCaseFieldSet, expr.Range
    | SynExpr.ArbitraryAfterError _ -> SynExpr_ArbitraryAfterError, expr.Range
    | SynExpr.FromParseError _ -> SynExpr_FromParseError, expr.Range
    | SynExpr.DiscardAfterMissingQualificationAfterDot _ -> SynExpr_DiscardAfterMissingQualificationAfterDot, expr.Range
    | SynExpr.Fixed _ -> SynExpr_Fixed, expr.Range
    | SynExpr.InterpolatedString _ -> SynExpr_InterpolatedString, expr.Range
    | SynExpr.Sequential (_, _, e, _, _) -> synExprToFsAstType e
    | SynExpr.IndexRange _ -> SynExpr_IndexRange, expr.Range
    | SynExpr.IndexFromEnd _ -> SynExpr_IndexFromEnd, expr.Range
    | SynExpr.DebugPoint (innerExpr = e) -> synExprToFsAstType e
    | SynExpr.Dynamic _ -> SynExpr_Dynamic, expr.Range

let synModuleSigDeclToFsAstType =
    function
    | SynModuleSigDecl.Val _ -> SynModuleSigDecl_Val
    | SynModuleSigDecl.Exception _ -> SynModuleSigDecl_Exception
    | SynModuleSigDecl.NestedModule _ -> SynModuleSigDecl_NestedModule
    | SynModuleSigDecl.Types _ -> SynModuleSigDecl_Types
    | SynModuleSigDecl.Open (SynOpenDeclTarget.ModuleOrNamespace _, _) -> SynModuleSigDecl_Open
    | SynModuleSigDecl.Open (SynOpenDeclTarget.Type _, _) -> SynModuleSigDecl_OpenType
    | SynModuleSigDecl.HashDirective _ -> SynModuleSigDecl_HashDirective
    | SynModuleSigDecl.NamespaceFragment _ -> SynModuleSigDecl_NamespaceFragment
    | SynModuleSigDecl.ModuleAbbrev _ -> SynModuleSigDecl_ModuleAbbrev

let synBindingToFsAstType (SynBinding (kind = kind)) =
    match kind with
    | SynBindingKind.StandaloneExpression -> SynBindingKind_StandaloneExpression
    | SynBindingKind.Normal -> SynBindingKind_Normal
    | SynBindingKind.Do -> SynBindingKind_Do

let synAccessToFsAstType (vis: SynAccess) =
    match vis with
    | SynAccess.Internal _ -> SynAccess_Internal
    | SynAccess.Private _ -> SynAccess_Private
    | SynAccess.Public _ -> SynAccess_Public
