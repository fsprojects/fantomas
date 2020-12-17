module internal Fantomas.SourceTransformer

open FSharp.Compiler.SyntaxTree
open Fantomas.Context
open Fantomas.SourceParser
open Fantomas.TriviaTypes

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
    | ConstExpr (Const "()", _)
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

let getByLookup range f x =
    fun ctx ->
        if ctx.Config.StrictMode then
            f x ctx
        else
            match lookup range ctx with
            | Some x' -> str x' ctx
            | None -> f x ctx

// A few active patterns for printing purpose

let rec (|DoExprAttributesL|_|) =
    function
    | DoExpr _
    | Attributes _ as x :: DoExprAttributesL (xs, ys) -> Some(x :: xs, ys)
    | DoExpr _
    | Attributes _ as x :: ys -> Some([ x ], ys)
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

let (|SigMultilineModuleDecl|_|) =
    function
    | SigHashDirective _
    | SigModuleAbbrev _
    | SigVal _
    | SigOpen _ -> None
    | md -> Some md

let rec (|SigMultilineModuleDeclL|_|) =
    function
    | SigMultilineModuleDecl x :: SigMultilineModuleDeclL (xs, ys) -> Some(x :: xs, ys)
    | SigMultilineModuleDecl x :: ys -> Some([ x ], ys)
    | _ -> None

/// Gather PropertyGetSet in one printing call.
/// Assume that PropertySet comes right after PropertyGet.
let (|PropertyWithGetSet|_|) =
    function
    | PropertyBinding (_, _, _, _, MFProperty PropertyGet, PatLongIdent (_, s1, _, _), _) as b1 :: bs ->
        match bs with
        | PropertyBinding (_, _, _, _, MFProperty PropertySet, PatLongIdent (_, s2, _, _), _) as b2 :: bs when s1 = s2 ->
            Some((b1, b2), bs)
        | _ -> None
    | PropertyBinding (_, _, _, _, MFProperty PropertySet, PatLongIdent (_, s2, _, _), _) as b2 :: bs ->
        match bs with
        | PropertyBinding (_, _, _, _, MFProperty PropertyGet, PatLongIdent (_, s1, _, _), _) as b1 :: bs when s1 = s2 ->
            Some((b1, b2), bs)
        | _ -> None
    | _ -> None

let (|PropertyWithGetSetMemberDefn|_|) =
    function
    | MDMember (x1) :: MDMember (x2) :: xs ->
        match [ x1; x2 ] with
        | PropertyWithGetSet ((x1, x2), []) -> Some((x1, x2), xs)
        | _ -> None
    | _ -> None

let addParenIfAutoNln synExpr f =
    let expr = f synExpr
    expressionFitsOnRestOfLine expr (ifElse (hasParenthesis synExpr) (sepOpenT +> expr +> sepCloseT) expr)

let addParenForTupleWhen f synExpr ctx =
    let condition e =
        match e with
        | ElIf _
        | SynExpr.Lambda _ -> true
        | _ -> false // "if .. then .. else" have precedence over ","

    let expr = f synExpr
    ifElse (condition synExpr) (sepOpenT +> expr +> sepCloseT) expr ctx

let lengthWhenSome f o =
    match o with
    | Some x -> f x
    | None -> 0

let getSynAccessLength ao =
    lengthWhenSome
        (function
        | SynAccess.Internal -> 8
        | SynAccess.Private -> 7
        | SynAccess.Public -> 6)
        ao

let rec getSynTypeLength (synType: SynType) =
    match synType with
    | TFun (t1, t2) ->
        getSynTypeLength (* -> *) t1
        + 2
        + getSynTypeLength t2
    | TVar (Typar (id, _)) -> id.Length
    | TApp (t, ts, _) ->
        getSynTypeLength t
        + List.sumBy getSynTypeLength ts
    | TLongIdent s -> s.Length
    | _ -> 0

let rec getSynPatLength (synPat: SynPat) =
    match synPat with
    | PatLongIdent (ao, s, ps, _) ->
        let accessLength = getSynAccessLength ao

        let patternLength =
            ps
            |> List.sumBy
                (fun (ident, pat) ->
                    let identLength = lengthWhenSome String.length ident
                    identLength + getSynPatLength pat)

        accessLength + patternLength + s.Length

    | PatParen pat -> 2 + getSynPatLength pat

    | PatNamed (ao, p, s) ->
        let accessLength = getSynAccessLength ao
        accessLength + getSynPatLength p + s.Length

    | PatTyped (p, t) -> getSynPatLength p + getSynTypeLength t

    | _ -> 0

let synModuleDeclToFsAstType =
    function
    | SynModuleDecl.DoExpr _ -> SynModuleDecl_DoExpr
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

let synExprToFsAstType =
    function
    | SynExpr.YieldOrReturn _ -> SynExpr_YieldOrReturn
    | SynExpr.IfThenElse _ -> SynExpr_IfThenElse
    | SynExpr.LetOrUseBang _ -> SynExpr_LetOrUseBang
    | SynExpr.Const _ -> SynExpr_Const
    | SynExpr.Lambda _ -> SynExpr_Lambda
    | SynExpr.Ident _ -> SynExpr_Ident
    | SynExpr.App _ -> SynExpr_App
    | SynExpr.Match _ -> SynExpr_Match
    | SynExpr.Record _ -> SynExpr_Record
    | SynExpr.Tuple _ -> SynExpr_Tuple
    | SynExpr.DoBang _ -> SynExpr_DoBang
    | SynExpr.Paren _ -> SynExpr_Paren
    | SynExpr.AnonRecd _ -> SynExpr_AnonRecd
    | SynExpr.ArrayOrListOfSeqExpr _ -> SynExpr_ArrayOrListOfSeqExpr
    | SynExpr.LongIdentSet _ -> SynExpr_LongIdentSet
    | SynExpr.New _ -> SynExpr_New
    | SynExpr.Quote _ -> SynExpr_Quote
    | SynExpr.DotIndexedSet _ -> SynExpr_DotIndexedSet
    | SynExpr.LetOrUse _ -> SynExpr_LetOrUse
    | SynExpr.TryWith _ -> SynExpr_TryWith
    | SynExpr.YieldOrReturnFrom _ -> SynExpr_YieldOrReturnFrom
    | SynExpr.While _ -> SynExpr_While
    | SynExpr.TryFinally _ -> SynExpr_TryFinally
    | SynExpr.Do _ -> SynExpr_Do
    | SynExpr.AddressOf _ -> SynExpr_AddressOf
    | SynExpr.Typed _ -> SynExpr_Typed
    | SynExpr.ArrayOrList _ -> SynExpr_ArrayOrList
    | SynExpr.ObjExpr _ -> SynExpr_ObjExpr
    | SynExpr.For _ -> SynExpr_For
    | SynExpr.ForEach _ -> SynExpr_ForEach
    | SynExpr.CompExpr _ -> SynExpr_CompExpr
    | SynExpr.MatchLambda _ -> SynExpr_MatchLambda
    | SynExpr.Assert _ -> SynExpr_Assert
    | SynExpr.TypeApp _ -> SynExpr_TypeApp
    | SynExpr.Lazy _ -> SynExpr_Lazy
    | SynExpr.LongIdent _ -> SynExpr_LongIdent
    | SynExpr.DotGet _ -> SynExpr_DotGet
    | SynExpr.DotSet _ -> SynExpr_DotSet
    | SynExpr.Set _ -> SynExpr_Set
    | SynExpr.DotIndexedGet _ -> SynExpr_DotIndexedGet
    | SynExpr.NamedIndexedPropertySet _ -> SynExpr_NamedIndexedPropertySet
    | SynExpr.DotNamedIndexedPropertySet _ -> SynExpr_DotNamedIndexedPropertySet
    | SynExpr.TypeTest _ -> SynExpr_TypeTest
    | SynExpr.Upcast _ -> SynExpr_Upcast
    | SynExpr.Downcast _ -> SynExpr_Downcast
    | SynExpr.InferredUpcast _ -> SynExpr_InferredUpcast
    | SynExpr.InferredDowncast _ -> SynExpr_InferredDowncast
    | SynExpr.Null _ -> SynExpr_Null
    | SynExpr.TraitCall _ -> SynExpr_TraitCall
    | SynExpr.JoinIn _ -> SynExpr_JoinIn
    | SynExpr.ImplicitZero _ -> SynExpr_ImplicitZero
    | SynExpr.SequentialOrImplicitYield _ -> SynExpr_SequentialOrImplicitYield
    | SynExpr.MatchBang _ -> SynExpr_MatchBang
    | SynExpr.LibraryOnlyILAssembly _ -> SynExpr_LibraryOnlyILAssembly
    | SynExpr.LibraryOnlyStaticOptimization _ -> SynExpr_LibraryOnlyStaticOptimization
    | SynExpr.LibraryOnlyUnionCaseFieldGet _ -> SynExpr_LibraryOnlyUnionCaseFieldGet
    | SynExpr.LibraryOnlyUnionCaseFieldSet _ -> SynExpr_LibraryOnlyUnionCaseFieldSet
    | SynExpr.ArbitraryAfterError _ -> SynExpr_ArbitraryAfterError
    | SynExpr.FromParseError _ -> SynExpr_FromParseError
    | SynExpr.DiscardAfterMissingQualificationAfterDot _ -> SynExpr_DiscardAfterMissingQualificationAfterDot
    | SynExpr.Fixed _ -> SynExpr_Fixed
    | SynExpr.InterpolatedString _ -> SynExpr_InterpolatedString
    | SynExpr.Sequential _ -> SynExpr_Sequential

let synModuleSigDeclToFsAstType =
    function
    | SynModuleSigDecl.Val _ -> ValSpfn_
    | SynModuleSigDecl.Exception _ -> SynModuleSigDecl_Exception
    | SynModuleSigDecl.NestedModule _ -> SynModuleSigDecl_NestedModule
    | SynModuleSigDecl.Types _ -> SynModuleSigDecl_Types
    | SynModuleSigDecl.Open _ -> SynModuleSigDecl_Open
    | SynModuleSigDecl.HashDirective _ -> SynModuleSigDecl_HashDirective
    | SynModuleSigDecl.NamespaceFragment _ -> SynModuleSigDecl_NamespaceFragment
    | SynModuleSigDecl.ModuleAbbrev _ -> SynModuleSigDecl_ModuleAbbrev

let synBindingToFsAstType (Binding (_, kind, _, _, _, _, _, _, _, _, _, _)) =
    match kind with
    | SynBindingKind.StandaloneExpression -> StandaloneExpression_
    | SynBindingKind.NormalBinding -> NormalBinding_
    | SynBindingKind.DoBinding -> DoBinding_
