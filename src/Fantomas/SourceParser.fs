module internal Fantomas.SourceParser

open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.PrettyNaming
open Fantomas.FormatConfig

/// Get source string content based on range value
let inline content (sc : SynConst) (c : Context) = 
    let r = sc.Range range.Zero
    if r.EndLine <= c.Positions.Length then
        let start = c.Positions.[r.StartLine-1] + r.StartColumn
        let finish = c.Positions.[r.EndLine-1] + r.EndColumn - 1
        let content = c.Content
        let s = content.[start..finish]
        if s.Contains("\n") then
            /// Terrible hack to compensate the offset made by F# compiler
            let lastLine = content.[c.Positions.[r.EndLine-1]..finish]
            let offset = lastLine.Length - lastLine.TrimStart(' ').Length
            if finish + offset > content.Length then content.[start..]
            else content.[start..finish + offset]
        else s
    else ""

/// Use infix operators in the short form
let inline (|OpName|) s =
    if IsActivePatternName s then sprintf "(%s)" (DecompileOpName s)
    elif IsPrefixOperator s then 
        let s' = DecompileOpName s
        if s'.[0] = '~' && s'.Length >= 2 && s'.[1] <> '~' then s'.Substring(1) else s'
    else DecompileOpName s

/// Operators in their declaration form
let inline (|OpNameFull|) s =
    if IsActivePatternName s || IsInfixOperator s || IsPrefixOperator s || IsTernaryOperator s || s = "op_Dynamic"
    then
        let s' = DecompileOpName s
        /// Use two spaces for symmetry
        if s'.StartsWith("*") && s' <> "*" then sprintf "( %s )" s'
        else sprintf "(%s)" s'
    else DecompileOpName s

let inline (|Ident|) (s: Ident) = s.idText

let inline (|LongIdent|) (li: LongIdent) = 
    li |> Seq.map (fun s -> s.idText) |> String.concat "."

let inline (|LongIdentWithDots|) (LongIdentWithDots(s, _)) = 
    s |> Seq.map (fun s -> s.idText) |> String.concat "."

// Type params

let inline (|Typar|) (SynTypar.Typar(Ident s, req , _)) = 
    match req with
    | NoStaticReq -> (s, false)
    | HeadTypeStaticReq -> (s, true)

let inline (|ValTyparDecls|) (SynValTyparDecls(tds, b, tcs)) = (tds, b, tcs)
    
// Literals

let (|Measure|) x = 
    let rec loop = function
        | SynMeasure.Var(Typar(s, _), _) -> s
        | SynMeasure.Anon _ -> "_"
        | SynMeasure.One -> "1"
        | SynMeasure.Product(m1, m2, _) -> 
            let s1 = loop m1
            let s2 = loop m2
            sprintf "%s*%s" s1 s2
        | SynMeasure.Divide(m1, m2, _) -> 
            let s1 = loop m1
            let s2 = loop m2
            sprintf "%s/%s" s1 s2
        | SynMeasure.Power(m, n, _) -> 
            let s = loop m
            sprintf "%s^%i" s n
        | SynMeasure.Seq(ms, _) -> 
            List.map loop ms |> String.concat " "
        | SynMeasure.Named(LongIdent s, _) -> s
    sprintf "<%s>" <| loop x

/// Lose information about kinds of literals
let rec (|Const|Unresolved|) = function
    | SynConst.Measure(Const c, Measure m) -> Const(c + m)
    | SynConst.UserNum(num, ty) -> Const(num + ty)
    | SynConst.Unit -> Const "()"
    | SynConst.Bool b -> Const(sprintf "%A" b)
    | SynConst.SByte s -> Const(sprintf "%A" s)
    | SynConst.Byte b -> Const(sprintf "%A" b)
    | SynConst.Int16 i -> Const(sprintf "%A" i)
    | SynConst.UInt16 u -> Const(sprintf "%A" u)
    | SynConst.Int32 i -> Const(sprintf "%A" i)
    | SynConst.UInt32 u -> Const(sprintf "%A" u)
    | SynConst.Int64 i -> Const(sprintf "%A" i)
    | SynConst.UInt64 u -> Const(sprintf "%A" u)
    | SynConst.IntPtr i -> Const(sprintf "%in" i)
    | SynConst.UIntPtr u -> Const(sprintf "%iun" u)
    | SynConst.Single s -> Const(sprintf "%A" s)
    | SynConst.Double d -> Const(sprintf "%A" d)
    | SynConst.Char c -> Const(sprintf "%A" c)
    | SynConst.Decimal d -> Const(sprintf "%A" d)
    | SynConst.String _ as c -> Unresolved(c)
    | SynConst.Bytes _ as c -> Unresolved(c)
    // Auto print may cut off the array
    | SynConst.UInt16s us -> Const(sprintf "%A" us)
    | _ -> invalidArg "c" "Ill-formed constants"

// File level patterns

let (|ImplFile|SigFile|) = function
    | ParsedInput.ImplFile im -> ImplFile im
    | ParsedInput.SigFile si -> SigFile si

let (|ParsedImplFileInput|) = function
    | ParsedImplFileInput.ParsedImplFileInput(_, _, _, _, hs, mns, _) -> (hs, mns)

let (|ParsedSigFileInput|) = function
    | ParsedSigFileInput.ParsedSigFileInput(_, _, _, hs, mns) -> (hs, mns)

let (|ModuleOrNamespace|) = function
    | SynModuleOrNamespace.SynModuleOrNamespace(LongIdent s, isModule, mds, px, ats, ao, _) -> (ats, px, ao, s, mds, isModule)

let (|SigModuleOrNamespace|) = function
    | SynModuleOrNamespaceSig.SynModuleOrNamespaceSig(LongIdent s, isModule, mds, px, ats, ao, _) -> (ats, px, ao, s, mds, isModule)

// Attribute
let (|Attribute|) (a : SynAttribute) =
    let (LongIdentWithDots s) = a.TypeName
    (s, a.ArgExpr, a.AppliesToGetterAndSetter)

// Access modifiers
let (|Access|) = function
    | SynAccess.Public -> "public"
    | SynAccess.Internal -> "internal"
    | SynAccess.Private -> "private"

let (|PreXmlDoc|) (px: PreXmlDoc) =
    match px.ToXmlDoc() with
    | XmlDoc lines -> lines

let (|ParsedHashDirective|) (ParsedHashDirective(s, ss, _)) = (s, String.concat "." ss)

// Module declarations (10 cases)

let (|Open|_|) = function
    | SynModuleDecl.Open(LongIdentWithDots s, _) -> Some s
    | _ -> None

let (|ModuleAbbrev|_|) = function
    | SynModuleDecl.ModuleAbbrev(Ident s1, LongIdent s2, _) -> Some(s1, s2)
    | _ -> None

let (|HashDirective|_|) = function
    | SynModuleDecl.HashDirective(p, _) -> Some p
    | _ -> None

let (|NamespaceFragment|_|) = function 
    | SynModuleDecl.NamespaceFragment m -> Some m 
    | _ -> None

let (|Attributes|_|) = function
    | SynModuleDecl.Attributes(ats, _) -> Some ats
    | _ -> None

let (|Let|_|) = function
    | SynModuleDecl.Let(false, xs, _) -> Some (List.head xs)
    | _ -> None

let (|LetRec|_|) = function
    | SynModuleDecl.Let(true, xs, _) -> Some xs
    | _ -> None

let (|DoExpr|_|) = function
    | SynModuleDecl.DoExpr(_, x, _) -> Some x
    | _ -> None

let (|Types|_|) = function
    | SynModuleDecl.Types(xs, _) -> Some xs
    | _ -> None

let (|NestedModule|_|) = function
    | SynModuleDecl.NestedModule(SynComponentInfo.ComponentInfo(ats, _, _, LongIdent s, px, _, ao, _), xs, _, _) -> 
        Some(ats, px, ao, s, xs)
    | _ -> None

let (|Exception|_|) = function
    | SynModuleDecl.Exception(ed, _) -> Some ed
    | _ -> None

// Module declaration signatures (8 cases)

let (|SigOpen|_|) = function
    | SynModuleSigDecl.Open(LongIdent s, _) -> Some s
    | _ -> None

let (|SigModuleAbbrev|_|) = function
    | SynModuleSigDecl.ModuleAbbrev(Ident s1, LongIdent s2, _) -> Some(s1, s2)
    | _ -> None

let (|SigHashDirective|_|) = function
    | SynModuleSigDecl.HashDirective(p, _) -> Some p
    | _ -> None

let (|SigNamespaceFragment|_|) = function 
    | SynModuleSigDecl.NamespaceFragment m -> Some m 
    | _ -> None

let (|SigVal|_|) = function
    | SynModuleSigDecl.Val(v, _) -> Some v
    | _ -> None

let (|SigTypes|_|) = function
    | SynModuleSigDecl.Types(tds, _) -> Some tds
    | _ -> None

let (|SigNestedModule|_|) = function
    | SynModuleSigDecl.NestedModule(SynComponentInfo.ComponentInfo(ats, _, _, LongIdent s, px, _, ao, _), xs, _) -> 
        Some(ats, px, ao, s, xs)
    | _ -> None

let (|SigException|_|) = function
    | SynModuleSigDecl.Exception(es, _) -> Some es
    | _ -> None

// Exception definitions

let (|ExceptionDef|) = function
    | SynExceptionDefn.ExceptionDefn(SynExceptionRepr.ExceptionDefnRepr(ats, uc, _, px, ao, _), ms, _) ->
        (ats, px, ao, uc, ms)

let (|SigExceptionDef|) = function
    | SynExceptionSig.ExceptionSig(SynExceptionRepr.ExceptionDefnRepr(ats, uc, _, px, ao, _), ms, _) ->
        (ats, px, ao, uc, ms)

let (|UnionCase|) = function
    | SynUnionCase.UnionCase(ats, Ident s, uct, px, ao, _) -> (ats, px, ao, s, uct)

let (|UnionCaseType|) = function
    | SynUnionCaseType.UnionCaseFields(fs) -> fs
    | SynUnionCaseType.UnionCaseFullType _ -> failwith "UnionCaseFullType should be used internally only."

let (|Field|) = function
    | SynField.Field(ats, isStatic, ido, t, isMutable, px, ao, _) -> (ats, px, ao, isStatic, isMutable, t, Option.map (|Ident|) ido)

let (|EnumCase|) = function
    | SynEnumCase.EnumCase(ats, Ident s, c, px, _) -> (ats, px, s, c)

// Member definitions (11 cases)

let (|MDNestedType|_|) = function               
    | SynMemberDefn.NestedType(td, ao, _) -> Some(td, ao)
    | _ -> None

let (|MDOpen|_|) = function               
    | SynMemberDefn.Open(LongIdent s, _) -> Some s
    | _ -> None

let (|MDImplicitInherit|_|) = function  
    | SynMemberDefn.ImplicitInherit(t, e, ido, _) -> Some(t, e, Option.map (|Ident|) ido)
    | _ -> None

let (|MDInherit|_|) = function  
    | SynMemberDefn.Inherit(t, ido, _) -> Some(t, Option.map (|Ident|) ido)
    | _ -> None

let (|MDValField|_|) = function  
    | SynMemberDefn.ValField(f, _) -> Some f
    | _ -> None

let (|MDImplicitCtor|_|) = function  
    | SynMemberDefn.ImplicitCtor(ao, ats,ps, ido, _) -> Some(ats, ao, ps, Option.map (|Ident|) ido)
    | _ -> None

let (|MDMember|_|) = function  
    | SynMemberDefn.Member(b, _) -> Some b
    | _ -> None

let (|MDLetBindings|_|) = function
    | SynMemberDefn.LetBindings(es, isStatic, isRec, _) -> Some(isStatic, isRec, es)
    | _ -> None

let (|MDAbstractSlot|_|) = function
    | SynMemberDefn.AbstractSlot(ValSpfn(ats, Ident s, tds, t, _, _, _, px, ao, _, _), mf, _) -> 
        Some(ats, px, ao, s, t, tds, mf)
    | _ -> None

let (|MDInterface|_|) = function
    | SynMemberDefn.Interface(t, mdo, _) -> Some(t, mdo)
    | _ -> None

let (|MDAutoProperty|_|) = function
    | SynMemberDefn.AutoProperty(ats, _, Ident s, _, mk, _, px, ao, e, _ , _) -> Some(ats, px, ao, mk, e, s)
    | _ -> None

// Interface impl

let (|InterfaceImpl|) = function
    | SynInterfaceImpl.InterfaceImpl(t, bs, _) -> (t, bs)

// Bindings

let (|PropertyGet|_|) = function
    | MemberKind.PropertyGet -> Some()
    | _ -> None

let (|PropertySet|_|) = function
    | MemberKind.PropertySet -> Some()
    | _ -> None

let (|PropertyGetSet|_|) = function
    | MemberKind.PropertyGetSet -> Some()
    | _ -> None

let (|MFProperty|_|) (mf : MemberFlags) =
    match mf.MemberKind with
    | MemberKind.PropertyGet | MemberKind.PropertySet | MemberKind.PropertyGetSet as mk -> Some mk
    | _ -> None

let (|MFMemberFlags|) (mf : MemberFlags) = mf.MemberKind

/// Find out which keyword to use
let (|MFMember|MFStaticMember|MFConstructor|MFOverride|) (mf : MemberFlags) =
    match mf.MemberKind with
    | MemberKind.ClassConstructor | MemberKind.Constructor -> MFConstructor()
    | MemberKind.Member | MemberKind.PropertyGet | MemberKind.PropertySet | MemberKind.PropertyGetSet as mk -> 
        if mf.IsInstance && mf.IsOverrideOrExplicitImpl then MFOverride mk
        elif mf.IsInstance then MFMember mk
        else MFStaticMember mk

let (|DoBinding|LetBinding|MemberBinding|PropertyBinding|ExplicitCtor|) = function
    | SynBinding.Binding(ao, _, _, _, ats, px, SynValData(Some MFConstructor, _, _), pat, _, expr, _, _) ->
        ExplicitCtor(ats, px, ao, pat, expr)
    | SynBinding.Binding(ao, _, isInline, _, ats, px, SynValData(Some(MFProperty _ as mf), _, _), pat, _, expr, _, _) ->
        PropertyBinding(ats, px, ao, isInline, mf, pat, expr)
    | SynBinding.Binding(ao, _, isInline, _, ats, px, SynValData(Some mf, _, _), pat, _, expr, _, _) ->
        MemberBinding(ats, px, ao, isInline, mf, pat, expr)
    | SynBinding.Binding(_, DoBinding, _, _, ats, px, _, _, _, expr, _, _) -> 
        DoBinding(ats, px, expr)
    | SynBinding.Binding(ao, _, isInline, isMutable, ats, px, _, pat, _, expr, _, _) -> 
        LetBinding(ats, px, ao, isInline, isMutable, pat, expr)
    
let (|BindingReturnInfo|) = function
    | SynBindingReturnInfo(t, _, ats) -> (ats, t)

// Expressions (55 cases, lacking to handle 11 cases)

let (|TraitCall|_|) = function
    | SynExpr.TraitCall(tps, msg, expr, _) ->
        Some(tps, msg, expr)
    | _ -> None

/// isRaw = true with <@@ and @@>
let (|Quote|_|) = function                                    
    | SynExpr.Quote(e1, isRaw, e2, _, _) -> Some(e1, e2, isRaw)
    | _ -> None

let (|Paren|_|) = function 
    | SynExpr.Paren(e, _, _, _) -> Some e
    | _ -> None

type ExprKind = 
    | InferredDowncast | InferredUpcast | Lazy | Assert | AddressOfSingle | AddressOfDouble
    | Yield | Return | YieldFrom | ReturnFrom | Do | DoBang
    override x.ToString() =
        match x with
        | InferredDowncast -> "downcast "
        | InferredUpcast -> "upcast "
        | Lazy -> "lazy "
        | Assert -> "assert "
        | AddressOfSingle -> "&"
        | AddressOfDouble -> "&&"        
        | Yield -> "yield "
        | Return -> "return "
        | YieldFrom -> "yield! "
        | ReturnFrom -> "return! "
        | Do -> "do "
        | DoBang -> "do! "

let (|SingleExpr|_|) = function 
    | SynExpr.InferredDowncast(e, _) -> Some(InferredDowncast, e)
    | SynExpr.InferredUpcast(e, _) -> Some(InferredUpcast, e)
    | SynExpr.Lazy(e, _) -> Some(Lazy, e)
    | SynExpr.Assert(e, _) -> Some(Assert, e)
    | SynExpr.AddressOf(true, e, _, _) -> Some(AddressOfSingle, e)
    | SynExpr.AddressOf(false, e, _, _) -> Some(AddressOfDouble, e)    
    | SynExpr.YieldOrReturn((true, _), e, _) -> Some(Yield, e)
    | SynExpr.YieldOrReturn((false, _), e, _) -> Some(Return, e)
    | SynExpr.YieldOrReturnFrom((true, _), e, _) -> Some(YieldFrom, e)
    | SynExpr.YieldOrReturnFrom((false, _), e, _) -> Some(ReturnFrom, e)
    | SynExpr.Do(e, _) -> Some(Do, e)
    | SynExpr.DoBang(e, _) -> Some(DoBang, e)
    | _ -> None

type TypedExprKind = TypeTest | New | Downcast | Upcast | Typed

let (|TypedExpr|_|) = function
    | SynExpr.TypeTest(e, t, _) -> Some(TypeTest, e, t)   
    | SynExpr.New(_, t, e, _) -> Some(New, e, t)
    | SynExpr.Downcast(e, t, _) -> Some(Downcast, e, t)
    | SynExpr.Upcast(e, t, _) -> Some(Upcast, e, t)
    | SynExpr.Typed(e, t, _) -> Some(Typed, e, t)
    | _ -> None 

let (|While|_|) = function 
    | SynExpr.While(_, e1, e2, _) -> Some(e1, e2)
    | _ -> None

let (|For|_|) = function 
    | SynExpr.For(_, Ident s, e1, isUp, e2, e3, _) -> Some(s, e1, e2, e3, isUp)
    | _ -> None

let (|NullExpr|_|) = function 
    | SynExpr.Null _ -> Some() 
    | _ -> None

let (|ConstExpr|_|) = function
    | SynExpr.Const(x, _) -> Some x
    | _ -> None

let (|TypeApp|_|) = function
    | SynExpr.TypeApp(e, _, ts, _, _, _, _) -> Some(e, ts)
    | _ -> None

let (|Match|_|) = function
    | SynExpr.Match(_, e, cs, _, _) -> Some(e, cs)
    | _ -> None

let private (|Sequential|_|) = function
    | SynExpr.Sequential(_, isSeq, e1, e2, _) -> Some(e1, e2, isSeq)
    | _ -> None

let rec (|Sequentials|_|) = function
    | Sequential(e1, e2, _) -> Some [e1; e2]
    | Sequential(e, Sequentials es, _) -> Some(e::es)
    | _ -> None

let (|SimpleExpr|_|) = function
    | SynExpr.Null _  
    | SynExpr.Ident _ 
    | SynExpr.LongIdent _
    | SynExpr.Const(Const _, _) as e -> Some e
    | _ -> None

/// Only recognize numbers; strings are ignored
let rec (|SequentialSimple|_|) = function
    | Sequential(SimpleExpr e1, SimpleExpr e2, true) -> Some [e1; e2]
    | Sequential(SimpleExpr e, SequentialSimple es, true) -> Some(e::es)
    | _ -> None

let (|ArrayOrList|_|) = function
    | SynExpr.ArrayOrList(isArray, xs, _) -> Some(isArray, xs)
    | _ -> None

let (|CompExpr|_|) = function
    | SynExpr.CompExpr(isArray, _, expr, _) -> Some(isArray, expr)
    | _ -> None

let (|ArrayOrListOfSeqExpr|_|) = function
    | SynExpr.ArrayOrListOfSeqExpr(isArray, expr, _) -> Some(isArray, expr)
    | _ -> None

let (|Tuple|_|) = function
    | SynExpr.Tuple(exprs, _, _) -> Some exprs
    | _ -> None

let (|IndexedVar|_|) = function
    /// We might have to narrow scope of this pattern to avoid incorrect usage
    | SynExpr.App(_, _, SynExpr.LongIdent(_, LongIdentWithDots "Microsoft.FSharp.Core.Some", _, _), e, _) -> 
        Some(Some e)
    | SynExpr.LongIdent(_, LongIdentWithDots "Microsoft.FSharp.Core.None", _, _) -> Some None
    | _ -> None

let (|Var|_|) = function
    | SynExpr.Ident(Ident s) -> Some(s)
    | SynExpr.LongIdent(isOpt, LongIdentWithDots s, _, _) -> 
        if isOpt then Some (sprintf "?%s" s) else Some(s)
    | _ -> None

/// Get all application params at once
let (|App|_|) e =
    let rec loop = function
        /// function application is left-recursive
        | SynExpr.App(_, _, e, e2, _) -> 
            let (e1, es) = loop e
            (e1, e2::es)
        | e -> (e, [])
    match loop e with
    | (_, []) -> None
    | (e, es) -> Some(e, List.rev es)

let (|CompApp|_|) = function
    | SynExpr.App(_, _, Var "seq", (SynExpr.App _ as e), _) -> Some("seq", e)
    | SynExpr.App(_, _, Var s, (SynExpr.CompExpr _ as e), _) -> Some(s, e)
    | _ -> None

let (|PrefixApp|_|) = function
    | SynExpr.App(_, _, Var s, e2, _) when IsPrefixOperator s -> Some((|OpName|) s, e2)
    | _ -> None

let private (|InfixApp|_|) = function
    | SynExpr.App(_, true, Var(OpName "::"), Tuple [e1; e2], _) -> Some("::", e1, e2)
    | SynExpr.App(_, _, SynExpr.App(_, true, Var(OpName s), e1, _), e2, _) -> Some(s, e1, e2)
    | _ -> None

/// Should return the whole triple for convenience check
let (|InfixApps|_|) e =
    let rec loop = function
        | InfixApp(s, e, e2) -> 
            let (e1, es) = loop e
            (e1, (s, e2)::es)
        | e -> (e, [])
    match loop e with
    | (_, []) -> None
    | (e, es) -> Some(e, List.rev es)

/// Gather all arguments in lambda
let rec (|Lambda|_|) = function
    | SynExpr.Lambda(_, _, pats, Lambda(e, patss), _) -> Some(e, pats::patss)
    | SynExpr.Lambda(_, _, pats, e, _) -> Some(e, [pats])
    | _ -> None

let (|MatchLambda|_|) = function
    | SynExpr.MatchLambda(isMember, _, pats, _, _) -> Some(pats, isMember)
    | _ -> None

let (|JoinIn|_|) = function
    | SynExpr.JoinIn(e1, _, e2, _) -> Some(e1, e2)
    | _ -> None

let (|LetOrUse|_|) = function
    | SynExpr.LetOrUse(isRec, isUse, xs, e, _) -> 
        Some(isRec, isUse, xs, e)
    | _ -> None

let (|LetOrUseBang|_|) = function
    | SynExpr.LetOrUseBang(_, isUse, _, p, e1, e2, _) -> Some(isUse, p, e1, e2)
    | _ -> None 
        
let (|ForEach|_|) = function
    | SynExpr.ForEach(_, SeqExprOnly true, _, pat, e1, SingleExpr(Yield, e2) ,_) -> Some (pat, e1, e2, true)
    | SynExpr.ForEach(_, SeqExprOnly isArrow, _, pat, e1, e2 ,_) -> Some (pat, e1, e2, isArrow)
    | _ -> None

let (|DotIndexedSet|_|) = function
    | SynExpr.DotIndexedSet(e1, es, e2, _, _, _) -> Some(e1, es, e2)
    | _ -> None

let (|DotIndexedGet|_|) = function
    | SynExpr.DotIndexedGet(e1, es, _, _) -> Some(e1, es)
    | _ -> None

let (|DotGet|_|) = function
    | SynExpr.DotGet(e, _, LongIdentWithDots s, _) -> Some(e, s)
    | _ -> None

let (|DotSet|_|) = function
    | SynExpr.DotSet(e1, LongIdentWithDots s, e2, _) -> Some(e1, s, e2)
    | _ -> None

let (|IfThenElse|_|) = function
    | SynExpr.IfThenElse(e1, e2, e3, _, _, _, _) -> Some(e1, e2, e3)
    | _ -> None

let rec (|ElIf|_|) = function
    | SynExpr.IfThenElse(e1, e2, Some(ElIf(es, e3)), _, _, _, _) -> Some((e1, e2)::es, e3)
    | SynExpr.IfThenElse(e1, e2, Some e3, _, _, _, _) -> Some([(e1, e2)], e3)
    | _ -> None

let (|Record|_|) = function
    | SynExpr.Record(_, eo, xs, _) -> Some(xs, Option.map fst eo)
    | _ -> None

let (|ObjExpr|_|) = function
    | SynExpr.ObjExpr(t, eio, bd, ims, _, _) -> Some (t, eio, bd, ims)
    | _ -> None

let (|LongIdentSet|_|) = function
    | SynExpr.LongIdentSet(LongIdentWithDots s, e, _) -> Some(s, e)
    | _ -> None

let (|TryWith|_|) = function
    | SynExpr.TryWith(e, _,cs, _, _, _, _) -> Some(e, cs)
    | _ -> None

let (|TryFinally|_|) = function
    | SynExpr.TryFinally(e1, e2, _, _, _) -> Some(e1, e2)
    | _ -> None

// Patterns (18 cases, lacking to handle 2 cases)

let (|PatOptionalVal|_|) = function
    | SynPat.OptionalVal(Ident s, _) -> Some s
    | _ -> None

let (|PatAttrib|_|) = function
    | SynPat.Attrib(p, ats, _) -> Some(p, ats)
    | _ -> None

let (|PatOr|_|) = function
    | SynPat.Or(p1, p2, _) -> Some(p1, p2)
    | _ -> None

let (|PatAnds|_|) = function
    | SynPat.Ands(ps, _) -> Some ps
    | _ -> None

type PatKind = PatNull | PatWild

let (|PatNullary|_|) = function
    | SynPat.Null _ -> Some PatNull
    | SynPat.Wild _ -> Some PatWild
    | _ -> None

type SeqPatKind = PatTuple | PatArray | PatList

let (|PatSeq|_|) = function
    | SynPat.Tuple(ps, _) -> Some(PatTuple, ps)
    | SynPat.ArrayOrList(true, ps, _) -> Some(PatArray, ps)
    | SynPat.ArrayOrList(false, ps, _) -> Some(PatList, ps)
    | _ -> None

let (|PatTyped|_|) = function
    | SynPat.Typed(p, t, _) -> Some(p, t)
    | _ -> None

/// Patterns could contain active patterns sometimes
let (|PatNamed|_|) = function
    | SynPat.Named(p, Ident (OpNameFull s), _, ao, _) -> Some(ao, p, s)
    | _ -> None

let (|PatLongIdent|_|) = function
    | SynPat.LongIdent(LongIdentWithDots (OpNameFull s), _, tpso, xs, ao, _) -> Some(ao, s, xs, tpso)
    | _ -> None

let (|PatParen|_|) = function
    | SynPat.Paren(p, _) -> Some p
    | _ -> None

let (|PatRecord|_|) = function
    | SynPat.Record(xs, _) -> Some xs
    | _ -> None

let (|PatConst|_|) = function
    | SynPat.Const(c, _) -> Some c
    | _ -> None

let (|PatIsInst|_|) = function
    | SynPat.IsInst(t, _) -> Some t
    | _ -> None

let (|PatQuoteExpr|_|) = function
    | SynPat.QuoteExpr(e, _) -> Some e
    | _ -> None

// Members

let (|SPAttrib|SPId|SPTyped|) = function
    | SynSimplePat.Attrib(sp, ats, _) -> SPAttrib(ats, sp)
    /// Not sure compiler generated SPIds are used elsewhere.
    | SynSimplePat.Id(Ident _, _, true, _, isOptArg, _) -> SPId("_", isOptArg, true)
    | SynSimplePat.Id(Ident s, _, isGen, _, isOptArg, _) -> SPId(s, isOptArg, isGen)
    | SynSimplePat.Typed(sp, t, _) -> SPTyped(sp, t)

let (|SimplePats|SPSTyped|) = function
    | SynSimplePats.SimplePats(ps, _) -> SimplePats ps
    | SynSimplePats.Typed(ps, t, _) -> SPSTyped(ps, t)

let (|RecordField|) = function
    | SynField.Field(ats, _, ido, _, _, px, ao, _) -> (ats, px, ao, Option.map (|Ident|) ido)

let (|Clause|) = function
    | SynMatchClause.Clause(p, eo, e, _, _) -> (p, e, eo)

let rec (|DesugaredMatch|_|) = function
    | SynExpr.Match(_, Var s, [Clause(PatNullary PatWild, DesugaredMatch(ss, e), None)], _, _) -> Some(s::ss, e)
    | SynExpr.Match(_, Var s, [Clause(PatNullary PatWild, e, None)], _, _) -> Some([s], e)
    | _ -> None

// Type definitions

let (|TDSREnum|TDSRUnion|TDSRRecord|TDSRNone|TDSRTypeAbbrev|TDSRGeneral|) = function
    | SynTypeDefnSimpleRepr.Enum(ecs, _) -> TDSREnum ecs
    | SynTypeDefnSimpleRepr.Union(ao, xs, _) -> TDSRUnion(ao, xs)
    | SynTypeDefnSimpleRepr.Record(ao, fs, _) -> TDSRRecord(ao, fs)
    | SynTypeDefnSimpleRepr.None _ -> TDSRNone()
    | SynTypeDefnSimpleRepr.TypeAbbrev(_, t, _) -> TDSRTypeAbbrev t
    | SynTypeDefnSimpleRepr.General _ -> TDSRGeneral() // expand later
    | SynTypeDefnSimpleRepr.LibraryOnlyILAssembly _ -> failwith "LibraryOnlyILAssembly is not supported yet"

let (|Simple|ObjectModel|) = function
    | SynTypeDefnRepr.Simple(tdsr, _) -> Simple tdsr
    | SynTypeDefnRepr.ObjectModel(tdk, mds, _) -> ObjectModel(tdk, mds)

let (|SigSimple|SigObjectModel|) = function
    | SynTypeDefnSigRepr.Simple(tdsr, _) -> SigSimple tdsr
    | SynTypeDefnSigRepr.ObjectModel(tdk, mds, _) -> SigObjectModel(tdk, mds)

type TypeDefnKindSingle = | TCUnspecified | TCClass | TCInterface | TCStruct | TCRecord
                          | TCUnion | TCAbbrev | TCHiddenRepr | TCAugmentation | TCILAssemblyCode

let (|TCSimple|TCDelegate|) = function
    | TyconUnspecified -> TCSimple TCUnspecified
    | TyconClass -> TCSimple TCClass
    | TyconInterface -> TCSimple TCInterface
    | TyconStruct -> TCSimple TCStruct 
    | TyconRecord -> TCSimple TCRecord
    | TyconUnion -> TCSimple TCUnion
    | TyconAbbrev -> TCSimple TCAbbrev
    | TyconHiddenRepr -> TCSimple TCHiddenRepr
    | TyconAugmentation -> TCSimple TCAugmentation
    | TyconILAssemblyCode -> TCSimple TCILAssemblyCode
    | TyconDelegate(t, vi) -> TCDelegate(t, vi)

let (|TypeDef|) = function
    | SynTypeDefn.TypeDefn(SynComponentInfo.ComponentInfo(ats, tds, tcs, LongIdent s, px, _, ao, _) , tdr, ms, _) ->
        (ats, px, ao, tds, tcs, tdr, ms, s)

let (|SigTypeDef|) = function
    | SynTypeDefnSig.TypeDefnSig(SynComponentInfo.ComponentInfo(ats, tds, tcs, LongIdent s, px, _, ao, _) , tdr, ms, _) ->
        (ats, px, ao, tds, tcs, tdr, ms, s)

let (|TyparDecl|) = function
    | SynTyparDecl.TyparDecl(ats, tp) -> (ats, tp)

// Types (15 cases)

let (|THashConstraint|_|) = function
    | SynType.HashConstraint(t, _) -> Some t
    | _ -> None

let (|TMeasurePower|_|) = function
    | SynType.MeasurePower(t, n, _) -> Some(t, n)
    | _ -> None

let (|TMeasureDivide|_|) = function
    | SynType.MeasureDivide(t1, t2, _) -> Some(t1, t2)
    | _ -> None

let (|TStaticConstant|_|) = function
    | SynType.StaticConstant(c, _) -> Some c
    | _ -> None

let (|TStaticConstantExpr|_|) = function
    | SynType.StaticConstantExpr(c, _) -> Some c
    | _ -> None

let (|TStaticConstantNamed|_|) = function
    | SynType.StaticConstantNamed(t1, t2, _) -> Some(t1, t2)
    | _ -> None

let (|TArray|_|) = function
    | SynType.Array(n, t, _) -> Some(t, n)
    | _ -> None

let (|TAnon|_|) = function    
    | SynType.Anon _ -> Some()
    | _ -> None

let (|TVar|_|) = function 
    | SynType.Var(tp, _) -> Some tp
    | _ -> None

let (|TFun|_|) = function 
    | SynType.Fun(t1, t2, _) -> Some(t1, t2)
    | _ -> None

let (|TApp|_|) = function 
    | SynType.App(t, _, ts, _, _, isPostfix, _) -> Some(t, ts, isPostfix)    
    | _ -> None

let (|TLongIdentApp|_|) = function
    | SynType.LongIdentApp(t, LongIdentWithDots s, _, ts, _, _, _) -> Some(t, s, ts)    
    | _ -> None    

let (|TTuple|_|) = function     
    | SynType.Tuple(ts, _) -> Some (List.map snd ts)
    | _ -> None

let (|TWithGlobalConstraints|_|) = function     
    | SynType.WithGlobalConstraints(t, tcs, _) -> Some(t, tcs)
    | _ -> None

let (|TLongIdent|_|) = function
    | SynType.LongIdent(LongIdentWithDots s) -> Some s
    | _ -> None

// Type parameter

type SingleTyparConstraintKind = 
    | TyparIsValueType | TyparIsReferenceType | TyparIsUnmanaged
    | TyparSupportsNull | TyparIsComparable | TyparIsEquatable
    override x.ToString() =
        match x with
        | TyparIsValueType -> "struct"
        | TyparIsReferenceType -> "not struct"
        | TyparIsUnmanaged -> "unmanaged"
        | TyparSupportsNull -> "null"
        | TyparIsComparable -> "comparison"
        | TyparIsEquatable -> "equality"

let (|TyparSingle|TyparDefaultsToType|TyparSubtypeOfType|TyparSupportsMember|TyparIsEnum|TyparIsDelegate|) = function  
    | WhereTyparIsValueType(tp, _) -> TyparSingle(TyparIsValueType, tp)
    | WhereTyparIsReferenceType(tp, _) -> TyparSingle(TyparIsReferenceType, tp)
    | WhereTyparIsUnmanaged(tp, _) -> TyparSingle(TyparIsUnmanaged, tp)
    | WhereTyparSupportsNull(tp, _) -> TyparSingle(TyparSupportsNull, tp)
    | WhereTyparIsComparable(tp, _) -> TyparSingle(TyparIsComparable, tp)
    | WhereTyparIsEquatable(tp, _) -> TyparSingle(TyparIsEquatable, tp)
    | WhereTyparDefaultsToType(tp, t, _) -> TyparDefaultsToType(tp, t)
    | WhereTyparSubtypeOfType(tp, t, _) -> TyparSubtypeOfType(tp, t)
    | WhereTyparSupportsMember(tps, msg, _) -> TyparSupportsMember(tps, msg)
    | WhereTyparIsEnum(tp, ts, _) -> TyparIsEnum(tp, ts)
    | WhereTyparIsDelegate(tp, ts, _) -> TyparIsDelegate(tp, ts)

let (|MSMember|MSInterface|MSInherit|MSValField|MSNestedType|) = function
    | SynMemberSig.Member(vs, mf, _) -> MSMember(vs, mf) 
    | SynMemberSig.Interface(t, _) -> MSInterface t
    | SynMemberSig.Inherit(t, _) -> MSInherit t
    | SynMemberSig.ValField(f, _) -> MSValField f
    | SynMemberSig.NestedType(tds, _) -> MSNestedType tds          

let (|Val|) (ValSpfn(ats, Ident(OpNameFull s), tds, t, vi, _, _, px, ao, _, _)) = 
    (ats, px, ao, s, t, vi, tds)

// Misc

let (|RecordFieldName|) ((LongIdentWithDots s, _) : RecordFieldName, eo : SynExpr option, _) = (s, eo)

let (|PatRecordFieldName|) ((LongIdent s1, Ident s2), p) = (s1, s2, p)

let (|ValInfo|) (SynValInfo(aiss, ai)) = (aiss, ai)

let (|ArgInfo|) (SynArgInfo(_, isOpt, ido)) = 
    (Option.map (|Ident|) ido, isOpt)

/// Extract function arguments with their associated info
let (|FunType|) (t, ValInfo(aiss, ai)) = 
    let rec loop = function
        | TFun(t1, t2), ais::aiss -> 
            (t1, ais)::loop(t2, aiss)
        | t, [] -> [(t, [ai])]
        | _ -> []
    loop(t, aiss)