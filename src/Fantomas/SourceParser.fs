module internal Fantomas.SourceParser

open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.PrettyNaming
open Fantomas.FormatConfig

/// Get source string content based on range value
let inline content (sc : SynConst) (c : Context) = 
    let r = sc.Range range.Zero
    let s =
        if r.StartLine <= c.Positions.Length && r.EndLine <= c.Positions.Length then
            // Off-by-one start column
            let start = c.Positions.[r.StartLine-1] + r.StartColumn
            let finish = c.Positions.[r.EndLine-1] + r.EndColumn - 1
            c.Content.[start..finish]
        else ""
    s

/// Use infix operators in the short form
let inline (|OpName|) s =
    if IsActivePatternName s then sprintf "(%s)" (DecompileOpName s)
    elif IsPrefixOperator s then 
        let s' = DecompileOpName s
        if s'.[0] = '~' then s'.Substring(1)
        else s'
    else DecompileOpName s

let inline (|OpNamePrefix|) s =
    if IsActivePatternName s || IsInfixOperator s || IsPrefixOperator s then sprintf "(%s)" (DecompileOpName s)
    else DecompileOpName s

let inline (|Ident|) (id: Ident) = id.idText

let inline (|LongIdent|) (li: LongIdent) = 
    li |> Seq.map (fun id -> id.idText) |> String.concat "."

let inline (|LongIdentWithDots|) (LongIdentWithDots(li, _)) = 
    li |> Seq.map (fun id -> id.idText) |> String.concat "."

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
        | SynMeasure.Named(LongIdent li, _) -> li
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
    | SynConst.UInt16s us -> Const(sprintf "%A" us)
    | c -> invalidArg "c" "Ill-formed constants"

// File level patterns

let (|ImplFile|SigFile|) = function
    | ParsedInput.ImplFile im -> ImplFile im
    | ParsedInput.SigFile si -> SigFile si

let (|ParsedImplFileInput|) = function
    | ParsedImplFileInput.ParsedImplFileInput(_, _, _, _, hs, mns, _) -> (hs, mns)

let (|ModuleOrNamespace|) = function
    | SynModuleOrNamespace.SynModuleOrNamespace(LongIdent s, isModule, mds, px, ats, ao, _) -> (ats, px, ao, s, mds, isModule)

// Attribute
let (|Attribute|) (a : SynAttribute) =
    let (LongIdentWithDots li) = a.TypeName
    (li, a.ArgExpr, a.AppliesToGetterAndSetter)

// Access modifiers
let (|Access|) = function
    | SynAccess.Public -> "public"
    | SynAccess.Internal -> "internal"
    | SynAccess.Private -> "private"

let (|PreXmlDoc|) (px: PreXmlDoc) =
    match px.ToXmlDoc() with
    | XmlDoc lines -> lines

// Module declarations (10 cases)

let (|Open|_|) = function
    | SynModuleDecl.Open(LongIdentWithDots li, _) -> Some li
    | _ -> None

let (|ModuleAbbrev|_|) = function
    | SynModuleDecl.ModuleAbbrev(Ident id, LongIdent li, _) -> Some(id, li)
    | _ -> None

let (|HashDirective|_|) = function
    | SynModuleDecl.HashDirective(ParsedHashDirective(s, ss, _), _) -> Some(s, String.concat "." ss)
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
    | SynModuleDecl.NestedModule(SynComponentInfo.ComponentInfo(ats, _, _, LongIdent li, px, _, ao, _), xs, _, _) -> 
        Some(ats, px, ao, li, xs)
    | _ -> None

let (|Exception|_|) = function
    | SynModuleDecl.Exception(ed, _) -> Some ed
    | _ -> None

// Exception definitions

let (|ExceptionDef|) = function
    | SynExceptionDefn.ExceptionDefn(SynExceptionRepr.ExceptionDefnRepr(ats, uc, _, px, ao, _), ms, _) ->
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
    | SynMemberDefn.Open(LongIdent li, _) -> Some li
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

let (|DoBinding|LetBinding|MemberBinding|PropertyBinding|) = function
    | SynBinding.Binding(ao, bk, isInline, isMutable, ats, px, SynValData(Some(MFProperty _ as mf), _, _), pat, bri, expr, _, _) ->
        PropertyBinding(ats, px, ao, isInline, mf, pat, expr, bk, bri)
    | SynBinding.Binding(ao, bk, isInline, isMutable, ats, px, SynValData(Some mf, _, _), pat, bri, expr, _, _) ->
        MemberBinding(ats, px, ao, isInline, mf, pat, expr, bk, bri)
    | SynBinding.Binding(_, DoBinding, _, _, ats, px, _, _, _, expr, _, _) -> 
        DoBinding(ats, px, expr)
    | SynBinding.Binding(ao, bk, isInline, isMutable, ats, px, _, pat, bri, expr, _, _) -> 
        LetBinding(ats, px, ao, isInline, isMutable, pat, expr, bk, bri)
    
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

type ExprKind = | InferredDowncast | InferredUpcast | Lazy | Assert | AddressOfSingle | AddressOfDouble
                | Yield | Return | YieldFrom | ReturnFrom | Do | DoBang
with override x.ToString() =
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
    | SynExpr.For(_, Ident id, e1, isUp, e2, e3, _) -> Some(id, e1, e2, e3, isUp)
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

let (|Sequential|_|) = function
    | SynExpr.Sequential(_, isSeq, e1, e2, _) -> Some(e1, e2, isSeq)
    | _ -> None

/// Only recognize numbers; strings are ignored
let rec (|SeqVals|_|) = function
    | Sequential(ConstExpr(Const _ as e1), ConstExpr(Const _ as e2), true) -> Some [e1; e2]
    | Sequential(ConstExpr(Const _ as e), SeqVals es, true) -> Some(e::es)
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

let (|Var|_|) = function
    | SynExpr.Ident(Ident s) -> Some(s)
    | SynExpr.LongIdent(_, LongIdentWithDots s, _, _) -> Some(s)
    | _ -> None

let (|App|_|) = function
    | SynExpr.App(_, _, e1, e2, _) -> Some(e1, e2)
    | _ -> None

let (|PrefixApp|_|) = function
    | SynExpr.App(_, _, Var s, e2, _) when IsPrefixOperator s -> Some((|OpName|) s, e2)
    | _ -> None

let (|InfixApp|_|) = function
    | SynExpr.App(_, true, Var(OpName "::"), Tuple [e1; e2], _) -> Some("::", e1, e2)
    | SynExpr.App(_, _, SynExpr.App(_, true, Var(OpName s), e1, _), e2, _) -> Some(s, e1, e2)
    | _ -> None

let (|Lambda|_|) = function
    | SynExpr.Lambda(isMember, _, pats, e, _) -> Some(e, pats, isMember)
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
    | SynExpr.ForEach(_, SeqExprOnly isArrow, _, pat, e1, e2 ,_) -> Some (pat, e1, e2, isArrow)
    | _ -> None

let (|DotIndexedSet|_|) = function
    | SynExpr.DotIndexedSet(e1, es, e2, _, _, _) -> Some(e1, es, e2)
    | _ -> None

let (|DotIndexedGet|_|) = function
    | SynExpr.DotIndexedGet(e1, es, _, _) -> Some(e1, es)
    | _ -> None

let (|DotGet|_|) = function
    | SynExpr.DotGet(e, _, LongIdentWithDots li, _) -> Some(e, li)
    | _ -> None

let (|DotSet|_|) = function
    | SynExpr.DotSet(e1, LongIdentWithDots li, e2, _) -> Some(e1, li, e2)
    | _ -> None

let (|IfThenElse|_|) = function
    | SynExpr.IfThenElse(e1, e2, e3, _, _, _, _) -> Some(e1, e2, e3)
    | _ -> None

let (|Record|_|) = function
    | SynExpr.Record(_, eo, xs, _) -> Some(xs, Option.map fst eo)
    | _ -> None

let (|ObjExpr|_|) = function
    | SynExpr.ObjExpr(t, x, bd, ims, _, _) -> Some (t, x, bd, ims)
    | _ -> None

let (|LongIdentSet|_|) = function
    | SynExpr.LongIdentSet(LongIdentWithDots li, e, _) -> Some(li, e)
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

let (|PatNamed|_|) = function
    | SynPat.Named(p, Ident id, _, ao, _) -> Some(ao, p, id)
    | _ -> None

let (|PatLongIdent|_|) = function
    | SynPat.LongIdent(LongIdentWithDots (OpName s), _, tpso, xs, ao, _) -> Some(ao, s, xs, tpso)
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

let (|SPatAttrib|SPatId|SPatTyped|) = function
    | SynSimplePat.Attrib(sp, ats, _) -> SPatAttrib(ats, sp)
    | SynSimplePat.Id(Ident s, _, _, _, isOptArg, _) -> SPatId(s, isOptArg)
    | SynSimplePat.Typed(sp, t, _) -> SPatTyped(sp, t)

let (|SimplePats|SPSTyped|) = function
    | SynSimplePats.SimplePats(ps, _) -> SimplePats ps
    | SynSimplePats.Typed(ps, t, _) -> SPSTyped(ps, t)

let (|RecordField|) = function
    | SynField.Field(ats, _, ido, _, _, px, ao, _) -> (ats, px, ao, Option.map (|Ident|) ido)

let (|Clause|) = function
    | SynMatchClause.Clause(p, eo, e, _, _) -> (p, e, eo)

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
    | SynTypeDefn.TypeDefn(SynComponentInfo.ComponentInfo(ats, tds, tcs, LongIdent li, px, _, ao, _) , tdr, ms, _) ->
        (ats, px, ao, tds, tcs, tdr, ms, li)

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
    | SynType.Anon(_) -> Some()
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
    | SynType.LongIdentApp(t, LongIdentWithDots li, _, ts, _, _, _) -> Some(t, li, ts)    
    | _ -> None    

let (|TTuple|_|) = function     
    | SynType.Tuple(ts, _) -> Some ts
    | _ -> None

let (|TWithGlobalConstraints|_|) = function     
    | SynType.WithGlobalConstraints(t, tcs, _) -> Some(t, tcs)
    | _ -> None

let (|TLongIdent|_|) = function
    | SynType.LongIdent(LongIdentWithDots li) -> Some li
    | _ -> None

// Type parameter

type SingleTyparConstraintKind = | TyparIsValueType | TyparIsReferenceType | TyparIsUnmanaged
                                 | TyparSupportsNull | TyparIsComparable | TyparIsEquatable
with override x.ToString() =
        match x with
        | TyparIsValueType -> "struct"
        | TyparIsReferenceType -> "not struct"
        | TyparIsUnmanaged -> "unmanaged"
        | TyparSupportsNull -> "null"
        | TyparIsComparable -> "comparison"
        | TyparIsEquatable -> "equality"

let (|TyparSingle|TyparDefaultsToType|TyparSubtypeOfType|TyparSupportsMember|TyparIsEnum|TyparIsDelegate|) = 
    function    
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

let (|ValSig|) (ValSpfn(ats, Ident(OpNamePrefix s), tds, t, _, _, _, px, ao, _, _)) = 
    (ats, px, ao, s, t, tds)

// Misc

let (|RecordFieldName|) ((LongIdentWithDots s, _) : RecordFieldName, eo : SynExpr option, _) = (s, eo)

let (|PatRecordFieldName|) ((LongIdent s1, Ident s2), p) = (s1, s2, p)
