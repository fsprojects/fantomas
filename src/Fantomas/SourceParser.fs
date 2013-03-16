module internal Fantomas.SourceParser

open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.PrettyNaming

let inline (|Ident|) (id: Ident) = id.idText

let inline (|LongIdent|) (li: LongIdent) = 
    li |> Seq.map (fun id -> id.idText) |> String.concat "."

let inline (|LongIdentWithDots|) (LongIdentWithDots(li, _)) = 
    li |> Seq.map (fun id -> id.idText) |> String.concat "."

let inline (|Typar|) (SynTypar.Typar(Ident s, _ , _)) = s
    
// Literals

let (|Measure|) x = 
    let rec loop = function
        | SynMeasure.Var(Typar s, _) -> s
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
let rec (|Const|) = function
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
    | SynConst.String(s, _) -> Const(sprintf "%A" s)
    | SynConst.Bytes(bs, _) -> failwith "Not implemented yet"
    | SynConst.UInt16s us -> failwith "Not implemented yet"

// File level patterns

let (|ImplFile|SigFile|) = function
    | ParsedInput.ImplFile im -> ImplFile im
    | ParsedInput.SigFile si -> SigFile si

let (|ParsedImplFileInput|) = function
    | ParsedImplFileInput.ParsedImplFileInput(_, _, _, _, hs, mns, _) -> (hs, mns)

let (|ModuleOrNamespace|) = function
    | SynModuleOrNamespace.SynModuleOrNamespace(li, _, mds, px, ats, ao, _) -> (ats, px, ao, li, mds)

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
    | SynField.Field(ats, isStatic, ido, t, _, px, ao, _) -> (ats, px, ao, isStatic, t, Option.map (|Ident|) ido)

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
    | SynMemberDefn.AbstractSlot(SynValSig.ValSpfn(ats, Ident s, _, _, _, _, _, px, ao, _, _),_,_) -> 
        Some(ats, px, ao, s)
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

let (|LetBinding|MemberBinding|) = function
    | SynBinding.Binding(ao, bk, isInline, isMutable, ats, px, SynValData(Some mf, _,_), pat, _, expr, _, _) -> 
        MemberBinding(ats, px, ao, isInline, mf.IsInstance, pat, expr, bk)
    | SynBinding.Binding(ao, bk, isInline, isMutable, ats, px, _, pat, _, expr, _, _) -> 
        LetBinding(ats, px, ao, isInline, isMutable, pat, expr, bk)

// Expressions (55 cases, lacking to handle 11 cases)

let (|TraitCall|_|) = function
    | SynExpr.TraitCall(ts, msig, expr, _) ->
        let ids = List.map (|Typar|) ts
        Some(ids, msig, expr)
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
        | InferredDowncast -> "downcast"
        | InferredUpcast -> "upcast"
        | Lazy -> "lazy"
        | Assert -> "assert"
        | AddressOfSingle -> "&"
        | AddressOfDouble -> "&&"        
        | Yield -> "yield"
        | Return -> "return"
        | YieldFrom -> "yield!"
        | ReturnFrom -> "return!"
        | Do -> "do"
        | DoBang -> "do!"

let (|SingleExpr|_|) = function 
    | SynExpr.InferredDowncast(e, _) -> Some(InferredDowncast, e)
    | SynExpr.InferredUpcast(e, _) -> Some(InferredUpcast, e)
    | SynExpr.Lazy(e, _) -> Some(Lazy, e)
    | SynExpr.Assert(e, _) -> Some(Assert, e)
    | SynExpr.AddressOf(false, e, _, _) -> Some(AddressOfSingle, e)
    | SynExpr.AddressOf(true, e, _, _) -> Some(AddressOfDouble, e)    
    | SynExpr.YieldOrReturn((false, _), e, _) -> Some(Yield, e)
    | SynExpr.YieldOrReturn((true, _), e, _) -> Some(Return, e)
    | SynExpr.YieldOrReturnFrom((false, _), e, _) -> Some(YieldFrom, e)
    | SynExpr.YieldOrReturnFrom((true, _), e, _) -> Some(ReturnFrom, e)
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

let (|TypeApp|_|) = function
    | SynExpr.TypeApp(e, _, ts, _, _, _, _) -> Some(e, ts)
    | _ -> None

let (|Match|_|) = function
    | SynExpr.Match(_, e, cs, _, _) -> Some(e, cs)
    | _ -> None

let (|Sequential|_|) = function
    | SynExpr.Sequential(_, _, e1, e2, _) -> Some(e1, e2)
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

let (|ConstExpr|_|) = function
    | SynExpr.Const(x, _) -> Some x
    | _ -> None

let (|Var|_|) = function
    | SynExpr.Ident(Ident s) -> Some(DecompileOpName s)
    | SynExpr.LongIdent(_, LongIdentWithDots s, _, _) -> Some(DecompileOpName s)
    | _ -> None

let (|App|_|) = function
    | SynExpr.App(_, _, e1, e2, _) -> Some(e1, e2)
    | _ -> None

let (|InfixApp|_|) = function
    | SynExpr.App(_, _, SynExpr.App(_, true, Var s, e1, _), e2, _) -> Some(s, e1, e2)
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
        if List.isEmpty xs then failwith "Illformed bindings"
        else Some (isRec, isUse, xs, e)
    | _ -> None

let (|LetOrUseBang|_|) = function
    | SynExpr.LetOrUseBang(_, isUse, _, p, e1, e2, _) -> Some(isUse, p, e1, e2)
    | _ -> None 
        
let (|ForEach|_|) = function                               
    | SynExpr.ForEach(_, _, _, pat, e1, e2 ,_) -> Some (pat, e1, e2)
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
    | SynExpr.Record(_, _, xs, _) -> Some xs
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
    | SynPat.OptionalVal(Ident id, _) -> Some id
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
    | SynPat.LongIdent(LongIdentWithDots li, _, _, xs, ao, _) -> Some(ao, li, xs)
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

let (|MSMember|) = function
    | SynMemberSig.Member(ValSpfn(ats, Ident s, _, t, _, _, _, px, ao, _, _), _, _) -> (ats, px, ao, s, t)
    | _ -> failwith "MemberSigMember: other patterns will be added later"
              
let (|SPatAttrib|SPatId|SPatTyped|) = function
    | SynSimplePat.Attrib(sp, ats, _) -> SPatAttrib(ats, sp)
    | SynSimplePat.Id(Ident s, _, _, _, _, _) -> SPatId s
    | SynSimplePat.Typed(sp, t, _) -> SPatTyped(sp, t)

let (|SimplePats|SPSTyped|) = function
    | SynSimplePats.SimplePats(ps, _) -> SimplePats ps
    | SynSimplePats.Typed(ps, t, _) -> SPSTyped(ps, t)

let (|RecordField|) = function
    | SynField.Field(ats, _, ido, _, _, px, ao, _) -> (ats, px, ao, Option.map (|Ident|) ido)

let (|Clause|) = function
    | SynMatchClause.Clause(p, _,e, _, _) -> (p, e)

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
    | SynTypeDefnRepr.ObjectModel(tdk, md, _) -> ObjectModel(tdk, md)

type TypeDefnKindSingle = 
    | TCUnspecified 
    | TCClass 
    | TCInterface 
    | TCStruct 
    | TCRecord
    | TCUnion
    | TCAbbrev
    | TCHiddenRepr
    | TCAugmentation
    | TCILAssemblyCode

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