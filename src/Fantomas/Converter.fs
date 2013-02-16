module Fantomas.Converter

// This module is a modified version of http://fsharprefactor.codeplex.com/SourceControl/changeset/view/96754#1719584
// There are two major changes:
// 1. Remove the continuation monad. I do not believe that ASTs are too deep to cause stackoverflow.
// 2. Modify patterns to compile with F# 3.0 AST. There are many internal changes between v2.0 and v3.0.

open Microsoft.FSharp.Compiler.Ast

open Fantomas.Utils
open Fantomas.Ast

/// TODO: revising this function
let longIdentToVar (LongIdentWithDots(ids, _)) = 
    let s = List.map (fun (id : Ident) -> id.idText) ids |> String.concat "."
    let l1 = List.head ids |> (fun id -> mkSrcLoc id.idRange)
    let l2 = System.Linq.Enumerable.Last ids |> (fun id -> mkSrcLoc id.idRange)
    Var(s, joinSrcLoc l1 l2)

let foldDecls decls =
    let rec loopDecl x =
        match x with
        | SynModuleDecl.NamespaceFragment _ -> NotSupported
        | SynModuleDecl.ModuleAbbrev(id, lid, _) ->
            let lidAcc = List.map (fun (x : Ident) -> x.idText) lid
            ModuleAbbrev(id.idText, lidAcc)
        | SynModuleDecl.Attributes(attrs, _) ->
            let attrsAcc = List.map loopAttribute attrs
            Attributes attrsAcc
        | SynModuleDecl.HashDirective(ParsedHashDirective(s, ss, _), _) ->
            HashDirective(s, ss)
        | SynModuleDecl.Let(isRec, xs, _) -> 
            let xsAcc = List.map loopBinding xs
            let xsAcc' = List.map (fun (nAcc, eAcc) -> Let(isRec, [nAcc, eAcc], Lit(Unit))) xsAcc
            Exp xsAcc'
        | SynModuleDecl.DoExpr(_, x, _) -> 
            let xAcc = loopExpr x
            Exp [xAcc] 
        | SynModuleDecl.Types(xs, _) -> 
            let xsAcc = List.map loopTypeDef xs
            Types xsAcc
        | SynModuleDecl.NestedModule(SynComponentInfo.ComponentInfo(_, _, _, longId, _, _, _, _), xs, _, _) -> 
            let xsAcc = List.map loopDecl xs
            NestedModule(List.map (fun (x : Ident) -> x.idText) longId, xsAcc)
        | SynModuleDecl.Open(LongIdentWithDots(xs, _), _) -> 
            Open(List.map (fun (x : Ident) -> x.idText) xs) 
        | SynModuleDecl.Exception(ed, _) ->
            let edAcc = loopExceptionDef ed
            Exception edAcc
    
    and loopExceptionDef x =
        match x with
        | SynExceptionDefn.ExceptionDefn(SynExceptionRepr.ExceptionDefnRepr(_, uc, _, _, _, _), ms, _) ->
            let(name, _) = loopUnionCases uc
            let msAcc = List.map loopClassMember ms                    
            ExceptionDef(name, msAcc)

    and loopBinding x = 
        match x with
        | SynBinding.Binding(_,_,_,_,_,_,_,name,_,expr,_,_) ->
            let nAcc = loopPat name
            let eAcc = loopExpr expr
            (nAcc, eAcc)

    and loopMemberBinding x = 
        match x with
        | SynBinding.Binding(_,_,_,_,_,_,SynValData(flags, _,_),name,_,expr,_,_) -> 
            let isInstance = if(flags.IsSome) then flags.Value.IsInstance else false                                     
            let nAcc = loopPat name
            let eAcc = loopExpr expr
            Member(isInstance, nAcc, eAcc)

    and loopExpr x =
        match x with
        // Not implemented AST sections
//        | SynExpr.DeprecatedTypeOf _ -> ArbitraryAfterError
//        | SynExpr.DiscardAfterError _ -> ArbitraryAfterError
        | SynExpr.DotNamedIndexedPropertySet _ -> ArbitraryAfterError
        | SynExpr.ImplicitZero _ -> ArbitraryAfterError
        | SynExpr.LibraryOnlyILAssembly _ -> ArbitraryAfterError
        | SynExpr.LibraryOnlyStaticOptimization _ -> ArbitraryAfterError
        | SynExpr.LibraryOnlyUnionCaseFieldGet _ -> ArbitraryAfterError
        | SynExpr.LibraryOnlyUnionCaseFieldSet _ -> ArbitraryAfterError
        | SynExpr.NamedIndexedPropertySet _ -> ArbitraryAfterError
        | SynExpr.TraitCall(ts, msig, e, _) ->
            let idsAcc = List.map (fun (SynTypar.Typar(i, _, b)) -> i.idText) ts
            let msigAcc = loopMemberSig msig
            let eAcc = loopExpr e
            TraitCall(idsAcc, msigAcc, eAcc)
        | SynExpr.TypeTest(e,t,_) ->
            let eAcc = loopExpr e
            let tAcc = loopType t
            TypeTest(eAcc, tAcc)                                
        | SynExpr.Quote(e1, _, e2, _, _) ->
            let e1Acc = loopExpr e1
            let e2Acc = loopExpr e2
            Quote(e1Acc, e2Acc)
        | SynExpr.InferredDowncast(e, _) ->
            let eAcc = loopExpr e
            InferredDowncast eAcc
        | SynExpr.InferredUpcast(e, _) ->
            let eAcc = loopExpr e
            InferredUpcast eAcc
        | SynExpr.Lazy(e, _) ->
            let eAcc = loopExpr e
            Lazy eAcc
        | SynExpr.While(_, e1, e2, _) ->
            let e1Acc = loopExpr e1
            let e2Acc = loopExpr e2
            While(e1Acc, e2Acc)
        | SynExpr.Assert(e, _) ->
            let eAcc = loopExpr e
            Assert eAcc
        | SynExpr.For(_, id, e1, _, e2, e3, _) ->
            let e1Acc = loopExpr e1
            let e2Acc = loopExpr e2
            let e3Acc = loopExpr e3
            For(PVar(id.idText, mkSrcLoc id.idRange), e1Acc, e2Acc, e3Acc)
        | SynExpr.Null _ -> Null
        | SynExpr.AddressOf(_, e, _, _) ->
            let eAcc = loopExpr e
            AddressOf eAcc
        | SynExpr.TypeApp(e, _, ts, _, _, _, _) ->
            let eAcc = loopExpr e
            let tsAcc = List.map loopType ts
            TypeApp(eAcc, tsAcc)
        | SynExpr.Match(_,e,cs,_,_) -> 
            let eAcc = loopExpr e
            let csAcc = List.map loopClause cs
            Match(eAcc, csAcc)
//        | SynExpr.Seq(_, _, e1, e2, _) -> 
//            let e1Acc = loopExpr e1
//            let e2Acc = loopExpr e2
//            List [e1Acc; e2Acc]
        | SynExpr.ArrayOrList(_, xs, _) -> 
            let xsAcc = List.map loopExpr xs
            List xsAcc
        | SynExpr.CompExpr(_, _, expr, _) -> 
            loopExpr expr
        | SynExpr.ArrayOrListOfSeqExpr(_, expr, _) -> 
            loopExpr expr
        | SynExpr.Tuple(exprs, _, _) -> 
            let esAcc = List.map loopExpr exprs
            Tuple esAcc
        | SynExpr.Const(x, _) -> loopConst x
        | SynExpr.Ident(id) -> 
            Var(id.idText, mkSrcLoc id.idRange) 
        | SynExpr.LongIdent(_, ids, _, _) ->                     
            longIdentToVar ids
        | SynExpr.App(_, _, x, y, _) -> 
            let xAcc = loopExpr x
            let yAcc = loopExpr y
            App(xAcc, yAcc)
        | SynExpr.Paren(x, _, _, _) -> 
            let xAcc = loopExpr x
            Paren xAcc
        | SynExpr.Lambda(_,_,x,y,_) -> 
            let xAcc = loopSimplePats x
            let yAcc = loopExpr y
            Lam(xAcc, yAcc)
        | SynExpr.LetOrUse(isRec,_,xs,x,_) -> 
            if List.isEmpty xs then ArbitraryAfterError
            else
                let bsAcc = List.map loopBinding xs
                let xAcc = loopExpr x
                Let(isRec, bsAcc, xAcc)
        | SynExpr.LetOrUseBang(_, _, _, p, e1, e2, _) ->
            let pAcc = loopPat p
            let e1Acc = loopExpr e1
            let e2Acc = loopExpr e2
            LetBang(pAcc, e1Acc, e2Acc)                        
        | SynExpr.ForEach(_, _, _, pat, e1, e2 ,_) -> 
            let pAcc = loopPat pat
            let e1Acc = loopExpr e1
            let e2Acc = loopExpr e2
            ForEach(pAcc, e1Acc, e2Acc)
        | SynExpr.YieldOrReturn(_, e, _) ->
            let eAcc = loopExpr e 
            YieldOrReturn eAcc
        | SynExpr.YieldOrReturnFrom(_, e, _) ->
            let eAcc = loopExpr e 
            YieldOrReturnFrom eAcc
        | SynExpr.DotIndexedSet(e1, es, e2, _, _, _) -> 
            let e1Acc = loopExpr e1
            let esAcc = List.map loopExpr es
            let e2Acc = loopExpr e2
            DotIndexedSet(e1Acc, esAcc, e2Acc)
        | SynExpr.DotIndexedGet(e1, es, _, _) -> 
            let e1Acc = loopExpr e1
            let esAcc = List.map loopExpr es
            DotIndexedGet(e1Acc, esAcc)
        | SynExpr.DotGet(e, _, li, _) -> 
            let eAcc = loopExpr e   
            let liAcc = longIdentToVar li                 
            DotGet(eAcc, liAcc)
        | SynExpr.DotSet(e1, li, e2, _) -> 
            let e1Acc = loopExpr e1
            let e2Acc = loopExpr e2
            let liAcc = longIdentToVar li
            DotSet(e1Acc, liAcc, e2Acc)
        | SynExpr.IfThenElse(e1,e2,e3,_,_,_, _) -> 
            let e1Acc = loopExpr e1
            let e2Acc = loopExpr e2
            if (e3.IsSome) then 
                let e3Acc = loopExpr e3.Value
                IfThenElse(e1Acc, e2Acc, Some e3Acc)
            else
                IfThenElse(e1Acc, e2Acc, Option.None)
        // Need some major changes
//        | SynExpr.Record(_,_,xs,_) -> 
//            let xsAcc = List.map loopRecordFieldInst xs
//            Exp.Record xsAcc
        | SynExpr.New(_,t,e,_) -> 
            let tAcc = loopType t
            let eAcc = loopExpr e
            New(tAcc, eAcc)
        | SynExpr.ObjExpr(t, x, bd, ims, _, _) ->
            let bdAcc = List.map loopMemberBinding bd
            ObjExpr bdAcc
        | SynExpr.Do(e, _) ->
            let eAcc = loopExpr e
            Do eAcc
        | SynExpr.DoBang(e, _) ->
            let eAcc = loopExpr e
            DoBang eAcc
        | SynExpr.Downcast(e, t, _) ->
            let eAcc = loopExpr e
            let tAcc = loopType t
            Downcast(eAcc, tAcc)
        | SynExpr.Upcast(e, t, _) ->
            let eAcc = loopExpr e
            let tAcc = loopType t
            Upcast(eAcc, tAcc)
        | SynExpr.LongIdentSet(li, e, _) ->
            let liAcc = longIdentToVar li
            let eAcc = loopExpr e
            LongVarSet(liAcc, eAcc)
        | SynExpr.TryWith(e,_,cl,_,_,_,_) ->
            let eAcc = loopExpr e
            let clAcc = List.map loopClause cl
            TryWith(eAcc, clAcc)
        | SynExpr.TryFinally(e1, e2, _, _, _) ->
            let e1Acc = loopExpr e1
            let e2Acc = loopExpr e2
            TryFinally(e1Acc, e2Acc)
        | SynExpr.Typed(e, t, _) ->
            let eAcc = loopExpr e
            let tAcc = loopType t
            Typed(eAcc, tAcc)
        | SynExpr.ArbitraryAfterError _ -> 
            ArbitraryAfterError
        | _ -> failwith "loopExpr: Unsupported pattern"

    and loopMemberSig x =
        match x with
        | SynMemberSig.Member(ValSpfn(_,_,_,t,_,_,_,_,_,_,_), _, _) ->
            let tAcc= loopType t
            MemberSig tAcc
        | _ -> failwith "loopMemberSig: Unsupported pattern"

    and loopRecordFieldInst(_, x : Ident, e) =
         let eAcc = loopExpr e
         ((x.idText, mkSrcLoc x.idRange), eAcc)

    and loopSimplePats x =
        match x with 
        | SynSimplePats.SimplePats(xs, _) -> 
            List.map loopSimplePat xs 
        | SynSimplePats.Typed(p, t, _) ->
            loopSimplePats p
              
    and loopSimplePat x =
        match x with
        | SynSimplePat.Attrib(p, a, _) ->
            loopSimplePat p
        | SynSimplePat.Id(ident, _, _, _, _, _) -> 
            PVar(ident.idText, mkSrcLoc(ident.idRange))
        | SynSimplePat.Typed(p,_,_) ->
            loopSimplePat p

    and loopMeasure x = 
        match x with
        | SynMeasure.Var((SynTypar.Typar(id, _, _)), _) -> MVar(id.idText)
        | SynMeasure.Anon _ -> Anon
        | SynMeasure.One -> One
        | SynMeasure.Product(m1, m2, _) -> 
            let m1Acc = loopMeasure m1
            let m2Acc = loopMeasure m2
            Product(m1Acc, m2Acc)
        | SynMeasure.Divide(m1, m2, _) -> 
            let m1Acc = loopMeasure m1
            let m2Acc = loopMeasure m2
            Divide(m1Acc, m2Acc)
        | SynMeasure.Power(m, n, _) -> 
            let mAcc = loopMeasure m
            Power(mAcc, n)
        | SynMeasure.Seq(ms, _) -> 
            let msAcc = List.map loopMeasure ms
            Seq msAcc 
        | SynMeasure.Named(li, _) -> 
            let liAcc = TLongIdent(List.map (fun (id:Ident) -> TIdent(id.idText, mkSrcLoc id.idRange)) li)
            Named liAcc

    and loopConst x =
        match x with
        | SynConst.UIntPtr _ -> Lit Unit // Ignored for now.
        | SynConst.UInt16s _ -> Lit Unit // Ignored for now.
        | SynConst.IntPtr _ -> Lit Unit // Ignored for now.
        | SynConst.Decimal _ -> Lit Unit // Ignored for now.
        | SynConst.Bytes(_, _) -> Lit Unit // Ignored for now.
        | SynConst.Measure(c, m) -> 
            let cAcc = loopConst c
            let mAcc = loopMeasure m
            Measure(cAcc, mAcc)
        | SynConst.Single x -> Lit(Single x)
        | SynConst.SByte x -> Lit(SByte x)
        | SynConst.Byte x -> Lit(Byte x)
        | SynConst.UInt16 x -> Lit(UInt16 x)
        | SynConst.UInt32 x -> Lit(UInt x)
        | SynConst.UInt64 x -> Lit(UInt64 x)
        | SynConst.Int16 x -> Lit(Int16 x)
        | SynConst.Int32 x -> Lit(Int x)
        | SynConst.Int64 x -> Lit(Int64 x)             
        | SynConst.UserNum(num, ty) -> 
            assert(ty = "I")
            Lit(BigInt(bigint.Parse num))
        | SynConst.Double x -> Lit(Double x)
        | SynConst.Unit -> Lit Unit
        | SynConst.String(x, _) -> Lit(String x)
        | SynConst.Char ch -> Lit(Char ch)
        | SynConst.Bool b -> Lit(Bool b)

    and loopTypeDef x =
        match x with
        | SynTypeDefn.TypeDefn(SynComponentInfo.ComponentInfo(_,_,_,ident,_,_,_,_) , x, ms,_) ->
            loopRep((List.head ident).idText) ms x

    and loopType x =
        match x with
        | SynType.HashConstraint(t, _) ->
            let tAcc = loopType t
            tAcc
//        | SynType.MeasureOne _ ->
//            TMeasureOne 
        | SynType.MeasurePower(t, n,_) ->
            let tAcc = loopType t
            TMeasurePower(tAcc, n)
        | SynType.Array(n,t,_) ->
            let tAcc = loopType t
            TArray(n, tAcc)
        | SynType.Anon(_) ->
            TAnon
        | SynType.LongIdent(LongIdentWithDots(ident, _)) ->
            TLongIdent(List.map (fun (id:Ident) -> TIdent(id.idText, mkSrcLoc id.idRange)) ident) 
        | SynType.Var(SynTypar.Typar(id, _, _), _) ->
            Type.TVar(TIdent(id.idText, mkSrcLoc id.idRange))
        | SynType.Fun(ty1, ty2, _) ->
            let ty1Acc = loopType ty1
            let ty2Acc = loopType ty2
            TFun (ty1Acc, ty2Acc)
        | SynType.App(t, _, ts, _, _, _, _) ->
            let tAcc = loopType t
            let tsAcc = List.map loopType ts
            TApp(tAcc, tsAcc) 
        | SynType.Tuple(ts, _) ->
            let tsAcc = List.map (fun (_, x) -> loopType x) ts
            TTuple tsAcc
         | _ -> failwith "loopType: unsupported pattern"

    and loopRep name ms x =
        match x with
        | SynTypeDefnRepr.Simple(x, _) -> 
            loopSimpleTypeRep name ms x
        | SynTypeDefnRepr.ObjectModel(_,ms',_) ->
            let msAcc1 = List.map loopClassMember ms'
            let msAcc2 = List.map loopClassMember ms
            Class(name, msAcc1 @ msAcc2)

    and loopClassMember x = 
        match x with                
        | SynMemberDefn.NestedType _ -> ClassMember.NotSupported
        | SynMemberDefn.Open _ -> ClassMember.NotSupported

        | SynMemberDefn.ImplicitInherit(t, e, id, _) ->
            let idAcc = Option.map (fun (x : Ident) -> TIdent(x.idText, mkSrcLoc x.idRange)) id
            let tAcc = loopType t
            let eAcc = loopExpr e
            ClassMember.ImplicitInherit(tAcc, eAcc, idAcc)
        | SynMemberDefn.Inherit(t, id, _) ->
            let idAcc = Option.map (fun (x : Ident) -> TIdent(x.idText, mkSrcLoc x.idRange)) id
            let tAcc = loopType t
            ClassMember.Inherit(tAcc, idAcc)
        | SynMemberDefn.ValField(SynField.Field(_,_,id,t,_,_,_,_), _) ->
            let idAcc = Option.map (fun (x : Ident) -> TIdent(x.idText, mkSrcLoc x.idRange)) id
            let tAcc = loopType t
            ClassMember.ValField(idAcc, tAcc)
        | SynMemberDefn.ImplicitCtor(_,_,ps,_,_) -> 
            let psAcc = List.map loopSimplePat ps
            ClassMember.ImplicitCtor psAcc
        | SynMemberDefn.Member(b, _) -> 
            loopMemberBinding b
        | SynMemberDefn.LetBindings(es,_,_,_) ->
            let esAcc = List.map loopBinding es
            let esAcc' = List.map (fun (pAcc, eAcc) -> Let(false, [pAcc, eAcc], Lit(Unit))) esAcc
            ClassMember.LetBindings esAcc'
        | SynMemberDefn.AbstractSlot(SynValSig.ValSpfn(_, ident, _, _, _, _, _, _, _, _, _),_,_) ->
            ClassMember.AbstractSlot(ident.idText)
        | SynMemberDefn.Interface(t, msOption, _) ->
            let tAcc = loopType t 
            let msAcc = Option.map (List.map loopClassMember) msOption
            ClassMember.Interface(tAcc, msAcc)
        | _ -> failwith "loopClassMember: Unsupported pattern"

    and loopSimpleTypeRep name ms x =
        match x with
        | SynTypeDefnSimpleRepr.Enum(ecs, _) ->
            let ecsAcc = List.map loopEnumCases ecs
            TypeDef.Enum(name, ecsAcc)
        | SynTypeDefnSimpleRepr.Union(_, xs, _) -> 
            let xsAcc = List.map loopUnionCases xs
            TypeDef.DisUnion(name, xsAcc)
        | SynTypeDefnSimpleRepr.Record(_, fields, _) -> 
            let fieldsAcc = List.map loopRecordFields fields
            let msAcc = List.map loopClassMember ms
            TypeDef.Record(name, fieldsAcc, msAcc)
        | SynTypeDefnSimpleRepr.None _ -> 
            TypeDef.None name 
        | SynTypeDefnSimpleRepr.TypeAbbrev(_, ty, _) ->
            let tAcc = loopType ty
            TypeDef.Abbrev(name, tAcc)
        | _ -> failwith "loopSimpleTypeRep: Unsupported pattern"

    and loopUnionCases x =
        match x with
        | SynUnionCase.UnionCase(_,x,_,_,_,_) ->
            (x.idText, mkSrcLoc x.idRange)

    and loopEnumCases x =
        match x with
        | SynEnumCase.EnumCase(_,x,c,_,_) ->
            match loopConst c with
            | Lit cAcc ->
                ((x.idText, mkSrcLoc x.idRange), cAcc)
            | _ -> failwith "loopEnumCases: Unexpected input"

    and loopRecordFields x =
        match x with 
        | SynField.Field(_, _, identOption, _, _, _, _, _) -> 
            Option.map (fun (x : Ident) ->(x.idText, mkSrcLoc x.idRange)) identOption

    and loopClause x = 
        match x with
        | SynMatchClause.Clause(p,_,e,_,_) -> 
            let pAcc = loopPat p
            let eAcc = loopExpr e
            Clause(pAcc, eAcc)

    and loopPat x =
        match x with
        | SynPat.OptionalVal(x, _) -> 
            PVar(x.idText, mkSrcLoc x.idRange)
        | SynPat.Attrib(p, attrs, _) ->
            let pAcc = loopPat p
            let attrsAcc = List.map loopAttribute attrs
            PAttribute(pAcc, attrsAcc)
        | SynPat.Or(p1, p2, _) ->
            let p1Acc = loopPat p1
            let p2Acc = loopPat p2
            POr(p1Acc, p2Acc)
        | SynPat.Ands(ps, _) ->
            let psAcc = List.map loopPat ps
            PAnds psAcc
        | SynPat.Null _ ->
            PNull
        | SynPat.Typed(pat, _, _) -> 
            loopPat pat // TODO: Add support for Typed patterns in AST
        | SynPat.Named(p, x, _, _, _) -> 
            let pAcc = loopPat p
            match pAcc with
            | PWild -> PVar(x.idText, mkSrcLoc x.idRange)
            | _ -> PNamed(pAcc, PVar(x.idText, mkSrcLoc x.idRange))
        | SynPat.LongIdent(LongIdentWithDots(xs, _), _, _, ys, _, _) -> 
            let x = xs |> Seq.map (fun (x : Ident) -> PVar(x.idText, mkSrcLoc x.idRange)) 
                       |> Seq.toList                               
                       |> fun xs' -> match xs' with
                                     | x'::[] -> match x' with
                                              // | PVar("True",_) -> PLit(Bool(true))
                                                 | _ -> x'
                                     | xs -> PLongVar xs
            if List.isEmpty ys then x
             else
                 buildPApp x (List.rev ys)
        | SynPat.Paren(x, _) -> 
            let xAcc = loopPat x
            PParen xAcc
        | SynPat.Tuple(xs, _) ->
            let xsAcc = List.map loopPat xs  
            PTuple xsAcc 
        | SynPat.Record(xs, _) ->
            let xsAcc = List.map (fun ((_, i : Ident), p) -> 
                            let pAcc = loopPat p
                            (i.idText, pAcc)) xs
            Pat.PRecord xsAcc
        | SynPat.Wild _ -> Pat.PWild
        | SynPat.Const(c, _) -> 
            match loopConst c with
            | Lit lit -> PLit lit
            | _ -> failwith "loopPat: Unexpected input"
        | SynPat.ArrayOrList(_,xs,_) -> 
            let xsAcc = List.map loopPat xs
            PList xsAcc
        | SynPat.IsInst(t, _) ->
            let tAcc = loopType t
            PIsInst tAcc
        | _ -> failwith "looPat: Unsupported pattern"

    and loopAttribute(x : SynAttribute) =
        let argExprAcc = loopExpr(x.ArgExpr)
        Attribute(argExprAcc)

    and buildPApp f xs = 
        match xs with
        | x::[] -> 
            let xAcc = loopPat x
            PApp(f, xAcc)
        | x::xs' -> 
            let xAcc = loopPat x
            let pAcc = buildPApp f xs'
            PApp(pAcc, xAcc)
        | _ -> failwith "buildPApp: Unexpected input"
       
    List.map loopDecl decls