module Fantomas.SourceParser

open Microsoft.FSharp.Compiler.Ast

let inline (|Ident|) (id: Ident) = id.idText

let inline (|LongIdent|) (li: LongIdent) = 
    li |> Seq.map (fun id -> id.idText) |> String.concat "."

let inline (|LongIdentWithDots|) (LongIdentWithDots(li, _)) = 
    li |> Seq.map (fun id -> id.idText) |> String.concat "."

// File level patterns

let (|ImplFile|SigFile|) = function
    | ParsedInput.ImplFile im -> ImplFile im
    | ParsedInput.SigFile si -> SigFile si

let (|ParsedImplFileInput|) = function
    | ParsedImplFileInput.ParsedImplFileInput(_, _, _, _, hs, mns, _) -> (hs, mns)

let (|ModuleOrNamespace|) = function
    | SynModuleOrNamespace.SynModuleOrNamespace(li, _, mds, px, ats, ao, _) -> (ats, px, ao, li, mds)

// Module declarations

let (|Open|_|) = function
    | SynModuleDecl.Open(LongIdentWithDots li, _) -> Some li
    | _ -> None

let (|ModuleAbbrev|_|) = function
    | SynModuleDecl.ModuleAbbrev(Ident id, LongIdent li, _) -> Some(id, li)
    | _ -> None

let (|HashDirective|_|) = function
    | SynModuleDecl.HashDirective(ParsedHashDirective(s, ss, _), _) -> Some(s, ss)
    | _ -> None

let (|NamespaceFragment|_|) = function 
    | SynModuleDecl.NamespaceFragment m -> Some m 
    | _ -> None

let (|Attributes|_|) = function
    | SynModuleDecl.Attributes(ats, _) -> Some ats
    | _ -> None

let (|Let|_|) = function
    | SynModuleDecl.Let(isRec, xs, _) -> Some(isRec, xs)
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

// Exception definition

let (|ExceptionDef|) = function
    | SynExceptionDefn.ExceptionDefn(SynExceptionRepr.ExceptionDefnRepr(ats, uc, _, px, ao, _), ms, _) ->
        (px, ats, ao, uc, ms)

let (|LetBinding|MemberBinding|) = function
    | SynBinding.Binding(ao, _, _, _, ats, px, SynValData(Some isInst, _,_), pat, _, expr, _, _) -> 
        MemberBinding(px, ats, ao, isInst, pat, expr)
    | SynBinding.Binding(ao, _, _, _, ats, px, _, pat, _, expr, _, _) -> 
        LetBinding(px, ats, ao, pat, expr)

// expressions

let (|TraitCall|_|) = function
    | SynExpr.TraitCall(ts, msig, expr, _) ->
        let ids = List.map (fun (SynTypar.Typar(Ident id, _, b)) -> id) ts
        Some(ids, msig, expr)
    | _ -> None

let (|Quote|_|) = function                                    
    | SynExpr.Quote(e1, _, e2, _, _) -> Some(e1, e2)
    | _ -> None

type ExprKind = | InferredDowncast | InferredUpcast | Lazy | Assert | AddressOf 
                | Paren | YieldOrReturn | YieldOrReturnFrom | Do | DoBang

let (|SingleExpr|_|) = function 
    | SynExpr.InferredDowncast(e, _) -> Some(InferredDowncast, e)
    | SynExpr.InferredUpcast(e, _) -> Some(InferredUpcast, e)
    | SynExpr.Lazy(e, _) -> Some(Lazy, e)
    | SynExpr.Assert(e, _) -> Some(Assert, e)
    | SynExpr.AddressOf(_, e, _, _) -> Some(AddressOf, e) // Might break into 2 cases
    | SynExpr.Paren(e, _, _, _) -> Some(Paren, e)
    | SynExpr.YieldOrReturn(_, e, _) -> Some(YieldOrReturn, e)
    | SynExpr.YieldOrReturnFrom(_, e, _) -> Some(YieldOrReturnFrom, e)
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
    | SynExpr.For(_, Ident id, e1, _, e2, e3, _) -> Some(id, e1, e2, e3)
    | _ -> None

let (|Null|_|) = function 
    | SynExpr.Null _ -> Some () 
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
    | SynExpr.ArrayOrList(_, xs, _) -> Some xs
    | _ -> None

let (|CompExpr|_|) = function
    | SynExpr.CompExpr(_, _, expr, _) -> Some expr
    | _ -> None

let (|ArrayOrListOfSeqExpr|_|) = function
    | SynExpr.ArrayOrListOfSeqExpr(_, expr, _) -> Some expr
    | _ -> None

let (|Tuple|_|) = function
    | SynExpr.Tuple(exprs, _, _) -> Some exprs
    | _ -> None

let (|ConstExpr|_|) = function
    | SynExpr.Const(x, _) -> Some x
    | _ -> None

let (|Var|_|) = function
    | SynExpr.Ident(Ident id) -> Some id
    | SynExpr.LongIdent(_, LongIdentWithDots li, _, _) -> Some li
    | _ -> None

let (|App|_|) = function
    | SynExpr.App(_, _, e1, e2, _) -> Some(e1, e2)
    | _ -> None

let (|Lambda|_|) = function
    | SynExpr.Lambda(_, _, pats, e, _) -> Some(pats, e)
    | _ -> None

let (|LetOrUse|_|) = function
    | SynExpr.LetOrUse(isRec, _, xs, e, _) -> 
        if List.isEmpty xs then failwith "Illformed bindings"
        else Some (isRec, xs, e)
    | _ -> None

let (|LetOrUseBang|_|) = function
    | SynExpr.LetOrUseBang(_, _, _, p, e1, e2, _) -> Some(p, e1, e2)
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

// Literals

let (|Measure|) x = 
    let rec loop = function
        | SynMeasure.Var((SynTypar.Typar(Ident id, _, _)), _) -> id
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
    | SynConst.Measure(Const c, Measure m) -> 
        Const(c + m)
    | SynConst.UserNum(num, ty) -> 
        Const(num + ty)
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
        