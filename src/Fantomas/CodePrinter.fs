module Fantomas.CodePrinter

open System
open Fantomas.SourceParser
open Fantomas.FormatConfig

let rec genParsedInput = function
    | ImplFile im -> genImpFile im
    | SigFile si -> genSigFile si

and genImpFile = function
    | ParsedImplFileInput(hs, mns) ->
        // Each module is separated by a number of blank lines
        mns |> Seq.map genModuleOrNamespace |> Seq.reduce (+>)

and genSigFile si = failwith "Not implemented yet"

and genModuleOrNamespace = function
    | ModuleOrNamespace(ats, px, ao, li, mds) ->
        mds |> Seq.map genModuleDecl |> Seq.reduce (+>)

and genModuleDecl = function
    | Attributes(a) -> !. "[Attributes]"
    | DoExpr(e) ->  genExpr e
    | Exception(ex) -> genException ex
    | HashDirective(s, ss) -> !. (sprintf "#%s %s" s <| String.concat "." ss)
    | Let(LetBinding(px, ats, ao, p, e)) -> !. "let " +> (genPat p) -- " = " +> (genExpr e)
    | LetRec(bs) -> !. "[LetRec]"
    | ModuleAbbrev(s1, s2) -> !. (sprintf "module %s = %s" s1 s2)
    | NamespaceFragment(m) -> !. "[NamespaceFragment]"
    | NestedModule(ats, px, ao, s, mds) -> 
        id ++ "[Attributes]" ++ "[XmlDocs]" 
        ++ sprintf "module %s%s = " (defaultArg (Option.map(sprintf "%O ") ao) "") s
        +> incIndent
        ++>> Seq.map genModuleDecl mds
    | Open(s) -> !. (sprintf "open %s" s)
    | Types(sts) -> Seq.map genTypeDefn sts |> Seq.reduce (+>)
    | md -> failwithf "Unexpected pattern: %O" md

and genExpr = function
    | SingleExpr(e) -> id
    | Quote(e1, e2) -> id
    | ConstExpr(Const s) -> ! s 
    | TypedExpr(_, e, t) -> id
    | Tuple(es) -> id
    | ArrayOrList(es) -> id
    | Record(xs) -> id
    | ObjExpr(t, x, bd, ims) -> id
    | While(e1, e2) -> id
    | For(s, e1, e2, e3) -> id
    | ForEach(p, e1, e2) -> id
    | CompExpr(e) -> id
    | ArrayOrListOfSeqExpr(e) -> id
//    | Lambda (_,_,_,_,m)
//    | Match (_,_,_,_,m)
//    | MatchLambda (_,_,_,_,m)
//    | Do (_,m)
//    | Assert (_,m)
//    | App (_,_,_,_,m)
//    | TypeApp (_,_,_,_,_,_,m)
//    | LetOrUse (_,_,_,_,m)
//    | TryWith (_,_,_,_,m,_,_)
//    | TryFinally (_,_,m,_,_)
//    | Sequential (_,_,_,_,m)
//    | ArbitraryAfterError(_,m)
//    | FromParseError (_,m) 
//    | DiscardAfterMissingQualificationAfterDot (_,m) 
//    | IfThenElse (_,_,_,_,_,_,m)
//    | LongIdent (_,_,_,m)
//    | LongIdentSet (_,_,m)
//    | NamedIndexedPropertySet (_,_,_,m)
//    | DotIndexedGet (_,_,_,m)
//    | DotIndexedSet (_,_,_,_,_,m)
//    | DotGet (_,_,_,m)
//    | DotSet (_,_,_,m)
//    | DotNamedIndexedPropertySet (_,_,_,_,m)
//    | LibraryOnlyUnionCaseFieldGet (_,_,_,m)
//    | LibraryOnlyUnionCaseFieldSet (_,_,_,_,m)
//    | LibraryOnlyILAssembly (_,_,_,_,m)
//    | LibraryOnlyStaticOptimization (_,_,_,m)
//    | TypeTest (_,_,m)
//    | Upcast (_,_,m)
//    | AddressOf (_,_,_,m)
//    | Downcast (_,_,m)
//    | JoinIn (_,_,_,m)
//    | InferredUpcast (_,m)
//    | InferredDowncast (_,m)
//    | Null m
//    | Lazy (_, m)
//    | TraitCall(_,_,_,m)
//    | ImplicitZero (m)
//    | YieldOrReturn (_,_,m)
//    | YieldOrReturnFrom (_,_,m)
//    | LetOrUseBang  (_,_,_,_,_,_,m)
//    | DoBang  (_,m) -> m
//    | Ident id -> id.idRange
    | e -> failwithf "Unexpected pattern: %O" e

and genException e = id

and genTypeDefn td = id

and genPat = function
    | PatOptionalVal(x) -> id
    | PatAttrib(p, attrs) -> id
    | PatOr(p1, p2) -> id
    | PatAnds(ps) -> id
    | PatNullary PatNull -> id
    | PatNullary PatWild -> id
    | PatTyped(p, t) -> id
    | PatNamed(ao, p, s) -> ! s
    | PatLongIdent(ao, li, ps) -> id
    | PatParen(p) -> id
    | PatSeq(PatTuple, ps) -> id
    | PatSeq(PatArray, ps) -> id
    | PatSeq(PatList, ps) -> id
    | PatRecord(xs) -> id
    | PatConst(Const s) -> ! s
    | PatIsInst(p) -> id
    | p -> failwithf "Unexpected pattern: %O" p
        
        




