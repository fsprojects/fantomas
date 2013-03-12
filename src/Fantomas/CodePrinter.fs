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
        col sepNln mns genModuleOrNamespace

and genSigFile si = failwith "Not implemented yet"

and genModuleOrNamespace = function
    | ModuleOrNamespace(ats, px, ao, li, mds) -> col sepNln mds genModuleDecl

and genModuleDecl = function
    | Attributes(ats) -> col sepNln ats genAttribute
    | DoExpr(e) ->  genExpr e
    | Exception(ex) -> genException ex
    | HashDirective(s1, s2) -> !- "#" -- s1 +> sepSpace -- sprintf "%A" s2 // print with quotes
    | Let(b) -> genBinding "let " b
    | LetRec(b::bs) -> genBinding "let rec " b +> sepNln +> col sepNln bs (genBinding "and ")
    | ModuleAbbrev(s1, s2) -> !- "module " -- s1 +> sepEq -- s2
    | NamespaceFragment(m) -> failwithf "NamespaceFragment is not supported yet: %O" m
    | NestedModule(ats, px, ao, s, mds) -> 
        colPost sepNln sepNln ats genAttribute 
        +> genPreXmlDoc px -- "module " +> opt sepSpace ao genAccess -- s +> sepEq
        +> indent +> sepNln +> col sepNln mds genModuleDecl +> unindent // sepNln forces evaluation
    | Open(s) -> !- "open " -- s
    | Types(sts) -> col sepNln sts genTypeDefn
    | md -> failwithf "Unexpected pattern: %O" md

and genAccess(Access s) = !- s

and genAttribute(Attribute(li, e, isGetSet)) = !- "[<" -- li +> genExpr e -- ">]"
    
and genPreXmlDoc(PreXmlDoc lines) = colPost sepNln sepNln lines (!-)

and genBinding prefix = function
    | LetBinding(ats, px, ao, isInline, isMutable, p, e, bk) ->
        // Figure out what to do with SynBindingKind
        colPost sepSpace sepNln ats genAttribute 
        +> genPreXmlDoc px -- prefix +> opt sepSpace ao genAccess
        +> ifElse isMutable (!- "mutable ") id +> ifElse isInline (!- "inline ") id
        +> genPat p +> sepEq +> genExpr e
    | MemberBinding(ats, px, ao, isInline, isInst, p, e, bk) ->
        // Refactor: prefix doesn't make sense here
        colPost sepSpace sepNln ats genAttribute
        +> genPreXmlDoc px
        +> ifElse isInst (!- "static member ") (!- "member ")
        +> ifElse isInline (!- "inline ") id
        +> opt sepSpace ao genAccess
        +> genPat p +> sepEq +> genExpr e

and genExpr = function
    // Superfluous paren in tuple
    | SingleExpr(Paren, (Tuple _ as e)) -> genExpr e
    | SingleExpr(Paren, e) -> !- "(" +> genExpr e -- ")"
    | SingleExpr(Do, e) -> !- "do " +> genExpr e
    | SingleExpr(kind, e) -> id
    | ConstExpr(Const s) -> !- s
    | NullExpr -> id
    | Quote(e1, e2) -> id
    | TypedExpr(_, e, t) -> id
    | Tuple(es) -> !- "(" +> col sepComma es genExpr -- ")"
    | ArrayOrList(es) -> id
    | Record(xs) -> id
    | ObjExpr(t, x, bd, ims) -> id
    | While(e1, e2) -> id
    | For(s, e1, e2, e3) -> id
    | ForEach(p, e1, e2) -> id
    | CompExpr(isList, e) -> id
    | ArrayOrListOfSeqExpr(e) -> id
    | Lambda(e, cs) -> id
    | Match(e, cs) -> id
    | Sequential(e1, e) -> id
    | App(e1, e2) -> genExpr e1 +> sepSpace +> genExpr e2
    | TypeApp(e, ts) -> id
    | LetOrUse(isRec, isUse, bs, e) -> id
    | TryWith(e, cs) -> id
    | TryFinally(e1, e2) -> id
    | Sequential(e1, e2) -> id
    | IfThenElse(e1, e2, e3) -> id
    | Var(li) -> !- li
    | LongIdentSet(e) -> id
    | DotIndexedGet(e, es) -> id
    | DotIndexedSet(e1, es, e2) -> id
    | DotGet(e, s) -> id
    | DotSet(e1,_s, e2) -> id
    | TraitCall(ss, msg, e) -> id
    | LetOrUseBang(isUse, p, e1, e2) -> id
    | e -> failwithf "Unexpected pattern: %O" e

and genTypeDefn(TypeDef(ats, px, ao, tds, tcs, tdr, ms, li)) = 
    let typeName = 
        colPost sepSpace sepNln ats genAttribute 
        +> genPreXmlDoc px -- "type " +> opt sepSpace ao genAccess -- li
    match tdr with
    | Simple(TDSREnum ecs) ->
        typeName +> sepEq 
        +> indent +> sepNln
        +> col sepNln ecs (genEnumCase true) +> sepNln
        +> unindent
    | Simple(TDSRUnion(ao', xs)) ->
        typeName +> sepEq 
        +> indent +> sepNln +> opt sepNln ao' genAccess 
        +> col sepNln xs (genUnionCase true) +> sepNln
        +> unindent
    | Simple(TDSRRecord(ao', fs)) ->
        typeName +> sepEq 
        +> indent +> sepNln +> opt sepNln ao' genAccess
        -- "{ " +> col sepSemiNln fs genField -- " }" +> sepNln
        +> unindent
    | Simple TDSRNone -> id
    | Simple(TDSRTypeAbbrev t)  -> id
    | Simple TDSRGeneral -> id
    | ObjectModel(tdk, md) -> failwith "Not implemented yet"

and genException(ExceptionDef(ats, px, ao, uc, ms)) = 
    colPost sepSpace sepNln ats genAttribute 
    +> genPreXmlDoc px -- "exception " +> opt sepSpace ao genAccess +> genUnionCase false uc
    +> sepNln +> colPost sepNln sepNln ms genMemberDefn

and genUnionCase hasBar (UnionCase(ats, px, ao, s, UnionCaseType fs)) = 
    // Access option doesn't seem relevant here
    genPreXmlDoc px
    +> ifElse hasBar (!- "| ") id -- s 
    +> colPre sepStar sepWordOf fs genField

and genEnumCase hasBar (EnumCase(ats, px, s, Const c)) =
    genPreXmlDoc px
    +> ifElse hasBar (!- "| ") id 
    +> colPost sepSpace sepSpace ats genAttribute -- s +> sepEq -- c    

and genField(Field(ats, px, ao, isStatic, t, so)) = 
    opt (!- " : ") so (!-) +> genType t

and genType = function
    | THashConstraint t -> id
    | TMeasurePower(t, n) -> id
    | TArray(t, n) -> id
    | TAnon -> id
    | TVar tp -> id
    | TFun(t1, t2) -> id
    | TApp(t, ts) -> id
    | TTuple ts -> id
    | TWithGlobalConstraints(t, ts) -> id
    | TLongIdent li -> !- li
    | t -> failwithf "Unexpected pattern: %O" t

and genMemberDefn = function
    | MDNestedType(td, ao) -> id
    | MDOpen(so) -> id
    | MDImplicitInherit(t, e, so) -> id
    | MDInherit(t, so) -> id
    | MDValField(ats, px, ao, t, so) -> id
    | MDImplicitCtor(ats, ao, ps, so) -> id
    | MDMember(bo) -> id
    | MDLetBindings(isStatic, isRec, bs) -> id
    | MDInterface(t, mdo) -> id
    | MDAutoProperty(ats, px, ao, mk, e, s) -> id
    | md -> failwithf "Unexpected pattern: %O" md

and genPat = function
    | PatOptionalVal(x) -> id
    | PatAttrib(p, attrs) -> id
    | PatOr(p1, p2) -> id
    | PatAnds(ps) -> id
    | PatNullary PatNull -> id
    | PatNullary PatWild -> id
    | PatTyped(p, t) -> id
    | PatNamed(ao, p, s) -> !- s
    | PatLongIdent(ao, li, ps) -> opt sepSpace ao genAccess -- li +> sepSpace +> col sepSpace ps genPat
    | PatParen(p) -> id
    | PatSeq(PatTuple, ps) -> id
    | PatSeq(PatArray, ps) -> id
    | PatSeq(PatList, ps) -> id
    | PatRecord(xs) -> id
    | PatConst(Const s) -> !- s
    | PatIsInst(p) -> id
    | p -> failwithf "Unexpected pattern: %O" p
