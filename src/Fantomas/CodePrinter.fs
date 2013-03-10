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
    | Attributes(ats) -> col sepArgs ats genAttribute
    | DoExpr(e) ->  genExpr e
    | Exception(ex) -> genException ex
    | HashDirective(s1, s2) -> !- "#" -- s1 +> sepSpace -- sprintf "%A" s2 // print with quotes
    | Let(b) -> !- "let " +> genBinding b
    | LetRec(b::bs) -> !- "let rec " +> genBinding b +> sepNln +> col sepNln bs (fun b -> !- "and " +> genBinding b)
    | ModuleAbbrev(s1, s2) -> !- "module " -- s1 +> sepEq -- s2
    | NamespaceFragment(m) -> !- "[NamespaceFragment]"
    | NestedModule(ats, px, ao, s, mds) -> 
        colOpt sepArgs sepNln ats genAttribute 
        +> genPreXmlDoc px -- "module " +> opt sepSpace ao genAccess -- s +> sepEq
        +> indent +> sepNln +> col sepNln mds genModuleDecl
    | Open(s) -> !- "open " -- s
    | Types(sts) -> col sepNln sts genTypeDefn
    | md -> failwithf "Unexpected pattern: %O" md

and genAccess(Access s) = !- s

and genAttribute(Attribute(li, e, isGetSet)) = !- "[<" -- li +> genExpr e -- ">]"
    
and genPreXmlDoc(PreXmlDoc lines) = colOpt sepNln sepNln lines (!-)

and genBinding = function
    | LetBinding(px, ats, ao, p, e) -> genPat p +> sepEq +> genExpr e
    | MemberBinding(px, ats, ao, isInst, pat, expr) -> failwith "Not implemented yet"

and genExpr = function
    // Superfluous paren in tuple
    | SingleExpr(Paren, (Tuple es as e)) -> genExpr e
    | SingleExpr(Paren, e) -> !- "(" +> genExpr e -- ")"
    | SingleExpr(Do, e) -> !- "do " +> genExpr e
    | SingleExpr(kind, e) -> id
    | ConstExpr(Const s) -> !- s
    | NullExpr -> id
    | Quote(e1, e2) -> id
    | TypedExpr(_, e, t) -> id
    | Tuple(es) -> !- "(" +> col sepArgs es genExpr -- ")"
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

and genTypeDefn e = id

and genException(ExceptionDef(ats, px, ao, uc, ms)) = 
    colOpt sepArgs sepNln ats genAttribute 
    +> genPreXmlDoc px -- "exception " +> opt sepSpace ao genAccess +> genUnionCase uc
    +> sepNln +> colOpt sepNln sepNln ms genMemberDefn

and genUnionCase(UnionCase(ats, px, ao, s, UnionCaseType fs)) = 
    opt sepSpace ao genAccess -- s +> sepWordOf +> col sepStar fs genField

and genField (Field(ats, px, ao, isStatic, t, so)) = genType t

and genType = function
    | TypeLongIdent li -> !- li
    | t -> failwithf "Unexpected pattern: %O" t

and genMemberDefn = function
    | MemberDefnNestedType(td, ao) -> id
    | MemberDefnOpen(so) -> id
    | MemberDefnImplicitInherit(t, e, so) -> id
    | MemberDefnInherit(t, so) -> id
    | MemberDefnValField(ats, px, ao, t, so) -> id
    | MemberDefnImplicitCtor(ats, ao, ps, so) -> id
    | MemberDefnMember(bo) -> id
    | MemberDefnLetBindings(isStatic, isRec, bs) -> id
    | MemberDefnInterface(t, mdo) -> id
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
