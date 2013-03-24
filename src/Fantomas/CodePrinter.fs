module internal Fantomas.CodePrinter

open System
open Fantomas.SourceParser
open Fantomas.FormatConfig

[<RequireQualifiedAccess>]
module List = 
    let inline atmostOne xs =
        match xs with
        | [] | [_] -> true
        | _ -> false

/// Check whether an expression should be broken into multiple lines
let rec multiline = function
    | Paren e | SingleExpr(_, e) | TypedExpr(_, e, _) -> multiline e
    | ConstExpr _ | NullExpr | Var _ -> false
    | Quote(e1, e2, _) -> multiline e1 || multiline e2
    | Tuple es -> List.exists multiline es
    /// An array or a list is multiline if there are at least two elements
    | ArrayOrList(_, es) -> 
        not (List.atmostOne es)
    /// A record is multiline if there is at least two fields present
    | Record(xs, _) -> 
        let fields = xs |> List.choose ((|RecordFieldName|) >> snd) 
        not (List.atmostOne fields) || List.exists multiline fields
    | ObjExpr _ | While _ | For _ | ForEach _ -> true
    | CompExpr(_, e) -> multiline e
    | ArrayOrListOfSeqExpr(_, e) -> multiline e
    | JoinIn(e1, e2) -> multiline e1 || multiline e2
    | Lambda(e, _) -> multiline e
    | MatchLambda _ -> true
    | Match(e, cs) -> multiline e || not (List.isEmpty cs)
    // This break abstraction since NewLineInfixOps is global var
    | InfixApp(s, e1, e2) -> NewLineInfixOps.Contains s || multiline e1 || multiline e2
    | App(e1, es) -> multiline e1 || List.exists multiline es
    | TypeApp(e, _) -> multiline e
    | LetOrUse(_, _, bs, e) -> not (List.isEmpty bs) || multiline e
    | SequentialSimple _ -> false
    | TryWith _ | TryFinally _ ->  true
    | Sequential _ -> true
    | IfThenElse _ -> true
    | LongIdentSet(_, e) -> multiline e
    | DotIndexedGet(e, es) -> multiline e || List.exists multiline es
    | DotIndexedSet(e1, es, e2) -> multiline e1 || multiline e2 || List.exists multiline es
    | DotGet(e, _) -> multiline e
    | DotSet(e1, _, e2) -> multiline e1 || multiline e2
    | TraitCall(_, _, e) -> multiline e
    | LetOrUseBang(_, _, e1, e2) -> multiline e1 || multiline e2
    /// Default mode is single-line
    | e -> false

/// Check if the expression already has surrounding parentheses
let hasParenthesis = function
    | Paren _ | ConstExpr(Const "()") | Tuple _ -> true
    | _ -> false

let hasParenInPat = function
    | PatOptionalVal _ -> false
    | PatAttrib _ -> false
    | PatOr _ -> true
    | PatAnds _ -> true
    | PatNullary _ -> false
    | PatTyped _ -> false
    | PatNamed _ -> false
    | PatLongIdent _ -> false
    | PatConst(Const "()") -> true
    | PatConst _ -> false
    | PatParen _ -> true
    | PatSeq _ -> false
    | PatRecord _ -> false
    | PatIsInst _ -> true
    | PatQuoteExpr e -> false
    | p -> failwithf "Unexpected pattern: %O" p

let inline genConst c =
    match c with
    | Const c -> !- c
    | Unresolved c -> fun ctx -> str (content c ctx) ctx

let rec genParsedInput = function
    | ImplFile im -> genImpFile im
    | SigFile si -> genSigFile si

and genImpFile(ParsedImplFileInput(hs, mns)) = col sepNln mns genModuleOrNamespace

and genSigFile si = failwith "F# signature files are not supported yet"

and genModuleOrNamespace = function
    | ModuleOrNamespace(ats, px, ao, s, mds, isModule) -> 
        genPreXmlDoc px
        +> colPost sepNln sepNln ats genAttribute
        /// Checking for Tmp is a bit fragile
        +> ifElse (s = "Tmp") sepNone (ifElse isModule (!- "module ") (!- "namespace ") -- s +> rep 2 sepNln)
        +> col sepNln mds genModuleDecl

and genModuleDecl = function
    | Attributes(ats) -> col sepNln ats genAttribute
    | DoExpr(e) -> genExpr e
    | Exception(ex) -> genException ex
    | HashDirective(s1, s2) -> 
        /// print strings with quotes
        !- "#" -- s1 +> sepSpace +> ifElse (s2 = "") sepNone (!- (sprintf "\"%O\"" s2))
    /// Add a new line after model-level let bindings
    | Let(b) -> genLetBinding "let " b +> sepNln
    | LetRec(b::bs) -> 
        genLetBinding "let rec " b 
        +> colPre (rep 2 sepNln) (rep 2 sepNln) bs (genLetBinding "and ") +> sepNln
    | ModuleAbbrev(s1, s2) -> !- "module " -- s1 +> sepEq -- s2
    | NamespaceFragment(m) -> failwithf "NamespaceFragment is not supported yet: %O" m
    | NestedModule(ats, px, ao, s, mds) -> 
        genPreXmlDoc px
        +> colPost sepNln sepNln ats genAttribute -- "module " +> opt sepSpace ao genAccess -- s +> sepEq
        +> indent +> sepNln +> col sepNln mds genModuleDecl +> unindent
    | Open(s) -> !- (sprintf "open %s" s) +> sepNln
    /// There is no nested types and they have newlines in the ends of definitions
    | Types(t::ts) -> genTypeDefn true t +> colPre sepNln sepNln ts (genTypeDefn false)
    | md -> failwithf "Unexpected module declaration: %O" md

and genAccess(Access s) = !- s

and genAttribute(Attribute(li, e, isGetSet)) = 
    match e with
    /// Special treatment for function application on attributes
    | ConstExpr(Const "()") -> !- (sprintf "[<%s>]" li)
    | e -> !- "[<" -- li +> genExpr e -- ">]"
    
and genPreXmlDoc(PreXmlDoc lines) = colPost sepNln sepNln lines (sprintf "///%s" >> (!-))

/// These inline functions have to be defined before their uses

and inline autoBreakNln e = 
    ifElse (multiline e) (indent +> sepNln +> genExpr e +> unindent) (genExpr e)

and inline autoBreakNlnBy l e = 
    ifElse (multiline e) (incrIndent l +> sepNln +> genExpr e +> decrIndent l) (genExpr e)

and inline genTyparList tps = 
    ifElse (List.atmostOne tps) (col wordOr tps genTypar) (sepOpenT +> col wordOr tps genTypar +> sepCloseT)

and inline genTypeParam tds tcs =
    ifElse (List.isEmpty tds) sepNone
        (!- "<" +> col sepComma tds genTyparDecl +> colPre (!- " when ") wordAnd tcs genTypeConstraint -- ">")

and genLetBinding pref = function
    | LetBinding(ats, px, ao, isInline, isMutable, p, e, bk, bri) ->
        /// Figure out what to do with SynBindingKind
        let prefix =
            genPreXmlDoc px
            +> colPost sepNln sepNone ats genAttribute -- pref +> opt sepSpace ao genAccess
            +> ifElse isMutable (!- "mutable ") sepNone +> ifElse isInline (!- "inline ") sepNone
            +> genPat p
        match e with
        | TypedExpr(Typed, e, t) -> prefix +> sepColon +> genType t +> sepEq +> autoBreakNln e
        | e -> prefix +> sepEq +> autoBreakNln e
    | DoBinding(ats, px, e) ->
        let prefix = if pref.Contains("let") then pref.Replace("let", "do") else "do "
        genPreXmlDoc px
        +> colPost sepNln sepNone ats genAttribute -- prefix +> autoBreakNlnBy 2 e
    | b -> failwithf "%O isn't a let binding" b

and genMemberBinding isInterface = function
    | PropertyBinding(ats, px, ao, isInline, mf, p, e, bk, bri) -> 
        let prefix =
            genPreXmlDoc px
            +> colPost sepSpace sepNone ats genAttribute +> genMemberFlags isInterface mf
            +> ifElse isInline (!- "inline ") sepNone
            +> opt sepSpace ao genAccess
        let tuplerize ps =
            let rec loop acc = function
                | [p] -> (List.rev acc, p)
                | p1::ps -> loop (p1::acc) ps
                | [] -> invalidArg "p" "Patterns should not be empty"
            loop [] ps
        match mf with
        | MFProperty PropertyGet ->
            match p with
            /// Too tedious in handling property get and set
            | PatLongIdent(_, s, [PatSeq(PatTuple, ps)], _) -> 
                let (ps, p) = tuplerize ps
                prefix -- s -- " with get " 
                +> ifElse (List.atmostOne ps) (col sepComma ps genPat +> sepSpace) 
                    (sepOpenT +> col sepComma ps genPat +> sepCloseT +> sepSpace)
                +> genPat p
                +> sepEq +> autoBreakNln e
            | PatLongIdent(_, s, ps, _) -> 
                prefix -- s -- " with get " 
                +> col sepSpace ps genPat
                +> sepEq +> autoBreakNln e
            | p -> failwithf "Unexpected pattern: %O" p
        | MFProperty PropertySet -> 
            match p with
            | PatLongIdent(_, s, [PatSeq(PatTuple, ps)], _) -> 
                let (ps, p) = tuplerize ps
                prefix -- s -- " with set " 
                +> ifElse (List.atmostOne ps) (col sepComma ps genPat +> sepSpace) 
                    (sepOpenT +> col sepComma ps genPat +> sepCloseT +> sepSpace)
                +> genPat p
                +> sepEq +> autoBreakNln e
            | PatLongIdent(_, s, ps, _) -> 
                prefix -- s -- " with set " 
                +> col sepSpace ps genPat
                +> sepEq +> autoBreakNln e
            | p -> failwithf "Unexpected pattern: %O" p
        | mf -> failwithf "Unexpected member flags: %O" mf
    | MemberBinding(ats, px, ao, isInline, mf, p, e, bk, bri) ->
        let prefix =
            genPreXmlDoc px
            +> colPost sepSpace sepNone ats genAttribute +> genMemberFlags isInterface mf
            +> ifElse isInline (!- "inline ") sepNone
            +> opt sepSpace ao genAccess +> genPat p
        match e with
        | TypedExpr(Typed, e, t) -> prefix +> sepColon +> genType t +> sepEq +> autoBreakNln e
        | e -> prefix +> sepEq +> autoBreakNln e
    | ExplicitCtor(ats, px, ao, p, e) ->
        let prefix =
            genPreXmlDoc px
            +> colPost sepSpace sepNone ats genAttribute
            +> opt sepSpace ao genAccess +> genPat p
        match e with
        // Handle special "then" block in constructors
        | Sequential(e1, e2, _) -> 
            prefix +> sepEq +> indent +> sepNln +> genExpr e1 ++ "then " +> autoBreakNln e2 +> unindent
        | e -> prefix +> sepEq +> autoBreakNln e
    | b -> failwithf "%O isn't a member binding" b

and genMemberFlags isInterface = function
    | MFMember _ -> !- "member "
    | MFStaticMember _ -> !- "static member "
    | MFConstructor _ -> sepNone
    | MFOverride _ -> ifElse isInterface (!- "member ") (!- "override ")

and inline genRecordFieldName(RecordFieldName(s, eo)) =
    opt sepNone eo (fun e -> !- s +> sepEq +> genExpr e)

and genExpr = function
    | Paren(ConstExpr(Const "()")) -> !- "()"
    | Paren e -> sepOpenT +> genExpr e +> sepCloseT
    | SingleExpr(kind, e) -> str kind +> genExpr e
    | ConstExpr(c) -> genConst c
    | NullExpr -> !- "null"
    /// Not sure about the role of e1
    | Quote(e1, e2, isRaw) ->         
        let e = genExpr e2
        ifElse isRaw (!- "<@@ " +> e -- " @@>") (!- "<@ " +> e -- " @>")
    | TypedExpr(TypeTest, e, t) -> genExpr e -- " :? " +> genType t
    | TypedExpr(New, e, t) -> 
        !- "new " +> genType t +> ifElse (hasParenthesis e) (sepBeforeArg +> genExpr e) (sepSpace +> genExpr e)
    | TypedExpr(Downcast, e, t) -> genExpr e -- " :?> " +> genType t
    | TypedExpr(Upcast, e, t) -> genExpr e -- " :> " +> genType t
    | TypedExpr(Typed, e, t) -> genExpr e +> sepColon +> genType t
    | Tuple es -> col sepComma es genExpr
    | ArrayOrList(isArray, xs) -> 
        ifElse isArray (sepOpenA +> atCurrentColumn (col sepSemiNln xs genExpr) +> sepCloseA) 
            (sepOpenL +> atCurrentColumn (col sepSemiNln xs genExpr) +> sepCloseL)
    | Record(xs, eo) -> 
        sepOpenS +> opt (!- " with ") eo genExpr
        +> atCurrentColumn (col sepSemiNln xs genRecordFieldName)
        +> sepCloseS
    | ObjExpr(t, eio, bd, ims) ->
        /// Check the role of the second part of eio
        let param = opt sepNone (Option.map fst eio) genExpr
        sepOpenS +> 
        atCurrentColumn (!- "new " +> genType t +> param -- " with" 
        +> indent +> sepNln +> col sepNln bd (genMemberBinding true) +> unindent
        +> colPre sepNln sepNln ims genInterfaceImpl) +> sepCloseS
    | While(e1, e2) -> 
        atCurrentColumn (!- "while " +> genExpr e1 -- " do" 
        +> indent +> sepNln +> genExpr e2 +> unindent)
    | For(s, e1, e2, e3, isUp) ->
        atCurrentColumn (!- (sprintf "for %s = " s) +> genExpr e1 
        +> ifElse isUp (!- " to ") (!- " downto ") +> genExpr e2 -- " do" 
        +> indent +> sepNln +> genExpr e3 +> unindent)
    /// May handle the form 'for i in e1 -> e2' in the future
    | ForEach(p, e1, e2, _) ->
        atCurrentColumn (!- "for " +> genPat p -- " in " +> genExpr e1 
        -- " do" +> indent +> sepNln +> genExpr e2 +> unindent
        )
    | CompExpr(isArrayOrList, e) ->
        ifElse isArrayOrList (genExpr e) (sepOpenS +> autoBreakNln e +> sepCloseS) 
    | ArrayOrListOfSeqExpr(isArray, e) -> 
        ifElse isArray (sepOpenA +> genExpr e +> sepCloseA) (sepOpenL +> genExpr e +> sepCloseL)
    | JoinIn(e1, e2) -> genExpr e1 -- " in " +> genExpr e2
    | Lambda(e, sps) -> 
        !- "fun " +> col sepSpace sps genSimplePats +> sepArrow +> autoBreakNln e
    | MatchLambda(sp, isMember) -> atCurrentColumn (!- "function " +> colPre sepNln sepNln sp genClause)
    | Match(e, cs) -> 
        atCurrentColumn (!- "match " +> genExpr e -- " with"
        +> colPre sepNln sepNln cs genClause)
    | SeqApp(e) ->
        !- "seq " +> sepOpenS +> genExpr e +> sepCloseS
    | App(Var(OpName ".. .."), [e1; e2; e3]) -> genExpr e1 -- ".." +> genExpr e2 -- ".." +> genExpr e3
    /// Separate two prefix ops by spaces
    | PrefixApp(s1, PrefixApp(s2, e)) -> !- (sprintf "%s %s" s1 s2) +> genExpr e
    | PrefixApp(s, e) -> !- s  +> genExpr e
    /// Handle spaces of infix application based on which category it belongs to
    | InfixApp(s, e1, e2) -> 
        ifElse (Set.contains s NewLineInfixOps) 
            (atCurrentColumn (genExpr e1 +> sepNln -- s +> sepSpace +> genExpr e2))
            (ifElse (Set.contains s NoSpaceInfixOps) (genExpr e1 -- s +> genExpr e2) 
                (genExpr e1 +> sepSpace -- s +> sepSpace +> genExpr e2))
    | App(e1, [e2]) -> 
        atCurrentColumn (genExpr e1 +> 
            ifElse (hasParenthesis e2) (sepBeforeArg +> genExpr e2) (sepSpace +> genExpr e2))
    /// Always spacing in multiple arguments
    | App(e, es) -> atCurrentColumn (genExpr e +> colPre sepSpace sepSpace es genExpr)
    | TypeApp(e, ts) -> genExpr e -- "<" +> col sepComma ts genType -- ">"
    | LetOrUse(isRec, isUse, bs, e) ->
        let prefix = 
            if isUse then "use "
            elif isRec then "let rec "
            else "let "
        atCurrentColumn (col sepSpace bs (genLetBinding prefix) +> sepNln +> genExpr e)
    /// Could customize a bit if e is single line
    | TryWith(e, cs) ->  
        atCurrentColumn (!- "try " +> indent +> sepNln +> genExpr e +> unindent ++ "with" 
        +> indentWith +> sepNln +> col sepNln cs genClause +> unindentWith)
    | TryFinally(e1, e2) -> 
        atCurrentColumn (!- "try " +> indent +> sepNln +> genExpr e1 +> unindent ++ "finally" 
        +> indent +> sepNln +> genExpr e2 +> unindent)    
    | SequentialSimple es -> atCurrentColumn (col sepSemi es genExpr)
    /// It seems too annoying to use sepSemiNln
    | Sequential(e1, e2, _) -> 
        atCurrentColumn (genExpr e1 +> sepNln +> genExpr e2)
    /// A generalization of IfThenElse
    | ElIf((e1,e2)::es, en) ->
        atCurrentColumn (!- "if " +> genExpr e1 
        ++ "then " +> autoBreakNln e2
        +> col sepNone es (fun (e1, e2) -> !+ "elif " +> genExpr e1 ++ "then " +> autoBreakNln e2)
        ++ "else " +> autoBreakNln en)
    | IfThenElse(e1, e2, None) -> 
        atCurrentColumn (!- "if " +> genExpr e1 ++ "then " +> autoBreakNln e2)
    /// At this stage, all symbolic operators have been handled.
    | Var(OpNamePrefix s) -> !- s
    | LongIdentSet(s, e) -> !- (sprintf "%s <- " s) +> genExpr e
    | DotIndexedGet(e, es) -> genExpr e -- "." +> sepOpenL +> genIndexedVars es +> sepCloseL
    | DotIndexedSet(e1, es, e2) -> genExpr e1 -- ".[" +> genIndexedVars es -- "] <- " +> genExpr e2
    | DotGet(e, s) -> genExpr e -- sprintf ".%s" s
    | DotSet(e1, s, e2) -> genExpr e1 -- sprintf ".%s <- " s +> genExpr e2
    | TraitCall(tps, msg, e) -> 
        sepOpenT +> genTyparList tps +> sepColon +> genMemberSig msg +> sepSpace +> genExpr e +> sepCloseT
    | LetOrUseBang(isUse, p, e1, e2) ->
        atCurrentColumn (ifElse isUse (!- "use! ") (!- "let! ") 
        +> genPat p -- " = " +> genExpr e1 +> sepNln +> genExpr e2)
    | e -> failwithf "Unexpected expression: %O" e

/// Use in indexed set and get only
and genIndexedVars es =
    match es with
    | IndexedVar eo1 :: es ->
        match es with
        | [IndexedVar eo2] -> 
            opt sepNone eo1 genExpr -- ".." +> opt sepNone eo2 genExpr
        | IndexedVar eo2 :: es -> 
            opt sepNone eo1 genExpr -- ".." +> opt sepNone eo2 genExpr 
            +> sepComma +> genIndexedVars es
        | _ -> 
            opt sepNone eo1 genExpr +> sepComma +> genIndexedVars es
    | [e] -> genExpr e
    | e :: es -> genExpr e +> sepComma +> genIndexedVars es
    | [] -> sepNone

and genTypeDefn isFirst (TypeDef(ats, px, ao, tds, tcs, tdr, ms, li)) = 
    let typeName = 
        genPreXmlDoc px 
        +> ifElse isFirst (colPost sepNln sepNln ats genAttribute -- "type ") 
            (!- "and " +> colPost sepSpace sepNone ats genAttribute) 
        +> opt sepSpace ao genAccess -- li
        +> genTypeParam tds tcs
    match tdr with
    | Simple(TDSREnum ecs) ->
        typeName +> sepEq 
        +> indent +> sepNln
        +> col sepNln ecs (genEnumCase true)
        +> colPre sepNln sepNln ms (genMemberDefn false)
        /// Add newline after un-indent to be spacing-correct
        +> unindent +> sepNln 
    | Simple(TDSRUnion(ao', xs)) ->
        typeName +> sepEq 
        +> indent +> sepNln +> opt sepNln ao' genAccess 
        +> col sepNln xs (genUnionCase true)
        +> colPre sepNln sepNln ms (genMemberDefn false)
        +> unindent +> sepNln
    | Simple(TDSRRecord(ao', fs)) ->
        typeName +> sepEq 
        +> indent +> sepNln +> opt sepNln ao' genAccess +> sepOpenS 
        +> atCurrentColumn (col sepSemiNln fs (genField "")) +> sepCloseS
        +> colPre sepNln sepNln ms (genMemberDefn false) 
        +> unindent +> sepNln 
    | Simple TDSRNone -> 
        typeName +> sepNln
    | Simple(TDSRTypeAbbrev t) -> 
        typeName +> sepEq +> genType t +> sepNln
    /// What is this case?
    | Simple TDSRGeneral -> id
    | ObjectModel(TCSimple (TCStruct | TCInterface | TCClass) as tdk, mds) ->
        typeName +> sepEq +> indent +> sepNln +> genTypeDefKind tdk
        +> indent +> colPre sepNln sepNln mds (genMemberDefn false) +> unindent
        ++ "end" +> unindent +> sepNln
    | ObjectModel(TCSimple TCAugmentation, _) ->
        typeName -- " with" +> indent +> sepNln 
        /// Remember that we use MemberDefn of parent node
        +> col sepNln ms (genMemberDefn false) +> unindent +> sepNln
    | ObjectModel(TCDelegate(t, ValInfo(aiss, _)), _) ->
        typeName +> sepEq -- "delegate of " +> genTypeGroup (aiss.Head.Length = 1) t
    | ObjectModel(tdk, mds) -> 
        /// Assume that there is at most one implicit constructor
        let impCtor = List.tryFind (function MDImplicitCtor _ -> true | _ -> false) mds
        /// Might need to sort so that let and do bindings come first
        let others =  List.filter (function MDImplicitCtor _ -> false | _ -> true) mds
        typeName +> opt sepNone impCtor (genMemberDefn false) +> sepEq +> indent +> sepNln 
        +> col sepNln others (genMemberDefn false) +> unindent +> sepNln

and genTyparDecl(TyparDecl(ats, tp)) = colPost sepSpace sepNone ats genAttribute +> genTypar tp

and genTypeDefKind = function
    | TCSimple TCUnspecified -> id
    | TCSimple TCClass -> !- "class"
    | TCSimple TCInterface -> !- "interface"
    | TCSimple TCStruct -> !- "struct"
    | TCSimple TCRecord -> id
    | TCSimple TCUnion -> id
    | TCSimple TCAbbrev -> id
    | TCSimple TCHiddenRepr -> id
    | TCSimple TCAugmentation -> sepNone
    | TCSimple TCILAssemblyCode -> id
    | TCDelegate(t, vi) -> id

and genException(ExceptionDef(ats, px, ao, uc, ms)) = 
    genPreXmlDoc px
    +> colPost sepNln sepNone ats genAttribute  -- "exception " 
    +> opt sepSpace ao genAccess +> genUnionCase false uc
    +> sepNln +> colPost sepNln sepNln ms (genMemberDefn false)

and genUnionCase hasBar (UnionCase(ats, px, _, s, UnionCaseType fs)) =
    genPreXmlDoc px
    +> ifElse hasBar sepBar sepNone -- s 
    +> colPre wordOf sepStar fs (genField "")

and genEnumCase hasBar (EnumCase(ats, px, s, c)) =
    genPreXmlDoc px +> ifElse hasBar sepBar sepNone 
    +> colPost sepSpace sepNone ats genAttribute -- s +> sepEq +> genConst c

and genField prefix (Field(ats, px, ao, isStatic, isMutable, t, so)) = 
    genPreXmlDoc px 
    +> colPost sepSpace sepNone ats genAttribute -- prefix
    +> opt sepSpace ao genAccess +> ifElse isStatic (!- "static ") sepNone
    +> ifElse isMutable (!- "mutable ") sepNone +> opt sepColon so (!-) +> genTypeGroup true t

and genType = function
    | THashConstraint t -> !- "#" +> genType t
    | TMeasurePower(t, n) -> genType t -- "^" +> str n
    | TMeasureDivide(t1, t2) -> genType t1 -- " / " +> genType t2
    | TStaticConstant(c) -> genConst c
    | TStaticConstantExpr(e) -> genExpr e
    /// Not sure about this case
    | TStaticConstantNamed(t1, t2) -> genType t1 -- "=" +> genType t2
    | TArray(t, n) -> genType t +> rep n (!- "[]")
    | TAnon -> sepWild
    | TVar tp -> genTypar tp 
    // TFun is left associated
    | TFun(t1, (TFun _ as t2)) -> genType t1 +> sepArrow +> sepOpenT +> genType t2 +> sepCloseT
    | TFun(t1, t2) -> genType t1 +> sepArrow +> genType t2
    | TApp(t, ts, isPostfix) -> 
        let postForm = 
            match ts with
            | [] -> invalidArg "ts" "List of types should not be empty"
            | [t'] -> genType t' +> sepSpace +> genType t
            | ts -> sepOpenT +> col sepComma ts genType -- ") " +> genType t
        ifElse isPostfix postForm (genType t -- "<" +> col sepComma ts genType -- ">")
    | TLongIdentApp(t, li, ts) -> genType t -- li -- "<" +> col sepComma ts genType -- ">"
    /// The surrounding brackets don't seem neccessary
    | TTuple ts -> col sepStar ts (snd >> genTypeGroup true)
    /// Revise this case later
    | TWithGlobalConstraints(t, tcs) -> genType t +> colPre (!- " when ") wordAnd tcs genTypeConstraint
    | TLongIdent li -> !- li
    | t -> failwithf "Unexpected type: %O" t

and genTypeGroup isGroup = function
    | TTuple ts -> 
        // Inner parts should have brackets for separation
        ifElse isGroup (sepOpenT +> col sepStar ts (snd >> genTypeGroup true) +> sepCloseT)
            (col sepStar ts (snd >> genTypeGroup true))
    | TFun(t1, (TFun _ as t2)) -> 
        genTypeGroup isGroup t1 +> sepArrow +> sepOpenT +> genTypeGroup isGroup t2 +> sepCloseT
    | TFun(t1, t2) -> genTypeGroup isGroup t1 +> sepArrow +> genTypeGroup isGroup t2
    | t -> genType t


and genTypar(Typar(s, isHead)) = 
    /// There is a potential parser bug with "<^T..."
    ifElse isHead (!- "^") (!-"'") -- s

and genTypeConstraint = function
    | TyparSingle(kind, tp) -> genTypar tp +> sepColon -- sprintf "%O" kind
    | TyparDefaultsToType(tp, t) -> !- "default " +> genTypar tp +> sepColon +> genType t
    | TyparSubtypeOfType(tp, t) -> genTypar tp -- " :> " +> genType t
    | TyparSupportsMember(tps, msg) -> genTyparList tps +> sepColon +> genMemberSig msg
    | TyparIsEnum(tp, ts) -> 
        genTypar tp +> sepColon -- "enum<" +> col sepComma ts genType -- ">"
    | TyparIsDelegate(tp, ts) ->
        genTypar tp +> sepColon -- "delegate<" +> col sepComma ts genType -- ">"

and genInterfaceImpl(InterfaceImpl(t, bs)) = 
    !- "interface " +> genType t -- " with"
    +> indent +> sepNln +> col sepNln bs (genMemberBinding true) +> unindent

and genClause(Clause(p, e, eo)) = 
    sepBar +> genPat p +> optPre (!- " when ") sepNone eo genExpr +> sepArrow +> autoBreakNln e

and genMemberSig = function
    | MSMember(ValSig(ats, px, ao, s, t, ValTyparDecls(_, _, tcs)), mf) -> 
        sepOpenT +> genMemberFlags false mf -- s +> sepColon +> genType t +> sepCloseT
    | MSInterface t -> id
    | MSInherit t -> id
    | MSValField f -> id
    | MSNestedType tds -> id

and genMemberDefn isInterface = function
    | MDNestedType(td, ao) -> invalidArg "md" "This functionality is not implemented in F# compiler"
    | MDOpen(s) -> !- s
    /// What is the role of so
    | MDImplicitInherit(t, e, so) -> !- "inherit " +> genType t +> genExpr e
    | MDInherit(t, so) -> !- "inherit " +> genType t
    | MDValField f -> genField "val " f
    | MDImplicitCtor(ats, ao, ps, so) -> 
        optPre sepSpace sepSpace ao genAccess +> sepOpenT
        +> colPost sepSpace sepNone ats genAttribute +> col sepComma ps genSimplePat +> sepCloseT
        +> optPre (!- " as ") sepNone so (!-)
    | MDMember(b) -> genMemberBinding isInterface b
    | MDLetBindings(isStatic, isRec, bs) ->
        let prefix = 
            if isStatic && isRec then "static let rec "
            elif isStatic then "static let "
            elif isRec then "let rec "
            else "let "
        col sepNln bs (genLetBinding prefix)
    | MDInterface(t, mdo) -> 
        !- "interface " +> genType t -- " with" 
        +> indent +> sepNln +> opt sepNone mdo (fun mds -> col sepNln mds (genMemberDefn true)) +> unindent
    | MDAutoProperty(ats, px, ao, mk, e, s) -> 
        genPreXmlDoc px
        +> colPost sepSpace sepNone ats genAttribute -- "member val " 
        +> opt sepSpace ao genAccess -- s +> sepEq +> genExpr e -- propertyKind mk
    | MDAbstractSlot(ats, px, ao, s, t, ValTyparDecls(tds, _, tcs), MFMemberFlags mk) ->
        genPreXmlDoc px 
        +> colPost sepSpace sepNone ats genAttribute
        +> opt sepSpace ao genAccess -- sprintf "abstract %s" s
        +> genTypeParam tds tcs
        +> sepColon +> genType t -- propertyKind mk
    | md -> failwithf "Unexpected member definition: %O" md

and propertyKind = function
    | PropertyGet -> " with get"
    | PropertySet -> " with set"
    | PropertyGetSet -> " with get, set"
    | _ -> ""

and genSimplePat = function
    | SPatId(s, isOptArg) -> ifElse isOptArg (!- (sprintf "?%s" s)) (!- s)
    | SPatTyped(sp, t) -> genSimplePat sp -- " : " +> genType t
    | SPatAttrib(ats, sp) -> colPost sepSpace sepNone ats genAttribute +> genSimplePat sp
    
and genSimplePats = function
    /// Remove parentheses on an extremely simple pattern
    | SimplePats [SPatId _ as sp] -> genSimplePat sp
    | SimplePats ps -> sepOpenT +> col sepComma ps genSimplePat +> sepCloseT
    | SPSTyped(ps, t) -> genSimplePats ps -- " : " +> genType t

and inline genPatRecordFieldName(PatRecordFieldName(s1, s2, p)) =
    ifElse (s1 = "") (!- (sprintf "%s = " s2)) (!- (sprintf "%s.%s = " s1 s2)) +> genPat p

and genPat = function
    | PatOptionalVal(s) -> !- (sprintf "?%s" s)
    | PatAttrib(p, ats) -> colPost sepSpace sepNone ats genAttribute +> genPat p
    | PatOr(p1, p2) -> genPat p1 -- " | " +> genPat p2
    | PatAnds(ps) -> col (!- " & ") ps genPat
    | PatNullary PatNull -> !- "null"
    | PatNullary PatWild -> sepWild
    | PatTyped(p, t) -> genPat p +> sepColon +> genType t
    | PatNamed(ao, PatNullary PatWild, s) ->  opt sepSpace ao genAccess -- s
    | PatNamed(ao, p, s) ->  opt sepSpace ao genAccess +> genPat p -- sprintf " as %s" s 
    | PatLongIdent(ao, li, ps, tpso) -> 
        let aoc = opt sepSpace ao genAccess
        let tpsoc = 
            opt sepNone tpso (fun (ValTyparDecls(tds, _, tcs)) -> genTypeParam tds tcs)
        match ps with
        | [] ->  aoc -- li +> tpsoc
        | [PatSeq(PatTuple, [p1; p2])] when li = "::" -> aoc +> genPat p1 -- " :: " +> genPat p2
        | [p] -> aoc -- li +> tpsoc +> ifElse (hasParenInPat p) (genPat p) (sepSpace +> genPat p)
        | ps -> aoc -- li +> tpsoc +> sepSpace +> col sepSpace ps genPat
    | PatParen(PatConst(c)) -> genConst c
    | PatParen(p) -> sepOpenT +> genPat p +> sepCloseT
    | PatSeq(PatTuple, ps) -> col sepComma ps genPat
    | PatSeq(PatList, ps) -> sepOpenL +> atCurrentColumn (col sepSemi ps genPat) +> sepCloseL
    | PatSeq(PatArray, ps) -> sepOpenA +> atCurrentColumn (col sepSemi ps genPat) +> sepCloseA
    | PatRecord(xs) -> 
        sepOpenS +> atCurrentColumn (col sepSemi xs genPatRecordFieldName) +> sepCloseS
    | PatConst(c) -> genConst c
    | PatIsInst(t) -> !- ":? " +> genType t
    // What about "<@@ " ?
    | PatQuoteExpr e -> !- "<@ " +> genExpr e -- " @>"
    | p -> failwithf "Unexpected pattern: %O" p
