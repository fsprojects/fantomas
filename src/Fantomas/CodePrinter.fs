module internal Fantomas.CodePrinter

open System

open Fantomas.FormatConfig
open Fantomas.SourceParser

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
    | DesugaredMatch(_, e) -> multiline e
    | Lambda(e, _) -> multiline e
    | MatchLambda _ -> true
    | Match(e, cs) -> multiline e || not (List.isEmpty cs)
    /// An infix app is multiline if it contains at least two new line infix ops
    | InfixApps(e, es) -> 
        multiline e || not (List.atmostOne (List.filter (fst >> NewLineInfixOps.Contains) es)) 
                    || List.exists (snd >> multiline) es
    | App(e1, es) -> multiline e1 || List.exists multiline es
    | TypeApp(e, _) -> multiline e
    | LetOrUse(_, _, bs, e) -> not (List.isEmpty bs) || multiline e
    | SequentialSimple _ -> false
    | TryWith _ | TryFinally _ ->  true
    | Sequentials _ -> true
    | IfThenElse _ -> true
    | LongIdentSet(_, e) -> multiline e
    | DotIndexedGet(e, es) -> multiline e || List.exists multiline es
    | DotIndexedSet(e1, es, e2) -> multiline e1 || multiline e2 || List.exists multiline es
    | DotGet(e, _) -> multiline e
    | DotSet(e1, _, e2) -> multiline e1 || multiline e2
    | TraitCall(_, _, e) -> multiline e
    | LetOrUseBang(_, _, e1, e2) -> multiline e1 || multiline e2
    /// Default mode is single-line
    | _ -> false

/// Check if the expression already has surrounding parentheses
let hasParenthesis = function
    | Paren _ | ConstExpr(Const "()") | Tuple _ -> true
    | _ -> false

let hasParenInPat = function
    | PatParen _ | PatConst(Const "()") -> true
    | _ -> false

let inline genConst c =
    match c with
    | Const c -> !- c
    | Unresolved c -> fun ctx -> str (content c ctx) ctx

/// Group similar operations into a batch for processing
let rec (|SingleModuleDecls|ComplexModuleDecls|Empty|) xs =
    match xs with
    | [Attributes _ | DoExpr _ | HashDirective _ | ModuleAbbrev _ | Open _ as x] ->
        SingleModuleDecls([x], [])
    | (Attributes _ | DoExpr _ | HashDirective _ | ModuleAbbrev _ | Open _ as x)::SingleModuleDecls(ys, zs) ->
        SingleModuleDecls(x::ys, zs)
    | (Attributes _ | DoExpr _ | HashDirective _ | ModuleAbbrev _ | Open _ as x)::xs' ->
        SingleModuleDecls([x], xs')
    | [x] ->
        ComplexModuleDecls([x], [])
    | x::ComplexModuleDecls(ys, zs) ->
        ComplexModuleDecls(x::ys, zs)
    | x::xs' ->
        ComplexModuleDecls([x], xs')
    | _ -> Empty

let rec genParsedInput = function
    | ImplFile im -> genImpFile im
    | SigFile si -> genSigFile si

and genImpFile(ParsedImplFileInput(hs, mns)) = 
    col sepNln hs genParsedHashDirective
    +> col sepNln mns genModuleOrNamespace

and genSigFile(ParsedSigFileInput(hs, mns)) =
    col sepNln hs genParsedHashDirective
    +> col sepNln mns genSigModuleOrNamespace

and genParsedHashDirective(ParsedHashDirective(s1, s2)) =
    /// print strings with quotes
    !- "#" -- s1 +> sepSpace +> ifElse (s2 = "") sepNone (!- (sprintf "\"%O\"" s2))

and genModuleOrNamespace = function
    | ModuleOrNamespace(ats, px, ao, s, mds, isModule) -> 
        genPreXmlDoc px
        +> colPost sepNln sepNln ats genAttribute
        /// Checking for Tmp is a bit fragile
        +> ifElse (s = "Tmp") sepNone (ifElse isModule (!- "module ") (!- "namespace ")
        +> opt sepSpace ao genAccess -- s +> rep 2 sepNln)
        +> genModuleDeclGroup mds

and genSigModuleOrNamespace = function
    | SigModuleOrNamespace(ats, px, ao, s, mds, isModule) -> 
        genPreXmlDoc px
        +> colPost sepNln sepNln ats genAttribute
        +> ifElse (s = "Tmp") sepNone (ifElse isModule (!- "module ") (!- "namespace ")
        +> opt sepSpace ao genAccess -- s +> rep 2 sepNln)
        +> col sepNln mds genSigModuleDecl

and genModuleDeclGroup = function
    | ComplexModuleDecls(ys, Empty) ->
        col sepNln ys genModuleDecl
    | ComplexModuleDecls(ys, zs) ->
        col sepNln ys genModuleDecl +> sepNln +> genModuleDeclGroup zs
    | SingleModuleDecls(ys, Empty) ->
        col sepNone ys genModuleDecl 
    | SingleModuleDecls(ys, zs) ->
        col sepNone ys genModuleDecl +> sepNln +> genModuleDeclGroup zs
    | Empty -> sepNone    

and genModuleDecl = function
    | Attributes(ats) -> col sepNln ats genAttribute +> sepNln
    | DoExpr(e) -> genExpr e +> sepNln
    | Exception(ex) -> genException ex +> sepNln
    | HashDirective(p) -> 
        genParsedHashDirective p +> sepNln
    /// Add a new line after module-level let bindings
    | Let(b) -> genLetBinding "let " b +> sepNln
    | LetRec(b::bs) -> 
        genLetBinding "let rec " b 
        +> colPre (rep 2 sepNln) (rep 2 sepNln) bs (genLetBinding "and ") +> sepNln
    | ModuleAbbrev(s1, s2) -> !- "module " -- s1 +> sepEq -- s2 +> sepNln
    | NamespaceFragment(m) -> failwithf "NamespaceFragment is not supported yet: %O" m
    | NestedModule(ats, px, ao, s, mds) -> 
        genPreXmlDoc px
        +> colPost sepNln sepNln ats genAttribute -- "module " +> opt sepSpace ao genAccess -- s +> sepEq
        +> indent +> sepNln +> genModuleDeclGroup mds +> unindent
    | Open(s) -> !- (sprintf "open %s" s) +> sepNln
    /// There is no nested types and they have newlines in the ends of definitions
    | Types(t::ts) -> genTypeDefn true t +> colPre sepNln sepNln ts (genTypeDefn false)
    | md -> failwithf "Unexpected module declaration: %O" md

and genSigModuleDecl = function
    | SigException(ex) -> genSigException ex +> sepNln
    | SigHashDirective(p) -> 
        genParsedHashDirective p +> sepNln
    | SigVal(v) -> genVal v +> sepNln
    | SigModuleAbbrev(s1, s2) -> !- "module " -- s1 +> sepEq -- s2 +> sepNln
    | SigNamespaceFragment(m) -> failwithf "NamespaceFragment is not supported yet: %O" m
    | SigNestedModule(ats, px, ao, s, mds) -> 
        genPreXmlDoc px
        +> colPost sepNln sepNln ats genAttribute -- "module " +> opt sepSpace ao genAccess -- s +> sepEq
        +> indent +> sepNln +> col sepNln mds genSigModuleDecl +> unindent
    | SigOpen(s) -> !- (sprintf "open %s" s) +> sepNln
    | SigTypes(t::ts) -> genSigTypeDefn true t +> colPre sepNln sepNln ts (genSigTypeDefn false)
    | md -> failwithf "Unexpected module signature declaration: %O" md

and genAccess(Access s) = !- s

and genAttribute(Attribute(s, e, _)) = 
    match e with
    /// Special treatment for function application on attributes
    | ConstExpr(Const "()") -> !- (sprintf "[<%s>]" s)
    | e -> !- "[<" -- s +> genExpr e -- ">]"
    
and genPreXmlDoc(PreXmlDoc lines) = 
    colPost sepNln sepNln lines (sprintf "///%s" >> (!-))

/// These inline functions have to be defined before their uses

and inline autoBreakNln e = 
    ifElse (multiline e) (indent +> sepNln +> genExpr e +> unindent) 
        (indent +> autoNln (genExpr e) +> unindent)

and inline genTyparList tps = 
    ifElse (List.atmostOne tps) (col wordOr tps genTypar) (sepOpenT +> col wordOr tps genTypar +> sepCloseT)

and inline genTypeParam tds tcs =
    ifElse (List.isEmpty tds) sepNone
        (!- "<" +> col sepComma tds genTyparDecl +> colPre (!- " when ") wordAnd tcs genTypeConstraint -- ">")

and genLetBinding pref = function
    | LetBinding(ats, px, ao, isInline, isMutable, p, e) ->
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
        +> colPost sepNln sepNone ats genAttribute -- prefix +> autoBreakNln e
    | b -> failwithf "%O isn't a let binding" b

and genMemberBinding isInterface = function
    | PropertyBinding(ats, px, ao, isInline, mf, p, e) -> 
        let prefix =
            genPreXmlDoc px
            +> colPost sepSpace sepNone ats genAttribute +> genMemberFlags isInterface mf
            +> ifElse isInline (!- "inline ") sepNone +> opt sepSpace ao genAccess
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
                +> genPat p +> sepEq +> autoBreakNln e
            | PatLongIdent(_, s, ps, _) -> 
                prefix -- s -- " with get " 
                +> col sepSpace ps genPat +> sepEq +> autoBreakNln e
            | p -> failwithf "Unexpected pattern: %O" p
        | MFProperty PropertySet -> 
            match p with
            | PatLongIdent(_, s, [PatSeq(PatTuple, ps)], _) -> 
                let (ps, p) = tuplerize ps
                prefix -- s -- " with set " 
                +> ifElse (List.atmostOne ps) (col sepComma ps genPat +> sepSpace) 
                    (sepOpenT +> col sepComma ps genPat +> sepCloseT +> sepSpace)
                +> genPat p +> sepEq +> autoBreakNln e
            | PatLongIdent(_, s, ps, _) -> 
                prefix -- s -- " with set " +> col sepSpace ps genPat
                +> sepEq +> autoBreakNln e
            | p -> failwithf "Unexpected pattern: %O" p
        | mf -> failwithf "Unexpected member flags: %O" mf
    | MemberBinding(ats, px, ao, isInline, mf, p, e) ->
        let prefix =
            genPreXmlDoc px
            +> colPost sepSpace sepNone ats genAttribute +> genMemberFlags isInterface mf
            +> ifElse isInline (!- "inline ") sepNone +> opt sepSpace ao genAccess +> genPat p
        match e with
        | TypedExpr(Typed, e, t) -> prefix +> sepColon +> genType t +> sepEq +> autoBreakNln e
        | e -> prefix +> sepEq +> autoBreakNln e
    | ExplicitCtor(ats, px, ao, p, e) ->
        let prefix =
            genPreXmlDoc px
            +> colPost sepSpace sepNone ats genAttribute
            +> opt sepSpace ao genAccess +> genPat p
        match e with
        /// Handle special "then" block in constructors
        | Sequentials [e1; e2] -> 
            prefix +> sepEq +> indent +> sepNln +> genExpr e1 ++ "then " +> autoBreakNln e2 +> unindent
        | e -> prefix +> sepEq +> autoBreakNln e
    | b -> failwithf "%O isn't a member binding" b

and genMemberFlags isInterface = function
    | MFMember _ -> !- "member "
    | MFStaticMember _ -> !- "static member "
    | MFConstructor _ -> sepNone
    | MFOverride _ -> ifElse isInterface (!- "member ") (!- "override ")

and genVal (Val(ats, px, ao, s, t, vi, _)) = 
    let (FunType ts) = (t, vi)
    genPreXmlDoc px
    +> colPost sepNln sepNone ats genAttribute 
    +> atCurrentColumn (indent -- "val " +> opt sepSpace ao genAccess -- s +> sepColon +> genTypeList ts +> unindent)

and inline genRecordFieldName(RecordFieldName(s, eo)) =
    opt sepNone eo (fun e -> !- s +> sepEq +> autoBreakNln e)

and genExpr = function
    | Paren(ConstExpr(Const "()")) -> !- "()"
    | Paren e -> sepOpenT +> genExpr e +> sepCloseT
    | SingleExpr(kind, e) -> str kind +> genExpr e
    | ConstExpr(c) -> genConst c
    | NullExpr -> !- "null"
    /// Not sure about the role of e1
    | Quote(_, e2, isRaw) ->         
        let e = genExpr e2
        ifElse isRaw (!- "<@@ " +> e -- " @@>") (!- "<@ " +> e -- " @>")
    | TypedExpr(TypeTest, e, t) -> genExpr e -- " :? " +> genType t
    | TypedExpr(New, e, t) -> 
        !- "new " +> genType t +> ifElse (hasParenthesis e) (sepBeforeArg +> genExpr e) (sepSpace +> genExpr e)
    | TypedExpr(Downcast, e, t) -> genExpr e -- " :?> " +> genType t
    | TypedExpr(Upcast, e, t) -> genExpr e -- " :> " +> genType t
    | TypedExpr(Typed, e, t) -> genExpr e +> sepColon +> genType t
    | Tuple es -> atCurrentColumn (col sepComma es (autoNln << genExpr))
    | ArrayOrList(isArray, xs) -> 
        ifElse isArray (sepOpenA +> atCurrentColumn (col sepSemi xs (autoNln << genExpr)) +> sepCloseA) 
            (sepOpenL +> atCurrentColumn (col sepSemi xs (autoNln << genExpr)) +> sepCloseL)
    | Record(xs, eo) -> 
        sepOpenS +> opt (!- " with ") eo genExpr
        +> atCurrentColumn (col sepSemi xs (autoNln << genRecordFieldName))
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
    /// Handle the form 'for i in e1 -> e2'
    | ForEach(p, e1, e2, isArrow) ->
        atCurrentColumn (!- "for " +> genPat p -- " in " +> genExpr e1 
            +> ifElse isArrow (sepArrow +> autoBreakNln e2) (!- " do" +> indent +> sepNln +> genExpr e2 +> unindent))
    | CompExpr(isArrayOrList, e) ->
        ifElse isArrayOrList (genExpr e) (autoBreakNln e) 
    | ArrayOrListOfSeqExpr(isArray, e) -> 
        ifElse isArray (sepOpenA +> genExpr e +> sepCloseA) (sepOpenL +> genExpr e +> sepCloseL)
    | JoinIn(e1, e2) -> genExpr e1 -- " in " +> genExpr e2
    | DesugaredMatch(_, e) -> genExpr e
    | Lambda(e, sps) -> 
        !- "fun " +> col sepSpace sps genSimplePats +> sepArrow +> autoBreakNln e
    | MatchLambda(sp, _) -> atCurrentColumn (!- "function " +> colPre sepNln sepNln sp genClause)
    | Match(e, cs) -> 
        atCurrentColumn (!- "match " +> genExpr e -- " with" +> colPre sepNln sepNln cs genClause)
    | CompApp(s, e) ->
        !- s +> sepSpace +> sepOpenS +> genExpr e +> sepCloseS
    | App(Var(OpName ".. .."), [e1; e2; e3]) -> genExpr e1 -- ".." +> genExpr e2 -- ".." +> genExpr e3
    /// Separate two prefix ops by spaces
    | PrefixApp(s1, PrefixApp(s2, e)) -> !- (sprintf "%s %s" s1 s2) +> genExpr e
    | PrefixApp(s, e) -> !- s  +> genExpr e
    /// Handle spaces of infix application based on which category it belongs to
    | InfixApps(e, es) -> 
        /// Only put |> on the same line in a very trivial expression
        let hasNewLine = multiline e || not (List.atmostOne es)
        atCurrentColumn (genExpr e +> genInfixApps hasNewLine es)
    /// Unlike infix app, function application needs a level of indentation
    | App(e1, [e2]) -> 
        atCurrentColumn (genExpr e1 +> 
            ifElse (hasParenthesis e2) (sepBeforeArg +> indent +> autoNln (genExpr e2) +> unindent) 
                (sepSpace +> indent +> autoNln (genExpr e2) +> unindent))
    /// Always spacing in multiple arguments
    | App(e, es) -> 
        atCurrentColumn 
            (genExpr e +> colPre sepSpace sepSpace es (fun e -> indent +> autoNln (genExpr e) +> unindent))
    | TypeApp(e, ts) -> genExpr e -- "<" +> col sepComma ts genType -- ">"
    | LetOrUse(isRec, isUse, bs, e) ->
        let prefix = 
            if isUse then "use "
            elif isRec then "let rec "
            else "let "
        match bs with
        | b::bs ->
            /// and is applicable for use binding
            atCurrentColumn (genLetBinding prefix b +> 
                colPre sepNln sepNln bs (genLetBinding "and ") +> sepNln +> genExpr e)
        | _ -> atCurrentColumn (col sepNln bs (genLetBinding prefix) +> sepNln +> genExpr e)
    /// Could customize a bit if e is single line
    | TryWith(e, cs) ->  
        atCurrentColumn (!- "try " +> indent +> sepNln +> genExpr e +> unindent ++ "with" 
            +> indentOnWith +> sepNln +> col sepNln cs genClause +> unindentOnWith)
    | TryFinally(e1, e2) -> 
        atCurrentColumn (!- "try " +> indent +> sepNln +> genExpr e1 +> unindent ++ "finally" 
            +> indent +> sepNln +> genExpr e2 +> unindent)    
    | SequentialSimple es -> atCurrentColumn (col sepSemi es (autoNln << genExpr))
    /// It seems too annoying to use sepSemiNln
    | Sequentials es -> 
        atCurrentColumn (col sepNln es genExpr)
    /// A generalization of IfThenElse
    | ElIf((e1,e2)::es, en) ->
        atCurrentColumn (!- "if " +> genExpr e1 
            ++ "then " +> autoBreakNln e2
            +> col sepNone es (fun (e1, e2) -> !+ "elif " +> genExpr e1 ++ "then " +> autoBreakNln e2)
            ++ "else " +> autoBreakNln en)
    | IfThenElse(e1, e2, None) -> 
        atCurrentColumn (!- "if " +> genExpr e1 ++ "then " +> autoBreakNln e2)
    /// At this stage, all symbolic operators have been handled.
    | Var(OpNameFull s) -> !- s
    | LongIdentSet(s, e) -> !- (sprintf "%s <- " s) +> genExpr e
    | DotIndexedGet(e, es) -> genExpr e -- "." +> sepOpenL +> genIndexedVars es +> sepCloseL
    | DotIndexedSet(e1, es, e2) -> genExpr e1 -- ".[" +> genIndexedVars es -- "] <- " +> genExpr e2
    | DotGet(e, s) -> genExpr e -- sprintf ".%s" s
    | DotSet(e1, s, e2) -> genExpr e1 -- sprintf ".%s <- " s +> genExpr e2
    | TraitCall(tps, msg, e) -> 
        sepOpenT +> genTyparList tps +> sepColon +> sepOpenT +> genMemberSig msg +> sepCloseT 
        +> sepSpace +> genExpr e +> sepCloseT
    | LetOrUseBang(isUse, p, e1, e2) ->
        atCurrentColumn (ifElse isUse (!- "use! ") (!- "let! ") 
            +> genPat p -- " = " +> genExpr e1 +> sepNln +> genExpr e2)
    | e -> failwithf "Unexpected expression: %O" e

and genInfixApps newline = function
    | (s, e)::es ->
        (ifElse (newline && NewLineInfixOps.Contains s) (sepNln -- s +> sepSpace +> genExpr e)
           (ifElse (NoSpaceInfixOps.Contains s) (!- s +> autoNln (genExpr e))
              (sepSpace +> autoNln (!- s +> sepSpace +> genExpr e))))
        +> genInfixApps newline es
    | [] -> sepNone

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

and genTypeDefn isFirst (TypeDef(ats, px, ao, tds, tcs, tdr, ms, s)) = 
    let typeName = 
        genPreXmlDoc px 
        +> ifElse isFirst (colPost sepNln sepNln ats genAttribute -- "type ") 
            (!- "and " +> colPost sepSpace sepNone ats genAttribute) 
        +> opt sepSpace ao genAccess -- s
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
    | ObjectModel(TCSimple (TCStruct | TCInterface | TCClass) as tdk, MemberDefnList(impCtor, others)) ->
        let isInterface =
            match tdk with
            | TCSimple TCInterface -> true
            | _ -> false
        typeName +> opt sepNone impCtor (genMemberDefn isInterface) +> sepEq 
        +> indent +> sepNln +> genTypeDefKind tdk
        +> indent +> colPre sepNln sepNln others (genMemberDefn isInterface) +> unindent
        ++ "end" +> unindent +> sepNln
    | ObjectModel(TCSimple TCAugmentation, _) ->
        typeName -- " with" +> indent +> sepNln 
        /// Remember that we use MemberDefn of parent node
        +> col sepNln ms (genMemberDefn false) +> unindent +> sepNln
    | ObjectModel(TCDelegate(FunType ts), _) ->
        typeName +> sepEq -- "delegate of " +> genTypeList ts
    | ObjectModel(_, MemberDefnList(impCtor, others)) ->
        typeName +> opt sepNone impCtor (genMemberDefn false) +> sepEq +> indent +> sepNln 
        +> col sepNln others (genMemberDefn false) +> unindent +> sepNln

and genSigTypeDefn isFirst (SigTypeDef(ats, px, ao, tds, tcs, tdr, ms, s)) = 
    let typeName = 
        genPreXmlDoc px 
        +> ifElse isFirst (colPost sepNln sepNln ats genAttribute -- "type ") 
            (!- "and " +> colPost sepSpace sepNone ats genAttribute) 
        +> opt sepSpace ao genAccess -- s
        +> genTypeParam tds tcs
    match tdr with
    | SigSimple(TDSREnum ecs) ->
        typeName +> sepEq 
        +> indent +> sepNln
        +> col sepNln ecs (genEnumCase true)
        +> colPre sepNln sepNln ms genMemberSig
        /// Add newline after un-indent to be spacing-correct
        +> unindent +> sepNln 
    | SigSimple(TDSRUnion(ao', xs)) ->
        typeName +> sepEq 
        +> indent +> sepNln +> opt sepNln ao' genAccess 
        +> col sepNln xs (genUnionCase true)
        +> colPre sepNln sepNln ms genMemberSig
        +> unindent +> sepNln
    | SigSimple(TDSRRecord(ao', fs)) ->
        typeName +> sepEq 
        +> indent +> sepNln +> opt sepNln ao' genAccess +> sepOpenS 
        +> atCurrentColumn (col sepSemiNln fs (genField "")) +> sepCloseS
        +> colPre sepNln sepNln ms genMemberSig
        +> unindent +> sepNln 
    | SigSimple TDSRNone -> 
        typeName +> sepNln
    | SigSimple(TDSRTypeAbbrev t) -> 
        typeName +> sepEq +> genType t +> sepNln
    /// What is this case?
    | SigSimple TDSRGeneral -> id
    | SigObjectModel(TCSimple (TCStruct | TCInterface | TCClass) as tdk, mds) ->
        typeName +> sepEq +> indent +> sepNln +> genTypeDefKind tdk
        +> indent +> colPre sepNln sepNln mds genMemberSig +> unindent
        ++ "end" +> unindent +> sepNln
    | SigObjectModel(TCSimple TCAugmentation, _) ->
        typeName -- " with" +> indent +> sepNln 
        /// Remember that we use MemberSig of parent node
        +> col sepNln ms genMemberSig +> unindent +> sepNln
    | SigObjectModel(TCDelegate(FunType ts), _) ->
        typeName +> sepEq -- "delegate of " +> genTypeList ts
    | SigObjectModel(_, mds) -> 
        typeName +> sepEq +> indent +> sepNln 
        +> col sepNln mds genMemberSig +> unindent +> sepNln

and genMemberSig = function
    | MSMember(Val(ats, px, ao, s, t, vi, _), mf) -> 
        let (FunType ts) = (t, vi)
        genPreXmlDoc px +> colPost sepSpace sepNone ats genAttribute 
        +> atCurrentColumn (indent +> genMemberFlags false mf +> opt sepNone ao genAccess -- s 
                                   +> sepColon +> genTypeList ts +> unindent)
    | MSInterface t -> !- "interface " +> genType t
    | MSInherit t -> !- "inherit " +> genType t
    | MSValField f -> genField "val " f
    | MSNestedType _ -> invalidArg "md" "This is not implemented in F# compiler"

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
    | TCDelegate _ -> sepNone

and genException(ExceptionDef(ats, px, ao, uc, ms)) = 
    genPreXmlDoc px
    +> colPost sepNln sepNone ats genAttribute  -- "exception " 
    +> opt sepSpace ao genAccess +> genUnionCase false uc
    +> colPre sepNln sepNln ms (genMemberDefn false)

and genSigException(SigExceptionDef(ats, px, ao, uc, ms)) = 
    genPreXmlDoc px
    +> colPost sepNln sepNone ats genAttribute  -- "exception " 
    +> opt sepSpace ao genAccess +> genUnionCase false uc
    +> colPre sepNln sepNln ms genMemberSig

and genUnionCase hasBar (UnionCase(_, px, _, s, UnionCaseType fs)) =
    genPreXmlDoc px
    +> ifElse hasBar sepBar sepNone -- s 
    +> colPre wordOf sepStar fs (genField "")

and genEnumCase hasBar (EnumCase(ats, px, s, c)) =
    genPreXmlDoc px +> ifElse hasBar sepBar sepNone 
    +> colPost sepSpace sepNone ats genAttribute -- s +> sepEq +> genConst c

and genField prefix (Field(ats, px, ao, isStatic, isMutable, t, so)) = 
    /// Being protective on union cases of functions 
    let t =
        match t with
        | TFun _ -> sepOpenT +> genComplexType t +> sepCloseT
        | _ -> genComplexType t
    genPreXmlDoc px 
    +> colPost sepSpace sepNone ats genAttribute -- prefix
    +> opt sepSpace ao genAccess +> ifElse isStatic (!- "static ") sepNone
    +> ifElse isMutable (!- "mutable ") sepNone +> opt sepColon so (!-) +> t

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
    | TFun(t1, t2) -> genType t1 +> sepArrow +> genComplexType t2
    | TApp(t, ts, isPostfix) -> 
        let postForm = 
            match ts with
            | [] ->  genType t
            | [t'] -> genType t' +> sepSpace +> genType t
            | ts -> sepOpenT +> col sepComma ts genType -- ") " +> genType t
        ifElse isPostfix postForm (genType t -- "<" +> col sepComma ts genType -- ">")
    | TLongIdentApp(t, li, ts) -> genType t -- li -- "<" +> col sepComma ts genType -- ">"
    /// The surrounding brackets aren't always neccessary
    | TTuple ts -> col sepStar ts genComplexType
    | TWithGlobalConstraints(t, tcs) -> genType t +> colPre (!- " when ") wordAnd tcs genTypeConstraint
    | TLongIdent li -> !- li
    | t -> failwithf "Unexpected type: %O" t

and genComplexType = function
    | TTuple ts -> 
        /// Inner parts should have brackets for separation
        sepOpenT +> col sepStar ts genComplexType +> sepCloseT
    | TFun(t1, t2) -> 
        sepOpenT +> genComplexType t1 +> sepArrow +> genComplexType t2 +> sepCloseT
    | t -> genType t

and genTypeList = function
    | [] -> sepNone
    | (t, [ArgInfo(so, isOpt)])::ts -> 
        let gt =
            match t with
            | TTuple _ | TFun _ ->
                /// Tuple or Fun is grouped by brackets
                sepOpenT +> optPre (ifElse isOpt (!- "?") sepNone) sepColonFixed so (!-) +> genType t +> sepCloseT
            | _ -> opt sepColonFixed so (!-) +> genType t
        gt +> ifElse ts.IsEmpty sepNone (autoNln (sepArrow +> genTypeList ts))
    | (TTuple ts', ais)::ts -> 
        let gt = col sepStar (Seq.zip ais ts') 
                    (fun (ArgInfo(so, isOpt), t) -> optPre (ifElse isOpt (!- "?") sepNone) 
                                                        sepColonFixed so (!-) +> genComplexType t)
        gt +> ifElse ts.IsEmpty sepNone (autoNln (sepArrow +> genTypeList ts))
    | (t, _)::ts -> 
        let gt = genType t
        gt +> ifElse ts.IsEmpty sepNone (autoNln (sepArrow +> genTypeList ts))

and genTypar(Typar(s, isHead)) = 
    /// There is a potential parser bug with "<^T..."
    ifElse isHead (!- "^") (!-"'") -- s

and genTypeConstraint = function
    | TyparSingle(kind, tp) -> genTypar tp +> sepColon -- sprintf "%O" kind
    | TyparDefaultsToType(tp, t) -> !- "default " +> genTypar tp +> sepColon +> genType t
    | TyparSubtypeOfType(tp, t) -> genTypar tp -- " :> " +> genType t
    | TyparSupportsMember(tps, msg) -> 
        genTyparList tps +> sepColon +> sepOpenT +> genMemberSig msg +> sepCloseT
    | TyparIsEnum(tp, ts) -> 
        genTypar tp +> sepColon -- "enum<" +> col sepComma ts genType -- ">"
    | TyparIsDelegate(tp, ts) ->
        genTypar tp +> sepColon -- "delegate<" +> col sepComma ts genType -- ">"

and genInterfaceImpl(InterfaceImpl(t, bs)) = 
    !- "interface " +> genType t -- " with"
    +> indent +> sepNln +> col sepNln bs (genMemberBinding true) +> unindent

and genClause(Clause(p, e, eo)) = 
    sepBar +> genPat p +> optPre (!- " when ") sepNone eo genExpr +> sepArrow +> autoBreakNln e

and genMemberDefn isInterface = function
    | MDNestedType _ -> invalidArg "md" "This is not implemented in F# compiler"
    | MDOpen(s) -> !- s
    /// What is the role of so
    | MDImplicitInherit(t, e, _) -> !- "inherit " +> genType t +> genExpr e
    | MDInherit(t, _) -> !- "inherit " +> genType t
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
    | SPId(s, isOptArg, _) -> ifElse isOptArg (!- (sprintf "?%s" s)) (!- s)
    | SPTyped(sp, t) -> genSimplePat sp -- " : " +> genType t
    | SPAttrib(ats, sp) -> colPost sepSpace sepNone ats genAttribute +> genSimplePat sp
    
and genSimplePats = function
    /// Remove parentheses on an extremely simple pattern
    | SimplePats [SPId _ as sp] -> genSimplePat sp
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
    | PatLongIdent(ao, s, ps, tpso) -> 
        let aoc = opt sepSpace ao genAccess
        let tpsoc = opt sepNone tpso (fun (ValTyparDecls(tds, _, tcs)) -> genTypeParam tds tcs)
        match ps with
        | [] ->  aoc -- s +> tpsoc
        | [PatSeq(PatTuple, [p1; p2])] when s = "(::)" -> aoc +> genPat p1 -- " :: " +> genPat p2
        | [p] -> aoc -- s +> tpsoc +> ifElse (hasParenInPat p) (genPat p) (sepSpace +> genPat p)
        /// This pattern is potentially long
        | ps -> atCurrentColumn (aoc -- s +> tpsoc +> sepSpace +> col sepSpace ps (autoNln << genPat))
    | PatParen(PatConst(c)) -> genConst c
    | PatParen(p) -> sepOpenT +> genPat p +> sepCloseT
    | PatSeq(PatTuple, ps) -> atCurrentColumn (col sepComma ps (autoNln << genPat))
    | PatSeq(PatList, ps) -> sepOpenL +> atCurrentColumn (col sepSemi ps (autoNln << genPat)) +> sepCloseL
    | PatSeq(PatArray, ps) -> sepOpenA +> atCurrentColumn (col sepSemi ps (autoNln << genPat)) +> sepCloseA
    | PatRecord(xs) -> 
        sepOpenS +> atCurrentColumn (col sepSemi xs (autoNln << genPatRecordFieldName)) +> sepCloseS
    | PatConst(c) -> genConst c
    | PatIsInst(t) -> !- ":? " +> genType t
    /// Quotes will be printed by inner expression
    | PatQuoteExpr e -> genExpr e
    | p -> failwithf "Unexpected pattern: %O" p

