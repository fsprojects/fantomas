module internal Fantomas.CodePrinter

open System
open System.Collections.Generic
open Microsoft.FSharp.Compiler.Ast
open Fantomas.FormatConfig
open Fantomas.SourceParser
open Fantomas.SourceTransformer

let rec addSpaceBeforeParensInFunCall functionOrMethod arg = 
    match functionOrMethod, arg with
    | _, ConstExpr(Const "()", _) -> false
    | SynExpr.LongIdent(_, LongIdentWithDots s, _, _), _ ->
        let parts = s.Split '.'
        not <| Char.IsUpper parts.[parts.Length - 1].[0]
    | SynExpr.Ident(Ident s), _ -> not <| Char.IsUpper s.[0]
    | SynExpr.TypeApp(e, _, _, _, _, _, _), _ -> addSpaceBeforeParensInFunCall e arg
    | _ -> true

let addSpaceBeforeParensInFunDef functionOrMethod args =
    match functionOrMethod, args with
    | _, PatParen (PatConst(Const "()", _)) -> false
    | "new", _ -> false
    | (s:string), _ -> 
        let parts = s.Split '.'
        not <| Char.IsUpper parts.[parts.Length - 1].[0]
    | _ -> true

let rec genParsedInput = function
    | ImplFile im -> genImpFile im
    | SigFile si -> genSigFile si

and genImpFile(ParsedImplFileInput(hs, mns)) = 
    col sepNln hs genParsedHashDirective
    +> col sepNln mns genModuleOrNamespace

and genSigFile(ParsedSigFileInput(hs, mns)) =
    col sepNln hs genParsedHashDirective
    +> col sepNln mns genSigModuleOrNamespace

and genParsedHashDirective(ParsedHashDirective(h, s)) =
    let gs =
        match s with
        | "" -> sepNone
        // Use verbatim string to escape '\' correctly
        | _ when s.Contains("\\") -> !- (sprintf "@\"%O\"" s)
        | _ -> !- (sprintf "\"%O\"" s)
    !- "#" -- h +> sepSpace +> gs

and genModuleOrNamespace(ModuleOrNamespace(ats, px, ao, s, mds, isModule)) =
    genPreXmlDoc px
    +> colPost sepNln sepNln ats genAttribute
    // Checking for Tmp is a bit fragile
    +> ifElse (s = "Tmp") sepNone (ifElse isModule (!- "module ") (!- "namespace ")
            +> opt sepSpace ao genAccess +> ifElse (s = "") (!- "global") (!- s) +> rep 2 sepNln)
    +> genModuleDeclList mds

and genSigModuleOrNamespace(SigModuleOrNamespace(ats, px, ao, s, mds, isModule)) =
    genPreXmlDoc px
    +> colPost sepNln sepNln ats genAttribute
    +> ifElse (s = "Tmp") sepNone (ifElse isModule (!- "module ") (!- "namespace ")
    +> opt sepSpace ao genAccess -- s +> rep 2 sepNln)
    +> genSigModuleDeclList mds

and genModuleDeclList = function
    | [x] -> genModuleDecl x

    | OpenL(xs, ys) ->
        fun ctx ->
            let xs = sortAndDeduplicate ((|Open|_|) >> Option.get) xs ctx
            match ys with
            | [] -> col sepNln xs genModuleDecl ctx
            | _ -> (col sepNln xs genModuleDecl +> rep 2 sepNln +> genModuleDeclList ys) ctx

    | DoExprAttributesL(xs, ys) 
    | HashDirectiveL(xs, ys) 
    | ModuleAbbrevL(xs, ys) 
    | OneLinerLetL(xs, ys) ->
        match ys with
        | [] -> col sepNln xs genModuleDecl
        | _ -> col sepNln xs genModuleDecl +> rep 2 sepNln +> genModuleDeclList ys

    | MultilineModuleDeclL(xs, ys) ->
        match ys with
        | [] -> col (rep 2 sepNln) xs genModuleDecl
        | _ -> col (rep 2 sepNln) xs genModuleDecl +> rep 2 sepNln +> genModuleDeclList ys
    | _ -> sepNone    

and genSigModuleDeclList = function
    | [x] -> genSigModuleDecl x

    | SigOpenL(xs, ys) ->
        fun ctx ->
            let xs = sortAndDeduplicate ((|SigOpen|_|) >> Option.get) xs ctx
            match ys with
            | [] -> col sepNln xs genSigModuleDecl ctx
            | _ -> (col sepNln xs genSigModuleDecl +> rep 2 sepNln +> genSigModuleDeclList ys) ctx

    | SigHashDirectiveL(xs, ys) 
    | SigModuleAbbrevL(xs, ys) 
    | SigValL(xs, ys) ->
        match ys with
        | [] -> col sepNln xs genSigModuleDecl
        | _ -> col sepNln xs genSigModuleDecl +> rep 2 sepNln +> genSigModuleDeclList ys

    | SigMultilineModuleDeclL(xs, ys) ->
        match ys with
        | [] -> col (rep 2 sepNln) xs genSigModuleDecl
        | _ -> col (rep 2 sepNln) xs genSigModuleDecl +> rep 2 sepNln +> genSigModuleDeclList ys

    | _ -> sepNone

and genModuleDecl = function
    | Attributes(ats) ->
        col sepNln ats genAttribute
    | DoExpr(e) ->
        genExpr e
    | Exception(ex) ->
        genException ex
    | HashDirective(p) -> 
        genParsedHashDirective p
    // Add a new line after module-level let bindings
    | Let(b) ->
        genLetBinding true "let " b
    | LetRec(b::bs) -> 
        genLetBinding true "let rec " b 
        +> colPre (rep 2 sepNln) (rep 2 sepNln) bs (genLetBinding false "and ")

    | ModuleAbbrev(s1, s2) ->
        !- "module " -- s1 +> sepEq -- s2
    | NamespaceFragment(m) ->
        failwithf "NamespaceFragment hasn't been implemented yet: %O" m
    | NestedModule(ats, px, ao, s, mds) -> 
        genPreXmlDoc px
        +> colPost sepNln sepNln ats genAttribute -- "module " +> opt sepSpace ao genAccess -- s +> sepEq
        +> indent +> sepNln +> genModuleDeclList mds +> unindent

    | Open(s) ->
        !- (sprintf "open %s" s)
    // There is no nested types and they are recursive if there are more than one definition
    | Types(t::ts) ->
        genTypeDefn true t +> colPre (rep 2 sepNln) (rep 2 sepNln) ts (genTypeDefn false)
    | md ->
        failwithf "Unexpected module declaration: %O" md

and genSigModuleDecl = function
    | SigException(ex) ->
        genSigException ex
    | SigHashDirective(p) -> 
        genParsedHashDirective p
    | SigVal(v) ->
        genVal v
    | SigModuleAbbrev(s1, s2) ->
        !- "module " -- s1 +> sepEq -- s2
    | SigNamespaceFragment(m) ->
        failwithf "NamespaceFragment is not supported yet: %O" m
    | SigNestedModule(ats, px, ao, s, mds) -> 
        genPreXmlDoc px
        +> colPost sepNln sepNln ats genAttribute -- "module " +> opt sepSpace ao genAccess -- s +> sepEq
        +> indent +> sepNln +> genSigModuleDeclList mds +> unindent

    | SigOpen(s) ->
        !- (sprintf "open %s" s)
    | SigTypes(t::ts) ->
        genSigTypeDefn true t +> colPre (rep 2 sepNln) (rep 2 sepNln) ts (genSigTypeDefn false)
    | md ->
        failwithf "Unexpected module signature declaration: %O" md

and genAccess(Access s) = !- s

and genAttribute(Attribute(s, e, _)) = 
    match e with
    // Special treatment for function application on attributes
    | ConstExpr(Const "()", _) -> !- (sprintf "[<%s>]" s)
    | e -> !- "[<" -- s +> genExpr e -- ">]"
    
and genOneLinerAttributes ats = 
    colPost sepSpace sepNone ats genAttribute

and genAttributes ats = 
    colPost sepNln sepNone ats genAttribute

and genPreXmlDoc (PreXmlDoc lines) ctx = 
    if ctx.Config.StrictMode then
        colPost sepNln sepNln lines (sprintf "///%s" >> (!-)) ctx
    else ctx

and breakNln brk e = 
    ifElse brk (indent +> sepNln +> genExpr e +> unindent) 
        (indent +> autoNln (genExpr e) +> unindent)

/// Preserve a break even if the expression is a one-liner
and preserveBreakNln e ctx = breakNln (checkPreserveBreakForExpr e ctx) e ctx

/// Break but doesn't indent the expression
and noIndentBreakNln e ctx = ifElse (checkPreserveBreakForExpr e ctx) (sepNln +> genExpr e) (autoNln (genExpr e)) ctx

and genTyparList tps = 
    ifElse (List.atMostOne tps) (col wordOr tps genTypar) (sepOpenT +> col wordOr tps genTypar +> sepCloseT)

and genTypeParam tds tcs =
    ifElse (List.isEmpty tds) sepNone
        (!- "<" +> col sepComma tds genTyparDecl +> colPre (!- " when ") wordAnd tcs genTypeConstraint -- ">")

and genLetBinding isFirst pref b = 
    match b with 
    | LetBinding(ats, px, ao, isInline, isMutable, p, e) ->
        let prefix =
            genPreXmlDoc px
            +> ifElse isFirst (genAttributes ats -- pref) 
                (!- pref +> genOneLinerAttributes ats)
            +> opt sepSpace ao genAccess
            +> ifElse isMutable (!- "mutable ") sepNone +> ifElse isInline (!- "inline ") sepNone
            +> genPat p

        match e with
        | TypedExpr(Typed, e, t) -> prefix +> sepColon +> genType false t +> sepEq +> preserveBreakNln e
        | e -> prefix +> sepEq +> preserveBreakNln e

    | DoBinding(ats, px, e) ->
        let prefix = if pref.Contains("let") then pref.Replace("let", "do") else "do "
        genPreXmlDoc px
        +> genAttributes ats -- prefix +> preserveBreakNln e

    | b ->
        failwithf "%O isn't a let binding" b

and genProperty prefix ps e =
    let tuplerize ps =
        let rec loop acc = function
            | [p] -> (List.rev acc, p)
            | p1::ps -> loop (p1::acc) ps
            | [] -> invalidArg "p" "Patterns should not be empty"
        loop [] ps

    match ps with
    | [PatSeq(PatTuple, ps)] -> 
        let (ps, p) = tuplerize ps
        !- prefix
        +> ifElse (List.atMostOne ps) (col sepComma ps genPat +> sepSpace) 
            (sepOpenT +> col sepComma ps genPat +> sepCloseT +> sepSpace)
        +> genPat p +> sepEq +> preserveBreakNln e

    | ps -> 
        !- prefix +> col sepSpace ps genPat +> sepEq +> preserveBreakNln e

and genPropertyWithGetSet inter (b1, b2) =
    match b1, b2 with
    | PropertyBinding(ats, px, ao, isInline, mf1, PatLongIdent(_, s1, ps1, _), e1), 
      PropertyBinding(_, _, _, _, _, PatLongIdent(_, _, ps2, _), e2) ->
        let prefix =
            genPreXmlDoc px
            +> genAttributes ats +> genMemberFlags inter mf1
            +> ifElse isInline (!- "inline ") sepNone +> opt sepSpace ao genAccess

        prefix -- s1 +> sepSpace +> indent +> sepNln
        +> genProperty "with get " ps1 e1 +> sepNln +> genProperty "and set " ps2 e2
        +> unindent
    | _ -> sepNone

/// Value inter indicates printing in a interface definition. 
/// Each member is separated by a new line.
and genMemberBindingList inter = function
    | [x] -> genMemberBinding inter x

    | MultilineBindingL(xs, ys) ->
        let prefix = sepNln +> col (rep 2 sepNln) xs (function 
                                   | Pair(x1, x2) -> genPropertyWithGetSet inter (x1, x2) 
                                   | Single x -> genMemberBinding inter x)
        match ys with
        | [] -> prefix
        | _ -> prefix +> rep 2 sepNln +> genMemberBindingList inter ys

    | OneLinerBindingL(xs, ys) ->
        match ys with
        | [] -> col sepNln xs (genMemberBinding inter)
        | _ -> col sepNln xs (genMemberBinding inter) +> sepNln +> genMemberBindingList inter ys
    | _ -> sepNone

and genMemberBinding inter b = 
    match b with 
    | PropertyBinding(ats, px, ao, isInline, mf, p, e) -> 
        let prefix =
            genPreXmlDoc px
            +> genAttributes ats +> genMemberFlags inter mf
            +> ifElse isInline (!- "inline ") sepNone +> opt sepSpace ao genAccess

        let propertyPref =
            match mf with
            | MFProperty PropertyGet -> " with get "
            | MFProperty PropertySet -> " with set "
            | mf -> failwithf "Unexpected member flags: %O" mf

        match p with
        // Too tedious in handling property get and set
        | PatLongIdent(_, s, ps, _) ->                 
            prefix -- s +> genProperty propertyPref ps e
        | p -> failwithf "Unexpected pattern: %O" p

    | MemberBinding(ats, px, ao, isInline, mf, p, e) ->
        let prefix =
            genPreXmlDoc px
            +> genAttributes ats +> genMemberFlags inter mf
            +> ifElse isInline (!- "inline ") sepNone +> opt sepSpace ao genAccess +> genPat p

        match e with
        | TypedExpr(Typed, e, t) -> prefix +> sepColon +> genType false t +> sepEq +> preserveBreakNln e
        | e -> prefix +> sepEq +> preserveBreakNln e

    | ExplicitCtor(ats, px, ao, p, e, so) ->
        let prefix =
            genPreXmlDoc px
            +> genAttributes ats
            +> opt sepSpace ao genAccess +> genPat p 
            +> opt sepNone so (sprintf " as %s" >> (!-))

        match e with
        // Handle special "then" block in constructors
        | Sequentials [e1; e2] -> 
            prefix +> sepEq +> indent +> sepNln 
            +> genExpr e1 ++ "then " +> preserveBreakNln e2 +> unindent

        | e -> prefix +> sepEq +> preserveBreakNln e

    | b -> failwithf "%O isn't a member binding" b

and genMemberFlags inter = function
    | MFMember _ -> !- "member "
    | MFStaticMember _ -> !- "static member "
    | MFConstructor _ -> sepNone
    | MFOverride _ -> ifElse inter (!- "member ") (!- "override ")

and genVal(Val(ats, px, ao, s, t, vi, _)) = 
    let (FunType ts) = (t, vi)
    genPreXmlDoc px
    +> genAttributes ats 
    +> atCurrentColumn (indent -- "val " +> opt sepSpace ao genAccess -- s 
                        +> sepColon +> genTypeList ts +> unindent)

and genRecordFieldName(RecordFieldName(s, eo)) =
    opt sepNone eo (fun e -> !- s +> sepEq +> preserveBreakNln e)

and genExpr = function
    | Paren e -> sepOpenT +> genExpr e +> sepCloseT
    | SingleExpr(kind, e) -> str kind +> genExpr e
    | ConstExpr(c) -> genConst c
    | NullExpr -> !- "null"
    // Not sure about the role of e1
    | Quote(_, e2, isRaw) ->         
        let e = genExpr e2
        ifElse isRaw (!- "<@@ " +> e -- " @@>") (!- "<@ " +> e -- " @>")
    | TypedExpr(TypeTest, e, t) -> genExpr e -- " :? " +> genType false t
    | TypedExpr(New, e, t) -> 
        !- "new " +> genType false t +> ifElse (hasParenthesis e) sepNone sepSpace +> genExpr e
    | TypedExpr(Downcast, e, t) -> genExpr e -- " :?> " +> genType false t
    | TypedExpr(Upcast, e, t) -> genExpr e -- " :> " +> genType false t
    | TypedExpr(Typed, e, t) -> genExpr e +> sepColon +> genType false t
    | Tuple es -> atCurrentColumn (colAutoNlnSkip0i sepComma es (fun i -> if i = 0 then genExpr else noIndentBreakNln))
    | ArrayOrList(isArray, [], _) -> 
        ifElse isArray (sepOpenAFixed +> sepCloseAFixed) (sepOpenLFixed +> sepCloseLFixed)
    | ArrayOrList(isArray, xs, isSimple) -> 
        let sep = ifElse isSimple sepSemi sepSemiNln
        ifElse isArray (sepOpenA +> atCurrentColumn (colAutoNlnSkip0 sep xs genExpr) +> sepCloseA) 
            (sepOpenL +> atCurrentColumn (colAutoNlnSkip0 sep xs genExpr) +> sepCloseL)

    | Record(xs, eo) -> 
        sepOpenS +> opt (!- " with ") eo genExpr
        +> atCurrentColumn (col sepSemiNln xs genRecordFieldName)
        +> sepCloseS

    | ObjExpr(t, eio, bd, ims) ->
        // Check the role of the second part of eio
        let param = opt sepNone (Option.map fst eio) genExpr
        sepOpenS +> 
        atCurrentColumn (!- "new " +> genType false t +> param -- " with" 
            +> indent +> sepNln +> genMemberBindingList true bd +> unindent
            +> colPre sepNln sepNln ims genInterfaceImpl) +> sepCloseS

    | While(e1, e2) -> 
        atCurrentColumn (!- "while " +> genExpr e1 -- " do" 
        +> indent +> sepNln +> genExpr e2 +> unindent)

    | For(s, e1, e2, e3, isUp) ->
        atCurrentColumn (!- (sprintf "for %s = " s) +> genExpr e1 
            +> ifElse isUp (!- " to ") (!- " downto ") +> genExpr e2 -- " do" 
            +> indent +> sepNln +> genExpr e3 +> unindent)

    // Handle the form 'for i in e1 -> e2'
    | ForEach(p, e1, e2, isArrow) ->
        atCurrentColumn (!- "for " +> genPat p -- " in " +> genExpr e1 
            +> ifElse isArrow (sepArrow +> preserveBreakNln e2) (!- " do" +> indent +> sepNln +> genExpr e2 +> unindent))

    | CompExpr(isArrayOrList, e) ->
        ifElse isArrayOrList (genExpr e) (preserveBreakNln e) 
    | ArrayOrListOfSeqExpr(isArray, e) -> 
        ifElse isArray (sepOpenA +> genExpr e +> sepCloseA) (sepOpenL +> genExpr e +> sepCloseL)
    | JoinIn(e1, e2) -> genExpr e1 -- " in " +> genExpr e2
    | DesugaredLambda(cps, e) -> 
        !- "fun " +>  col sepSpace cps genComplexPats +> sepArrow +> preserveBreakNln e 
    | Lambda(e, sps) -> 
        !- "fun " +> col sepSpace sps genSimplePats +> sepArrow +> noIndentBreakNln e
    | MatchLambda(sp, _) -> atCurrentColumn (!- "function " +> colPre sepNln sepNln sp (genClause true))
    | Match(e, cs) -> 
        atCurrentColumn (!- "match " +> genExpr e -- " with" +> colPre sepNln sepNln cs (genClause true))
    | CompApp(s, e) ->
        !- s +> sepSpace +> sepOpenS +> genExpr e 
        +> ifElse (checkBreakForExpr e) (sepNln +> sepCloseSFixed) sepCloseS

    | App(Var ".. ..", [e1; e2; e3]) -> genExpr e1 -- ".." +> genExpr e2 -- ".." +> genExpr e3
    // Separate two prefix ops by spaces
    | PrefixApp(s1, PrefixApp(s2, e)) -> !- (sprintf "%s %s" s1 s2) +> genExpr e
    | PrefixApp(s, e) -> !- s +> genExpr e
    // Handle spaces of infix application based on which category it belongs to
    | InfixApps(e, es) -> 
        // Only put |> on the same line in a very trivial expression
        let hasNewLine = multiline e || not (List.atMostOne es)
        atCurrentColumn (genExpr e +> genInfixApps hasNewLine es)

    | TernaryApp(e1,e2,e3) -> 
        atCurrentColumn (genExpr e1 +> !- "?" +> genExpr e2 +> sepSpace +> !- "<-" +> sepSpace +> genExpr e3)

    // This filters a few long examples of App
    | DotGetAppSpecial(s, es) ->
        !- s 
        +> atCurrentColumn 
             (colAutoNlnSkip0 sepNone es (fun (s, e) ->
                                (!- (sprintf ".%s" s) 
                                    +> ifElse (hasParenthesis e) sepNone sepSpace +> genExpr e)))

    | DotGetApp(e, es) -> 
        noNln (genExpr e)
        +> indent 
        +> (col sepNone es (fun (s, e) -> 
                                autoNln (!- (sprintf ".%s" s) 
                                    +> ifElse (hasParenthesis e) sepNone sepSpace +> genExpr e)))
        +> unindent

    // Unlike infix app, function application needs a level of indentation
    | App(e1, [e2]) -> 
        atCurrentColumn (genExpr e1 +> 
            ifElse (hasParenthesis e2) (ifElse (addSpaceBeforeParensInFunCall e1 e2) sepBeforeArg sepNone) sepSpace 
            +> indent +> autoNln (genExpr e2) +> unindent)

    // Always spacing in multiple arguments
    | App(e, es) -> 
        atCurrentColumn 
            (genExpr e +> colPre sepSpace sepSpace es (fun e -> indent +> autoNln (genExpr e) +> unindent))

    | TypeApp(e, ts) -> genExpr e -- "<" +> col sepComma ts (genType false) -- ">"
    | LetOrUses(bs, e) ->
        atCurrentColumn (genLetOrUseList bs +> sepNln +> genExpr e)

    // Could customize a bit if e is single line
    | TryWith(e, cs) -> 
        let prefix = !- "try " +> indent +> sepNln +> genExpr e +> unindent ++ "with"
        match cs with
        | [c] -> 
            atCurrentColumn (prefix +> sepSpace +> genClause false c)
        | _ -> 
            atCurrentColumn (prefix +> indentOnWith +> sepNln +> col sepNln cs (genClause true) +> unindentOnWith)

    | TryFinally(e1, e2) -> 
        atCurrentColumn (!- "try " +> indent +> sepNln +> genExpr e1 +> unindent ++ "finally" 
            +> indent +> sepNln +> genExpr e2 +> unindent)    

    | SequentialSimple es -> atCurrentColumn (colAutoNlnSkip0 sepSemi es genExpr)
    // It seems too annoying to use sepSemiNln
    | Sequentials es -> atCurrentColumn (col sepNln es genExpr)
    // A generalization of IfThenElse
    | ElIf((e1,e2, _)::es, en) ->
        atCurrentColumn (!- "if " +> ifElse (checkBreakForExpr e1) (genExpr e1 ++ "then") (genExpr e1 +- "then") -- " " 
            +> preserveBreakNln e2
            +> fun ctx -> col sepNone es (fun (e1, e2, r) ->
                             ifElse (startWith "elif" r ctx) (!+ "elif ") (!+ "else if ")
                             +> ifElse (checkBreakForExpr e1) (genExpr e1 ++ "then") (genExpr e1 +- "then") 
                             -- " " +> preserveBreakNln e2) ctx
            ++ "else " +> preserveBreakNln en)

    | IfThenElse(e1, e2, None) -> 
        atCurrentColumn (!- "if " +> ifElse (checkBreakForExpr e1) (genExpr e1 ++ "then") (genExpr e1 +- "then") 
                         -- " " +> preserveBreakNln e2)
    // At this stage, all symbolic operators have been handled.
    | OptVar(s, isOpt) -> ifElse isOpt (!- "?") sepNone -- s
    | LongIdentSet(s, e) -> !- (sprintf "%s <- " s) +> genExpr e
    | DotIndexedGet(e, es) -> genExpr e -- "." +> sepOpenLFixed +> genIndexedVars es +> sepCloseLFixed
    | DotIndexedSet(e1, es, e2) -> genExpr e1 -- ".[" +> genIndexedVars es -- "] <- " +> genExpr e2
    | DotGet(e, s) -> genExpr e -- sprintf ".%s" s
    | DotSet(e1, s, e2) -> genExpr e1 -- sprintf ".%s <- " s +> genExpr e2
    | TraitCall(tps, msg, e) -> 
        sepOpenT +> genTyparList tps +> sepColon +> sepOpenT +> genMemberSig msg +> sepCloseT 
        +> sepSpace +> genExpr e +> sepCloseT

    | LetOrUseBang(isUse, p, e1, e2) ->
        atCurrentColumn (ifElse isUse (!- "use! ") (!- "let! ") 
            +> genPat p -- " = " +> genExpr e1 +> sepNln +> genExpr e2)

    | ParsingError _ -> raise <| FormatException "Unable to parse this code fragment."
    | UnsupportedExpr _ -> raise <| FormatException "This code fragment consists of unsupported construct(s)."
    | e -> failwithf "Unexpected expression: %O" e

and genLetOrUseList = function
    | [p, x] -> genLetBinding true p x
    | OneLinerLetOrUseL(xs, ys) ->
        match ys with
        | [] -> 
            col sepNln xs (fun (p, x) -> genLetBinding (p <> "and ") p x)
        | _ -> 
            col sepNln xs (fun (p, x) -> genLetBinding (p <> "and ") p x) 
            +> rep 2 sepNln +> genLetOrUseList ys

    | MultilineLetOrUseL(xs, ys) ->
        match ys with
        | [] -> 
            col (rep 2 sepNln) xs (fun (p, x) -> genLetBinding (p <> "and ") p x)
            // Add a trailing new line to separate these with the main expression
            +> sepNln 
        | _ -> 
            col (rep 2 sepNln) xs (fun (p, x) -> genLetBinding (p <> "and ") p x) 
            +> rep 2 sepNln +> genLetOrUseList ys

    | _ -> sepNone   

and genInfixApps newline = function
    | (s, e)::es ->
        (ifElse (newline && NewLineInfixOps.Contains s) (sepNln -- s +> sepSpace +> genExpr e)
           (ifElse (NoSpaceInfixOps.Contains s) (!- s +> autoNln (genExpr e))
              (ifElse (NoBreakInfixOps.Contains s) (sepSpace -- s +> sepSpace +> genExpr e)
                (sepSpace +> autoNln (!- s +> sepSpace +> genExpr e)))))
        +> genInfixApps newline es

    | [] -> sepNone

/// Use in indexed set and get only
and genIndexedVars es =
    match es with
    | IndexedVar eo1 :: es ->
        match es with
        | IndexedVar eo2 :: es -> 
            ifElse (eo1.IsNone && eo2.IsNone) (!- "*") 
                (opt sepNone eo1 genExpr -- ".." +> opt sepNone eo2 genExpr)
            +> ifElse es.IsEmpty sepNone (sepComma +> genIndexedVars es)
        | _ -> 
            opt sepNone eo1 genExpr +> ifElse es.IsEmpty sepNone (sepComma +> genIndexedVars es)

    | [e] -> genExpr e
    | e :: es -> genExpr e +> sepComma +> genIndexedVars es
    | [] -> sepNone

and genTypeDefn isFirst (TypeDef(ats, px, ao, tds, tcs, tdr, ms, s)) = 
    let typeName = 
        genPreXmlDoc px 
        +> ifElse isFirst (colPost sepNln sepNln ats genAttribute -- "type ") 
            (!- "and " +> genOneLinerAttributes ats) 
        +> opt sepSpace ao genAccess -- s
        +> genTypeParam tds tcs

    match tdr with
    | Simple(TDSREnum ecs) ->
        typeName +> sepEq 
        +> indent +> sepNln
        +> col sepNln ecs (genEnumCase true)
        +> genMemberDefnList false ms
        // Add newline after un-indent to be spacing-correct
        +> unindent

    | Simple(TDSRUnion(ao', xs)) ->
        typeName +> sepEq 
        +> indent +> sepNln +> opt sepNln ao' genAccess 
        +> col sepNln xs (genUnionCase true)
        +> genMemberDefnList false ms
        +> unindent

    | Simple(TDSRRecord(ao', fs)) ->
        typeName +> sepEq 
        +> indent +> sepNln +> opt sepNln ao' genAccess +> sepOpenS 
        +> atCurrentColumn (col sepSemiNln fs (genField false "")) +> sepCloseS
        +> genMemberDefnList false ms 
        +> unindent 

    | Simple TDSRNone -> 
        typeName
    | Simple(TDSRTypeAbbrev t) -> 
        typeName +> sepEq +> genType false t
    | ObjectModel(TCSimple (TCStruct | TCInterface | TCClass) as tdk, MemberDefnList(impCtor, others)) ->
        let inter =
            match tdk with
            | TCSimple TCInterface -> true
            | _ -> false

        typeName +> opt sepNone impCtor (genMemberDefn inter) +> sepEq 
        +> indent +> sepNln +> genTypeDefKind tdk
        +> indent +> genMemberDefnList inter others +> unindent
        ++ "end" +> unindent

    | ObjectModel(TCSimple TCAugmentation, _) ->
        typeName -- " with" +> indent
        // Remember that we use MemberDefn of parent node
        +> genMemberDefnList false ms +> unindent

    | ObjectModel(TCDelegate(FunType ts), _) ->
        typeName +> sepEq -- "delegate of " +> genTypeList ts
    | ObjectModel(_, MemberDefnList(impCtor, others)) ->
        typeName +> opt sepNone impCtor (genMemberDefn false) +> sepEq +> indent
        +> genMemberDefnList false others +> unindent

and genSigTypeDefn isFirst (SigTypeDef(ats, px, ao, tds, tcs, tdr, ms, s)) = 
    let typeName = 
        genPreXmlDoc px 
        +> ifElse isFirst (colPost sepNln sepNln ats genAttribute -- "type ") 
            (!- "and " +> genOneLinerAttributes ats) 
        +> opt sepSpace ao genAccess -- s
        +> genTypeParam tds tcs

    match tdr with
    | SigSimple(TDSREnum ecs) ->
        typeName +> sepEq 
        +> indent +> sepNln
        +> col sepNln ecs (genEnumCase true)
        +> colPre sepNln sepNln ms genMemberSig
        // Add newline after un-indent to be spacing-correct
        +> unindent
         
    | SigSimple(TDSRUnion(ao', xs)) ->
        typeName +> sepEq 
        +> indent +> sepNln +> opt sepNln ao' genAccess 
        +> col sepNln xs (genUnionCase true)
        +> colPre sepNln sepNln ms genMemberSig
        +> unindent

    | SigSimple(TDSRRecord(ao', fs)) ->
        typeName +> sepEq 
        +> indent +> sepNln +> opt sepNln ao' genAccess +> sepOpenS 
        +> atCurrentColumn (col sepSemiNln fs (genField false "")) +> sepCloseS
        +> colPre sepNln sepNln ms genMemberSig
        +> unindent 

    | SigSimple TDSRNone -> 
        typeName
    | SigSimple(TDSRTypeAbbrev t) -> 
        typeName +> sepEq +> genType false t
    | SigObjectModel(TCSimple (TCStruct | TCInterface | TCClass) as tdk, mds) ->
        typeName +> sepEq +> indent +> sepNln +> genTypeDefKind tdk
        +> indent +> colPre sepNln sepNln mds genMemberSig +> unindent
        ++ "end" +> unindent

    | SigObjectModel(TCSimple TCAugmentation, _) ->
        typeName -- " with" +> indent +> sepNln 
        // Remember that we use MemberSig of parent node
        +> col sepNln ms genMemberSig +> unindent

    | SigObjectModel(TCDelegate(FunType ts), _) ->
        typeName +> sepEq -- "delegate of " +> genTypeList ts
    | SigObjectModel(_, mds) -> 
        typeName +> sepEq +> indent +> sepNln 
        +> col sepNln mds genMemberSig +> unindent

and genMemberSig = function
    | MSMember(Val(ats, px, ao, s, t, vi, _), mf) -> 
        let (FunType ts) = (t, vi)
        genPreXmlDoc px +> genOneLinerAttributes ats 
        +> atCurrentColumn (indent +> genMemberFlags false mf +> opt sepNone ao genAccess
                                   +> ifElse (s = "``new``") (!- "new") (!- s) 
                                   +> sepColon +> genTypeList ts +> unindent)

    | MSInterface t -> !- "interface " +> genType false t
    | MSInherit t -> !- "inherit " +> genType false t
    | MSValField f -> genField false "val " f
    | MSNestedType _ -> invalidArg "md" "This is not implemented in F# compiler"

and genTyparDecl(TyparDecl(ats, tp)) =
    genOneLinerAttributes ats +> genTypar tp

and genTypeDefKind = function
    | TCSimple TCUnspecified -> sepNone
    | TCSimple TCClass -> !- "class"
    | TCSimple TCInterface -> !- "interface"
    | TCSimple TCStruct -> !- "struct"
    | TCSimple TCRecord -> sepNone
    | TCSimple TCUnion -> sepNone
    | TCSimple TCAbbrev -> sepNone
    | TCSimple TCHiddenRepr -> sepNone
    | TCSimple TCAugmentation -> sepNone
    | TCSimple TCILAssemblyCode -> sepNone
    | TCDelegate _ -> sepNone

and genException(ExceptionDef(ats, px, ao, uc, ms)) = 
    genPreXmlDoc px
    +> genAttributes ats  -- "exception " 
    +> opt sepSpace ao genAccess +> genUnionCase false uc
    +> genMemberDefnList false ms

and genSigException(SigExceptionDef(ats, px, ao, uc, ms)) = 
    genPreXmlDoc px
    +> genAttributes ats  -- "exception " 
    +> opt sepSpace ao genAccess +> genUnionCase false uc
    +> colPre sepNln sepNln ms genMemberSig

and genUnionCase hasBar (UnionCase(ats, px, _, s, UnionCaseType fs)) =
    genPreXmlDoc px
    +> ifElse hasBar sepBar sepNone
    +> genOneLinerAttributes ats -- s 
    +> colPre wordOf sepStar fs (genField true "")

and genEnumCase hasBar (EnumCase(ats, px, _, c)) =
    genPreXmlDoc px 
    +> ifElse hasBar sepBar sepNone 
    +> genOneLinerAttributes ats +> genConst c

and genField isUnion prefix (Field(ats, px, ao, isStatic, isMutable, t, so)) = 
    // Being protective on union case declaration
    let t = genType isUnion t

    genPreXmlDoc px 
    +> genOneLinerAttributes ats -- prefix
    +> opt sepSpace ao genAccess +> ifElse isStatic (!- "static ") sepNone
    +> ifElse isMutable (!- "mutable ") sepNone +> opt sepColon so (!-) +> t

and genType outerBracket t =
    let rec loop = function
        | THashConstraint t -> !- "#" +> loop t
        | TMeasurePower(t, n) -> loop t -- "^" +> str n
        | TMeasureDivide(t1, t2) -> loop t1 -- " / " +> loop t2
        | TStaticConstant(c) -> genConst c
        | TStaticConstantExpr(e) -> genExpr e
        | TStaticConstantNamed(t1, t2) -> loop t1 -- "=" +> loop t2
        | TArray(t, n) -> loop t -- " [" +> rep (n - 1) (!- ",") -- "]"
        | TAnon -> sepWild
        | TVar tp -> genTypar tp
        // Drop bracket around tuples before an arrow
        | TFun(TTuple ts, t) -> sepOpenT +> col sepStar ts loop +> sepArrow +> loop t +> sepCloseT
        // Do similar for tuples after an arrow
        | TFun(t, TTuple ts) -> sepOpenT +> loop t +> sepArrow +> col sepStar ts loop +> sepCloseT
        | TFuns ts -> sepOpenT +> col sepArrow ts loop +> sepCloseT
        | TApp(t, ts, isPostfix) -> 
            let postForm = 
                match ts with
                | [] ->  loop t
                | [t'] -> loop t' +> sepSpace +> loop t
                | ts -> sepOpenT +> col sepComma ts loop +> sepCloseT +> loop t

            ifElse isPostfix postForm (loop t +> genPrefixTypes ts)

        | TLongIdentApp(t, s, ts) -> loop t -- sprintf ".%s" s +> genPrefixTypes ts
        | TTuple ts -> sepOpenT +> col sepStar ts loop +> sepCloseT
        | TWithGlobalConstraints(t, tcs) -> loop t +> colPre (!- " when ") wordAnd tcs genTypeConstraint
        | TLongIdent s -> !- s
        | t -> failwithf "Unexpected type: %O" t
    match t with
    | TFun(TTuple ts, t) -> 
        ifElse outerBracket (sepOpenT +> col sepStar ts loop +> sepArrow +> loop t +> sepCloseT)
            (col sepStar ts loop +> sepArrow +> loop t)
    | TFuns ts -> ifElse outerBracket (sepOpenT +> col sepArrow ts loop +> sepCloseT) (col sepArrow ts loop)
    | TTuple ts -> ifElse outerBracket (sepOpenT +> col sepStar ts loop +> sepCloseT) (col sepStar ts loop)
    | _ -> loop t

and genPrefixTypes = function
    | [] -> sepNone
    // Some patterns without spaces could cause a parsing error
    | (TStaticConstant _ | TStaticConstantExpr _ | TStaticConstantNamed _ | TVar(Typar(_, true)) as t)::ts -> 
        !- "< " +> col sepComma (t::ts) (genType false) -- " >"
    | ts -> !- "<" +> col sepComma ts (genType false) -- ">"

and genTypeList = function
    | [] -> sepNone
    | (t, [ArgInfo(so, isOpt)])::ts -> 
        let gt =
            match t with
            | TTuple _ ->
                opt sepColonFixed so (if isOpt then (sprintf "?%s" >> (!-)) else (!-)) +> genType (not ts.IsEmpty) t 
            | TFun _ ->
                // Fun is grouped by brackets inside 'genType true t'
                opt sepColonFixed so (if isOpt then (sprintf "?%s" >> (!-)) else (!-)) +> genType true t
            | _ -> opt sepColonFixed so (!-) +> genType false t
        gt +> ifElse ts.IsEmpty sepNone (autoNln (sepArrow +> genTypeList ts))

    | (TTuple ts', ais)::ts -> 
        let gt = col sepStar (Seq.zip ais ts') 
                    (fun (ArgInfo(so, isOpt), t) ->
                        opt sepColonFixed so (if isOpt then (sprintf "?%s" >> (!-)) else (!-))
                        +> genType (not ts.IsEmpty) t)
        gt +> ifElse ts.IsEmpty sepNone (autoNln (sepArrow +> genTypeList ts))

    | (t, _)::ts -> 
        let gt = genType false t
        gt +> ifElse ts.IsEmpty sepNone (autoNln (sepArrow +> genTypeList ts))

and genTypar (Typar(s, isHead)) = 
    ifElse isHead (!- "^") (!-"'") -- s

and genTypeConstraint = function
    | TyparSingle(kind, tp) -> genTypar tp +> sepColon -- sprintf "%O" kind
    | TyparDefaultsToType(tp, t) -> !- "default " +> genTypar tp +> sepColon +> genType false t
    | TyparSubtypeOfType(tp, t) -> genTypar tp -- " :> " +> genType false t
    | TyparSupportsMember(tps, msg) -> 
        genTyparList tps +> sepColon +> sepOpenT +> genMemberSig msg +> sepCloseT
    | TyparIsEnum(tp, ts) -> 
        genTypar tp +> sepColon -- "enum<" +> col sepComma ts (genType false) -- ">"
    | TyparIsDelegate(tp, ts) ->
        genTypar tp +> sepColon -- "delegate<" +> col sepComma ts (genType false) -- ">"

and genInterfaceImpl(InterfaceImpl(t, bs)) = 
    match bs with
    | [] -> !- "interface " +> genType false t
    | bs ->
        !- "interface " +> genType false t -- " with"
        +> indent +> sepNln +> genMemberBindingList true bs +> unindent

and genClause hasBar (Clause(p, e, eo)) = 
    ifElse hasBar sepBar sepNone +> genPat p 
    +> optPre (!- " when ") sepNone eo genExpr +> sepArrow +> preserveBreakNln e

/// Each multiline member definition has a pre and post new line. 
and genMemberDefnList inter = function
    | [x] -> sepNln +> genMemberDefn inter x

    | MDOpenL(xs, ys) ->
        fun ctx ->
            let xs = sortAndDeduplicate ((|MDOpen|_|) >> Option.get) xs ctx
            match ys with
            | [] -> col sepNln xs (genMemberDefn inter) ctx
            | _ -> (col sepNln xs (genMemberDefn inter) +> rep 2 sepNln +> genMemberDefnList inter ys) ctx

    | MultilineMemberDefnL(xs, []) ->
        rep 2 sepNln 
        +> col (rep 2 sepNln) xs (function
                | Pair(x1, x2) -> genPropertyWithGetSet inter (x1, x2)
                | Single x -> genMemberDefn inter x)

    | MultilineMemberDefnL(xs, ys) ->
        rep 2 sepNln 
        +> col (rep 2 sepNln) xs (function
                | Pair(x1, x2) -> genPropertyWithGetSet inter (x1, x2)
                | Single x -> genMemberDefn inter x) 
        +> sepNln +> genMemberDefnList inter ys

    | OneLinerMemberDefnL(xs, ys) ->
        sepNln +> col sepNln xs (genMemberDefn inter) +> genMemberDefnList inter ys
    | _ -> sepNone

and genMemberDefn inter = function
    | MDNestedType _ -> invalidArg "md" "This is not implemented in F# compiler"
    | MDOpen(s) -> !- (sprintf "open %s" s)
    // What is the role of so
    | MDImplicitInherit(t, e, _) -> !- "inherit " +> genType false t +> genExpr e
    | MDInherit(t, _) -> !- "inherit " +> genType false t
    | MDValField f -> genField false "val " f
    | MDImplicitCtor(ats, ao, ps, so) -> 
        optPre sepSpace sepSpace ao genAccess +> sepOpenT
        +> genOneLinerAttributes ats +> col sepComma ps genSimplePat +> sepCloseT
        +> optPre (!- " as ") sepNone so (!-)

    | MDMember(b) -> genMemberBinding inter b
    | MDLetBindings(isStatic, isRec, b::bs) ->
        let prefix = 
            if isStatic && isRec then "static let rec "
            elif isStatic then "static let "
            elif isRec then "let rec "
            else "let "

        genLetBinding true prefix b +> colPre sepNln sepNln bs (genLetBinding false "and")

    | MDInterface(t, mdo) -> 
        !- "interface " +> genType false t
        +> opt sepNone mdo 
            (fun mds -> !- " with" +> indent +> genMemberDefnList true mds +> unindent)

    | MDAutoProperty(ats, px, ao, mk, e, s) -> 
        genPreXmlDoc px
        +> genOneLinerAttributes ats -- "member val " 
        +> opt sepSpace ao genAccess -- s +> sepEq +> genExpr e -- propertyKind mk

    | MDAbstractSlot(ats, px, ao, s, t, ValTyparDecls(tds, _, tcs), MFMemberFlags mk) ->
        genPreXmlDoc px 
        +> genOneLinerAttributes ats
        +> opt sepSpace ao genAccess -- sprintf "abstract %s" s
        +> genTypeParam tds tcs
        +> sepColon +> genType false t -- propertyKind mk

    | md -> failwithf "Unexpected member definition: %O" md

and propertyKind = function
    | PropertyGet -> " with get"
    | PropertySet -> " with set"
    | PropertyGetSet -> " with get, set"
    | _ -> ""

and genSimplePat = function
    | SPId(s, isOptArg, _) -> ifElse isOptArg (!- (sprintf "?%s" s)) (!- s)
    | SPTyped(sp, t) -> genSimplePat sp +> sepColon +> genType false t
    | SPAttrib(ats, sp) -> genOneLinerAttributes ats +> genSimplePat sp
    
and genSimplePats = function
    // Remove parentheses on an extremely simple pattern
    | SimplePats [SPId _ as sp] -> genSimplePat sp
    | SimplePats ps -> sepOpenT +> col sepComma ps genSimplePat +> sepCloseT
    | SPSTyped(ps, t) -> genSimplePats ps +> sepColon +> genType false t

and genComplexPat = function
    | CPId p -> genPat p
    | CPSimpleId(s, isOptArg, _) -> ifElse isOptArg (!- (sprintf "?%s" s)) (!- s)
    | CPTyped(sp, t) -> genComplexPat sp +> sepColon +> genType false t
    | CPAttrib(ats, sp) -> colPost sepSpace sepNone ats genAttribute +> genComplexPat sp

and genComplexPats = function
    | ComplexPats [c] -> genComplexPat c
    | ComplexPats ps -> sepOpenT +> col sepComma ps genComplexPat +> sepCloseT
    | ComplexTyped(ps, t) -> genComplexPats ps +> sepColon +> genType false t

and genPatRecordFieldName(PatRecordFieldName(s1, s2, p)) =
    ifElse (s1 = "") (!- (sprintf "%s = " s2)) (!- (sprintf "%s.%s = " s1 s2)) +> genPat p

and genPat = function
    | PatOptionalVal(s) -> !- (sprintf "?%s" s)
    | PatAttrib(p, ats) -> genOneLinerAttributes ats +> genPat p
    | PatOr(p1, p2) -> genPat p1 -- " | " +> genPat p2
    | PatAnds(ps) -> col (!- " & ") ps genPat
    | PatNullary PatNull -> !- "null"
    | PatNullary PatWild -> sepWild
    | PatTyped(p, t) -> genPat p +> sepColon +> genType false t
    | PatNamed(ao, PatNullary PatWild, s) -> opt sepSpace ao genAccess -- s
    | PatNamed(ao, p, s) -> opt sepSpace ao genAccess +> genPat p -- sprintf " as %s" s 
    | PatLongIdent(ao, s, ps, tpso) -> 
        let aoc = opt sepSpace ao genAccess
        let tpsoc = opt sepNone tpso (fun (ValTyparDecls(tds, _, tcs)) -> genTypeParam tds tcs)
        // Override escaped new keyword
        let s = if s = "``new``" then "new" else s
        match ps with
        | [] ->  aoc -- s +> tpsoc
        | [PatSeq(PatTuple, [p1; p2])] when s = "(::)" -> aoc +> genPat p1 -- " :: " +> genPat p2
        | [p] -> aoc -- s +> tpsoc +> ifElse (hasParenInPat p) (ifElse (addSpaceBeforeParensInFunDef s p) sepBeforeArg sepNone) sepSpace +> genPat p
        // This pattern is potentially long
        | ps -> atCurrentColumn (aoc -- s +> tpsoc +> sepSpace +> colAutoNlnSkip0 sepSpace ps genPat)

    | PatParen(PatConst(Const "()", _)) -> !- "()"
    | PatParen(p) -> sepOpenT +> genPat p +> sepCloseT
    | PatSeq(PatTuple, ps) -> atCurrentColumn (colAutoNlnSkip0 sepComma ps genPat)
    | PatSeq(PatList, ps) -> 
        ifElse ps.IsEmpty (sepOpenLFixed +> sepCloseLFixed) 
            (sepOpenL +> atCurrentColumn (colAutoNlnSkip0 sepSemi ps genPat) +> sepCloseL)

    | PatSeq(PatArray, ps) -> 
        ifElse ps.IsEmpty (sepOpenAFixed +> sepCloseAFixed)
            (sepOpenA +> atCurrentColumn (colAutoNlnSkip0 sepSemi ps genPat) +> sepCloseA)

    | PatRecord(xs) -> 
        sepOpenS +> atCurrentColumn (colAutoNlnSkip0 sepSemi xs genPatRecordFieldName) +> sepCloseS
    | PatConst(c) -> genConst c
    | PatIsInst(t) -> !- ":? " +> genType false t
    // Quotes will be printed by inner expression
    | PatQuoteExpr e -> genExpr e
    | p -> failwithf "Unexpected pattern: %O" p

