module internal Fantomas.CodePrinter

open System
open System.Text.RegularExpressions
open FSharp.Compiler.Ast
open FSharp.Compiler.Range
open Fantomas
open Fantomas.FormatConfig
open Fantomas.SourceParser
open Fantomas.SourceTransformer
open Fantomas.Context
open Fantomas.TriviaTypes

/// This type consists of contextual information which is important for formatting
type ASTContext =
    {
      /// Original file name without extension of the parsed AST 
      TopLevelModuleName: string 
      /// Current node is the first child of its parent
      IsFirstChild: bool
      /// Current node is a subnode deep down in an interface
      InterfaceRange: range option
      /// This pattern matters for formatting extern declarations
      IsCStylePattern: bool
      /// Range operators are naked in 'for..in..do' constructs
      IsNakedRange: bool
      /// The optional `|` in pattern matching and union type definitions
      HasVerticalBar: bool
      /// A field is rendered as union field or not
      IsUnionField: bool
      /// First type param might need extra spaces to avoid parsing errors on `<^`, `<'`, etc.
      IsFirstTypeParam: bool
      /// Check whether the context is inside DotGet to suppress whitespaces
      IsInsideDotGet: bool
    }
    static member Default =
        { TopLevelModuleName = "" 
          IsFirstChild = false; InterfaceRange = None 
          IsCStylePattern = false; IsNakedRange = false
          HasVerticalBar = false; IsUnionField = false
          IsFirstTypeParam = false; IsInsideDotGet = false }

let rec addSpaceBeforeParensInFunCall functionOrMethod arg = 
    match functionOrMethod, arg with
    | _, ConstExpr(Const "()", _) ->
        false
    | SynExpr.LongIdent(_, LongIdentWithDots s, _, _), _ ->
        let parts = s.Split '.'
        not <| Char.IsUpper parts.[parts.Length - 1].[0]
    | SynExpr.Ident(_), SynExpr.Ident(_) ->
        true
    | SynExpr.Ident(Ident s), _ ->
        not <| Char.IsUpper s.[0]
    | SynExpr.TypeApp(e, _, _, _, _, _, _), _ ->
        addSpaceBeforeParensInFunCall e arg
    | _ -> true

let addSpaceBeforeParensInFunDef functionOrMethod args =
    match functionOrMethod, args with
    | _, PatParen (PatConst(Const "()", _)) -> false
    | "new", _ -> false
    | (s:string), _ -> 
        let parts = s.Split '.'
        not <| Char.IsUpper parts.[parts.Length - 1].[0]
    | _ -> true

let rec genParsedInput astContext = function
    | ImplFile im -> genImpFile astContext im
    | SigFile si -> genSigFile astContext si

(*
    See https://github.com/fsharp/FSharp.Compiler.Service/blob/master/src/fsharp/ast.fs#L1518
    hs = hashDirectives : ParsedHashDirective list 
    mns = modules : SynModuleOrNamespace list
*)
and genImpFile astContext (ParsedImplFileInput(hs, mns)) = 
    col sepNone hs genParsedHashDirective +> (if hs.IsEmpty then sepNone else sepNln)
    +> col sepNln mns (genModuleOrNamespace astContext)

and genSigFile astContext (ParsedSigFileInput(hs, mns)) =
    col sepNone hs genParsedHashDirective +> (if hs.IsEmpty then sepNone else sepNln)
    +> col sepNln mns (genSigModuleOrNamespace astContext)

and genParsedHashDirective (ParsedHashDirective(h, s, r)) =
    let printArgument arg =
        match arg with
        | "" -> sepNone
        // Use verbatim string to escape '\' correctly
        | _ when arg.Contains("\\") -> !- (sprintf "@\"%O\"" arg)
        | _ -> !- (sprintf "\"%O\"" arg)
        
    let printIdent (ctx:Context) =
        ctx.Trivia
        |> List.tryFind (fun t -> t.Range = r)
        |> Option.bind(fun t -> t.ContentBefore
                                |> List.choose (fun tc ->
                                    match tc with
                                    | Keyword({ TokenInfo = {TokenName = "KEYWORD_STRING"}; Content = c }) -> Some c
                                    | _ -> None)
                                |> List.tryHead)
        |> function
           | Some kw -> !- kw
           | None -> col sepSpace s printArgument
        <| ctx

    !- "#" -- h +> sepSpace +> printIdent
    |> genTrivia r

and genModuleOrNamespace astContext (ModuleOrNamespace(ats, px, ao, s, mds, isRecursive, moduleKind) as node) =
    let sepModuleAndFirstDecl =
        let firstDecl = List.tryHead mds
        match firstDecl with
        | None -> rep 2 sepNln
        | Some mdl ->
            sepNlnConsideringTriviaContentBefore mdl.Range +> sepNln

    let genTriviaForLongIdent (f: Context -> Context) =
        match node with
        | SynModuleOrNamespace.SynModuleOrNamespace(lid,_, SynModuleOrNamespaceKind.DeclaredNamespace,_,_,_,_,_) ->
            lid
            |> List.fold (fun (acc: Context -> Context) (ident:Ident) -> acc |> (genTrivia ident.idRange)) f
        | _ -> f

    let moduleOrNamespace = ifElse moduleKind.IsModule (!- "module ") (!- "namespace ")
    let recursive = ifElse isRecursive (!- "rec ") sepNone
    let namespaceFn = ifElse (s = "") (!- "global") (!- s)

    genPreXmlDoc px
    +> genAttributes astContext ats
    +> ifElse (moduleKind = AnonModule)
         sepNone
         (genTriviaForLongIdent (moduleOrNamespace +> opt sepSpace ao genAccess +> recursive +> namespaceFn +> sepModuleAndFirstDecl))
    +> genModuleDeclList astContext mds
    |> genTrivia node.Range

and genSigModuleOrNamespace astContext (SigModuleOrNamespace(ats, px, ao, s, mds, _, moduleKind) as node) =
    let range = match node with | SynModuleOrNamespaceSig(_,_,_,_,_,_,_,range) -> range
    let sepModuleAndFirstDecl =
        let firstDecl = List.tryHead mds
        match firstDecl with
        | None -> rep 2 sepNln
        | Some mdl ->
            sepNlnConsideringTriviaContentBefore mdl.Range +> sepNln
    
    genPreXmlDoc px
    +> genAttributes astContext ats
    +> ifElse (moduleKind = AnonModule) sepNone 
            (ifElse moduleKind.IsModule (!- "module ") (!- "namespace ")
                +> opt sepSpace ao genAccess -- s +> sepModuleAndFirstDecl)
    +> genSigModuleDeclList astContext mds
    |> genTrivia range

and genModuleDeclList astContext e =
    match e with
    | [x] -> genModuleDecl astContext x

    | OpenL(xs, ys) ->
        fun ctx ->
            let originalOpens =
                xs
                |> List.map (fun x -> x.Range, ctx.Trivia |> List.tryFind (fun t -> t.Range = x.Range))
            
            let xs = sortAndDeduplicate ((|Open|_|) >> Option.get) xs ctx
            
            // Restore the range of the open statement after sorting, this way comments stay on the same place.
            let xs' : SynModuleDecl list =
                if List.length xs = List.length originalOpens then
                    List.map2 (fun (range, trivia) (sortedOpen: SynModuleDecl) ->
                        match range <> sortedOpen.Range, trivia, sortedOpen with
                        | true, Some _, SynModuleDecl.Open(longDotId, _) ->
                            SynModuleDecl.Open(longDotId, range)
                        | _ -> sortedOpen
                            
                    ) originalOpens xs
                else
                    xs
            
            match ys with
            | [] -> col sepNln xs' (genModuleDecl astContext) ctx
            | _ ->
                let sepModuleDecl =
                    match List.tryHead ys with
                    | Some ysh ->
                        let attrs = getRangesFromAttributesFromModuleDeclaration ysh
                        sepNln +> sepNlnConsideringTriviaContentBeforeWithAttributes ysh.Range attrs
                    | None -> rep 2 sepNln
                
                (col sepNln xs' (genModuleDecl astContext) +> sepModuleDecl +> genModuleDeclList astContext ys) ctx

    | HashDirectiveL(xs, ys)
    | DoExprAttributesL(xs, ys) 
    | ModuleAbbrevL(xs, ys) 
    | OneLinerLetL(xs, ys) ->
        let sepXsYs =
            match List.tryHead ys with
            | Some ysh -> sepNln +> sepNlnConsideringTriviaContentBefore ysh.Range
            | None -> rep 2 sepNln
        
        match ys with
        | [] -> col sepNln xs (genModuleDecl astContext)
        | _ -> col sepNln xs (genModuleDecl astContext) +> sepXsYs +> genModuleDeclList astContext ys

    | MultilineModuleDeclL(xs, ys) ->
        match ys with
        | [] ->
            colEx (fun (mdl: SynModuleDecl) -> 
                let r = mdl.Range
                let ar = getRangesFromAttributesFromModuleDeclaration mdl
                sepNln +> sepNlnConsideringTriviaContentBeforeWithAttributes r ar
            ) xs (genModuleDecl astContext)
            
        | _ ->
            let sepXsYs =
                match List.tryHead ys with
                | Some ysh -> sepNln +> sepNlnConsideringTriviaContentBefore ysh.Range
                | None -> rep 2 sepNln
            
            let sepXs =
                colEx (fun (mdl: SynModuleDecl) ->
                    let r = mdl.Range
                    let ar = getRangesFromAttributesFromModuleDeclaration mdl
                    sepNln +> sepNlnConsideringTriviaContentBeforeWithAttributes r ar
                )

            sepXs xs (genModuleDecl astContext) +> sepXsYs +> genModuleDeclList astContext ys
    | _ -> sepNone    
    // |> genTrivia e , e is a list, genTrivia will probably need to occur after each item.

and genSigModuleDeclList astContext node =
    match node with
    | [x] -> genSigModuleDecl astContext x

    | SigOpenL(xs, ys) ->
        fun ctx ->
            let xs = sortAndDeduplicate ((|SigOpen|_|) >> Option.get) xs ctx
            match ys with
            | [] -> col sepNln xs (genSigModuleDecl astContext) ctx
            | _ -> (col sepNln xs (genSigModuleDecl astContext) +> rep 2 sepNln +> genSigModuleDeclList astContext ys) ctx

    | SigHashDirectiveL(xs, ys) ->
        match ys with
        | [] -> col sepNone xs (genSigModuleDecl astContext)
        | _ -> col sepNone xs (genSigModuleDecl astContext) +> sepNln +> genSigModuleDeclList astContext ys

    | SigModuleAbbrevL(xs, ys) 
    | SigValL(xs, ys) ->
        match ys with
        | [] -> col sepNln xs (genSigModuleDecl astContext)
        | _ ->
            let sepXsYs =
                match List.tryHead ys with
                | Some ysh -> sepNln +> sepNlnConsideringTriviaContentBefore ysh.Range
                | None -> rep 2 sepNln
            col sepNln xs (genSigModuleDecl astContext) +> sepXsYs +> genSigModuleDeclList astContext ys

    | SigMultilineModuleDeclL(xs, ys) ->
        match ys with
        | [] -> col (rep 2 sepNln) xs (genSigModuleDecl astContext)
        | _ -> col (rep 2 sepNln) xs (genSigModuleDecl astContext) +> rep 2 sepNln +> genSigModuleDeclList astContext ys

    | _ -> sepNone
    // |> genTrivia node, see genModuleDeclList

and genModuleDecl astContext node =
    match node with
    | Attributes(ats) ->
        col sepNone ats
            (fun a -> col sepNln a.Attributes (genAttribute astContext)
                      |> genTrivia a.Range)
    | DoExpr(e) ->
        genExpr astContext e
    | Exception(ex) ->
        genException astContext ex
    | HashDirective(p) -> 
        genParsedHashDirective p
    | Extern(ats, px, ao, t, s, ps) ->
        genPreXmlDoc px
        +> genAttributes astContext ats
        -- "extern " +> genType { astContext with IsCStylePattern = true } false t +> sepSpace +> opt sepSpace ao genAccess
        -- s +> sepOpenT +> col sepComma ps (genPat { astContext with IsCStylePattern = true }) +> sepCloseT
    // Add a new line after module-level let bindings
    | Let(b) ->
        genLetBinding { astContext with IsFirstChild = true } "let " b
    | LetRec(b::bs) ->
        let sepBAndBs =
            match List.tryHead bs with
            | Some b' ->
                let r = b'.RangeOfBindingSansRhs
                sepNln +> sepNlnConsideringTriviaContentBefore r
            | None -> id
        
        genLetBinding { astContext with IsFirstChild = true } "let rec " b
        +> sepBAndBs
        +> colEx (fun (b': SynBinding) ->
                let r = b'.RangeOfBindingSansRhs
                sepNln +> sepNlnConsideringTriviaContentBefore r
            ) bs (genLetBinding { astContext with IsFirstChild = false } "and ")

    | ModuleAbbrev(s1, s2) ->
        !- "module " -- s1 +> sepEq +> sepSpace -- s2
    | NamespaceFragment(m) ->
        failwithf "NamespaceFragment hasn't been implemented yet: %O" m
    | NestedModule(ats, px, ao, s, isRecursive, mds) -> 
        genPreXmlDoc px
        +> genAttributes astContext ats
        +> (!- "module ")
        +> opt sepSpace ao genAccess
        +> ifElse isRecursive (!- "rec ") sepNone -- s +> sepEq
        +> indent +> sepNln
        +> genModuleDeclList astContext mds +> unindent

    | Open(s) ->
        !- (sprintf "open %s" s)
    // There is no nested types and they are recursive if there are more than one definition
    | Types(t::ts) ->
        let sepTs =
            match List.tryHead ts with
            | Some tsh -> sepNln +> sepNlnConsideringTriviaContentBefore tsh.Range
            | None -> rep 2 sepNln
        
        genTypeDefn { astContext with IsFirstChild = true } t 
        +> colPre sepTs (rep 2 sepNln) ts (genTypeDefn { astContext with IsFirstChild = false })
    | md ->
        failwithf "Unexpected module declaration: %O" md
    |> genTrivia node.Range

and genSigModuleDecl astContext node =
    match node with
    | SigException(ex) ->
        genSigException astContext ex
    | SigHashDirective(p) -> 
        genParsedHashDirective p
    | SigVal(v) ->
        genVal astContext v
    | SigModuleAbbrev(s1, s2) ->
        !- "module " -- s1 +> sepEq +> sepSpace -- s2
    | SigNamespaceFragment(m) ->
        failwithf "NamespaceFragment is not supported yet: %O" m
    | SigNestedModule(ats, px, ao, s, mds) -> 
        genPreXmlDoc px
        +> genAttributes astContext ats -- "module " +> opt sepSpace ao genAccess -- s +> sepEq
        +> indent +> sepNln +> genSigModuleDeclList astContext mds +> unindent

    | SigOpen(s) ->
        !- (sprintf "open %s" s)
    | SigTypes(t::ts) ->
        genSigTypeDefn { astContext with IsFirstChild = true } t 
        +> colPre (rep 2 sepNln) (rep 2 sepNln) ts (genSigTypeDefn { astContext with IsFirstChild = false })
    | md ->
        failwithf "Unexpected module signature declaration: %O" md
    |> genTrivia node.Range

and genAccess (Access s) = !- s

and genAttribute astContext (Attribute(s, e, target)) =
    match e with
    // Special treatment for function application on attributes
    | ConstExpr(Const "()", _) -> 
        !- "[<" +> opt sepColonFixed target (!-) -- s -- ">]"
    | e -> 
        let argSpacing =
            if SourceTransformer.hasParenthesis e then id else sepSpace
        !- "[<"  +> opt sepColonFixed target (!-) -- s +> argSpacing +> genExpr astContext e -- ">]"
    |> genTrivia e.Range
    
and genAttributesCore astContext (ats: SynAttribute seq) =
    let genAttributeExpr astContext (Attribute(s, e, target) as attr) =
        match e with
        | ConstExpr(Const "()", _) -> 
            opt sepColonFixed target (!-) -- s
        | e ->
            let argSpacing =
                if SourceTransformer.hasParenthesis e then id else sepSpace
            opt sepColonFixed target (!-) -- s +> argSpacing +> genExpr astContext e
        |> genTrivia attr.Range
    ifElse (Seq.isEmpty ats) sepNone (!- "[<" +> col sepSemi ats (genAttributeExpr astContext) -- ">]")

and genOnelinerAttributes astContext ats =
    let ats = List.collect (fun a -> a.Attributes) ats
    ifElse (Seq.isEmpty ats) sepNone (genAttributesCore astContext ats +> sepSpace)

/// Try to group attributes if they are on the same line
/// Separate same-line attributes by ';'
/// Each bucket is printed in a different line
and genAttributes astContext (ats: SynAttributes) =
    ats
    |> List.fold (fun acc a ->
        fun (ctx:Context) ->
            let dontAddNewline =
                TriviaHelpers.``has content after that ends with``
                    (fun t -> t.Range = a.Range)
                    (function | Directive(_) -> true | _ -> false)
                    ctx.Trivia
            let chain =
                acc +>
                (genAttributesCore astContext a.Attributes |> genTrivia a.Range)
                +> ifElse dontAddNewline sepNone sepNln
            chain ctx
    ) sepNone

//    col sepNln ats
//            (fun a -> col sepNln a.Attributes (genAttribute astContext)
//                      |> genTrivia a.Range)
//    let genTriviaAttributeList (f: Context -> Context) =
//        Seq.foldBack (fun  (attr: SynAttributeList) (acc: Context -> Context) -> acc |> (genTrivia attr.Range)) ats f
//
//    (ats
//    |> List.collect (fun a -> a.Attributes)
//    |> Seq.groupBy (fun at -> at.Range.StartLine)
//    |> Seq.map snd
//    |> Seq.toList
//    |> fun ats' -> (colPost sepNln sepNln ats' (genAttributesCore astContext)))
//    |> genTriviaAttributeList

and genPreXmlDoc (PreXmlDoc lines) ctx = 
    if ctx.Config.StrictMode then
        colPost sepNln sepNln lines (sprintf "///%s" >> (!-)) ctx
    else ctx

and breakNln astContext brk e = 
    ifElse brk (indent +> sepNln +> genExpr astContext e +> unindent) 
        (indent +> autoNln (genExpr astContext e) +> unindent)

and breakNlnOrAddSpace astContext brk e =
    ifElse brk (indent +> sepNln +> genExpr astContext e +> unindent)
        (indent +> autoNlnOrSpace (genExpr astContext e) +> unindent)

/// Preserve a break even if the expression is a one-liner
and preserveBreakNln astContext e ctx =
    let brk = checkPreserveBreakForExpr e ctx || futureNlnCheck (genExpr astContext e) ctx
    breakNln astContext brk e ctx

and preserveBreakNlnOrAddSpace astContext e ctx =
    breakNlnOrAddSpace astContext (checkPreserveBreakForExpr e ctx) e ctx

and genExprSepEqPrependType astContext prefix (pat:SynPat) e ctx =
    let multilineCheck = 
        match e with
        | MatchLambda _ -> false
        | _ -> futureNlnCheck (genExpr astContext e) ctx
    match e with
    | TypedExpr(Typed, e, t) -> (prefix +> sepColon +> genType astContext false t +> sepEq
                                +> breakNlnOrAddSpace astContext (multilineCheck || checkPreserveBreakForExpr e ctx) e) ctx
    | e ->
        let hasCommentAfterEqual =
            ctx.Trivia
            |> List.exists (fun tn ->
                match tn.Type with
                | TriviaTypes.Token(tok) ->
                    tok.TokenInfo.TokenName = "EQUALS" && tn.Range.StartLine = pat.Range.StartLine
                | _ -> false
            )
        (prefix +> sepEq +> leaveEqualsToken pat.Range +> breakNlnOrAddSpace astContext (hasCommentAfterEqual || multilineCheck || checkPreserveBreakForExpr e ctx) e) ctx

/// Break but doesn't indent the expression
and noIndentBreakNln astContext e ctx = 
    ifElse (checkPreserveBreakForExpr e ctx) (sepNln +> genExpr astContext e) (autoNlnByFuture (genExpr astContext e)) ctx

and genTyparList astContext tps = 
    ifElse (List.atMostOne tps) (col wordOr tps (genTypar astContext)) (sepOpenT +> col wordOr tps (genTypar astContext) +> sepCloseT)

and genTypeAndParam astContext typeName tds tcs preferPostfix =
    let types openSep closeSep =
        (!- openSep +> coli sepComma tds (fun i decl -> genTyparDecl { astContext with IsFirstTypeParam = i = 0 } decl) 
         +> colPre (!- " when ") wordAnd tcs (genTypeConstraint astContext) -- closeSep)
    if List.isEmpty tds then !- typeName
    elif preferPostfix then !- typeName +> types "<" ">"
    elif List.atMostOne tds then types "" "" -- " " -- typeName
    else types "(" ")" -- " " -- typeName

and genTypeParamPostfix astContext tds tcs = genTypeAndParam astContext "" tds tcs true

and genLetBinding astContext pref b = 
    match b with 
    | LetBinding(ats, px, ao, isInline, isMutable, p, e) ->
        let prefix =
            genPreXmlDoc px
            +> ifElse astContext.IsFirstChild (genAttributes astContext ats -- pref) 
                (!- pref +> genOnelinerAttributes astContext ats)
            +> opt sepSpace ao genAccess
            +> ifElse isMutable (!- "mutable ") sepNone +> ifElse isInline (!- "inline ") sepNone
            +> genPat astContext p

        genExprSepEqPrependType astContext prefix p e

    | DoBinding(ats, px, e) ->
        let prefix = if pref.Contains("let") then pref.Replace("let", "do") else "do "
        genPreXmlDoc px
        +> genAttributes astContext ats -- prefix +> preserveBreakNln astContext e

    | b ->
        failwithf "%O isn't a let binding" b
    |> genTrivia b.RangeOfBindingSansRhs

and genShortGetProperty astContext (pat:SynPat) e = 
    genExprSepEqPrependType astContext !- "" pat e

and genProperty astContext prefix ao propertyKind ps e =
    let tuplerize ps =
        let rec loop acc = function
            | [p] -> (List.rev acc, p)
            | p1::ps -> loop (p1::acc) ps
            | [] -> invalidArg "p" "Patterns should not be empty"
        loop [] ps

    match ps with
    | [PatTuple ps] -> 
        let (ps, p) = tuplerize ps
        !- prefix +> opt sepSpace ao genAccess -- propertyKind
        +> ifElse (List.atMostOne ps) (col sepComma ps (genPat astContext) +> sepSpace) 
            (sepOpenT +> col sepComma ps (genPat astContext) +> sepCloseT +> sepSpace)
        +> genPat astContext p +> genExprSepEqPrependType astContext !- "" p e

    | ps ->
        let (_,p) = tuplerize ps
        !- prefix +> opt sepSpace ao genAccess -- propertyKind +> col sepSpace ps (genPat astContext) 
        +> genExprSepEqPrependType astContext !- "" p e
    |> genTrivia e.Range

and genPropertyWithGetSet astContext (b1, b2) =
    match b1, b2 with
    | PropertyBinding(ats, px, ao, isInline, mf1, PatLongIdent(ao1, s1, ps1, _), e1), 
      PropertyBinding(_, _, _, _, _, PatLongIdent(ao2, _, ps2, _), e2) ->
        let prefix =
            genPreXmlDoc px
            +> genAttributes astContext ats +> genMemberFlags astContext mf1
            +> ifElse isInline (!- "inline ") sepNone +> opt sepSpace ao genAccess
        assert(ps1 |> Seq.map fst |> Seq.forall Option.isNone)
        assert(ps2 |> Seq.map fst |> Seq.forall Option.isNone)
        let ps1 = List.map snd ps1
        let ps2 = List.map snd ps2
        prefix
        +> genTrivia b1.RangeOfBindingAndRhs
            (!- s1 +> indent +> sepNln
            +> genProperty astContext "with " ao1 "get " ps1 e1 +> sepNln) 
        +> genTrivia b2.RangeOfBindingAndRhs
            (genProperty astContext "and " ao2 "set " ps2 e2 +> unindent)
    | _ -> sepNone

/// Each member is separated by a new line.
and genMemberBindingList astContext node =
    match node with
    | [x] -> genMemberBinding astContext x

    | MultilineBindingL(xs, ys) ->
        let prefix = sepNln +> col (rep 2 sepNln) xs (function 
                                   | Pair(x1, x2) -> genPropertyWithGetSet astContext (x1, x2) 
                                   | Single x -> genMemberBinding astContext x)
        match ys with
        | [] -> prefix
        | _ -> prefix +> rep 2 sepNln +> genMemberBindingList astContext ys

    | OneLinerBindingL(xs, ys) ->
        match ys with
        | [] -> col sepNln xs (genMemberBinding astContext)
        | _ -> col sepNln xs (genMemberBinding astContext) +> sepNln +> genMemberBindingList astContext ys
    | _ -> sepNone

and genMemberBinding astContext b = 
    match b with 
    | PropertyBinding(ats, px, ao, isInline, mf, p, e) -> 
        let prefix =
            genPreXmlDoc px
            +> genAttributes astContext ats +> genMemberFlags astContext mf
            +> ifElse isInline (!- "inline ") sepNone +> opt sepSpace ao genAccess

        let propertyKind =
            match mf with
            | MFProperty PropertyGet -> "get "
            | MFProperty PropertySet -> "set "
            | mf -> failwithf "Unexpected member flags: %O" mf

        match p with
        | PatLongIdent(ao, s, ps, _) ->   
            assert (ps |> Seq.map fst |> Seq.forall Option.isNone)
            match ao, propertyKind, ps with
            | None, "get ", [_, PatParen(PatConst(Const "()", _))] ->
                // Provide short-hand notation `x.Member = ...` for `x.Member with get()` getters
                prefix -- s +> genShortGetProperty astContext p e
            | _ ->
                let ps = List.map snd ps              
                prefix -- s +> indent +> sepNln +> 
                genProperty astContext "with " ao propertyKind ps e
                +> unindent
        | p -> failwithf "Unexpected pattern: %O" p

    | MemberBinding(ats, px, ao, isInline, mf, p, e) ->
        let prefix =
            genPreXmlDoc px
            +> genAttributes astContext ats +> genMemberFlagsForMemberBinding astContext mf b.RangeOfBindingAndRhs
            +> ifElse isInline (!- "inline ") sepNone +> opt sepSpace ao genAccess +> genPat astContext p

        match e with
        | TypedExpr(Typed, e, t) -> prefix +> sepColon +> genType astContext false t +> sepEq +> preserveBreakNlnOrAddSpace astContext e
        | e -> prefix +> sepEq +> preserveBreakNlnOrAddSpace astContext e

    | ExplicitCtor(ats, px, ao, p, e, so) ->
        let prefix =
            genPreXmlDoc px
            +> genAttributes astContext ats
            +> opt sepSpace ao genAccess +> genPat astContext p 
            +> opt sepNone so (sprintf " as %s" >> (!-))

        match e with
        // Handle special "then" block i.e. fake sequential expressions in constructors
        | Sequential(e1, e2, false) -> 
            prefix +> sepEq +> indent +> sepNln 
            +> genExpr astContext e1 ++ "then " +> preserveBreakNln astContext e2 +> unindent

        | e -> prefix +> sepEq +> preserveBreakNlnOrAddSpace astContext e

    | b -> failwithf "%O isn't a member binding" b
    |> genTrivia b.RangeOfBindingSansRhs

and genMemberFlags astContext node =
    match node with
    | MFMember _ -> !- "member "
    | MFStaticMember _ -> !- "static member "
    | MFConstructor _ -> sepNone
    | MFOverride _ -> ifElse astContext.InterfaceRange.IsSome (!- "member ") (!- "override ")
    // |> genTrivia node check each case

and genMemberFlagsForMemberBinding astContext (mf:MemberFlags) (rangeOfBindingAndRhs: range) = 
    fun ctx ->
         match mf with
         | MFMember _
         | MFStaticMember _
         | MFConstructor _ -> 
            genMemberFlags astContext mf
         | MFOverride _ ->
             (fun (ctx: Context) ->
                ctx.Trivia
                |> List.tryFind(fun { Range = r}  -> r = rangeOfBindingAndRhs) //r.StartLine = rangeOfBindingAndRhs.StartLine && r.StartColumn < rangeOfBindingAndRhs.StartColumn)
                |> Option.bind(fun tn ->
                    tn.ContentBefore
                    |> List.choose (fun tc ->
                        match tc with
                        | Keyword({ Content = kw}) when (kw = "override" || kw = "default") -> Some (!- (sprintf "%s " kw))
                        | _ -> None)
                    |> List.tryHead
                )
                |> Option.defaultValue (!- "member ")
                <| ctx
             )
        <| ctx

and genVal astContext (Val(ats, px, ao, s, t, vi, _) as node) =
    let range = match node with | ValSpfn(_,_,_,_,_,_,_,_,_,_,range) -> range
    let (FunType namedArgs) = (t, vi)
    genPreXmlDoc px
    +> genAttributes astContext ats 
    +> atCurrentColumn (indent -- "val " +> opt sepSpace ao genAccess -- s 
                        +> sepColon +> genTypeList astContext namedArgs +> unindent)
    |> genTrivia range

and genRecordFieldName astContext (RecordFieldName(s, eo) as node) =
    let (rfn,_,_) = node
    let range = (fst rfn).Range
    opt sepNone eo (fun e -> !- s +> sepEq +> preserveBreakNlnOrAddSpace astContext e)
    |> genTrivia range

and genAnonRecordFieldName astContext (AnonRecordFieldName(s, e)) =
    !- s +> sepEq +> preserveBreakNlnOrAddSpace astContext e

and genTuple astContext es =
    atCurrentColumn (coli sepComma es (fun i -> 
            if i = 0 then genExpr astContext else noIndentBreakNln astContext
            |> addParenWhen (fun e ->
                match e with
                |ElIf _
                | SynExpr.Lambda _ -> true
                |_ -> false) // "if .. then .. else" have precedence over ","
        ))

and genExpr astContext synExpr = 
    let appNlnFun e =
        match e with
        | CompExpr _
        | Lambda _
        | MatchLambda _
        | Paren (Lambda _)
        | Paren (MatchLambda _) -> autoNln
        | _ -> autoNlnByFuture
    
    let sepOpenT = tokN synExpr.Range "LPAREN" sepOpenT
    let sepCloseT = tokN synExpr.Range "RPAREN" sepCloseT
    
    match synExpr with
    | SingleExpr(Lazy, e) -> 
        // Always add braces when dealing with lazy
        let addParens = hasParenthesis e || multiline e
        str "lazy "
        +> ifElse addParens id sepOpenT 
        +> breakNln astContext (multiline e) e
        +> ifElse addParens id sepCloseT
    | SingleExpr(kind, e) -> str kind +> genExpr astContext e
    | ConstExpr(c,r) -> genConst c r
    | NullExpr -> !- "null"
    // Not sure about the role of e1
    | Quote(_, e2, isRaw) ->         
        let e = genExpr astContext e2
        ifElse isRaw (!- "<@@ " +> e -- " @@>") (!- "<@ " +> e -- " @>")
    | TypedExpr(TypeTest, e, t) -> genExpr astContext e -- " :? " +> genType astContext false t
    | TypedExpr(New, e, t) -> 
        !- "new " +> genType astContext false t +> ifElse (hasParenthesis e) sepNone sepSpace +> genExpr astContext e
    | TypedExpr(Downcast, e, t) -> genExpr astContext e -- " :?> " +> genType astContext false t
    | TypedExpr(Upcast, e, t) -> genExpr astContext e -- " :> " +> genType astContext false t
    | TypedExpr(Typed, e, t) -> genExpr astContext e +> sepColon +> genType astContext false t
    | Tuple es -> genTuple astContext es
    | StructTuple es -> !- "struct " +> sepOpenT +> genTuple astContext es +> sepCloseT
    | ArrayOrList(isArray, [], _) -> 
        ifElse isArray (sepOpenAFixed +> sepCloseAFixed) (sepOpenLFixed +> sepCloseLFixed)
    | ArrayOrList(isArray, xs, isSimple) as alNode ->
        let isMultiline (ctx:Context) =
            xs
            |> List.fold (fun (isMultiline, f) e ->
                if isMultiline || futureNlnCheck (f +> genExpr astContext e) ctx then
                    true, sepNone
                else
                    false, f +> genExpr astContext e
            ) (false,sepNone)
            |> fst

        let sep = ifElse isSimple sepSemi sepSemiNln
        
        let hasLineCommentAfter range (ctx:Context) =
            ctx.Trivia
            |> List.tryFind (fun t -> t.Range = range)
            |> Option.map (fun t -> List.exists (fun tc -> match tc with | Comment(LineCommentAfterSourceCode(_)) -> true | _ -> false) t.ContentAfter)
            |> Option.defaultValue false

        let isLastItem (x:SynExpr) =
            List.tryLast xs
            |> Option.map (fun i -> i.Range = x.Range)
            |> Option.defaultValue false

        fun ctx ->
            let isArrayOrListMultiline = isMultiline ctx
            let expr =
                 xs
                 |> List.fold (fun acc e ->
                     fun (ctx: Context) ->
                        let isLastItem = isLastItem e
                        if isArrayOrListMultiline then
                            (acc +> genExpr astContext e +> ifElse isLastItem sepNone sepNln) ctx
                        else
                            let hasLineComment = hasLineCommentAfter e.Range ctx
                            let afterExpr = ifElse isLastItem sepNone (ifElse hasLineComment sepNln sep)
                            (acc +> genExpr astContext e +> afterExpr) ctx
                 ) sepNone
                 |> atCurrentColumn
            ifElse isArray (sepOpenA +> expr +> sepCloseA) (sepOpenL +> expr +> enterRightBracket alNode.Range +> sepCloseL)
            <| ctx


    | Record(inheritOpt, xs, eo) -> 
        let recordExpr = 
            let fieldsExpr = col sepSemiNln xs (genRecordFieldName astContext)
            eo |> Option.map (fun e ->
                genExpr astContext e +> ifElseCtx (futureNlnCheck fieldsExpr) (!- " with" +> indent +> sepNln +> fieldsExpr +> unindent) (!- " with " +> fieldsExpr))
            |> Option.defaultValue fieldsExpr

        sepOpenS
        +> (fun (ctx:Context) -> { ctx with RecordBraceStart = (ctx.Writer.Column)::ctx.RecordBraceStart })
        +> atCurrentColumnIndent (leaveLeftBrace synExpr.Range +> opt (if xs.IsEmpty then sepNone else ifElseCtx (futureNlnCheck recordExpr) sepNln sepSemi) inheritOpt
            (fun (typ, expr) -> !- "inherit " +> genType astContext false typ +> genExpr astContext expr) +> recordExpr)
        +> (fun ctx ->
            match ctx.RecordBraceStart with
            | rbs::rest ->
                if ctx.Writer.Column < rbs then
                    let offset = (if ctx.Config.SpaceAroundDelimiter then 2 else 1) + 1
                    let delta = Math.Max((rbs - ( ctx.Writer.Column)) - offset, 0)
                    (!- System.String.Empty.PadRight(delta)) ({ctx with RecordBraceStart = rest})
                else
                    sepNone ({ctx with RecordBraceStart = rest})
            | [] ->
                    sepNone ctx)
        +> sepCloseS

    | AnonRecord(isStruct, fields, copyInfo) -> 
        let recordExpr = 
            let fieldsExpr = col sepSemiNln fields (genAnonRecordFieldName astContext)
            copyInfo |> Option.map (fun e ->
                genExpr astContext e +> ifElseCtx (futureNlnCheck fieldsExpr) (!- " with" +> indent +> sepNln +> fieldsExpr +> unindent) (!- " with " +> fieldsExpr))
            |> Option.defaultValue fieldsExpr
        ifElse isStruct !- "struct " sepNone 
        +> sepOpenAnonRecd
        +> atCurrentColumnIndent recordExpr
        +> sepCloseAnonRecd

    | ObjExpr(t, eio, bd, ims, range) ->
        // Check the role of the second part of eio
        let param = opt sepNone (Option.map fst eio) (genExpr astContext)
        sepOpenS +> 
        atCurrentColumn (!- "new " +> genType astContext false t +> param -- " with" 
            +> indent +> sepNln +> genMemberBindingList { astContext with InterfaceRange = Some range } bd +> unindent
            +> colPre sepNln sepNln ims (genInterfaceImpl astContext)) +> sepCloseS

    | While(e1, e2) -> 
        atCurrentColumn (!- "while " +> genExpr astContext e1 -- " do" 
        +> indent +> sepNln +> genExpr astContext e2 +> unindent)

    | For(s, e1, e2, e3, isUp) ->
        atCurrentColumn (!- (sprintf "for %s = " s) +> genExpr astContext e1 
            +> ifElse isUp (!- " to ") (!- " downto ") +> genExpr astContext e2 -- " do" 
            +> indent +> sepNln +> genExpr astContext e3 +> unindent)

    // Handle the form 'for i in e1 -> e2'
    | ForEach(p, e1, e2, isArrow) ->
        atCurrentColumn (!- "for " +> genPat astContext p -- " in " +> genExpr { astContext with IsNakedRange = true } e1 
            +> ifElse isArrow (sepArrow +> preserveBreakNln astContext e2) (!- " do" +> indent +> sepNln +> genExpr astContext e2 +> unindent))

    | CompExpr(isArrayOrList, e) ->
        let astContext = { astContext with IsNakedRange = true }
        ifElse isArrayOrList (genExpr astContext e) 
            (sepOpenS +> noIndentBreakNln astContext e 
             +> ifElse (checkBreakForExpr e) (unindent +> sepNln +> sepCloseSFixed) sepCloseS) 

    | ArrayOrListOfSeqExpr(isArray, e) as aNode ->
        let astContext = { astContext with IsNakedRange = true }
        let expr =
            ifElse isArray
                (sepOpenA +> genExpr astContext e +> enterRightBracket aNode.Range +> sepCloseA)
                (sepOpenL +> genExpr astContext e +> enterRightBracket aNode.Range +> sepCloseL)
        expr
    | JoinIn(e1, e2) -> genExpr astContext e1 -- " in " +> genExpr astContext e2
    | Paren(DesugaredLambda(cps, e)) ->
        sepOpenT -- "fun " +>  col sepSpace cps (genComplexPats astContext) +> sepArrow +> noIndentBreakNln astContext e +> sepCloseT
    | DesugaredLambda(cps, e) -> 
        !- "fun " +>  col sepSpace cps (genComplexPats astContext) +> sepArrow +> preserveBreakNln astContext e 
    | Paren(Lambda(e, sps)) ->
        sepOpenT -- "fun " +> col sepSpace sps (genSimplePats astContext) +> sepArrow +> noIndentBreakNln astContext e +> sepCloseT
    // When there are parentheses, most likely lambda will appear in function application
    | Lambda(e, sps) -> 
        !- "fun " +> col sepSpace sps (genSimplePats astContext) +> sepArrow +> preserveBreakNln astContext e
    | MatchLambda(sp, _) -> !- "function " +> colPre sepNln sepNln sp (genClause astContext true)
    | Match(e, cs) -> 
        atCurrentColumn (!- "match " +> genExpr astContext e -- " with" +> colPre sepNln sepNln cs (genClause astContext true))
    | MatchBang(e, cs) -> 
        atCurrentColumn (!- "match! " +> genExpr astContext e -- " with" +> colPre sepNln sepNln cs (genClause astContext true))    
    | TraitCall(tps, msg, e) -> 
        genTyparList astContext tps +> sepColon +> sepOpenT +> genMemberSig astContext msg +> sepCloseT 
        +> sepSpace +> genExpr astContext e

    | Paren (ILEmbedded r) -> 
        // Just write out original code inside (# ... #) 
        fun ctx -> !- (defaultArg (lookup r ctx) "") ctx
    | Paren e -> 
        // Parentheses nullify effects of no space inside DotGet
        sepOpenT +> genExpr { astContext with IsInsideDotGet = false } e +> sepCloseT
    | CompApp(s, e) ->
        !- s +> sepSpace +> sepOpenS +> genExpr { astContext with IsNakedRange = true } e 
        +> ifElse (checkBreakForExpr e) (sepNln +> sepCloseSFixed) sepCloseS
    // This supposes to be an infix function, but for some reason it isn't picked up by InfixApps
    | App(Var "?", e::es) ->
        match es with
        | SynExpr.Const(SynConst.String(_,_),_)::_ ->
            genExpr astContext e -- "?" +> col sepSpace es (genExpr astContext)
        | _ ->
            genExpr astContext e -- "?" +> sepOpenT +> col sepSpace es (genExpr astContext) +> sepCloseT

    | App(Var "..", [e1; e2]) ->
        let expr = genExpr astContext e1 +> sepSpace -- ".." +> sepSpace +> genExpr astContext e2
        ifElse astContext.IsNakedRange expr (sepOpenS +> expr +> sepCloseS)
    | App(Var ".. ..", [e1; e2; e3]) -> 
        let expr = genExpr astContext e1 +> sepSpace -- ".." +> sepSpace +> genExpr astContext e2 +> sepSpace -- ".." +> sepSpace +> genExpr astContext e3
        ifElse astContext.IsNakedRange expr (sepOpenS +> expr +> sepCloseS)
    // Separate two prefix ops by spaces
    | PrefixApp(s1, PrefixApp(s2, e)) -> !- (sprintf "%s %s" s1 s2) +> genExpr astContext e
    | PrefixApp(s, e) -> !- s +> genExpr astContext e
    // Handle spaces of infix application based on which category it belongs to
    | InfixApps(e, es) ->
        // Only put |> on the same line in a very trivial expression
        atCurrentColumn (genExpr astContext e +> genInfixApps astContext (checkNewLine e es) es)

    | TernaryApp(e1,e2,e3) -> 
        atCurrentColumn (genExpr astContext e1 +> !- "?" +> genExpr astContext e2 +> sepSpace +> !- "<-" +> sepSpace +> genExpr astContext e3)

    // This filters a few long examples of App
    | DotGetAppSpecial(s, es) ->
        !- s 
        +> atCurrentColumn 
             (colAutoNlnSkip0 sepNone es (fun ((s,r), e) ->
                sepNlnIfTriviaBefore r +>
                ((!- (sprintf ".%s" s) |> genTrivia r) 
                    +> ifElse (hasParenthesis e) sepNone sepSpace +> genExpr astContext e)
                ))

    | DotGetApp(e, es) as appNode ->
        fun (ctx: Context) ->
            // find all the lids recursively + range of do expr
            let dotGetFuncExprIdents =
                let rec selectIdent appNode =
                    match appNode with
                    | SynExpr.App(_,_,(SynExpr.DotGet(_,_,LongIdentWithDots.LongIdentWithDots(lids,_),_) as dotGet), argExpr,_) ->
                        let lids = List.map (fun lid -> (argExpr.Range, lid)) lids
                        let childLids = selectIdent dotGet
                        lids @ childLids
                    | SynExpr.DotGet(aExpr,_,_,_) ->
                        selectIdent aExpr
                    | _ -> []
                selectIdent appNode

            let hasLineCommentAfterExpression (currentLine) =
                let findTrivia tn = tn.Range.EndLine = currentLine
                let predicate = function | Comment _ -> true | _ -> false
                TriviaHelpers.``has content after after that matches`` findTrivia predicate ctx.Trivia

            let lineCommentsAfter =
                [ yield (e.Range.EndLine, hasLineCommentAfterExpression e.Range.EndLine)
                  yield! (es |> List.map (fun ((_,re'),_) -> re'.EndLine , hasLineCommentAfterExpression re'.EndLine)) ]
                |> Map.ofList

            let hasLineCommentOn lineNumber =
                Map.tryFind lineNumber lineCommentsAfter
                |> Option.defaultValue false

            let dotGetExprRange = e.Range

            let expr =
                match e with
                | App(e1, [e2]) ->
                    noNln (genExpr astContext e1 +> ifElse (hasParenthesis e2) sepNone sepSpace +> genExpr astContext e2)
                | _ ->
                    genExpr astContext e

            expr
            +> indent
            +> (col sepNone es (fun ((s,_), e) ->
                    let currentExprRange = e.Range
                    let genTriviaOfIdent =
                        dotGetFuncExprIdents
                        |> List.tryFind (fun (er, _) -> er = e.Range)
                        |> Option.map (snd >> (fun lid -> genTrivia lid.idRange))
                        |> Option.defaultValue (id)

                    let writeExpr = ((genTriviaOfIdent (!- (sprintf ".%s" s))) +> ifElse (hasParenthesis e) sepNone sepSpace
                                     +> (fun ctx -> ctx |> ifElse (futureNlnCheck (genExpr astContext e) ctx) sepNln sepNone)
                                     +> genExpr astContext e)

                    let addNewlineIfNeeded (ctx: Context) =
                        if ctx.Config.KeepNewlineAfter then
                            let willAddAutoNewline:bool =
                                autoNlnCheck writeExpr sepNone ctx

                            let expressionOnNextLine = dotGetExprRange.StartLine < currentExprRange.StartLine
                            let addNewline = (not willAddAutoNewline) && expressionOnNextLine

                            ctx
                            |> ifElse addNewline sepNln sepNone
                        else
                            // If the line before ended with a line comment, it should add a newline
                            (ifElse (hasLineCommentOn (currentExprRange.EndLine - 1)) sepNln sepNone) ctx

                    addNewlineIfNeeded +> autoNln writeExpr))
            +> unindent
            <| ctx

    // Unlike infix app, function application needs a level of indentation
    | App(e1, [e2]) ->
        let hasPar = hasParenthesis e2
        let addSpaceBefore = addSpaceBeforeParensInFunCall e1 e2
        atCurrentColumn (genExpr astContext e1 +> 
            ifElse (not astContext.IsInsideDotGet)
                (ifElse hasPar
                    (ifElse addSpaceBefore sepBeforeArg sepNone)
                    sepSpace)
                sepNone
            +> indent +> (ifElse (not hasPar && addSpaceBefore) sepSpace sepNone) +> appNlnFun e2 (genExpr astContext e2) +> unindent)

    // Always spacing in multiple arguments
    | App(e, es) -> 
        atCurrentColumn (genExpr astContext e +> 
            colPre sepSpace sepSpace es (fun e ->
                indent +> appNlnFun e (genExpr astContext e) +> unindent))

    | TypeApp(e, ts) -> genExpr astContext e -- "<" +> col sepComma ts (genType astContext false) -- ">"
    | LetOrUses(bs, e) ->
        let isFromAst (ctx: Context) = ctx.Content = String.Empty
        let isInSameLine ctx =
            match bs with
            | [_, LetBinding(_, _, _, _, _, p, _)] -> 
                not (isFromAst ctx) && p.Range.EndLine = e.Range.StartLine && not(checkBreakForExpr e)
            | _ -> false

        let sepNlnBeforeExpr =
            match e with
            | SynExpr.Sequential(_,_,e1,_,_) -> sepNlnConsideringTriviaContentBefore e1.Range
            | _ -> (sepNlnConsideringTriviaContentBefore e.Range)

        atCurrentColumn (genLetOrUseList astContext bs +> ifElseCtx isInSameLine (!- " in ") sepNlnBeforeExpr  +> genExpr astContext e)

    // Could customize a bit if e is single line
    | TryWith(e, cs) -> 
        let prefix = !- "try " +> indent +> sepNln +> genExpr astContext e +> unindent ++ "with"
        match cs with
        | [SynMatchClause.Clause(SynPat.Or(_,_,_),_,_,_,_)] ->
            atCurrentColumn (prefix +> indentOnWith +> sepNln +> col sepNln cs (genClause astContext true) +> unindentOnWith)
        | [c] ->
            atCurrentColumn (prefix +> sepSpace +> genClause astContext false c)
        | _ -> 
            atCurrentColumn (prefix +> indentOnWith +> sepNln +> col sepNln cs (genClause astContext true) +> unindentOnWith)

    | TryFinally(e1, e2) -> 
        atCurrentColumn (!- "try " +> indent +> sepNln +> genExpr astContext e1 +> unindent ++ "finally" 
            +> indent +> sepNln +> genExpr astContext e2 +> unindent)    

    | SequentialSimple es -> atCurrentColumn (colAutoNlnSkip0 sepSemi es (genExpr astContext))
    // It seems too annoying to use sepSemiNln
    | Sequentials es ->
        // This is one of those weird situations where the newlines need to printed before atCurrentColumn
        // If the newline would be printed in a AtCurrentColumn block that code would be started too far of.
        // See https://github.com/fsprojects/fantomas/issues/478
        let firstNewline (ctx: Context) =
            es
            |> List.tryHead
            |> Option.bind (fun e -> TriviaHelpers.findByRange ctx.Trivia e.Range)
            |> fun cb ->
                match cb with
                | Some ({ ContentBefore = Newline::rest } as tn)  ->
                    let updatedTriviaNodes =
                        ctx.Trivia
                        |> List.map (fun t ->
                            if t = tn then
                                { tn with ContentBefore = rest }
                            else t
                        )

                    let ctx' = { ctx with Trivia = updatedTriviaNodes }
                    printTriviaContent Newline ctx'
                | _ -> sepNone ctx

        firstNewline +> atCurrentColumn (col sepSemiNln es (genExpr astContext))
    
    | IfThenElse(e1, e2, None) -> 
        atCurrentColumn (!- "if " +> ifElse (checkBreakForExpr e1) (genExpr astContext e1 ++ "then") (genExpr astContext e1 +- "then") 
                         -- " " +> preserveBreakNln astContext e2)
    // A generalization of IfThenElse
    | ElIf((e1,e2, _, fullRange, _)::es, enOpt) ->
        let printIfKeyword separator fallback range (ctx:Context) =
            ctx.Trivia
            |> List.tryFind (fun {Range = r} -> r = range)
            |> Option.bind (fun tv ->
                tv.ContentBefore
                |> List.map (function | Keyword kw -> Some kw | _ -> None)
                |> List.choose id
                |> List.tryHead
            )
            |> Option.map (fun ({Content = kw}) -> sprintf "%s " kw |> separator)
            |> Option.defaultValue (separator fallback)
            <| ctx
        
        let ifTokenKw r f s = tokN r "IF" (printIfKeyword f s r)
        let ifToken r f = tokN r "IF" f
        let thenToken r f = tokN r "THEN" f
        let elseToken r f = tokN r "ELSE" f

        let anyExpressIsMultiline =
            multiline e2 || (Option.map multiline enOpt |> Option.defaultValue false) || (List.exists (fun (_, e, _, _, _) -> multiline e) es)

        let printBranch prefix astContext expr = prefix +> ifElse anyExpressIsMultiline (breakNln astContext true expr) (preserveBreakNln astContext expr)
        
        // track how many indents was called, so we can correctly unindent.
        // TODO: do it without mutable
        let mutable indented = 0

        atCurrentColumn (
            ifToken fullRange !-"if " +> ifElse (checkBreakForExpr e1) (genExpr astContext e1 +> thenToken fullRange !+"then") (genExpr astContext e1 +> thenToken fullRange !+-"then") -- " "
            +> printBranch id astContext e2
            +> fun ctx -> colPost (rep indented unindent) sepNone es (fun (e1, e2, _, fullRangeInner, node) ->
                                 let rangeBeforeInner = mkRange "" fullRange.Start fullRangeInner.Start
                                 let elsePart =
                                     ifTokenKw fullRangeInner (fun kw ctx ->
                                         let hasContentBeforeIf =
                                             ctx.Trivia
                                             |> List.tryFind (fun tv -> tv.Range = fullRangeInner)
                                             |> Option.map (fun tv ->
                                                 tv.ContentBefore
                                                 |> List.exists (fun cb ->
                                                     match cb with
                                                     | Comment(_) ->  true
                                                     | _ -> false
                                                )
                                             )
                                             |> Option.defaultValue false

                                         // Trivia knows if the keyword is "elif" or "else if"
                                         // Next we need to be sure that the are no comments between else and if
                                         match kw with
                                         | "if " when hasContentBeforeIf ->
                                             indented <- indented + 1
                                             (elseToken rangeBeforeInner !+"else" +> indent +> sepNln +> genTrivia fullRangeInner (ifToken node.Range !-"if "))
                                         | "if " ->
                                             (elseToken rangeBeforeInner !+"else if ")
                                         | _ (* "elif" *) ->
                                            !+ kw
                                        <| ctx
                                     ) "if "

                                 elsePart +>
                                 genTrivia node.Range (ifElse (checkBreakForExpr e1)
                                                           (genExpr astContext e1 +> thenToken node.Range !+"then")
                                                           (genExpr astContext e1 +> thenToken node.Range !+-"then")
                                                       -- " " +> printBranch id astContext e2)
                            ) ctx
            +> opt sepNone enOpt (fun en -> printBranch (elseToken fullRange !+~"else ") astContext en)
        )

    // At this stage, all symbolic operators have been handled.
    | OptVar(s, isOpt) -> ifElse isOpt (!- "?") sepNone -- s
    | LongIdentSet(s, e, _) -> 
        !- (sprintf "%s <- " s) +> autoIndentNlnByFuture (genExpr astContext e)
    | DotIndexedGet(e, es) -> addParenIfAutoNln e (genExpr astContext) -- "." +> sepOpenLFixed +> genIndexers astContext es +> sepCloseLFixed
    | DotIndexedSet(e1, es, e2) -> addParenIfAutoNln e1 (genExpr astContext) -- ".[" +> genIndexers astContext es -- "] <- " +> genExpr astContext e2
    | DotGet(e, (s,_)) -> 
        let exprF = genExpr { astContext with IsInsideDotGet = true }
        addParenIfAutoNln e exprF -- (sprintf ".%s" s)
    | DotSet(e1, s, e2) -> addParenIfAutoNln e1 (genExpr astContext) -- sprintf ".%s <- " s +> genExpr astContext e2

    | SynExpr.Set(e1,e2, _) ->
        addParenIfAutoNln e1 (genExpr astContext) -- sprintf " <- " +> genExpr astContext e2
        
    | LetOrUseBang(isUse, p, e1, e2) ->
        atCurrentColumn (ifElse isUse (!- "use! ") (!- "let! ") 
            +> genPat astContext p -- " = " +> genExpr astContext e1 +> sepNln +> genExpr astContext e2)

    | ParsingError r -> 
        raise <| FormatException (sprintf "Parsing error(s) between line %i column %i and line %i column %i" 
            r.StartLine (r.StartColumn + 1) r.EndLine (r.EndColumn + 1))
    | UnsupportedExpr r -> 
        raise <| FormatException (sprintf "Unsupported construct(s) between line %i column %i and line %i column %i" 
            r.StartLine (r.StartColumn + 1) r.EndLine (r.EndColumn + 1))
    | e -> failwithf "Unexpected expression: %O" e
    |> genTrivia synExpr.Range

and genLetOrUseList astContext expr =
    match expr with
    | [p, x] -> genLetBinding { astContext with IsFirstChild = true } p x
    | OneLinerLetOrUseL(xs, ys) ->
        let sepXsYs =
            match List.tryHead ys with
            | Some (_,ysh) -> sepNln +> sepNlnConsideringTriviaContentBefore ysh.RangeOfBindingSansRhs
            | None -> rep 2 sepNln

        match ys with
        | [] -> 
            col sepNln xs (fun (p, x) -> genLetBinding { astContext with IsFirstChild = p <> "and" } p x)
        | _ ->
            colEx (fun (_,lx:SynBinding) -> sepNlnConsideringTriviaContentBefore lx.RangeOfBindingSansRhs) xs (fun (p, x) -> genLetBinding { astContext with IsFirstChild = p <> "and" } p x)
            +> sepXsYs +> genLetOrUseList astContext ys

    | MultilineLetOrUseL(xs, ys) ->
        match ys with
        | [] -> 
            colEx (fun (_,synB:SynBinding) -> sepNln +> sepNlnConsideringTriviaContentBefore synB.RangeOfBindingSansRhs) xs (fun (p, x) -> genLetBinding { astContext with IsFirstChild = p <> "and" } p x)
            // Add a trailing new line to separate these with the main expression
            +> sepNln
        | _ ->
            let sepXsYs =
                match List.tryHead ys with
                | Some (_,ysh) -> sepNln +> sepNlnConsideringTriviaContentBefore ysh.RangeOfBindingSansRhs
                | None -> rep 2 sepNln

            colEx (fun (_,lx:SynBinding) -> sepNlnConsideringTriviaContentBefore lx.RangeOfBindingAndRhs) xs (fun (p, x) -> genLetBinding { astContext with IsFirstChild = p <> "and" } p x)
            +> sepXsYs +> genLetOrUseList astContext ys

    | _ -> sepNone

/// When 'hasNewLine' is set, the operator is forced to be in a new line
and genInfixApps astContext hasNewLine synExprs = 
    match synExprs with
    | (s, opE, e)::es when (NoBreakInfixOps.Contains s) -> 
        (sepSpace +> tok opE.Range s
         +> (fun ctx ->
                let isEqualOperator =
                    match opE with
                    | SynExpr.Ident(Ident("op_Equality")) -> true
                    | _ -> false
                let genExpr =
                    if isEqualOperator && (futureNlnCheck (genExpr astContext e) ctx) then
                        indent +> sepNln +> genExpr astContext e +> unindent
                    else
                        sepSpace +> genExpr astContext e
                genExpr ctx))
        +> genInfixApps astContext (hasNewLine || checkNewLine e es) es
    | (s, opE, e)::es when(hasNewLine) ->
        (sepNln +> tok opE.Range s +> sepSpace +> genExpr astContext e)
        +> genInfixApps astContext (hasNewLine || checkNewLine e es) es
    | (s, opE, e)::es when(NoSpaceInfixOps.Contains s) ->
        let wrapExpr f =
            match synExprs with
            | ("?", SynExpr.Ident(Ident("op_Dynamic")), SynExpr.Ident(_))::_ ->
                sepOpenT +> f +> sepCloseT
            | _ -> f
        (tok opE.Range s +> autoNln (wrapExpr (genExpr astContext e)))
        +> genInfixApps astContext (hasNewLine || checkNewLine e es) es
    | (s, opE, e)::es ->
        (sepSpace +> autoNln (tok opE.Range s +> sepSpace +> genCommentsAfterInfix (Some opE.Range) +> genExpr astContext e))
        +> genInfixApps astContext (hasNewLine || checkNewLine e es) es
    | [] -> sepNone

/// Use in indexed set and get only
and genIndexers astContext node =
    match node with
    | Indexer(Pair(IndexedVar eo1, IndexedVar eo2)) :: es ->
        ifElse (eo1.IsNone && eo2.IsNone) (!- "*") 
            (opt sepNone eo1 (genExpr astContext) -- ".." +> opt sepNone eo2 (genExpr astContext))
        +> ifElse es.IsEmpty sepNone (sepComma +> genIndexers astContext es)
    | Indexer(Single(IndexedVar eo)) :: es -> 
        ifElse eo.IsNone (!- "*") (opt sepNone eo (genExpr astContext))
        +> ifElse es.IsEmpty sepNone (sepComma +> genIndexers astContext es)
    | Indexer(Single e) :: es -> 
            genExpr astContext e +> ifElse es.IsEmpty sepNone (sepComma +> genIndexers astContext es)
    | _ -> sepNone
    // |> genTrivia node, it a list

and genTypeDefn astContext (TypeDef(ats, px, ao, tds, tcs, tdr, ms, s, preferPostfix) as node) = 
    let typeName = 
        genPreXmlDoc px 
        +> ifElse astContext.IsFirstChild (genAttributes astContext ats -- "type ") 
            (!- "and " +> genOnelinerAttributes astContext ats) 
        +> opt sepSpace ao genAccess
        +> genTypeAndParam astContext s tds tcs preferPostfix

    match tdr with
    | Simple(TDSREnum ecs) ->
        typeName +> sepEq 
        +> indent +> sepNln
        +> genTrivia tdr.Range
            (col sepNln ecs (genEnumCase { astContext with HasVerticalBar = true })
            +> genMemberDefnList { astContext with InterfaceRange = None } ms
            // Add newline after un-indent to be spacing-correct
            +> unindent)

    | Simple(TDSRUnion(ao', xs) as unionNode) ->
        let sepNlnBasedOnTrivia =
            fun (ctx: Context) ->
                let trivia =
                    ctx.Trivia
                    |> List.tryFind (fun t -> t.Range = unionNode.Range && not (List.isEmpty t.ContentBefore))
                    
                match trivia with
                | Some _ -> sepNln
                | None -> sepNone
                <| ctx
        
        let unionCases =  
            match xs with
            | [] -> id
            | [x] when List.isEmpty ms -> 
                indent +> sepSpace +> sepNlnBasedOnTrivia
                +> genTrivia tdr.Range
                    (opt sepSpace ao' genAccess
                    +> genUnionCase { astContext with HasVerticalBar = false } x)
            | xs ->
                indent +> sepNln
                +> genTrivia tdr.Range
                    (opt sepNln ao' genAccess 
                    +> col sepNln xs (genUnionCase { astContext with HasVerticalBar = true }))

        typeName +> sepEq
        +> unionCases +> genMemberDefnList { astContext with InterfaceRange = None } ms
        +> unindent

    | Simple(TDSRRecord(ao', fs)) ->
        typeName +> sepEq 
        +> indent +> sepNln +> opt sepSpace ao' genAccess
        +> genTrivia tdr.Range
            (sepOpenS 
            +> atCurrentColumn (leaveLeftBrace tdr.Range +> col sepSemiNln fs (genField astContext "")) +> sepCloseS
            +> genMemberDefnList { astContext with InterfaceRange = None } ms
            +> unindent)

    | Simple TDSRNone -> 
        typeName
    | Simple(TDSRTypeAbbrev t) -> 
        typeName +> sepEq +> sepSpace
        +> genTrivia tdr.Range
            (genType astContext false t
            +> ifElse (List.isEmpty ms) (!- "") 
                (indent ++ "with" +> indent +> genMemberDefnList { astContext with InterfaceRange = None } ms
            +> unindent +> unindent))
    | Simple(TDSRException(ExceptionDefRepr(ats, px, ao, uc))) ->
        genExceptionBody astContext ats px ao uc
        |> genTrivia tdr.Range

    | ObjectModel(TCSimple (TCInterface | TCClass) as tdk, MemberDefnList(impCtor, others), range) ->
        let interfaceRange =
            match tdk with
            | TCSimple TCInterface -> Some range
            | _ -> None
        let astContext = { astContext with InterfaceRange = interfaceRange }
        typeName +> opt sepNone impCtor (genMemberDefn astContext) +> sepEq
        +> indent +> sepNln
        +> genTrivia tdr.Range
            (genTypeDefKind tdk
            +> indent +> genMemberDefnList astContext others +> unindent
            ++ "end")
        +> unindent
    
    | ObjectModel(TCSimple (TCStruct) as tdk, MemberDefnList(impCtor, others), _) ->
        let sepMem =
            match ms with
            | [] -> sepNone
            | _ -> sepNln
        typeName +> opt sepNone impCtor (genMemberDefn astContext) +> sepEq 
        +> indent +> sepNln 
        +> genTrivia tdr.Range
            (genTypeDefKind tdk
            +> indent +> genMemberDefnList astContext others +> unindent
            ++ "end"
            +> sepMem
            // Prints any members outside the struct-end construct
            +> genMemberDefnList astContext ms)
        +> unindent
    
    | ObjectModel(TCSimple TCAugmentation, _, _) ->
        typeName -- " with" +> indent
        // Remember that we use MemberDefn of parent node
        +> genTrivia tdr.Range (genMemberDefnList { astContext with InterfaceRange = None } ms)
        +> unindent

    | ObjectModel(TCDelegate(FunType ts), _, _) ->
        typeName +> sepEq +> sepSpace +> genTrivia tdr.Range (!- "delegate of " +> genTypeList astContext ts)
    
    | ObjectModel(TCSimple TCUnspecified, MemberDefnList(impCtor, others), _) when not(List.isEmpty ms) ->
        typeName +> opt sepNone impCtor (genMemberDefn { astContext with InterfaceRange = None }) +> sepEq +> indent
        +> genTrivia tdr.Range
            (genMemberDefnList { astContext with InterfaceRange = None } others +> sepNln
            -- "with" +> indent
            +> genMemberDefnList { astContext with InterfaceRange = None } ms +> unindent)
        +> unindent
    
    | ObjectModel(_, MemberDefnList(impCtor, others), _) ->
        typeName +> opt sepNone impCtor (genMemberDefn { astContext with InterfaceRange = None }) +> sepEq
        +> indent
        +> genMemberDefnList { astContext with InterfaceRange = None } others
        +> unindent

    | ExceptionRepr(ExceptionDefRepr(ats, px, ao, uc)) ->
        genExceptionBody astContext ats px ao uc
        |> genTrivia tdr.Range
    |> genTrivia node.Range

and genSigTypeDefn astContext (SigTypeDef(ats, px, ao, tds, tcs, tdr, ms, s, preferPostfix) as node) =
    let range = match node with | SynTypeDefnSig.TypeDefnSig(_,_,_,r) -> r
    let typeName = 
        genPreXmlDoc px 
        +> ifElse astContext.IsFirstChild (genAttributes astContext ats -- "type ") 
            (!- "and " +> genOnelinerAttributes astContext ats) 
        +> opt sepSpace ao genAccess
        +> genTypeAndParam astContext s tds tcs preferPostfix

    match tdr with
    | SigSimple(TDSREnum ecs) ->
        typeName +> sepEq 
        +> indent +> sepNln
        +> col sepNln ecs (genEnumCase { astContext with HasVerticalBar = true })
        +> colPre sepNln sepNln ms (genMemberSig astContext)
        // Add newline after un-indent to be spacing-correct
        +> unindent
         
    | SigSimple(TDSRUnion(ao', xs)) ->
        typeName +> sepEq 
        +> indent +> sepNln +> opt sepNln ao' genAccess 
        +> col sepNln xs (genUnionCase { astContext with HasVerticalBar = true })
        +> colPre sepNln sepNln ms (genMemberSig astContext)
        +> unindent

    | SigSimple(TDSRRecord(ao', fs)) ->
        typeName +> sepEq 
        +> indent +> sepNln +> opt sepNln ao' genAccess +> sepOpenS 
        +> atCurrentColumn (col sepSemiNln fs (genField astContext "")) +> sepCloseS
        +> colPre sepNln sepNln ms (genMemberSig astContext)
        +> unindent 

    | SigSimple TDSRNone -> 
        typeName
    | SigSimple(TDSRTypeAbbrev t) -> 
        typeName +> sepEq +> sepSpace +> genType astContext false t
    | SigSimple(TDSRException(ExceptionDefRepr(ats, px, ao, uc))) ->
            genExceptionBody astContext ats px ao uc

    | SigObjectModel(TCSimple (TCStruct | TCInterface | TCClass) as tdk, mds) ->
        typeName +> sepEq +> indent +> sepNln +> genTypeDefKind tdk
        +> indent +> colPre sepNln sepNln mds (genMemberSig astContext) +> unindent
        ++ "end" +> unindent

    | SigObjectModel(TCSimple TCAugmentation, _) ->
        typeName -- " with" +> indent +> sepNln 
        // Remember that we use MemberSig of parent node
        +> col sepNln ms (genMemberSig astContext) +> unindent

    | SigObjectModel(TCDelegate(FunType ts), _) ->
        typeName +> sepEq +> sepSpace -- "delegate of " +> genTypeList astContext ts
    | SigObjectModel(_, mds) -> 
        typeName +> sepEq +> indent +> sepNln 
        +> col sepNln mds (genMemberSig astContext) +> unindent

    | SigExceptionRepr(SigExceptionDefRepr(ats, px, ao, uc)) ->
        genExceptionBody astContext ats px ao uc
    |> genTrivia range

and genMemberSig astContext node =
    let range =
        match node with
        | SynMemberSig.Member(_,_, r)
        | SynMemberSig.Interface(_,r)
        | SynMemberSig.Inherit(_,r)
        | SynMemberSig.ValField(_,r)
        | SynMemberSig.NestedType(_,r) -> r
    
    match node with
    | MSMember(Val(ats, px, ao, s, t, vi, _), mf) -> 
        let (FunType namedArgs) = (t, vi)
        genPreXmlDoc px +> genAttributes astContext ats 
        +> atCurrentColumn (indent +> genMemberFlags { astContext with InterfaceRange = None } mf +> opt sepSpace ao genAccess
                                   +> ifElse (s = "``new``") (!- "new") (!- s) 
                                   +> sepColon +> genTypeList astContext namedArgs +> unindent)

    | MSInterface t -> !- "interface " +> genType astContext false t
    | MSInherit t -> !- "inherit " +> genType astContext false t
    | MSValField f -> genField astContext "val " f
    | MSNestedType _ -> invalidArg "md" "This is not implemented in F# compiler"
    |> genTrivia range

and genTyparDecl astContext (TyparDecl(ats, tp)) =
    genOnelinerAttributes astContext ats +> genTypar astContext tp

and genTypeDefKind node =
    match node with
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
    // |> genTrivia node

and genExceptionBody astContext ats px ao uc = 
    genPreXmlDoc px
    +> genAttributes astContext ats  -- "exception " 
    +> opt sepSpace ao genAccess +> genUnionCase { astContext with HasVerticalBar = false } uc

and genException astContext (ExceptionDef(ats, px, ao, uc, ms) as node) =
    genExceptionBody astContext ats px ao uc 
    +> ifElse ms.IsEmpty sepNone 
        (!- " with" +> indent +> genMemberDefnList { astContext with InterfaceRange = None } ms +> unindent)
    |> genTrivia node.Range

and genSigException astContext (SigExceptionDef(ats, px, ao, uc, ms) as node) =
    let range = match node with SynExceptionSig(_,_,range) -> range
    genExceptionBody astContext ats px ao uc 
    +> colPre sepNln sepNln ms (genMemberSig astContext)
    |> genTrivia range

and genUnionCase astContext (UnionCase(ats, px, _, s, UnionCaseType fs) as node) =
    genPreXmlDoc px
    +> genTriviaBeforeClausePipe node.Range
    +> ifElse astContext.HasVerticalBar sepBar sepNone
    +> genOnelinerAttributes astContext ats -- s 
    +> colPre wordOf sepStar fs (genField { astContext with IsUnionField = true } "")
    |> genTrivia node.Range

and genEnumCase astContext (EnumCase(ats, px, _, (_,r)) as node) =
    let genCase =
        match node with
        | SynEnumCase.EnumCase(_, ident, c,_,_) ->
            !- ident.idText +> !- " = " +> genConst c r

    genPreXmlDoc px
    +> ifElse astContext.HasVerticalBar sepBar sepNone 
    +> genOnelinerAttributes astContext ats 
    +> genCase
    |> genTrivia node.Range

and genField astContext prefix (Field(ats, px, ao, isStatic, isMutable, t, so) as node) =
    let range = match node with SynField.Field(_,_,_,_,_,_,_,range) -> range
    // Being protective on union case declaration
    let t = genType astContext astContext.IsUnionField t
    genPreXmlDoc px
    +> genAttributes astContext ats +> ifElse isStatic (!- "static ") sepNone -- prefix
    +> ifElse isMutable (!- "mutable ") sepNone +> opt sepSpace ao genAccess  
    +> opt sepColon so (!-) +> t
    |> genTrivia range

and genTypeByLookup astContext (t: SynType) = getByLookup t.Range (genType astContext false) t

and genType astContext outerBracket t =
    let rec loop = function
        | THashConstraint t -> !- "#" +> loop t
        | TMeasurePower(t, n) -> loop t -- "^" +> str n
        | TMeasureDivide(t1, t2) -> loop t1 -- " / " +> loop t2
        | TStaticConstant(c,r) -> genConst c r
        | TStaticConstantExpr(e) -> genExpr astContext e
        | TStaticConstantNamed(t1, t2) -> loop t1 -- "=" +> loop t2
        | TArray(t, n) -> loop t -- " [" +> rep (n - 1) (!- ",") -- "]"
        | TAnon -> sepWild
        | TVar tp -> genTypar astContext tp
        // Drop bracket around tuples before an arrow
        | TFun(TTuple ts, t) -> sepOpenT +> loopTTupleList ts +> sepArrow +> loop t +> sepCloseT
        // Do similar for tuples after an arrow
        | TFun(t, TTuple ts) -> sepOpenT +> loop t +> sepArrow +> loopTTupleList ts +> sepCloseT
        | TFuns ts -> sepOpenT +> col sepArrow ts loop +> sepCloseT
        | TApp(t, ts, isPostfix) -> 
            let postForm = 
                match ts with
                | [] ->  loop t
                | [t'] -> loop t' +> sepSpace +> loop t
                | ts -> sepOpenT +> col sepComma ts loop +> sepCloseT +> loop t

            ifElse isPostfix postForm (loop t +> genPrefixTypes astContext ts)

        | TLongIdentApp(t, s, ts) -> loop t -- sprintf ".%s" s +> genPrefixTypes astContext ts
        | TTuple ts -> sepOpenT +> loopTTupleList ts +> sepCloseT
        | TStructTuple ts -> !- "struct " +> sepOpenT +> loopTTupleList ts +> sepCloseT
        | TWithGlobalConstraints(TVar _, [TyparSubtypeOfType _ as tc]) -> genTypeConstraint astContext tc
        | TWithGlobalConstraints(TFuns ts, tcs) -> col sepArrow ts loop +> colPre (!- " when ") wordAnd tcs (genTypeConstraint astContext)        
        | TWithGlobalConstraints(t, tcs) -> loop t +> colPre (!- " when ") wordAnd tcs (genTypeConstraint astContext)
        | TLongIdent s -> ifElse astContext.IsCStylePattern (genTypeByLookup astContext t) (!- s)
        | TAnonRecord(isStruct, fields) ->
            ifElse isStruct !- "struct " sepNone
            +> sepOpenAnonRecd
            +> col sepSemi fields (genAnonRecordFieldType astContext)
            +> sepCloseAnonRecd
        | t -> failwithf "Unexpected type: %O" t

    and loopTTupleList = function
        | [] -> sepNone
        | [(_, t)] -> loop t
        | (isDivide, t) :: ts ->
            loop t -- (if isDivide then " / " else " * ") +> loopTTupleList ts

    match t with
    | TFun(TTuple ts, t) -> 
        ifElse outerBracket (sepOpenT +> loopTTupleList ts +> sepArrow +> loop t +> sepCloseT)
            (loopTTupleList ts +> sepArrow +> loop t)
    | TFuns ts -> ifElse outerBracket (sepOpenT +> col sepArrow ts loop +> sepCloseT) (col sepArrow ts loop)
    | TTuple ts -> ifElse outerBracket (sepOpenT +> loopTTupleList ts +> sepCloseT) (loopTTupleList ts)
    | _ -> loop t
    |> genTrivia t.Range
  
and genAnonRecordFieldType astContext (AnonRecordFieldType(s, t)) =
    !- s +> sepColon +> (genType astContext false t)
  
and genPrefixTypes astContext node =
    match node with
    | [] -> sepNone
    // Where <  and ^ meet, we need an extra space. For example:  seq< ^a >
    | (TVar(Typar(_, true)) as t)::ts -> 
        !- "< " +> col sepComma (t::ts) (genType astContext false) -- " >"
    | ts ->
        !- "<" +> col sepComma ts (genType astContext false) -- ">"
    // |> genTrivia node

and genTypeList astContext node =
    match node with
    | [] -> sepNone
    | (t, [ArgInfo(ats, so, isOpt)])::ts -> 
        let gt =
            match t with
            | TTuple _ -> not ts.IsEmpty
            | TFun _ -> true // Fun is grouped by brackets inside 'genType astContext true t'
            | _ -> false
            |> fun hasBracket ->
                opt sepColonFixed so (if isOpt then (sprintf "?%s" >> (!-)) else (!-))
                +> genType astContext hasBracket t
        genOnelinerAttributes astContext ats
        +> gt +> ifElse ts.IsEmpty sepNone (autoNln (sepArrow +> genTypeList astContext ts))

    | (TTuple ts', argInfo)::ts -> 
        // The '/' separator shouldn't appear here
        let hasBracket = not ts.IsEmpty
        let gt = col sepStar (Seq.zip argInfo (Seq.map snd ts')) 
                    (fun (ArgInfo(ats, so, isOpt), t) ->
                        genOnelinerAttributes astContext ats
                        +> opt sepColonFixed so (if isOpt then (sprintf "?%s" >> (!-)) else (!-))
                        +> genType astContext hasBracket t)
        gt +> ifElse ts.IsEmpty sepNone (autoNln (sepArrow +> genTypeList astContext ts))

    | (t, _)::ts -> 
        let gt = genType astContext false t
        gt +> ifElse ts.IsEmpty sepNone (autoNln (sepArrow +> genTypeList astContext ts))
    // |> genTrivia node

and genTypar astContext (Typar(s, isHead) as node) = 
    ifElse isHead (ifElse astContext.IsFirstTypeParam (!- " ^") (!- "^")) (!-"'") -- s
    |> genTrivia node.Range
    
and genTypeConstraint astContext node =
    match node with
    | TyparSingle(kind, tp) -> genTypar astContext tp +> sepColon -- sprintf "%O" kind
    | TyparDefaultsToType(tp, t) -> !- "default " +> genTypar astContext tp +> sepColon +> genType astContext false t
    | TyparSubtypeOfType(tp, t) -> genTypar astContext tp -- " :> " +> genType astContext false t
    | TyparSupportsMember(tps, msg) -> 
        genTyparList astContext tps +> sepColon +> sepOpenT +> genMemberSig astContext msg +> sepCloseT
    | TyparIsEnum(tp, ts) -> 
        genTypar astContext tp +> sepColon -- "enum<" +> col sepComma ts (genType astContext false) -- ">"
    | TyparIsDelegate(tp, ts) ->
        genTypar astContext tp +> sepColon -- "delegate<" +> col sepComma ts (genType astContext false) -- ">"
    // |> genTrivia node no idea

and genInterfaceImpl astContext (InterfaceImpl(t, bs, range)) = 
    match bs with
    | [] -> !- "interface " +> genType astContext false t
    | bs ->
        !- "interface " +> genType astContext false t -- " with"
        +> indent +> sepNln +> genMemberBindingList { astContext with InterfaceRange = Some range } bs +> unindent
    // |> genTrivia node

and genClause astContext hasBar (Clause(p, e, eo) as node) =
    let clauseBody e (ctx: Context) =
        let find tn =
            match tn with
            | ({ Type = Token({ TokenInfo = {TokenName = "RARROW" } }); Range = r  }) -> r.StartLine = p.Range.EndLine // search for `->` token after p
            | _ -> false
        let newlineAfter = function | NewlineAfter -> true | _ -> false
        if TriviaHelpers.``has content after after that matches`` find newlineAfter ctx.Trivia then
            breakNln astContext true e ctx
        else
            preserveBreakNln astContext e ctx

    genTriviaBeforeClausePipe p.Range +>
    ifElse hasBar sepBar sepNone +> genPat astContext p
    +> optPre (!- " when ") sepNone eo (genExpr astContext) +> sepArrow
    +> clauseBody e
    |> genTrivia node.Range

/// Each multiline member definition has a pre and post new line. 
and genMemberDefnList astContext node =
    match node with
    | [x] -> sepNlnConsideringTriviaContentBefore x.Range +> genMemberDefn astContext x

    | MDOpenL(xs, ys) ->
        fun ctx ->
            let xs = sortAndDeduplicate ((|MDOpen|_|) >> Option.get) xs ctx
            match ys with
            | [] -> col sepNln xs (genMemberDefn astContext) ctx
            | _ -> (col sepNln xs (genMemberDefn astContext) +> rep 2 sepNln +> genMemberDefnList astContext ys) ctx

    | MultilineMemberDefnL(xs, []) ->
        let sepMember (m:Composite<SynMemberDefn, SynBinding>) =
            match m with
            | Pair(x1,_) ->
                let attributes = getRangesFromAttributesFromSynBinding x1
                sepNln +> sepNlnConsideringTriviaContentBeforeWithAttributes x1.RangeOfBindingSansRhs attributes
            | Single x ->
                let attributes = getRangesFromAttributesFromSynMemberDefinition x
                sepNln +> sepNlnConsideringTriviaContentBeforeWithAttributes x.Range attributes

        let firstTwoNln =
            match List.tryHead xs with
            | Some xsh -> sepMember xsh
            | None -> rep 2 sepNln
        
        firstTwoNln
        +> colEx sepMember xs (function
                | Pair(x1, x2) -> genPropertyWithGetSet astContext (x1, x2)
                | Single x -> genMemberDefn astContext x)

    | MultilineMemberDefnL(xs, ys) ->
        let sepNlnFirstExpr =
            match List.tryHead xs with
            | Some (Single xsh) ->
                let attributes =
                    match xsh with
                    | SynMemberDefn.Member(SynBinding.Binding(_,_,_,_, _, _,_,_,_,_,_,_) as sb, _) -> getRangesFromAttributesFromSynBinding sb
                    | _ -> []
                sepNlnConsideringTriviaContentBeforeWithAttributes xsh.Range attributes
            | _ -> sepNln
        
        sepNln +> sepNlnFirstExpr 
        +> col (rep 2 sepNln) xs (function
                | Pair(x1, x2) -> genPropertyWithGetSet astContext (x1, x2)
                | Single x -> genMemberDefn astContext x) 
        +> sepNln +> genMemberDefnList astContext ys

    | OneLinerMemberDefnL(xs, ys) ->
        let sepNlnFirstExpr =
            match List.tryHead xs with
            | Some xsh -> sepNlnConsideringTriviaContentBefore xsh.Range
            | None -> sepNln
        sepNlnFirstExpr +> col sepNln xs (genMemberDefn astContext) +> genMemberDefnList astContext ys
        //colEx (fun (mdf:SynMemberDefn) -> sepNlnConsideringTriviaContentBefore mdf.Range) xs (genMemberDefn astContext) +> genMemberDefnList astContext ys
    | _ -> sepNone
    // |> genTrivia node

and genMemberDefn astContext node =
    match node with
    | MDNestedType _ -> invalidArg "md" "This is not implemented in F# compiler"
    | MDOpen(s) -> !- (sprintf "open %s" s)
    // What is the role of so
    | MDImplicitInherit(t, e, _) -> !- "inherit " +> genType astContext false t +> genExpr astContext e
    | MDInherit(t, _) -> !- "inherit " +> genType astContext false t
    | MDValField f -> genField astContext "val " f
    | MDImplicitCtor(ats, ao, ps, so) ->
        let rec simplePats ps =
            match ps with
            | SynSimplePats.SimplePats(pats, _) -> pats
            | SynSimplePats.Typed(spts, _, _) -> simplePats spts
        
        // In implicit constructor, attributes should come even before access qualifiers
        ifElse ats.IsEmpty sepNone (sepSpace +> genOnelinerAttributes astContext ats)
        +> optPre sepSpace sepSpace ao genAccess +> sepOpenT
        +> col sepComma (simplePats ps) (genSimplePat astContext) +> sepCloseT
        +> optPre (!- " as ") sepNone so (!-)

    | MDMember(b) -> genMemberBinding astContext b
    | MDLetBindings(isStatic, isRec, b::bs) ->
        let prefix = 
            if isStatic && isRec then "static let rec "
            elif isStatic then "static let "
            elif isRec then "let rec "
            else "let "

        genLetBinding { astContext with IsFirstChild = true } prefix b 
        +> colPre sepNln sepNln bs (genLetBinding { astContext with IsFirstChild = false } "and ")

    | MDInterface(t, mdo, range) ->
        !- "interface " +> genType astContext false t
        +> opt sepNone mdo 
            (fun mds -> !- " with" +> indent +> genMemberDefnList { astContext with InterfaceRange = Some range } mds +> unindent)

    | MDAutoProperty(ats, px, ao, mk, e, s, _isStatic, typeOpt, memberKindToMemberFlags) ->
        let isFunctionProperty =
            match typeOpt with
            | Some (TFun _) -> true
            | _ -> false
        genPreXmlDoc px
        +> genAttributes astContext ats +> genMemberFlags astContext (memberKindToMemberFlags mk) +> str "val "
        +> opt sepSpace ao genAccess -- s +> optPre sepColon sepNone typeOpt (genType astContext false)
         +> sepEq +> sepSpace +> genExpr astContext e -- genPropertyKind (not isFunctionProperty) mk

    | MDAbstractSlot(ats, px, ao, s, t, vi, ValTyparDecls(tds, _, tcs), MFMemberFlags mk) ->
        let (FunType namedArgs) = (t, vi)
        let isFunctionProperty =
            match t with
            | TFun _ -> true
            | _ -> false

        let sepColonX =
            match tds with
            | [] -> sepColon
            | _ -> sepColonWithSpacesFixed
            
        genPreXmlDoc px 
        +> genAttributes astContext ats
        +> opt sepSpace ao genAccess -- sprintf "abstract %s" s
        +> genTypeParamPostfix astContext tds tcs
        +> sepColonX +> genTypeList astContext namedArgs -- genPropertyKind (not isFunctionProperty) mk

    | md -> failwithf "Unexpected member definition: %O" md
    |> genTrivia node.Range

and genPropertyKind useSyntacticSugar node =
    match node with
    | PropertyGet -> 
        // Try to use syntactic sugar on real properties (not methods in disguise)
        if useSyntacticSugar then "" else " with get"
    | PropertySet -> " with set"
    | PropertyGetSet -> " with get, set"
    | _ -> ""

and genSimplePat astContext node =
    let range =
        match node with
        | SynSimplePat.Attrib(_,_,r)
        | SynSimplePat.Id(_,_,_,_,_,r)
        | SynSimplePat.Typed(_,_,r) -> r
        
    match node with
    | SPId(s, isOptArg, _) -> ifElse isOptArg (!- (sprintf "?%s" s)) (!- s)
    | SPTyped(sp, t) -> genSimplePat astContext sp +> sepColon +> genType astContext false t
    | SPAttrib(ats, sp) -> genOnelinerAttributes astContext ats +> genSimplePat astContext sp
    |> genTrivia range
    
and genSimplePats astContext node =
    let range =
        match node with
        | SynSimplePats.SimplePats(_,r)
        | SynSimplePats.Typed(_,_,r) -> r
    match node with
    // Remove parentheses on an extremely simple pattern
    | SimplePats [SPId _ as sp] -> genSimplePat astContext sp
    | SimplePats ps -> sepOpenT +> col sepComma ps (genSimplePat astContext) +> sepCloseT
    | SPSTyped(ps, t) -> genSimplePats astContext ps +> sepColon +> genType astContext false t
    |> genTrivia range

and genComplexPat astContext node =
    match node with
    | CPId p -> genPat astContext p
    | CPSimpleId(s, isOptArg, _) -> ifElse isOptArg (!- (sprintf "?%s" s)) (!- s)
    | CPTyped(sp, t) -> genComplexPat astContext sp +> sepColon +> genType astContext false t
    | CPAttrib(ats, sp) -> genOnelinerAttributes astContext ats +> genComplexPat astContext sp

and genComplexPats astContext node =
    match node with
    | ComplexPats [CPId _ as c]
    | ComplexPats [CPSimpleId _ as c] -> genComplexPat astContext c
    | ComplexPats ps -> sepOpenT +> col sepComma ps (genComplexPat astContext) +> sepCloseT
    | ComplexTyped(ps, t) -> genComplexPats astContext ps +> sepColon +> genType astContext false t

and genPatRecordFieldName astContext (PatRecordFieldName(s1, s2, p) as node) =
    let ((_, idn),_) = node
    ifElse (s1 = "") (!- (sprintf "%s = " s2)) (!- (sprintf "%s.%s = " s1 s2)) +> genPat astContext p
    |> genTrivia idn.idRange

and genPatWithIdent astContext (ido, p) = 
    opt (sepEq +> sepSpace) ido (!-) +> genPat astContext p

and genPat astContext pat =
    match pat with
    | PatOptionalVal(s) -> !- (sprintf "?%s" s)
    | PatAttrib(p, ats) -> genOnelinerAttributes astContext ats +> genPat astContext p
    | PatOr(p1, p2) -> genPat astContext p1 +> sepNln -- "| " +> genPat astContext p2
    | PatAnds(ps) -> col (!- " & ") ps (genPat astContext)
    | PatNullary PatNull -> !- "null"
    | PatNullary PatWild -> sepWild
    | PatTyped(p, t) -> 
        // CStyle patterns only occur on extern declaration so it doesn't escalate to expressions
        // We lookup sources to get extern types since it has quite many exceptions compared to normal F# types
        ifElse astContext.IsCStylePattern (genTypeByLookup astContext t +> sepSpace +> genPat astContext p)
            (genPat astContext p +> sepColon +> genType astContext false t) 
    | PatNamed(ao, PatNullary PatWild, s) -> opt sepSpace ao genAccess +> infixOperatorFromTrivia pat.Range s
    | PatNamed(ao, p, s) -> opt sepSpace ao genAccess +> genPat astContext p -- sprintf " as %s" s 
    | PatLongIdent(ao, s, ps, tpso) -> 
        let aoc = opt sepSpace ao genAccess
        let tpsoc = opt sepNone tpso (fun (ValTyparDecls(tds, _, tcs)) -> genTypeParamPostfix astContext tds tcs)
        // Override escaped new keyword
        let s = if s = "``new``" then "new" else s
        match ps with
        | [] ->  aoc -- s +> tpsoc
        | [(_, PatTuple [p1; p2])] when s = "(::)" -> 
            aoc +> genPat astContext p1 -- " :: " +> genPat astContext p2
        | [(ido, p) as ip] ->
            aoc +> infixOperatorFromTrivia pat.Range s +> tpsoc +> 
            ifElse (hasParenInPat p || Option.isSome ido) (ifElse (addSpaceBeforeParensInFunDef s p) sepBeforeArg sepNone) sepSpace 
            +> ifElse (Option.isSome ido) (sepOpenT +> genPatWithIdent astContext ip +> sepCloseT) (genPatWithIdent astContext ip)
        // This pattern is potentially long
        | ps -> 
            let hasBracket = ps |> Seq.map fst |> Seq.exists Option.isSome
            atCurrentColumn (aoc -- s +> tpsoc +> sepSpace 
                +> ifElse hasBracket sepOpenT sepNone 
                +> colAutoNlnSkip0 (ifElse hasBracket sepSemi sepSpace) ps (genPatWithIdent astContext)
                +> ifElse hasBracket sepCloseT sepNone)

    | PatParen(PatConst(Const "()", _)) -> !- "()"
    | PatParen(p) -> sepOpenT +> genPat astContext p +> sepCloseT
    | PatTuple ps -> 
        atCurrentColumn (colAutoNlnSkip0 sepComma ps (genPat astContext))
    | PatStructTuple ps -> 
        !- "struct " +> sepOpenT +> atCurrentColumn (colAutoNlnSkip0 sepComma ps (genPat astContext)) +> sepCloseT
    | PatSeq(PatList, ps) -> 
        ifElse ps.IsEmpty (sepOpenLFixed +> sepCloseLFixed) 
            (sepOpenL +> atCurrentColumn (colAutoNlnSkip0 sepSemi ps (genPat astContext)) +> sepCloseL)

    | PatSeq(PatArray, ps) -> 
        ifElse ps.IsEmpty (sepOpenAFixed +> sepCloseAFixed)
            (sepOpenA +> atCurrentColumn (colAutoNlnSkip0 sepSemi ps (genPat astContext)) +> sepCloseA)

    | PatRecord(xs) -> 
        sepOpenS +> atCurrentColumn (colAutoNlnSkip0 sepSemi xs (genPatRecordFieldName astContext)) +> sepCloseS
    | PatConst(c,r) -> genConst c r
    | PatIsInst(TApp(_, [_], _) as t)
    | PatIsInst(TArray(_) as t) -> 
        // special case for things like ":? (int seq) ->"
        !- ":? " +> sepOpenT +> genType astContext false t +> sepCloseT
    | PatIsInst(t) -> 
        // Should have brackets around in the type test patterns
        !- ":? " +> genType astContext true t
    // Quotes will be printed by inner expression
    | PatQuoteExpr e -> genExpr astContext e
    | p -> failwithf "Unexpected pattern: %O" p
    |> genTrivia pat.Range

and genConst (c:SynConst) (r:range) =
    match c with
    | SynConst.Unit ->
            fun (ctx: Context) ->
                let innerComments =
                    ctx.Trivia
                    |> List.tryFind (fun t ->
                        let rangeMatch = t.Range.StartLine = r.StartLine && t.Range.StartColumn = r.StartColumn
                        match rangeMatch, t.Type with
                        | true, Token({TokenInfo = ti}) when (ti.TokenName = "LPAREN") -> true
                        | _ -> false
                    )
                    |> Option.map (fun tv -> tv.ContentAfter |> List.choose(function | Comment(BlockComment(bc,_,_)) -> Some bc | _ -> None))
                    |> Option.defaultValue []

                match innerComments with
                | [] -> !- "()"
                | comments ->
                    !- "(" +> !- (String.concat " " comments) +> !- ")"
                <| ctx
    | SynConst.Bool(b) -> !- (if b then "true" else "false")
    | SynConst.Byte(_)
    | SynConst.SByte(_)
    | SynConst.Int16(_)
    | SynConst.Int32(_)
    | SynConst.Int64(_)
    | SynConst.UInt16(_)
    | SynConst.UInt16s(_)
    | SynConst.UInt32(_)
    | SynConst.UInt64(_)
    | SynConst.Double(_)
    | SynConst.Single(_)
    | SynConst.Decimal(_)
    | SynConst.IntPtr(_)
    | SynConst.UInt64(_)
    | SynConst.UIntPtr(_)
    | SynConst.UserNum(_,_) -> genConstNumber c r
    | SynConst.String(s,_) ->
        fun (ctx: Context) ->
            let trivia =
                ctx.Trivia
                |> List.tryFind (fun tv -> tv.Range = r)

            let triviaStringContent =
                trivia
                |> Option.bind(fun tv ->
                    match tv.ContentItself with
                    | Some(StringContent(sc)) -> Some sc
                    | _ -> None
                )

            match triviaStringContent, trivia with
            | Some stringContent, Some _ ->
                !- stringContent
            | None, Some({ ContentBefore = [Keyword({TokenInfo = { TokenName = "KEYWORD_STRING"; }; Content = kw})] }) ->
                !- kw
            | None, Some({ ContentBefore = [Keyword({TokenInfo = { TokenName = "QMARK" }})] }) ->
                !- s
            | _ ->
                let escaped = Regex.Replace(s, "\"{1}", "\\\"")
                !- (sprintf "\"%s\"" escaped)
            <| ctx
    | SynConst.Char(c) ->
        let escapedChar = Char.escape c
        !- (sprintf "\'%s\'" escapedChar)
    | SynConst.Bytes(bytes,_) -> genConstBytes bytes r
    | SynConst.Measure(c, m) ->
        let measure =
            match m with
            | Measure m -> !- m
            
        genConstNumber c r +> measure

and genConstNumber (c:SynConst) (r: range) =
    fun (ctx: Context) ->
        ctx.Trivia
        |> List.tryFind (fun t -> t.Range = r)
        |> Option.bind(fun tn ->
            match tn.ContentItself with | Some(Number(n)) -> Some n | _ -> None
        )
        |> fun n ->
            match n with
            | Some n -> !- n
            | None ->
                match c with
                | SynConst.Byte(v) -> !- (sprintf "%A" v)
                | SynConst.SByte(v) -> !- (sprintf "%A" v)
                | SynConst.Int16(v) -> !- (sprintf "%A" v)
                | SynConst.Int32(v) -> !- (sprintf "%A" v)
                | SynConst.Int64(v) -> !- (sprintf "%A" v)
                | SynConst.UInt16(v) -> !- (sprintf "%A" v)
                | SynConst.UInt16s(v) -> !- (sprintf "%A" v)
                | SynConst.UInt32(v) -> !- (sprintf "%A" v)
                | SynConst.UInt64(v) -> !- (sprintf "%A" v)
                | SynConst.Double(v) -> !- (sprintf "%A" v)
                | SynConst.Single(v) -> !- (sprintf "%A" v)
                | SynConst.Decimal(v) -> !- (sprintf "%A" v)
                | SynConst.IntPtr(v) -> !- (sprintf "%A" v)
                | SynConst.UIntPtr(v) -> !- (sprintf "%A" v)
                | SynConst.UserNum(v,s) -> !- (sprintf "%A%s" v s)
                | _ -> failwithf "Cannot generating Const number for %A" c
        <| ctx

and genConstBytes (bytes: byte []) (r: range) =
    fun (ctx: Context) ->
        let trivia =
            ctx.Trivia
            |> List.tryFind(fun t -> t.Range = r)
            |> Option.bind (fun tv ->
                match tv.ContentItself with
                | Some(StringContent(content)) -> Some content
                | _ -> None
            )

        match trivia with
        | Some t -> !- t
        | None -> !- (sprintf "%A" bytes)
        <| ctx

and genTrivia (range: range) f =
    enterNode range +> f +> leaveNode range

and tok (range: range) (s: string) =
    enterNodeToken range +> (!-s) +> leaveNodeToken range

and tokN (range: range) (tokenName: string) f =
    enterNodeTokenByName range tokenName +> f +> leaveNodeTokenByName range tokenName

and infixOperatorFromTrivia range fallback (ctx: Context) =
    ctx.Trivia
    |> List.choose(fun t ->
        match t.Range = range with
        | true ->
            match t.ContentItself with
            | Some(IdentOperatorAsWord(iiw)) -> Some iiw
            | Some(IdentBetweenTicks(iiw)) -> Some iiw // Used when value between ``...``
            | _ -> None
        | _ -> None)
    |> List.tryHead
    |> fun iiw ->
        match iiw with
        | Some iiw -> !- iiw
        | None ->  !- fallback
    <| ctx