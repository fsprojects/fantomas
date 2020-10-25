module internal Fantomas.CodePrinter

open System
open System.Text.RegularExpressions
open FSharp.Compiler.Range
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.SyntaxTree
open Fantomas
open Fantomas.FormatConfig
open Fantomas.SourceParser
open Fantomas.SourceTransformer
open Fantomas.Context
open Fantomas.TriviaTypes
open Fantomas.TriviaContext

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
      /// Check whether the context is inside a SynMemberDefn.Member(memberDefn,range)
      /// This is required to correctly detect the setting SpaceBeforeMember
      IsMemberDefinition: bool
      /// Check whether the context is inside a SynExpr.DotIndexedGet
      IsInsideDotIndexed: bool }
    static member Default =
        { TopLevelModuleName = ""
          IsFirstChild = false
          InterfaceRange = None
          IsCStylePattern = false
          IsNakedRange = false
          HasVerticalBar = false
          IsUnionField = false
          IsFirstTypeParam = false
          IsInsideDotGet = false
          IsMemberDefinition = false
          IsInsideDotIndexed = false }

let rec addSpaceBeforeParensInFunCall functionOrMethod arg (ctx: Context) =
    match functionOrMethod, arg with
    | SynExpr.TypeApp (e, _, _, _, _, _, _), _ -> addSpaceBeforeParensInFunCall e arg ctx
    | SynExpr.Paren _, _ -> true
    | UppercaseSynExpr, ConstExpr (Const "()", _) -> ctx.Config.SpaceBeforeUppercaseInvocation
    | LowercaseSynExpr, ConstExpr (Const "()", _) -> ctx.Config.SpaceBeforeLowercaseInvocation
    | SynExpr.Ident _, SynExpr.Ident _ -> true
    | UppercaseSynExpr, Paren _ -> ctx.Config.SpaceBeforeUppercaseInvocation
    | LowercaseSynExpr, Paren _ -> ctx.Config.SpaceBeforeLowercaseInvocation
    | _ -> true

let addSpaceBeforeParensInFunDef (astContext: ASTContext) (functionOrMethod: string) args (ctx: Context) =
    let isLastPartUppercase =
        let parts = functionOrMethod.Split '.'
        Char.IsUpper parts.[parts.Length - 1].[0]

    match functionOrMethod, args with
    | "new", _ -> false
    | _, PatParen _ ->
        if astContext.IsMemberDefinition then ctx.Config.SpaceBeforeMember else ctx.Config.SpaceBeforeParameter
    | (_: string), _ -> not isLastPartUppercase
    | _ -> true

let rec genParsedInput astContext =
    function
    | ImplFile im -> genImpFile astContext im
    | SigFile si -> genSigFile astContext si

(*
    See https://github.com/fsharp/FSharp.Compiler.Service/blob/master/src/fsharp/ast.fs#L1518
    hs = hashDirectives : ParsedHashDirective list
    mns = modules : SynModuleOrNamespace list
*)
and genImpFile astContext (ParsedImplFileInput (hs, mns)) =
    col sepNone hs genParsedHashDirective
    +> (if hs.IsEmpty then sepNone else sepNln)
    +> col sepNln mns (genModuleOrNamespace astContext)

and genSigFile astContext (ParsedSigFileInput (hs, mns)) =
    col sepNone hs genParsedHashDirective
    +> (if hs.IsEmpty then sepNone else sepNln)
    +> col sepNln mns (genSigModuleOrNamespace astContext)

and genParsedHashDirective (ParsedHashDirective (h, s, r)) =
    let printArgument arg =
        match arg with
        | "" -> sepNone
        // Use verbatim string to escape '\' correctly
        | _ when arg.Contains("\\") -> !-(sprintf "@\"%O\"" arg)
        | _ -> !-(sprintf "\"%O\"" arg)

    let printIdent (ctx: Context) =
        Map.tryFind ParsedHashDirective_ ctx.TriviaMainNodes
        |> Option.defaultValue []
        |> List.tryFind (fun t -> RangeHelpers.rangeEq t.Range r)
        |> Option.bind (fun t ->
            t.ContentBefore
            |> List.choose (fun tc ->
                match tc with
                | Keyword ({ TokenInfo = { TokenName = "KEYWORD_STRING" }; Content = c }) -> Some c
                | _ -> None)
            |> List.tryHead)
        |> function
        | Some kw -> !-kw
        | None -> col sepSpace s printArgument
        <| ctx

    !- "#" -- h +> sepSpace +> printIdent

and genModuleOrNamespace astContext (ModuleOrNamespace (ats, px, ao, s, mds, isRecursive, moduleKind) as node) =
    let sepModuleAndFirstDecl =
        let firstDecl = List.tryHead mds

        match firstDecl with
        | None ->
            if moduleKind.IsModule then
                sepNlnForEmptyModule SynModuleOrNamespace_NamedModule node.Range
                +> sepNln
            else
                sepNlnForEmptyNamespace node.Range +> sepNln
        | Some mdl ->
            let attrs =
                getRangesFromAttributesFromModuleDeclaration mdl

            sepNln
            +> sepNlnConsideringTriviaContentBeforeWithAttributesFor (synModuleDeclToFsAstType mdl) mdl.Range attrs

    let genTriviaForLongIdent (f: Context -> Context) =
        match node with
        | SynModuleOrNamespace.SynModuleOrNamespace (lid, _, SynModuleOrNamespaceKind.DeclaredNamespace, _, _, _, _, _) ->
            lid
            |> List.fold (fun (acc: Context -> Context) (ident: Ident) -> acc |> (genTriviaFor Ident_ ident.idRange)) f
        | _ -> f

    let moduleOrNamespace =
        ifElse moduleKind.IsModule (!- "module ") (!- "namespace ")

    let recursive = ifElse isRecursive (!- "rec ") sepNone
    let namespaceFn = ifElse (s = "") (!- "global") (!-s)
    let namespaceIsGlobal = not moduleKind.IsModule && s = ""

    let sep =
        if namespaceIsGlobal then sepNone else sepModuleAndFirstDecl

    let expr =
        genPreXmlDoc px
        +> genAttributes astContext ats
        +> ifElse
            (moduleKind = AnonModule)
               sepNone
               (genTriviaForLongIdent
                   (moduleOrNamespace
                    +> opt sepSpace ao genAccess
                    +> recursive
                    +> namespaceFn
                    +> sep))

    if namespaceIsGlobal then
        expr
        +> genTriviaFor SynModuleOrNamespace_GlobalNamespace node.Range (genModuleDeclList astContext mds)
    else
        expr +> genModuleDeclList astContext mds
        |> (match moduleKind with
            | SynModuleOrNamespaceKind.AnonModule -> genTriviaFor SynModuleOrNamespace_AnonModule node.Range
            | SynModuleOrNamespaceKind.NamedModule -> genTriviaFor SynModuleOrNamespace_NamedModule node.Range
            | _ -> id)

and genSigModuleOrNamespace astContext (SigModuleOrNamespace (ats, px, ao, s, mds, _, moduleKind) as node) =
    let range =
        match node with
        | SynModuleOrNamespaceSig (_, _, _, _, _, _, _, range) -> range

    let sepModuleAndFirstDecl =
        let firstDecl = List.tryHead mds

        match firstDecl with
        | None ->
            if moduleKind.IsModule then
                sepNlnForEmptyModule SynModuleOrNamespaceSig_NamedModule range
                +> rep 2 sepNln
            else
                sepNlnForEmptyNamespace range +> sepNln
        | Some mdl ->
            match mdl with
            | SynModuleSigDecl.Types _ ->
                let attrs =
                    getRangesFromAttributesFromSynModuleSigDeclaration mdl

                sepNlnConsideringTriviaContentBeforeWithAttributesFor SynModuleSigDecl_Types mdl.Range attrs
            | SynModuleSigDecl.Val _ -> sepNlnConsideringTriviaContentBeforeForMainNode ValSpfn_ mdl.Range
            | _ -> sepNone
            +> sepNln

    let genTriviaForLongIdent (f: Context -> Context) =
        match node with
        | SynModuleOrNamespaceSig (lid, _, SynModuleOrNamespaceKind.DeclaredNamespace, _, _, _, _, _) ->
            lid
            |> List.fold (fun (acc: Context -> Context) (ident: Ident) -> acc |> (genTriviaFor Ident_ ident.idRange)) f
        | _ -> f

    let moduleOrNamespace =
        ifElse moduleKind.IsModule (!- "module ") (!- "namespace ")

    // Don't generate trivia before in case the SynModuleOrNamespaceKind is a DeclaredNamespace
    // The range of the namespace is not correct, see https://github.com/dotnet/fsharp/issues/7680
    ifElse moduleKind.IsModule (enterNodeFor SynModuleOrNamespaceSig_NamedModule range) sepNone
    +> genPreXmlDoc px
    +> genAttributes astContext ats
    +> ifElse
        (moduleKind = AnonModule)
           sepNone
           (genTriviaForLongIdent
               (moduleOrNamespace +> opt sepSpace ao genAccess
                -- s
                +> sepModuleAndFirstDecl))
    +> genSigModuleDeclList astContext mds
    +> leaveNodeFor SynModuleOrNamespaceSig_NamedModule range

and genModuleDeclList astContext e =
    let rec collectItems e =
        match e with
        | [] -> []
        | OpenL (xs, ys) ->
            let expr = col sepNln xs (genModuleDecl astContext)

            let r = List.head xs |> fun mdl -> mdl.Range
            // SynModuleDecl.Open cannot have attributes
            let sepNln =
                sepNlnConsideringTriviaContentBeforeForMainNode SynModuleDecl_Open r

            [ expr, sepNln, r ] @ collectItems ys
        | AttributesL (xs, y :: rest) ->
            let attrs =
                getRangesFromAttributesFromModuleDeclaration y

            let expr =
                col sepNln xs (genModuleDecl astContext)
                +> sepNlnConsideringTriviaContentBeforeWithAttributesFor (synModuleDeclToFsAstType y) y.Range attrs
                +> genModuleDecl astContext y

            let r = List.head xs |> fun mdl -> mdl.Range

            let sepNln =
                sepNlnConsideringTriviaContentBeforeForMainNode SynModuleDecl_Attributes r

            [ expr, sepNln, r ] @ collectItems rest
        | m :: rest ->
            let attrs =
                getRangesFromAttributesFromModuleDeclaration m

            let sepNln =
                sepNlnConsideringTriviaContentBeforeWithAttributesFor (synModuleDeclToFsAstType m) m.Range attrs

            let expr = genModuleDecl astContext m
            (expr, sepNln, m.Range) :: (collectItems rest)

    collectItems e |> colWithNlnWhenItemIsMultiline

and genSigModuleDeclList astContext node =
    match node with
    | [ x ] -> genSigModuleDecl astContext x

    | SigOpenL (xs, ys) ->
        let sepXsAndYs =
            match List.tryHead ys with
            | Some _ -> sepNln
            | None -> rep 2 sepNln

        fun ctx ->
            match ys with
            | [] -> col sepNln xs (genSigModuleDecl astContext) ctx
            | _ ->
                (col sepNln xs (genSigModuleDecl astContext)
                 +> sepXsAndYs
                 +> genSigModuleDeclList astContext ys) ctx

    | SigHashDirectiveL (xs, ys) ->
        match ys with
        | [] -> col sepNone xs (genSigModuleDecl astContext)
        | _ ->
            col sepNone xs (genSigModuleDecl astContext)
            +> sepNln
            +> genSigModuleDeclList astContext ys

    | SigModuleAbbrevL (xs, ys)
    | SigValL (xs, ys) ->
        match ys with
        | [] -> col sepNln xs (genSigModuleDecl astContext)
        | _ ->
            let sepXsYs =
                match List.tryHead ys with
                | Some _ -> sepNln
                | None -> rep 2 sepNln

            col sepNln xs (genSigModuleDecl astContext)
            +> sepXsYs
            +> genSigModuleDeclList astContext ys

    | SigMultilineModuleDeclL (xs, ys) ->
        match ys with
        | [] ->
            colEx (fun (x: SynModuleSigDecl) ->
                let attrs =
                    getRangesFromAttributesFromSynModuleSigDeclaration x

                sepNln
                +> sepNlnConsideringTriviaContentBeforeWithAttributesFor (synModuleSigDeclToFsAstType x) x.Range attrs)
                xs (genSigModuleDecl astContext)
        | _ ->
            let sepXsYs =
                match List.tryHead ys with
                | Some y ->
                    sepNln
                    +> sepNlnConsideringTriviaContentBeforeForMainNode (synModuleSigDeclToFsAstType y) y.Range
                | None -> rep 2 sepNln

            colEx (fun (x: SynModuleSigDecl) ->
                let attrs =
                    getRangesFromAttributesFromSynModuleSigDeclaration x

                sepNln
                +> sepNlnConsideringTriviaContentBeforeWithAttributesFor (synModuleSigDeclToFsAstType x) x.Range attrs)
                xs (genSigModuleDecl astContext)
            +> sepXsYs
            +> genSigModuleDeclList astContext ys

    | _ -> sepNone

and genModuleDecl astContext (node: SynModuleDecl) =
    match node with
    | Attributes (ats) ->
        fun ctx ->
            let attributesExpr =
                // attributes can have trivia content before or after
                // we do extra detection to ensure no additional newline is introduced
                // first attribute should not have a newline anyway
                List.fold (fun (prevContentAfterPresent, prevExpr) (a: SynAttributeList) ->
                    let expr =
                        ifElse
                            prevContentAfterPresent
                            sepNone
                            (sepNlnConsideringTriviaContentBeforeForMainNode SynModuleDecl_Attributes a.Range)
                        +> ((col sepNln a.Attributes (genAttribute astContext))
                            |> genTriviaFor SynAttributeList_ a.Range)

                    let hasContentAfter =
                        TriviaHelpers.``has content after after that matches`` (fun tn ->
                            RangeHelpers.rangeEq tn.Range a.Range) (function
                            | Newline
                            | Comment (LineCommentOnSingleLine _)
                            | Directive _ -> true
                            | _ -> false) (Map.tryFindOrEmptyList SynAttributeList_ ctx.TriviaMainNodes)

                    (hasContentAfter, prevExpr +> expr)) (true, sepNone) ats
                |> snd

            (attributesExpr) ctx
    | DoExpr (e) -> genExpr astContext e
    | Exception (ex) -> genException astContext ex
    | HashDirective (p) -> genParsedHashDirective p
    | Extern (ats, px, ao, t, s, ps) ->
        genPreXmlDoc px +> genAttributes astContext ats
        -- "extern "
        +> genType
            { astContext with
                  IsCStylePattern = true }
               false
               t
        +> sepSpace
        +> opt sepSpace ao genAccess
        -- s
        +> sepOpenT
        +> col
            sepComma
               ps
               (genPat
                   { astContext with
                         IsCStylePattern = true })
        +> sepCloseT
    // Add a new line after module-level let bindings
    | Let (b) -> genLetBinding { astContext with IsFirstChild = true } "let " b
    | LetRec (b :: bs) ->
        let sepBAndBs =
            match List.tryHead bs with
            | Some b' ->
                let r = b'.RangeOfBindingAndRhs

                sepNln
                +> sepNlnConsideringTriviaContentBeforeForMainNode (synBindingToFsAstType b) r
            | None -> id

        genLetBinding { astContext with IsFirstChild = true } "let rec " b
        +> sepBAndBs
        +> colEx (fun (b': SynBinding) ->
            let r = b'.RangeOfBindingAndRhs

            sepNln
            +> sepNlnConsideringTriviaContentBeforeForMainNode (synBindingToFsAstType b) r) bs (fun andBinding ->
               enterNodeFor (synBindingToFsAstType b) andBinding.RangeOfBindingAndRhs
               +> genLetBinding { astContext with IsFirstChild = false } "and " andBinding)

    | ModuleAbbrev (s1, s2) -> !- "module " -- s1 +> sepEq +> sepSpace -- s2
    | NamespaceFragment (m) -> failwithf "NamespaceFragment hasn't been implemented yet: %O" m
    | NestedModule (ats, px, ao, s, isRecursive, mds) ->
        genPreXmlDoc px
        +> genAttributes astContext ats
        +> (!- "module ")
        +> opt sepSpace ao genAccess
        +> ifElse isRecursive (!- "rec ") sepNone
        -- s
        +> sepEq
        +> indent
        +> sepNln
        +> genModuleDeclList astContext mds
        +> unindent

    | Open (s) -> !-(sprintf "open %s" s)
    // There is no nested types and they are recursive if there are more than one definition
    | Types (t :: ts) ->
        let sepTs =
            match List.tryHead ts with
            | Some t ->
                sepNln
                +> sepNlnConsideringTriviaContentBeforeForMainNode TypeDefn_ t.Range
            | None -> rep 2 sepNln

        genTypeDefn { astContext with IsFirstChild = true } t
        +> colPreEx sepTs (fun (ty: SynTypeDefn) ->
               sepNln
               +> sepNlnConsideringTriviaContentBeforeForMainNode TypeDefn_ ty.Range) ts
               (genTypeDefn { astContext with IsFirstChild = false })
    | md -> failwithf "Unexpected module declaration: %O" md
    |> genTriviaFor (synModuleDeclToFsAstType node) node.Range

and genSigModuleDecl astContext node =
    match node with
    | SigException (ex) -> genSigException astContext ex
    | SigHashDirective (p) -> genParsedHashDirective p
    | SigVal (v) -> genVal astContext v
    | SigModuleAbbrev (s1, s2) -> !- "module " -- s1 +> sepEq +> sepSpace -- s2
    | SigNamespaceFragment (m) -> failwithf "NamespaceFragment is not supported yet: %O" m
    | SigNestedModule (ats, px, ao, s, mds) ->
        genPreXmlDoc px +> genAttributes astContext ats
        -- "module "
        +> opt sepSpace ao genAccess
        -- s
        +> sepEq
        +> indent
        +> sepNln
        +> genSigModuleDeclList astContext mds
        +> unindent

    | SigOpen (s) -> !-(sprintf "open %s" s)
    | SigTypes (t :: ts) ->
        genSigTypeDefn { astContext with IsFirstChild = true } t
        +> colPre (rep 2 sepNln) (rep 2 sepNln) ts (genSigTypeDefn { astContext with IsFirstChild = false })
    | md -> failwithf "Unexpected module signature declaration: %O" md
    |> (match node with
        | SynModuleSigDecl.Types _ -> genTriviaFor SynModuleSigDecl_Types node.Range
        | SynModuleSigDecl.NestedModule _ -> genTriviaFor SynModuleSigDecl_NestedModule node.Range
        | SynModuleSigDecl.Open _ -> genTriviaFor SynModuleSigDecl_Open node.Range
        | _ -> id)

and genAccess (Access s) = !-s

and genAttribute astContext (Attribute (s, e, target)) =
    match e with
    // Special treatment for function application on attributes
    | ConstExpr (Const "()", _) -> !- "[<" +> opt sepColon target (!-) -- s -- ">]"
    | e ->
        let argSpacing =
            if hasParenthesis e then id else sepSpace

        !- "[<" +> opt sepColon target (!-) -- s
        +> argSpacing
        +> genExpr astContext e
        -- ">]"

and genAttributesCore astContext (ats: SynAttribute seq) =
    let genAttributeExpr astContext (Attribute (s, e, target) as attr) =
        match e with
        | ConstExpr (Const "()", _) -> opt sepColon target (!-) -- s
        | e ->
            let argSpacing =
                if hasParenthesis e then id else sepSpace

            opt sepColon target (!-) -- s
            +> argSpacing
            +> genExpr astContext e
        |> genTriviaFor SynAttribute_ attr.Range

    let shortExpression =
        !- "[<"
        +> atCurrentColumn (col sepSemi ats (genAttributeExpr astContext))
        -- ">]"

    let longExpression =
        !- "[<"
        +> atCurrentColumn (col (sepSemi +> sepNln) ats (genAttributeExpr astContext))
        -- ">]"

    ifElse (Seq.isEmpty ats) sepNone (expressionFitsOnRestOfLine shortExpression longExpression)

and genOnelinerAttributes astContext ats =
    let ats = List.collect (fun a -> a.Attributes) ats
    ifElse (Seq.isEmpty ats) sepNone (genAttributesCore astContext ats +> sepSpace)

/// Try to group attributes if they are on the same line
/// Separate same-line attributes by ';'
/// Each bucket is printed in a different line
and genAttributes astContext (ats: SynAttributes) =
    ats
    |> List.fold (fun acc a (ctx: Context) ->
        let dontAddNewline =
            TriviaHelpers.``has content after that ends with`` (fun t -> RangeHelpers.rangeEq t.Range a.Range) (function
                | Directive _
                | Newline
                | Comment (LineCommentOnSingleLine _) -> true
                | _ -> false) (Map.tryFindOrEmptyList SynAttributeList_ ctx.TriviaMainNodes)

        let chain =
            acc
            +> (genAttributesCore astContext a.Attributes
                |> genTriviaFor SynAttributeList_ a.Range)
            +> ifElse dontAddNewline sepNone sepNln

        chain ctx) sepNone

and genPreXmlDoc (PreXmlDoc lines) ctx =
    if ctx.Config.StrictMode
    then colPost sepNln sepNln lines (sprintf "///%s" >> (!-)) ctx
    else ctx

and addSpaceAfterGenericConstructBeforeColon ctx =
    if not ctx.Config.SpaceBeforeColon then
        match lastWriteEventOnLastLine ctx
              |> Option.bind Seq.tryLast with
        | Some ('>') -> sepSpace
        | _ -> sepNone
    else
        sepNone
    <| ctx

and genExprSepEqPrependType astContext
                            (pat: SynPat)
                            (e: SynExpr)
                            (valInfo: SynValInfo option)
                            (isPrefixMultiline: bool)
                            ctx
                            =
    let hasTriviaContentAfterEqual =
        Map.tryFindOrEmptyList EQUALS ctx.TriviaTokenNodes
        |> List.exists (fun tn -> tn.Range.StartLine = pat.Range.StartLine)

    let sepEqual predicate =
        if predicate then
            fun ctx ->
                let alreadyHasNewline = lastWriteEventIsNewline ctx

                if alreadyHasNewline then
                    // Column could be 0 when a hash directive was just written
                    if ctx.Column = 0
                    then (rep ctx.Config.IndentSize (!- " ") +> !- "=") ctx
                    else !- "=" ctx
                elif ctx.Config.AlignFunctionSignatureToIndentation then
                    (indent +> sepNln +> !- "=" +> unindent) ctx
                else
                    (sepEq +> sepSpace) ctx
        else
            (sepEq +> sepSpace)

    let maxWidth =
        if isFunctionBinding pat then ctx.Config.MaxFunctionBindingWidth else ctx.Config.MaxValueBindingWidth

    match e with
    | TypedExpr (Typed, e, t) ->
        let addExtraSpaceBeforeGenericType =
            match pat with
            | SynPat.LongIdent (_, _, Some (SynValTyparDecls _), _, _, _) -> addSpaceAfterGenericConstructBeforeColon
            | _ -> sepNone

        let genCommentBeforeColon =
            atCurrentColumn (enterNodeFor SynBindingReturnInfo_ t.Range)

        let genMetadataAttributes =
            match valInfo with
            | Some (SynValInfo (_, SynArgInfo (attributes, _, _))) -> genOnelinerAttributes astContext attributes
            | None -> sepNone

        (addExtraSpaceBeforeGenericType
         +> genCommentBeforeColon
         +> sepColon
         +> genMetadataAttributes
         +> genType astContext false t
         +> sepEqual (isPrefixMultiline)
         +> ifElse
             (isPrefixMultiline || hasTriviaContentAfterEqual)
                (indent
                 +> sepNln
                 +> genExpr astContext e
                 +> unindent)
                (isShortExpressionOrAddIndentAndNewline maxWidth (genExpr astContext e))) ctx
    | e ->
        let genE =
            match e with
            | MultilineString _
            | _ when (TriviaHelpers.``has content itself that is multiline string``
                          e.Range
                          (Map.tryFindOrEmptyList SynExpr_Const ctx.TriviaMainNodes)) -> genExpr astContext e
            | _ -> isShortExpressionOrAddIndentAndNewline maxWidth (genExpr astContext e)

        (sepEqual isPrefixMultiline
         +> leaveEqualsToken pat.Range
         +> ifElse
             (isPrefixMultiline || hasTriviaContentAfterEqual)
                (indent
                 +> sepNln
                 +> genExpr astContext e
                 +> unindent)
                genE) ctx

and genTyparList astContext tps =
    ifElse
        (List.atMostOne tps)
        (col wordOr tps (genTypar astContext))
        (sepOpenT
         +> col wordOr tps (genTypar astContext)
         +> sepCloseT)

and genTypeAndParam astContext typeName tds tcs preferPostfix =
    let types openSep closeSep =
        (!-openSep
         +> coli sepComma tds (fun i ->
                genTyparDecl
                    { astContext with
                          IsFirstTypeParam = i = 0 })
         +> colPre (!- " when ") wordAnd tcs (genTypeConstraint astContext)
         -- closeSep)

    if List.isEmpty tds then
        !-typeName
    elif preferPostfix then
        !-typeName +> types "<" ">"
    elif List.atMostOne tds then
        genTyparDecl
            { astContext with
                  IsFirstTypeParam = true }
            (List.head tds)
        +> sepSpace
        -- typeName
        +> colPre (!- " when ") wordAnd tcs (genTypeConstraint astContext)
    else
        types "(" ")" -- " " -- typeName

and genTypeParamPostfix astContext tds tcs =
    genTypeAndParam astContext "" tds tcs true

and genLetBinding astContext pref b =
    match b with
    | LetBinding (ats, px, ao, isInline, isMutable, p, e, valInfo) ->
        let genPat =
            match e, p with
            | TypedExpr (Typed, _, t), PatLongIdent (ao, s, ps, tpso) when (List.isNotEmpty ps) ->
                genPatWithReturnType ao s ps tpso (Some t) astContext
            | _, PatLongIdent (ao, s, ps, tpso) when (List.length ps > 1) ->
                genPatWithReturnType ao s ps tpso None astContext
            | _, PatTuple _ ->
                expressionFitsOnRestOfLine (genPat astContext p) (sepOpenT +> genPat astContext p +> sepCloseT)
            | _ -> genPat astContext p

        let genAttr =
            ifElse
                astContext.IsFirstChild
                (genAttributes astContext ats -- pref)
                (!-pref +> genOnelinerAttributes astContext ats)

        let afterLetKeyword =
            opt sepSpace ao genAccess
            +> ifElse isMutable (!- "mutable ") sepNone
            +> ifElse isInline (!- "inline ") sepNone

        let rangeBetweenBindingPatternAndExpression =
            mkRange "range between binding pattern and expression" b.RangeOfBindingSansRhs.End e.Range.Start

        genPreXmlDoc px
        +> genAttr // this already contains the `let` or `and` keyword
        +> leadingExpressionIsMultiline
            (afterLetKeyword
             +> genPat
             +> enterNodeTokenByName rangeBetweenBindingPatternAndExpression EQUALS)
               (genExprSepEqPrependType astContext p e (Some(valInfo)))

    | DoBinding (ats, px, e) ->
        let prefix =
            if pref.Contains("let") then pref.Replace("let", "do") else "do "

        genPreXmlDoc px +> genAttributes astContext ats
        -- prefix
        +> autoIndentAndNlnIfExpressionExceedsPageWidth (genExpr astContext e)

    | b -> failwithf "%O isn't a let binding" b
    +> leaveNodeFor (synBindingToFsAstType b) b.RangeOfBindingAndRhs

and genShortGetProperty astContext (pat: SynPat) e =
    genExprSepEqPrependType astContext pat e None false

and genProperty astContext prefix ao propertyKind ps e =
    let tuplerize ps =
        let rec loop acc =
            function
            | [ p ] -> (List.rev acc, p)
            | p1 :: ps -> loop (p1 :: acc) ps
            | [] -> invalidArg "p" "Patterns should not be empty"

        loop [] ps

    match ps with
    | [ PatTuple ps ] ->
        let (ps, p) = tuplerize ps

        !-prefix +> opt sepSpace ao genAccess
        -- propertyKind
        +> ifElse
            (List.atMostOne ps)
               (col sepComma ps (genPat astContext) +> sepSpace)
               (sepOpenT
                +> col sepComma ps (genPat astContext)
                +> sepCloseT
                +> sepSpace)
        +> genPat astContext p
        +> genExprSepEqPrependType astContext p e None false

    | ps ->
        let (_, p) = tuplerize ps

        !-prefix +> opt sepSpace ao genAccess
        -- propertyKind
        +> col sepSpace ps (genPat astContext)
        +> genExprSepEqPrependType astContext p e None false

and genPropertyWithGetSet astContext (b1, b2) rangeOfMember =
    match b1, b2 with
    | PropertyBinding (ats, px, ao, isInline, mf1, PatLongIdent (ao1, s1, ps1, _), e1),
      PropertyBinding (_, _, _, _, _, PatLongIdent (ao2, _, ps2, _), e2) ->
        let prefix =
            genPreXmlDoc px
            +> genAttributes astContext ats
            +> genMemberFlags astContext mf1
            +> ifElse isInline (!- "inline ") sepNone
            +> opt sepSpace ao genAccess

        assert (ps1 |> Seq.map fst |> Seq.forall Option.isNone)
        assert (ps2 |> Seq.map fst |> Seq.forall Option.isNone)
        let ps1 = List.map snd ps1
        let ps2 = List.map snd ps2

        prefix
        +> !-s1
        +> indent
        +> sepNln
        +> optSingle (fun rom -> enterNodeTokenByName rom WITH) rangeOfMember
        +> genProperty astContext "with " ao1 "get " ps1 e1
        +> sepNln
        +> genProperty astContext "and " ao2 "set " ps2 e2
        +> unindent
    | _ -> sepNone

and genMemberBindingList astContext node =
    let rec collectItems (node: SynBinding list) =
        match node with
        | [] -> []
        | mb :: rest ->
            let expr = genMemberBinding astContext mb
            let r = mb.RangeOfBindingAndRhs

            let sepNln =
                sepNlnConsideringTriviaContentBeforeForMainNode (synBindingToFsAstType mb) r

            (expr, sepNln, r) :: (collectItems rest)

    collectItems node |> colWithNlnWhenItemIsMultiline

and genMemberBinding astContext b =
    match b with
    | PropertyBinding (ats, px, ao, isInline, mf, p, e) ->
        let prefix =
            genPreXmlDoc px
            +> genAttributes astContext ats
            +> genMemberFlags astContext mf
            +> ifElse isInline (!- "inline ") sepNone
            +> opt sepSpace ao genAccess

        let propertyKind =
            match mf with
            | MFProperty PropertyGet -> "get "
            | MFProperty PropertySet -> "set "
            | mf -> failwithf "Unexpected member flags: %O" mf

        match p with
        | PatLongIdent (ao, s, ps, _) ->
            assert (ps |> Seq.map fst |> Seq.forall Option.isNone)

            match ao, propertyKind, ps with
            | None, "get ", [ _, PatParen (PatConst (Const "()", _)) ] ->
                // Provide short-hand notation `x.Member = ...` for `x.Member with get()` getters
                prefix -- s +> genShortGetProperty astContext p e
            | _ ->
                let ps = List.map snd ps

                prefix -- s
                +> indent
                +> sepNln
                +> genProperty astContext "with " ao propertyKind ps e
                +> unindent
        | p -> failwithf "Unexpected pattern: %O" p

    | MemberBinding (ats, px, ao, isInline, mf, p, e) ->
        let genAttributesAndXmlDoc =
            genPreXmlDoc px +> genAttributes astContext ats

        let prefix =
            genMemberFlagsForMemberBinding astContext mf b.RangeOfBindingAndRhs
            +> ifElse isInline (!- "inline ") sepNone
            +> opt sepSpace ao genAccess

        let rec genNameAndParameters p =
            match p with
            | PatLongIdent (ao, s, ps, tpso) ->
                let aoc = opt sepSpace ao genAccess

                let tpsoc =
                    opt sepNone tpso (fun (ValTyparDecls (tds, _, tcs)) -> genTypeParamPostfix astContext tds tcs)

                let hasBracket =
                    ps |> Seq.map snd |> Seq.exists hasParenInPat

                let hasParameters = List.isNotEmpty ps
                let multipleParameters = List.length ps > 1

                let spaceAfter ctx =
                    onlyIf
                        (ctx.Config.SpaceBeforeMember && hasParameters
                         || not hasBracket && hasParameters
                         || multipleParameters)
                        sepSpace
                        ctx

                let name =
                    aoc
                    +> infixOperatorFromTrivia p.Range s
                    +> tpsoc
                    +> spaceAfter

                let parameters =
                    let astCtx =
                        { astContext with
                              IsMemberDefinition = true }

                    let shortExpr =
                        atCurrentColumn (col sepSpace ps (genPatWithIdent astCtx))

                    let longExpr ctx =
                        if ctx.Config.AlternativeLongMemberDefinitions then
                            (indent
                             +> sepNln
                             +> col sepNln ps (genPatWithIdent astCtx)
                             +> unindent) ctx
                        else
                            (atCurrentColumn (col sepNln ps (genPatWithIdent astCtx))) ctx

                    expressionFitsOnRestOfLine shortExpr longExpr

                name, parameters
            | PatTyped (p, t) ->
                let n, p = genNameAndParameters p
                n +> sepColon +> genType astContext false t, p
            | PatParen (p) -> genNameAndParameters p
            | PatNamed (ao, _, s) -> optSingle genAccess ao +> sepSpace -- s, sepNone
            | _ -> sepNone, sepNone

        let hasParenthesis =
            match p with
            | PatParen _ -> true
            | _ -> false

        let isSingleTuple =
            match p with
            | PatLongIdent (_, _, [ _, PatParen (PatTuple _) ], _)
            | PatLongIdent (_, _, [ _, PatParen (PatNamed _) ], _) -> true
            | _ -> false

        let genName, genParameters = genNameAndParameters p

        match e with
        | TypedExpr (Typed, e, t) ->
            let memberDefinition =
                let shortExpr =
                    genName
                    +> genParameters
                    +> sepColon
                    +> genType astContext false t
                    +> sepEq

                let longExpr ctx =
                    let expr =
                        if ctx.Config.AlternativeLongMemberDefinitions then
                            genName
                            +> genParameters
                            +> indent
                            +> sepNln
                            +> sepColon
                            +> genType astContext false t
                            +> sepNln
                            +> sepEqFixed
                            +> unindent
                        else
                            genName
                            +> atCurrentColumn
                                (genParameters
                                 +> sepNln
                                 +> onlyIf isSingleTuple (!- " ")
                                 +> sepColon
                                 +> genType astContext false t
                                 +> sepEq)

                    expr ctx

                prefix
                +> expressionFitsOnRestOfLine shortExpr longExpr

            genAttributesAndXmlDoc
            +> leadingExpressionIsMultiline memberDefinition (fun mdLong ctx ->
                   let maxWidth =
                       if isFunctionBinding p then
                           ctx.Config.MaxFunctionBindingWidth
                       else
                           ctx.Config.MaxValueBindingWidth

                   ifElse
                       mdLong
                       (indent
                        +> sepNln
                        +> genExpr astContext e
                        +> unindent)
                       (sepSpace
                        +> isShortExpressionOrAddIndentAndNewline maxWidth (genExpr astContext e))
                       ctx)
        | e ->
            let memberDefinition =
                let shortExpr =
                    onlyIf hasParenthesis sepOpenT
                    +> genName
                    +> onlyIf hasParenthesis sepCloseT
                    +> genParameters
                    +> sepEq
                    +> sepSpace

                let longExpr ctx =
                    if ctx.Config.AlternativeLongMemberDefinitions then
                        (genName
                         +> genParameters
                         +> indent
                         +> sepNln
                         +> sepEqFixed
                         +> unindent) ctx
                    else
                        (onlyIf hasParenthesis sepOpenT
                         +> genName
                         +> onlyIf hasParenthesis sepCloseT
                         +> atCurrentColumn
                             (genParameters
                              +> ifElse isSingleTuple sepEq (sepNln +> sepEqFixed))) ctx

                prefix
                +> expressionFitsOnRestOfLine shortExpr longExpr

            genAttributesAndXmlDoc
            +> leadingExpressionIsMultiline memberDefinition (fun mdLong ctx ->
                   let maxWidth =
                       if isFunctionBinding p then
                           ctx.Config.MaxFunctionBindingWidth
                       else
                           ctx.Config.MaxValueBindingWidth

                   ifElse
                       mdLong
                       (indent
                        +> sepNln
                        +> genExpr astContext e
                        +> unindent)
                       (isShortExpressionOrAddIndentAndNewline maxWidth (genExpr astContext e))
                       ctx)

    | ExplicitCtor (ats, px, ao, p, e, so) ->
        let prefix =
            genPreXmlDoc px
            +> genAttributes astContext ats
            +> opt sepSpace ao genAccess
            +> genPat astContext p
            +> opt sepNone so (sprintf " as %s" >> (!-))

        match e with
        // Handle special "then" block i.e. fake sequential expressions in constructors
        | Sequential (e1, e2, false) ->
            prefix
            +> sepEq
            +> indent
            +> sepNln
            +> genExpr astContext e1
            ++ "then "
            +> autoIndentAndNlnIfExpressionExceedsPageWidth (genExpr astContext e2)
            +> unindent

        | e ->
            prefix
            +> sepEq
            +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (genExpr astContext e)

    | b -> failwithf "%O isn't a member binding" b
    |> genTriviaFor (synBindingToFsAstType b) b.RangeOfBindingAndRhs

and genMemberFlags astContext (mf: MemberFlags) =
    match mf with
    | MFMember _ -> !- "member "
    | MFStaticMember _ -> !- "static member "
    | MFConstructor _ -> sepNone
    | MFOverride _ -> ifElse astContext.InterfaceRange.IsSome (!- "member ") (!- "override ")

and genMemberFlagsForMemberBinding astContext (mf: MemberFlags) (rangeOfBindingAndRhs: range) =
    fun ctx ->
        let keywordFromTrivia =
            [ yield! (Map.tryFindOrEmptyList SynMemberDefn_Member ctx.TriviaMainNodes)
              yield! (Map.tryFindOrEmptyList SynMemberSig_Member ctx.TriviaMainNodes)
              yield! (Map.tryFindOrEmptyList MEMBER ctx.TriviaTokenNodes) ]
            |> List.tryFind (fun { Type = t; Range = r } ->
                match t with
                | MainNode SynMemberDefn_Member
                | MainNode SynMemberSig_Member -> // trying to get AST trivia
                    RangeHelpers.``range contains`` r rangeOfBindingAndRhs

                | Token (MEMBER, _) -> // trying to get token trivia
                    r.StartLine = rangeOfBindingAndRhs.StartLine

                | _ -> false)
            |> Option.bind (fun tn ->
                tn.ContentItself
                |> Option.bind (fun tc ->
                    match tc with
                    | Keyword ({ Content = ("override"
                                 | "default"
                                 | "member"
                                 | "abstract"
                                 | "abstract member") as kw }) -> Some(!-(kw + " "))
                    | _ -> None))

        match mf with
        | MFStaticMember _
        | MFConstructor _ -> genMemberFlags astContext mf
        | MFMember _ ->
            keywordFromTrivia
            |> Option.defaultValue (genMemberFlags astContext mf)
        | MFOverride _ ->
            keywordFromTrivia
            |> Option.defaultValue (!- "override ")
        <| ctx

and genVal astContext (Val (ats, px, ao, s, t, vi, isInline, _) as node) =
    let range, synValTyparDecls =
        match node with
        | ValSpfn (_, _, synValTyparDecls, _, _, _, _, _, _, _, range) -> range, synValTyparDecls

    let genericParams =
        match synValTyparDecls with
        | SynValTyparDecls ([], _, _) -> sepNone
        | SynValTyparDecls (tpd, _, cst) -> genTypeParamPostfix astContext tpd cst

    let (FunType namedArgs) = (t, vi)

    genPreXmlDoc px
    +> genAttributes astContext ats
    +> atCurrentColumn
        (indent -- "val "
         +> onlyIf isInline (!- "inline ")
         +> opt sepSpace ao genAccess
         -- s
         +> genericParams
         +> addSpaceAfterGenericConstructBeforeColon
         +> sepColon
         +> ifElse
             (List.isNotEmpty namedArgs)
                (autoNlnIfExpressionExceedsPageWidth (genTypeList astContext namedArgs))
                (genConstraints astContext t)
         +> unindent)
    |> genTriviaFor ValSpfn_ range

and genRecordFieldName astContext (RecordFieldName (s, eo) as node) =
    let (rfn, _, _) = node
    let range = (fst rfn).Range

    opt sepNone eo (fun e ->
        let expr =
            match e with
            | MultilineString _ -> sepSpace +> genExpr astContext e
            | _ -> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (genExpr astContext e)

        !-s +> sepEq +> expr)
    |> genTriviaFor RecordField_ range

and genAnonRecordFieldName astContext (AnonRecordFieldName (s, e)) =
    let expr =
        match e with
        | MultilineString _ -> sepSpace +> genExpr astContext e
        | _ -> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (genExpr astContext e)

    !-s +> sepEq +> expr

and genTuple astContext es =
    let genExpr e =
        let expr e =
            match e with
            | InfixApp (equal, operatorExpr, e1, e2) when (equal = "=") ->
                genNamedArgumentExpr astContext operatorExpr e1 e2
            | _ -> genExpr astContext e

        addParenForTupleWhen expr e

    let shortExpression = col sepComma es genExpr
    let longExpression = col (sepComma +> sepNln) es genExpr

    atCurrentColumn (expressionFitsOnRestOfLine shortExpression longExpression)

and genNamedArgumentExpr (astContext: ASTContext) operatorExpr e1 e2 =
    let short =
        genExpr astContext e1
        +> sepSpace
        +> genInfixOperator "=" operatorExpr
        +> sepSpace
        +> genExpr astContext e2

    match e2 with
    | MultilineString _ -> short
    | _ ->
        let long =
            genExpr astContext e1
            +> sepSpace
            +> genInfixOperator "=" operatorExpr
            +> indent
            +> sepNln
            +> genExpr astContext e2
            +> unindent

        expressionFitsOnRestOfLine short long

and genExpr astContext synExpr ctx =
    let appNlnFun e =
        match e with
        | MultilineString _ -> id
        | _ -> autoNlnIfExpressionExceedsPageWidth

    let kw tokenName f = tokN synExpr.Range tokenName f

    let sepOpenTFor r =
        tokN (Option.defaultValue synExpr.Range r) LPAREN sepOpenT

    let sepCloseTFor r =
        tokN (Option.defaultValue synExpr.Range r) RPAREN sepCloseT

    let expr =
        match synExpr with
        | ElmishReactWithoutChildren (identifier, isArray, children) when (not ctx.Config.DisableElmishSyntax) ->
            fun ctx ->
                let shortExpression =
                    let noChildren =
                        ifElse isArray sepOpenAFixed sepOpenLFixed
                        +> ifElse isArray sepCloseAFixed sepCloseLFixed

                    let genChildren =
                        ifElse isArray sepOpenA sepOpenL
                        +> col sepSemi children (genExpr astContext)
                        +> enterNodeTokenByName synExpr.Range (if isArray then BAR_RBRACK else RBRACK)
                        +> ifElse isArray sepCloseA sepCloseL

                    !-identifier
                    +> sepSpace
                    +> ifElse (List.isEmpty children) noChildren genChildren

                let elmishExpression =
                    !-identifier
                    +> sepSpace
                    +> ifElse isArray sepOpenA sepOpenL
                    +> atCurrentColumn
                        (col sepNln children (genExpr astContext)
                         +> enterNodeTokenByName synExpr.Range (if isArray then BAR_RBRACK else RBRACK))
                    +> ifElse isArray sepCloseA sepCloseL
                    +> leaveNodeTokenByName synExpr.Range (if isArray then BAR_RBRACK else RBRACK)

                let felizExpression =
                    atCurrentColumn
                        (!-identifier
                         +> sepSpace
                         +> ifElse isArray sepOpenAFixed sepOpenLFixed
                         +> indent
                         +> sepNln
                         +> col sepNln children (genExpr astContext)
                         +> unindent
                         +> sepNln
                         +> enterNodeTokenByName synExpr.Range (if isArray then BAR_RBRACK else RBRACK)
                         +> ifElse isArray sepCloseAFixed sepCloseLFixed
                         +> leaveNodeTokenByName synExpr.Range (if isArray then BAR_RBRACK else RBRACK))

                let multilineExpression =
                    ifElse ctx.Config.SingleArgumentWebMode felizExpression elmishExpression

                let size =
                    getListOrArrayExprSize ctx ctx.Config.MaxElmishWidth children

                let smallExpression =
                    isSmallExpression size shortExpression multilineExpression

                isShortExpression ctx.Config.MaxElmishWidth smallExpression multilineExpression ctx

        | ElmishReactWithChildren ((identifier, _, _), attributes, (isArray, children, childrenRange)) when (not
                                                                                                                 ctx.Config.DisableElmishSyntax) ->
            let genChildren isShort =
                match children with
                | [] when (not isArray) ->
                    sepOpenLFixed
                    +> sepCloseLFixed
                    +> leaveNodeTokenByName childrenRange RBRACK
                | [] when isArray -> sepOpenAFixed +> sepCloseAFixed
                | [ singleChild ] ->
                    if isShort then
                        ifElse isArray sepOpenA sepOpenL
                        +> genExpr astContext singleChild
                        +> ifElse
                            isArray
                               sepCloseA
                               (sepCloseL
                                +> leaveNodeTokenByName childrenRange RBRACK)
                    else
                        ifElse isArray sepOpenA sepOpenL
                        +> indent
                        +> sepNln
                        +> genExpr astContext singleChild
                        +> unindent
                        +> sepNln
                        +> ifElse
                            isArray
                               sepCloseAFixed
                               (sepCloseLFixed
                                +> leaveNodeTokenByName childrenRange RBRACK)

                | children ->
                    if isShort then
                        ifElse isArray sepOpenA sepOpenL
                        +> col sepSemi children (genExpr astContext)
                        +> ifElse isArray sepCloseA sepCloseL
                    else
                        ifElse isArray sepOpenA sepOpenL
                        +> indent
                        +> sepNln
                        +> col sepNln children (genExpr astContext)
                        +> unindent
                        +> sepNln
                        +> ifElse isArray sepCloseAFixed sepCloseLFixed

            let shortExpression =
                !-identifier
                +> sepSpace
                +> genExpr astContext attributes
                +> sepSpace
                +> genChildren true

            let longExpression =
                atCurrentColumn
                    (!-identifier
                     +> sepSpace
                     +> atCurrentColumn (genExpr astContext attributes)
                     +> sepSpace
                     +> genChildren false)

            fun ctx ->
                let size =
                    getListOrArrayExprSize ctx ctx.Config.MaxElmishWidth children

                let smallExpression =
                    isSmallExpression size shortExpression longExpression

                isShortExpression ctx.Config.MaxElmishWidth smallExpression longExpression ctx

        | SingleExpr (Lazy, e) ->
            // Always add braces when dealing with lazy
            let hasParenthesis = hasParenthesis e

            let isInfixExpr =
                match e with
                | InfixApp _ -> true
                | _ -> false

            let genInfixExpr (ctx: Context) =
                isShortExpression
                    ctx.Config.MaxInfixOperatorExpression
                    // if this fits on the rest of line right after the lazy keyword, it should be wrapped in parenthesis.
                    (sepOpenT +> genExpr astContext e +> sepCloseT)
                    // if it is multiline there is no need for parenthesis, because of the indentation
                    (indent
                     +> sepNln
                     +> genExpr astContext e
                     +> unindent)
                    ctx

            let genNonInfixExpr =
                autoIndentAndNlnIfExpressionExceedsPageWidth
                    (onlyIfNot hasParenthesis sepOpenT
                     +> genExpr astContext e
                     +> onlyIfNot hasParenthesis sepCloseT)

            str "lazy "
            +> ifElse isInfixExpr genInfixExpr genNonInfixExpr

        | SingleExpr (kind, e) ->
            enterNodeFor SynExpr_Do synExpr.Range
            +> str kind
            +> (match kind with
                | YieldFrom
                | Yield
                | Return
                | ReturnFrom -> autoIndentAndNlnIfExpressionExceedsPageWidth (genExpr astContext e)
                | _ -> genExpr astContext e)

        | ConstExpr (c, r) -> genConst c r
        | NullExpr -> !- "null"
        // Not sure about the role of e1
        | Quote (_, e2, isRaw) ->
            let e = genExpr astContext e2
            ifElse isRaw (!- "<@@ " +> e -- " @@>") (!- "<@ " +> e -- " @>")
        | TypedExpr (TypeTest, e, t) ->
            genExpr astContext e -- " :? "
            +> genType astContext false t
        | TypedExpr (New, e, t) ->
            !- "new "
            +> genType astContext false t
            +> ifElse (hasParenthesis e) sepNone sepSpace
            +> genExpr astContext e
        | TypedExpr (Downcast, e, t) ->
            genExpr astContext e -- " :?> "
            +> genType astContext false t
        | TypedExpr (Upcast, e, t) ->
            genExpr astContext e -- " :> "
            +> genType astContext false t
        | TypedExpr (Typed, e, t) ->
            genExpr astContext e
            +> sepColon
            +> genType astContext false t
        | Tuple es -> genTuple astContext es
        | StructTuple es ->
            !- "struct "
            +> sepOpenT
            +> genTuple astContext es
            +> sepCloseT
        | ArrayOrList (isArray, [], _) ->
            ifElse
                isArray
                (enterNodeTokenByName synExpr.Range LBRACK_BAR
                 +> sepOpenAFixed
                 +> leaveNodeTokenByName synExpr.Range LBRACK_BAR
                 +> enterNodeTokenByName synExpr.Range BAR_RBRACK
                 +> sepCloseAFixed
                 +> leaveNodeTokenByName synExpr.Range BAR_RBRACK)
                (enterNodeTokenByName synExpr.Range LBRACK
                 +> sepOpenLFixed
                 +> leaveNodeTokenByName synExpr.Range LBRACK
                 +> enterNodeTokenByName synExpr.Range RBRACK
                 +> sepCloseLFixed
                 +> leaveNodeTokenByName synExpr.Range RBRACK)
        | ArrayOrList (isArray, xs, _) as alNode ->
            let smallExpression =
                ifElse isArray sepOpenA sepOpenL
                +> col sepSemi xs (genExpr astContext)
                +> ifElse isArray sepCloseA sepCloseL

            let multilineExpression =
                ifAlignBrackets
                    (genMultiLineArrayOrListAlignBrackets isArray xs alNode astContext)
                    (genMultiLineArrayOrList isArray xs alNode astContext)

            fun ctx ->
                // If an array or list has any form of line comments inside them, they cannot fit on a single line
                // check for any comments inside the range of the node
                if TriviaHelpers.``has line comments inside``
                    alNode.Range
                       (TriviaHelpers.getNodesForTypes
                           [ LBRACK_BAR
                             LBRACK
                             BAR_RBRACK
                             RBRACK ]
                            ctx.TriviaTokenNodes)
                   || List.exists isIfThenElseWithYieldReturn xs then
                    multilineExpression ctx
                else
                    let size =
                        getListOrArrayExprSize ctx ctx.Config.MaxArrayOrListWidth xs

                    isSmallExpression size smallExpression multilineExpression ctx

        | Record (inheritOpt, xs, eo) ->
            let smallRecordExpr =
                sepOpenS
                +> leaveLeftBrace synExpr.Range
                +> optSingle (fun (inheritType, inheritExpr) ->
                    !- "inherit "
                    +> genType astContext false inheritType
                    +> genExpr astContext inheritExpr
                    +> onlyIf (List.isNotEmpty xs) sepSemi) inheritOpt
                +> optSingle (fun e -> genExpr astContext e +> !- " with ") eo
                +> col sepSemi xs (genRecordFieldName astContext)
                +> sepCloseS
                +> leaveNodeTokenByName synExpr.Range RBRACE

            let multilineRecordExpr =
                ifAlignBrackets
                    (genMultilineRecordInstanceAlignBrackets inheritOpt xs eo synExpr astContext)
                    (genMultilineRecordInstance inheritOpt xs eo synExpr astContext)

            fun ctx ->
                let size = getRecordSize ctx xs
                isSmallExpression size smallRecordExpr multilineRecordExpr ctx

        | AnonRecord (isStruct, fields, copyInfo) ->
            let smallExpression =
                onlyIf isStruct !- "struct "
                +> sepOpenAnonRecd
                +> optSingle (fun e -> genExpr astContext e +> !- " with ") copyInfo
                +> col sepSemi fields (genAnonRecordFieldName astContext)
                +> sepCloseAnonRecd

            let longExpression =
                ifAlignBrackets
                    (genMultilineAnonRecordAlignBrackets isStruct fields copyInfo astContext)
                    (genMultilineAnonRecord isStruct fields copyInfo astContext)

            fun (ctx: Context) ->
                let size = getRecordSize ctx fields
                isSmallExpression size smallExpression longExpression ctx

        | ObjExpr (t, eio, bd, ims, range) ->
            ifAlignBrackets
                (genObjExprAlignBrackets t eio bd ims range astContext)
                (genObjExpr t eio bd ims range astContext)

        | While (e1, e2) ->
            atCurrentColumn
                (!- "while " +> genExpr astContext e1 -- " do"
                 +> indent
                 +> sepNln
                 +> genExpr astContext e2
                 +> unindent)

        | For (s, e1, e2, e3, isUp) ->
            atCurrentColumn
                (!-(sprintf "for %s = " s)
                 +> genExpr astContext e1
                 +> ifElse isUp (!- " to ") (!- " downto ")
                 +> genExpr astContext e2
                 -- " do"
                 +> indent
                 +> sepNln
                 +> genExpr astContext e3
                 +> unindent)

        // Handle the form 'for i in e1 -> e2'
        | ForEach (p, e1, e2, isArrow) ->
            atCurrentColumn
                (!- "for " +> genPat astContext p -- " in "
                 +> genExpr { astContext with IsNakedRange = true } e1
                 +> ifElse
                     isArrow
                        (sepArrow
                         +> autoIndentAndNlnIfExpressionExceedsPageWidth (genExpr astContext e2))
                        (!- " do"
                         +> indent
                         +> sepNln
                         +> genExpr astContext e2
                         +> unindent))

        | CompExpr (isArrayOrList, e) ->
            ifElse
                isArrayOrList
                (genExpr astContext e)
                // The opening { of the CompExpr is being added at the App(_,_,Ident(_),CompExr(_)) level
                (expressionFitsOnRestOfLine
                    (genExpr astContext e +> sepCloseS)
                     (genExpr astContext e
                      +> unindent
                      +> enterNodeTokenByName synExpr.Range RBRACE
                      +> sepNlnUnlessLastEventIsNewline
                      +> sepCloseSFixed))

        | CompExprBody (expr) ->
            let statements =
                collectComputationExpressionStatements expr

            let genCompExprStatement astContext ces =
                match ces with
                | LetOrUseStatement (isRecursive, isUse, binding) ->
                    let prefix =
                        sprintf "%s%s" (if isUse then "use " else "let ") (if isRecursive then "rec " else "")

                    enterNodeFor (synBindingToFsAstType binding) binding.RangeOfBindingAndRhs
                    +> genLetBinding astContext prefix binding
                | LetOrUseBangStatement (isUse, pat, expr, r) ->
                    enterNodeFor SynExpr_LetOrUseBang r // print Trivia before entire LetBang expression
                    +> ifElse isUse (!- "use! ") (!- "let! ")
                    +> genPat astContext pat
                    -- " = "
                    +> autoIndentAndNlnIfExpressionExceedsPageWidth (genExpr astContext expr)
                | AndBangStatement (pat, expr, andRange) ->
                    enterNodeTokenByName andRange AND_BANG
                    +> !- "and! "
                    +> genPat astContext pat
                    -- " = "
                    +> autoIndentAndNlnIfExpressionExceedsPageWidth (genExpr astContext expr)
                | OtherStatement expr -> genExpr astContext expr

            let getRangeOfCompExprStatement ces =
                match ces with
                | LetOrUseStatement (_, _, binding) -> binding.RangeOfBindingAndRhs
                | LetOrUseBangStatement (_, _, _, r) -> r
                | AndBangStatement (_, _, r) -> r
                | OtherStatement expr -> expr.Range

            let getSepNln ces r =
                match ces with
                | LetOrUseStatement (_, _, b) ->
                    sepNlnConsideringTriviaContentBeforeForMainNode (synBindingToFsAstType b) r
                | LetOrUseBangStatement _ -> sepNlnConsideringTriviaContentBeforeForMainNode SynExpr_LetOrUseBang r
                | AndBangStatement _ -> sepNlnConsideringTriviaContentBeforeForToken AND_BANG r
                | OtherStatement e -> sepNlnConsideringTriviaContentBeforeForMainNode (synExprToFsAstType e) r

            statements
            |> List.map (fun ces ->
                let expr = genCompExprStatement astContext ces
                let r = getRangeOfCompExprStatement ces
                let sepNln = getSepNln ces r
                expr, sepNln, r)
            |> colWithNlnWhenItemIsMultiline

        | ArrayOrListOfSeqExpr (isArray, e) as aNode ->
            let astContext = { astContext with IsNakedRange = true }

            let shortExpression =
                ifElse
                    isArray
                    (sepOpenA
                     +> genExpr astContext e
                     +> enterRightBracketBar aNode.Range
                     +> sepCloseA)
                    (sepOpenL
                     +> genExpr astContext e
                     +> enterRightBracket aNode.Range
                     +> sepCloseL
                     +> leaveNodeTokenByName aNode.Range RBRACK)

            let bracketsOnSameColumn =
                ifElse isArray sepOpenAFixed sepOpenLFixed
                +> indent
                +> sepNln
                +> genExpr astContext e
                +> unindent
                +> sepNln
                +> ifElse isArray sepCloseAFixed sepCloseLFixed

            let multilineExpression =
                ifAlignBrackets bracketsOnSameColumn shortExpression

            fun ctx -> isShortExpression ctx.Config.MaxArrayOrListWidth shortExpression multilineExpression ctx

        | JoinIn (e1, e2) ->
            genExpr astContext e1 -- " in "
            +> genExpr astContext e2
        | Paren (lpr, DesugaredLambda (cps, e), rpr) ->
            fun (ctx: Context) ->
                let arrowRange =
                    List.last cps
                    |> snd
                    |> fun lastPatRange -> mkRange "arrow range" lastPatRange.End e.Range.Start

                let hasLineCommentAfterArrow =
                    findTriviaTokenFromName RARROW arrowRange ctx
                    |> Option.isSome

                let astCtx =
                    { astContext with
                          IsInsideDotGet = false }

                let expr =
                    sepOpenTFor (Some lpr) -- "fun "
                    +> col sepSpace cps (fst >> genComplexPats astContext)
                    +> indent
                    +> triviaAfterArrow arrowRange
                    +> (fun ctx ->
                        if hasLineCommentAfterArrow
                        then (genExpr astCtx e +> sepCloseTFor rpr) ctx
                        else (autoNlnIfExpressionExceedsPageWidth (genExpr astCtx e +> sepCloseTFor rpr)) ctx)
                    +> unindent

                expr ctx

        | DesugaredLambda (cps, e) ->
            !- "fun "
            +> col sepSpace cps (fst >> genComplexPats astContext)
            +> sepArrow
            +> autoIndentAndNlnIfExpressionExceedsPageWidth (genExpr astContext e)
        | Paren (lpr, Lambda (e, sps), rpr) ->
            fun (ctx: Context) ->
                let hasLineCommentAfterArrow =
                    findTriviaTokenFromName RARROW synExpr.Range ctx
                    |> Option.isSome

                let astCtx =
                    { astContext with
                          IsInsideDotGet = false }

                let expr =
                    sepOpenTFor (Some lpr) -- "fun "
                    +> col sepSpace sps (genSimplePats astContext)
                    +> indent
                    +> triviaAfterArrow synExpr.Range
                    +> (fun ctx ->
                        if hasLineCommentAfterArrow
                        then (genExpr astCtx e +> sepCloseTFor rpr) ctx
                        else autoNlnIfExpressionExceedsPageWidth (genExpr astCtx e +> sepCloseTFor rpr) ctx)
                    +> unindent

                expr ctx

        // When there are parentheses, most likely lambda will appear in function application
        | Lambda (e, sps) ->
            atCurrentColumn
                (!- "fun "
                 +> col sepSpace sps (genSimplePats astContext)
                 +> sepArrow
                 +> autoIndentAndNlnIfExpressionExceedsPageWidth (genExpr astContext e))
        | MatchLambda (sp, _) ->
            !- "function "
            +> leaveNodeTokenByName synExpr.Range FUNCTION
            +> colPre sepNln sepNln sp (genClause astContext true)
        | Match (e, cs) ->
            atCurrentColumn
                (!- "match "
                 +> genExpr astContext e
                 +> enterNodeTokenByName synExpr.Range WITH
                 // indent 'with' further if trivia was printed so that is appear after the match keyword.
                 +> ifElseCtx lastWriteEventIsNewline (rep 5 !- " ") sepNone
                 -- " with"
                 +> leaveNodeTokenByName synExpr.Range WITH
                 +> colPre sepNln sepNln cs (genClause astContext true))
        | MatchBang (e, cs) ->
            atCurrentColumn
                (!- "match! " +> genExpr astContext e -- " with"
                 +> colPre sepNln sepNln cs (genClause astContext true))
        | TraitCall (tps, msg, e) ->
            genTyparList astContext tps
            +> sepColon
            +> sepOpenT
            +> genMemberSig astContext msg
            +> sepCloseT
            +> sepSpace
            +> genExpr astContext e

        | Paren (_, ILEmbedded r, _) ->
            // Just write out original code inside (# ... #)
            fun ctx -> !- (defaultArg (lookup r ctx) "") ctx
        | Paren (lpr, e, rpr) ->
            match e with
            | MultilineString _ ->
                sepOpenTFor (Some lpr)
                +> atCurrentColumn
                    (genExpr
                        { astContext with
                              IsInsideDotGet = false }
                         e
                     +> indentIfNeeded sepNone)
                +> sepCloseTFor rpr
            | InfixApp (equal, operatorExpr, e1, e2) when (equal = "=") ->
                sepOpenTFor (Some lpr)
                +> genNamedArgumentExpr astContext operatorExpr e1 e2
                +> sepCloseTFor rpr
            | _ ->
                // Parentheses nullify effects of no space inside DotGet
                sepOpenTFor (Some lpr)
                +> genExpr
                    { astContext with
                          IsInsideDotGet = false }
                       e
                +> sepCloseTFor rpr
        | CompApp (s, e) ->
            !-s
            +> sepSpace
            +> sepOpenS
            +> genExpr { astContext with IsNakedRange = true } e
            +> sepCloseS
        // This supposes to be an infix function, but for some reason it isn't picked up by InfixApps
        | App (Var "?", e :: es) ->
            match es with
            | SynExpr.Const (SynConst.String _, _) :: _ ->
                genExpr astContext e -- "?"
                +> col sepSpace es (genExpr astContext)
            | _ ->
                genExpr astContext e -- "?"
                +> sepOpenT
                +> col sepSpace es (genExpr astContext)
                +> sepCloseT

        | App (Var "..", [ e1; e2 ]) ->
            let expr =
                genExpr astContext e1 +> sepSpace -- ".."
                +> sepSpace
                +> genExpr astContext e2

            ifElse astContext.IsNakedRange expr (sepOpenS +> expr +> sepCloseS)
        | App (Var ".. ..", [ e1; e2; e3 ]) ->
            let expr =
                genExpr astContext e1 +> sepSpace -- ".."
                +> sepSpace
                +> genExpr astContext e2
                +> sepSpace
                -- ".."
                +> sepSpace
                +> genExpr astContext e3

            ifElse astContext.IsNakedRange expr (sepOpenS +> expr +> sepCloseS)
        // Separate two prefix ops by spaces
        | PrefixApp (s1, PrefixApp (s2, e)) -> !-(sprintf "%s %s" s1 s2) +> genExpr astContext e
        | PrefixApp (s, e) ->
            let extraSpaceBeforeString =
                match e with
                | String _ -> sepSpace
                | _ -> sepNone

            !-s
            +> extraSpaceBeforeString
            +> genExpr astContext e

        | NewlineInfixApp (operatorText, operatorExpr, (Lambda _ as e1), e2) ->
            genMultilineInfixExpr astContext e1 operatorText operatorExpr e2

        | NewlineInfixApps (e, es) ->
            let smallExpr =
                genExpr astContext e
                +> sepSpace
                +> col sepSpace es (fun (s, oe, e) ->
                       genInfixOperator s oe
                       +> sepSpace
                       +> genExpr astContext e)

            let multilineExpr =
                genExpr astContext e
                +> sepNln
                +> col sepNln es (fun (s, oe, e) ->
                       genInfixOperator s oe
                       +> sepSpace
                       +> genExpr astContext e)

            fun ctx ->
                let size = getInfixOperatorExpressionSize ctx es
                atCurrentColumn (isSmallExpression size smallExpr multilineExpr) ctx

        | NewlineInfixApp (operatorText, operatorExpr, e1, e2) when (ctx.Config.MultilineInfixMultilineFormatter = MultilineFormatterType.NumberOfItems) ->
            let expr sep =
                genExpr astContext e1
                +> sep
                +> genInfixOperator operatorText operatorExpr
                +> sepSpace
                +> genExpr astContext e2

            fun ctx ->
                let size =
                    getInfixOperatorExpressionSize ctx [ operatorExpr ]

                let smallExpr = expr sepSpace
                let multilineExpr = expr sepNln
                atCurrentColumn (isSmallExpression size smallExpr multilineExpr) ctx

        | SameInfixApps (e, es) ->
            let shortExpr =
                genExpr astContext e
                +> sepSpace
                +> col sepSpace es (fun (s, oe, e) ->
                       genInfixOperator s oe
                       +> sepSpace
                       +> genExpr astContext e)

            let multilineExpr =
                genExpr astContext e
                +> sepNln
                +> col sepNln es (fun (s, oe, e) ->
                       genInfixOperator s oe
                       +> sepSpace
                       +> genExpr astContext e)

            fun ctx ->
                atCurrentColumn (isShortExpression ctx.Config.MaxInfixOperatorExpression shortExpr multilineExpr) ctx

        | InfixApp (operatorText, operatorExpr, e1, e2) ->
            fun ctx ->
                isShortExpression
                    ctx.Config.MaxInfixOperatorExpression
                    (genOnelinerInfixExpr astContext e1 operatorText operatorExpr e2)
                    (genMultilineInfixExpr astContext e1 operatorText operatorExpr e2)
                    ctx

        | TernaryApp (e1, e2, e3) ->
            atCurrentColumn
                (genExpr astContext e1
                 +> !- "?"
                 +> genExpr astContext e2
                 +> sepSpace
                 +> !- "<-"
                 +> sepSpace
                 +> genExpr astContext e3)

        | DotGetApp (e, es) as appNode ->
            fun (ctx: Context) ->
                // find all the lids recursively + range of do expr
                let dotGetFuncExprIdents =
                    let rec selectIdent appNode =
                        match appNode with
                        | SynExpr.App (_,
                                       _,
                                       (SynExpr.DotGet (_, _, LongIdentWithDots.LongIdentWithDots (lids, _), _) as dotGet),
                                       argExpr,
                                       _) ->
                            let lids =
                                List.map (fun lid -> (argExpr.Range, lid)) lids

                            let childLids = selectIdent dotGet
                            lids @ childLids
                        | SynExpr.DotGet (aExpr, _, _, _) -> selectIdent aExpr
                        | _ -> []

                    selectIdent appNode

                let expr sep =
                    match e with
                    | App (TypeApp (LongIdentPieces (lids), ts), [ e2 ]) when (List.moreThanOne lids) ->
                        !-(List.head lids)
                        +> indent
                        +> sep
                        +> col sep (List.tail lids) (fun s -> !-(sprintf ".%s" s))
                        +> genGenericTypeParameters astContext ts
                        +> genExpr astContext e2
                        +> unindent
                    | App (LongIdentPieces (lids), [ e2 ]) when (List.moreThanOne lids) ->
                        !-(List.head lids)
                        +> indent
                        +> sep
                        +> col sep (List.tail lids) (fun s -> !-(sprintf ".%s" s))
                        +> genExpr astContext e2
                        +> unindent
                    | App (e1, [ e2 ]) ->
                        noNln
                            (genExpr astContext e1
                             +> ifElse (hasParenthesis e2) sepNone sepSpace
                             +> genExpr
                                 { astContext with
                                       IsInsideDotGet = true }
                                    e2)
                    | _ -> genExpr astContext e

                let genExpr sep =
                    let genTriviaOfIdent f (e: SynExpr) =
                        dotGetFuncExprIdents
                        |> List.tryFind (fun (er, _) -> er = e.Range)
                        |> Option.map
                            (snd
                             >> (fun lid -> genTriviaFor Ident_ lid.idRange))
                        |> Option.defaultValue id
                        |> fun genTrivia -> genTrivia f

                    match es with
                    | [ (s, _), e, [] ] when (not (hasParenthesis e)) ->
                        expr sep
                        +> genTriviaOfIdent (!-(sprintf ".%s" s)) e
                        +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (genExpr astContext e)
                    | _ ->
                        expr sep
                        +> indent
                        +> sep
                        +> coli sep es (fun idx ((s, _), e, ts) ->
                               let currentIdentifier =
                                   genTriviaOfIdent (!-(sprintf ".%s" s)) e
                                   +> genGenericTypeParameters astContext ts

                               let hasParenthesis = hasParenthesis e

                               let isLastAndTyped =
                                   (List.length es - 1) = idx && List.isNotEmpty ts

                               currentIdentifier
                               +> (fun ctx ->
                                   let isUpper = Char.IsUpper(s.[0])

                                   let expr =
                                       if (isLastAndTyped
                                           && isUpper
                                           && ctx.Config.SpaceBeforeUppercaseInvocation)
                                          || (isLastAndTyped
                                              && not isUpper
                                              && ctx.Config.SpaceBeforeLowercaseInvocation)
                                          || (not isLastAndTyped && not hasParenthesis) then
                                           sepSpace
                                       else
                                           sepNone

                                   expr ctx)
                               +> genExpr
                                   { astContext with
                                         IsInsideDotGet = true }
                                      e)
                        +> unindent

                isShortExpression ctx.Config.MaxDotGetExpressionWidth (genExpr sepNone) (genExpr sepNln) ctx

        // Multiline strings are a bit of an edge case in Fantomas, they are not immediately seen as multiline constructs.
        | App (e, (MultilineString _ as h :: rest as es)) ->
            let shortExpression =
                atCurrentColumn
                    (genExpr astContext e
                     +> sepSpace
                     +> col sepSpace es (genExpr astContext))

            let longExpression =
                atCurrentColumn
                    (genExpr astContext e
                     +> sepSpace
                     +> genExpr astContext h
                     +> indent
                     +> onlyIf (List.isNotEmpty rest) sepNln
                     +> col sepNln rest (genExpr astContext)
                     +> unindent)

            expressionFitsOnRestOfLine shortExpression longExpression

        // Always spacing in multiple arguments
        | App (e, es) ->
            let shortExpression =
                let addFirstSpace =
                    ifElseCtx (fun ctx ->
                        match es with
                        | [] -> false
                        | [ h ]
                        | h :: _ ->
                            not
                                (astContext.IsInsideDotGet
                                 || astContext.IsInsideDotIndexed)
                            && addSpaceBeforeParensInFunCall e h ctx) sepSpace sepNone

                let addSpace = indentIfNeeded sepSpace

                let genEx e =
                    if isCompExpr e then
                        sepSpace
                        +> sepOpenSFixed
                        +> sepSpace
                        +> indent
                        +> appNlnFun e (genExpr astContext e)
                        +> unindent
                    else
                        genExpr astContext e

                atCurrentColumn
                    (genExpr astContext e
                     +> addFirstSpace
                     +> col addSpace es genEx)

            let longExpression =
                atCurrentColumn
                    (genExpr astContext e
                     +> indent
                     +> sepNln
                     +> col sepNln es (fun e -> genExpr astContext e)
                     +> unindent)

            if List.exists (function
                | MultilineString _
                | CompExpr _ -> true
                | _ -> false) es then
                shortExpression
            else
                expressionFitsOnRestOfLine shortExpression longExpression

        | TypeApp (e, ts) ->
            genExpr astContext e
            +> genGenericTypeParameters astContext ts
        | LetOrUses (bs, e) ->
            let inKeyWordTrivia (binding: SynBinding) ctx =
                let inRange =
                    mkRange "IN" binding.RangeOfBindingAndRhs.End e.Range.Start

                Map.tryFindOrEmptyList IN ctx.TriviaTokenNodes
                |> TriviaHelpers.``keyword token after start column and on same line`` inRange
                |> List.tryHead

            let isInSameLine ctx =
                match bs with
                | [ _, (LetBinding (_, _, _, _, _, p, _, _) as binding) ] ->
                    Option.isSome (inKeyWordTrivia binding ctx)
                    && p.Range.EndLine = e.Range.StartLine
                    && not (futureNlnCheck (genExpr astContext e) ctx)
                | _ -> false

            let genInKeyword (binding: SynBinding) (ctx: Context) =
                match inKeyWordTrivia binding ctx with
                | Some (_, tn) ->
                    (printContentBefore tn
                     +> !- " in "
                     +> printContentAfter tn) ctx
                | None -> sepNone ctx

            fun ctx ->
                if isInSameLine ctx then
                    // short expression with in keyword
                    // f.ex. let a in ()
                    atCurrentColumn
                        (optSingle (fun (p, x) ->
                            genLetBinding
                                { astContext with
                                      IsFirstChild = p <> "and" }
                                p
                                x
                            +> genInKeyword x) (List.tryHead bs)
                         +> genExpr astContext e)
                        ctx
                else
                    let letBindings (bs: (string * SynBinding) list) =
                        bs
                        |> List.map (fun (p, x) ->
                            let expr =
                                enterNodeFor (synBindingToFsAstType x) x.RangeOfBindingAndRhs
                                +> genLetBinding
                                    { astContext with
                                          IsFirstChild = p <> "and" }
                                       p
                                       x
                                +> genInKeyword x

                            let range = x.RangeOfBindingAndRhs

                            let sepNln =
                                sepNlnConsideringTriviaContentBeforeForMainNode (synBindingToFsAstType x) range

                            expr, sepNln, range)

                    let sepNlnForNonSequential e r =
                        sepNlnConsideringTriviaContentBeforeForMainNode (synExprToFsAstType e) r

                    let rec synExpr e =
                        match e with
                        | LetOrUses (bs, e) -> letBindings bs @ synExpr e
                        | Sequentials (s) -> s |> List.collect synExpr
                        | _ ->
                            let r = e.Range
                            [ genExpr astContext e, sepNlnForNonSequential e r, r ]

                    let items = letBindings bs @ synExpr e
                    atCurrentColumn (colWithNlnWhenItemIsMultiline items) ctx

        // Could customize a bit if e is single line
        | TryWith (e, cs) ->
            let prefix =
                kw TRY !- "try "
                +> indent
                +> sepNln
                +> genExpr astContext e
                +> unindent
                +> kw WITH !+~ "with"

            match cs with
            | [ SynMatchClause.Clause (SynPat.Or _, _, _, _, _) ] ->
                atCurrentColumn
                    (prefix
                     +> indentOnWith
                     +> sepNln
                     +> col sepNln cs (genClause astContext true)
                     +> unindentOnWith)
            | [ c ] -> atCurrentColumn (prefix +> sepSpace +> genClause astContext false c)
            | _ ->
                atCurrentColumn
                    (prefix
                     +> indentOnWith
                     +> sepNln
                     +> col sepNln cs (genClause astContext true)
                     +> unindentOnWith)

        | TryFinally (e1, e2) ->
            atCurrentColumn
                (kw TRY !- "try "
                 +> indent
                 +> sepNln
                 +> genExpr astContext e1
                 +> unindent
                 +> kw FINALLY !+~ "finally"
                 +> indent
                 +> sepNln
                 +> genExpr astContext e2
                 +> unindent)

        | SequentialSimple es
        | Sequentials es ->
            let items =
                es
                |> List.map (fun e ->
                    let expr = genExpr astContext e

                    let fsAstType, r =
                        match e with
                        | LetOrUses ((_, fb) :: _, _) -> (synBindingToFsAstType fb), fb.RangeOfBindingAndRhs
                        | _ -> synExprToFsAstType e, e.Range

                    let sepNln =
                        sepConsideringTriviaContentBeforeForMainNode sepSemiNln fsAstType r

                    expr, sepNln, r)

            atCurrentColumn (colWithNlnWhenItemIsMultiline items)

        | IfThenElse (e1, e2, None, mIfToThen) ->
            fun (ctx: Context) ->
                let maxWidth = ctx.Config.MaxIfThenElseShortWidth
                let keepIfThenInSameLine = ctx.Config.KeepIfThenInSameLine

                let thenKeywordHasLineComment =
                    TriviaHelpers.``has content after after that matches`` (fun tn ->
                        // there is like a one of bug here
                        let correctedRange =
                            mkRange "correctedRange" tn.Range.Start (mkPos tn.Range.EndLine (tn.Range.EndColumn + 1))

                        RangeHelpers.rangeEndEq correctedRange mIfToThen) (function
                        | Comment (LineCommentAfterSourceCode _) -> true
                        | _ -> false) (Map.tryFindOrEmptyList THEN ctx.TriviaTokenNodes)

                let thenExpr = tokN mIfToThen THEN

                (leadingExpressionResult (!- "if " +> genExpr astContext e1) (fun ((lb, cb), (la, ca)) ->
                     let thenExpressionIsMultiline =
                         thenKeywordHasLineComment
                         || futureNlnCheck (genExpr astContext e2) ctx

                     if lb < la
                        || thenExpressionIsMultiline
                        || keepIfThenInSameLine then // if or then expression was multiline
                         thenExpr (!- " then")
                         +> indent
                         +> sepNln
                         +> genExpr astContext e2
                         +> unindent
                     elif (lb = la && (ca - cb) > maxWidth)
                          && not thenExpressionIsMultiline then // if expression is longer than maxWidth but not multiline
                         sepNln
                         +> thenExpr (!- "then ")
                         +> genExpr astContext e2
                     elif (exceedsWidth maxWidth (genExpr astContext e2) ctx)
                          && not thenExpressionIsMultiline then // then is longer than maxWidth but not multiline
                         sepNln
                         +> thenExpr (!- "then ")
                         +> genExpr astContext e2
                     else
                         // write out as short expression
                         thenExpr (!- " then ") +> genExpr astContext e2)
                 |> atCurrentColumn) ctx

        // A generalization of IfThenElse
        | ElIf ((e1, e2, _, _, _) :: es, enOpt) ->
            // https://docs.microsoft.com/en-us/dotnet/fsharp/style-guide/formatting#formatting-if-expressions
            fun ctx ->
                let correctedElifRanges =
                    es
                    |> List.pairwise
                    |> List.map (fun ((_, beforeNode, _, _, _), (_, _, _, _, node)) -> (beforeNode.Range, node.Range))
                    |> fun tail ->
                        match es with
                        | (_, _, _, _, hn) :: _ -> (e2.Range, hn.Range) :: tail
                        | _ -> tail
                    |> List.indexed
                    |> List.choose (fun (idx, (beforeRange, elseIfRange)) ->
                        let rangeBetween =
                            mkRange "between" beforeRange.End elseIfRange.Start

                        let keywordsFoundInBetween =
                            TriviaHelpers.``keyword token inside range``
                                rangeBetween
                                (Map.tryFindOrEmptyList ELSE ctx.TriviaTokenNodes)

                        match List.tryHead keywordsFoundInBetween with
                        | Some (_, elseKeyword) ->
                            (idx, mkRange "else if" elseKeyword.Range.Start elseIfRange.End)
                            |> Some
                        | _ -> None)
                    |> Map.ofList

                let elfis =
                    List.indexed es
                    |> List.map (fun (idx, (elf1, elf2, _, fullRange, _)) ->
                        // In some scenarios the last else keyword of the 'else if' combination is not included in inner IfThenElse syn expr
                        // f.ex if  a then b else // meh
                        //          if c then d else e
                        let correctedRange =
                            Map.tryFind idx correctedElifRanges
                            |> Option.defaultValue fullRange

                        (elf1, elf2, correctedRange))

                let hasElfis = not (List.isEmpty elfis)

                let commentAfterKeyword keyword rangePredicate (ctx: Context) =
                    (Map.tryFindOrEmptyList keyword ctx.TriviaTokenNodes)
                    |> TriviaHelpers.``has content after after that matches`` (fun t -> rangePredicate t.Range) (function
                           | Comment (LineCommentAfterSourceCode _) -> true
                           | _ -> false)

                let hasCommentAfterBoolExpr =
                    TriviaHelpers.``has content after after that matches`` (fun tn ->
                        RangeHelpers.rangeEq tn.Range e1.Range) (function
                        | Comment (LineCommentAfterSourceCode _) -> true
                        | _ -> false)
                        (TriviaHelpers.getNodesForTypes [ SynExpr_Ident; SynExpr_Const ] ctx.TriviaMainNodes)

                let hasCommentAfterIfKeyword =
                    commentAfterKeyword IF (RangeHelpers.``have same range start`` synExpr.Range) ctx

                let ``has line comment after source code for range`` range =
                    TriviaHelpers.``has content after after that matches`` (fun tn ->
                        RangeHelpers.rangeEq tn.Range range) (function
                        | Comment (LineCommentAfterSourceCode _) -> true
                        | _ -> false)
                        (TriviaHelpers.getNodesForTypes [ SynExpr_Ident; SynExpr_Const ] ctx.TriviaMainNodes)

                let hasCommentAfterIfBranchExpr =
                    ``has line comment after source code for range`` e2.Range

                let hasCommentAfterIfBranchThenKeyword =
                    commentAfterKeyword THEN (RangeHelpers.``range contains`` synExpr.Range) ctx

                let hasCommentAfterElseKeyword =
                    commentAfterKeyword ELSE (RangeHelpers.``range contains`` synExpr.Range) ctx

                let isConditionMultiline =
                    hasCommentAfterIfKeyword
                    || hasCommentAfterBoolExpr
                    || hasCommentAfterIfBranchThenKeyword
                    || futureNlnCheck (!- "if " +> genExpr astContext e1) ctx

                let isIfBranchMultiline =
                    futureNlnCheck (genExpr astContext e2) ctx

                let isElseBranchMultiline =
                    match enOpt with
                    | Some e3 ->
                        hasCommentAfterElseKeyword
                        || futureNlnCheck (!- " else " +> genExpr astContext e3) ctx
                    | None -> false

                let genIf ifElseRange = tokN ifElseRange IF (!- "if ")
                let genThen ifElseRange = tokN ifElseRange THEN (!- "then ")
                let genElse ifElseRange = tokN ifElseRange ELSE (!- "else ")

                let genElifOneliner ((elf1: SynExpr), (elf2: SynExpr), fullRange) =
                    let hasCommentAfterBoolExpr =
                        TriviaHelpers.``has content after after that matches`` (fun tn ->
                            RangeHelpers.rangeEq tn.Range elf1.Range) (function
                            | Comment (LineCommentAfterSourceCode _) -> true
                            | _ -> false)
                            (TriviaHelpers.getNodesForTypes [ SynExpr_Ident; SynExpr_Const ] ctx.TriviaMainNodes)

                    let hasCommentAfterThenKeyword =
                        commentAfterKeyword THEN (RangeHelpers.``range contains`` fullRange) ctx

                    TriviaContext.``else if / elif`` fullRange
                    +> genExpr astContext elf1
                    +> sepSpace
                    +> ifElse hasCommentAfterBoolExpr sepNln sepNone
                    +> genThen fullRange
                    +> ifElse hasCommentAfterThenKeyword sepNln sepNone
                    +> genExpr astContext elf2
                    |> genTriviaFor SynExpr_IfThenElse fullRange

                let genElifTwoLiner ((elf1: SynExpr), (elf2: SynExpr), fullRange) =
                    let hasCommentAfterThenKeyword =
                        commentAfterKeyword THEN (RangeHelpers.``range contains`` fullRange) ctx

                    TriviaContext.``else if / elif`` fullRange
                    +> genExpr astContext elf1
                    +> sepNln
                    +> genThen fullRange
                    +> ifElse hasCommentAfterThenKeyword sepNln sepNone
                    +> genExpr astContext elf2

                let isAnyElifBranchMultiline =
                    elfis
                    |> List.exists (fun elf -> futureNlnCheck (genElifOneliner elf) ctx)

                let anyElifBranchHasCommentAfterBranchExpr =
                    elfis
                    |> List.exists (fun (_, e, _) -> ``has line comment after source code for range`` e.Range)

                let isAnyExpressionIsLongerButNotMultiline =
                    let longerSetting = ctx.Config.MaxIfThenElseShortWidth

                    let elseExceedsWith =
                        match enOpt with
                        | Some e4 -> exceedsWidth longerSetting (genExpr astContext e4) ctx
                        | None -> false

                    exceedsWidth longerSetting (genExpr astContext e1) ctx
                    || exceedsWidth longerSetting (genExpr astContext e2) ctx
                    || elseExceedsWith

                let isAnyExpressionIsMultiline =
                    isConditionMultiline
                    || isIfBranchMultiline
                    || isElseBranchMultiline
                    || isAnyElifBranchMultiline

                let genElifMultiLine ((elf1: SynExpr), elf2, fullRange) (ctx: Context) =
                    let indentAfterThenKeyword =
                        TriviaHelpers.getNodesForTypes [ IF; ELIF ] ctx.TriviaTokenNodes
                        |> TriviaHelpers.``keyword token inside range`` fullRange
                        |> List.tryHead
                        |> Option.map (fun (_, t) ->
                            if TriviaHelpers.``has line comment after`` t then
                                // don't indent because comment after if/elif keyword
                                // TriviaContext.``else if / elif`` fullRange will already placed indentation
                                sepNone
                            else
                                indent)
                        |> Option.defaultValue indent

                    let hasCommentAfterBoolExpr =
                        TriviaHelpers.``has content after after that matches`` (fun tn ->
                            RangeHelpers.rangeEq tn.Range elf1.Range) (function
                            | Comment (LineCommentAfterSourceCode _) -> true
                            | _ -> false)
                            (TriviaHelpers.getNodesForTypes [ SynExpr_Ident; SynExpr_Const ] ctx.TriviaMainNodes)

                    let elifExpr =
                        TriviaContext.``else if / elif`` fullRange
                        +> genExpr astContext elf1
                        +> ifElse hasCommentAfterBoolExpr sepNln sepSpace
                        +> genThen fullRange
                        +> indentAfterThenKeyword
                        +> sepNln
                        +> genExpr astContext elf2
                        +> unindent

                    (elifExpr
                     |> genTriviaFor SynExpr_IfThenElse fullRange) ctx

                let genOneliner =
                    genIf synExpr.Range
                    +> genExpr astContext e1
                    +> sepSpace
                    +> genThen synExpr.Range
                    +> genExpr astContext e2
                    +> col sepNone elfis (fun elf -> sepSpace +> genElifOneliner elf)
                    +> opt id enOpt (fun e4 ->
                           (sepSpace
                            +> genElse synExpr.Range
                            +> genExpr astContext e4))

                // This is a simplistic check to see if everything fits on one line
                let isOneLiner =
                    not hasElfis
                    && not isAnyExpressionIsLongerButNotMultiline
                    && not isAnyExpressionIsMultiline
                    && not hasCommentAfterIfBranchExpr
                    && not anyElifBranchHasCommentAfterBranchExpr
                    && not (futureNlnCheck genOneliner ctx)

                let keepIfThenInSameLine = ctx.Config.KeepIfThenInSameLine

                let formatIfElseExpr =
                    if isOneLiner then
                        // Indentation of conditionals depends on the sizes of the expressions that make them up. If cond, e1 and e2 are short, simply write them on one line:
                        // if cond then e1 else e2
                        genOneliner

                    elif not isOneLiner
                         && not isAnyExpressionIsMultiline
                         && isAnyExpressionIsLongerButNotMultiline
                         && not keepIfThenInSameLine then
                        // If either cond, e1 or e2 are longer, but not multi-line:
                        // if cond
                        // then e1
                        // else e2

                        genIf synExpr.Range
                        +> genExpr astContext e1
                        +> sepNln
                        +> genThen synExpr.Range
                        +> genExpr astContext e2
                        +> sepNln
                        +> colPost sepNln sepNln elfis genElifTwoLiner
                        +> opt id enOpt (fun e4 -> genElse synExpr.Range +> genExpr astContext e4)

                    elif hasElfis
                         && not isAnyExpressionIsMultiline
                         && not isAnyExpressionIsLongerButNotMultiline then
                        // Multiple conditionals with elif and else are indented at the same scope as the if:
                        // if cond1 then e1
                        // elif cond2 then e2
                        // elif cond3 then e3
                        // else e4

                        genIf synExpr.Range
                        +> genExpr astContext e1
                        +> sepSpace
                        +> genThen synExpr.Range
                        +> genExpr astContext e2
                        +> sepNln
                        +> col sepNln elfis genElifOneliner
                        +> opt id enOpt (fun e4 ->
                               let correctedElseRange =
                                   match List.tryLast elfis with
                                   | Some (_, te, _) -> mkRange "correctedElseRange" te.Range.End synExpr.Range.End
                                   | None -> synExpr.Range

                               sepNln
                               +> genElse correctedElseRange
                               +> genExpr astContext e4)

                    else if hasCommentAfterIfBranchExpr && not hasElfis then
                        // f.ex
                        // if x then 0 // meh
                        // else 1
                        genIf synExpr.Range
                        +> genExpr astContext e1
                        +> sepNlnWhenWriteBeforeNewlineNotEmpty sepSpace
                        +> genThen synExpr.Range
                        +> genExpr astContext e2
                        +> sepNln
                        +> opt id enOpt (fun e4 -> genElse synExpr.Range +> genExpr astContext e4)

                    else
                        // If any of the expressions are multi-line:
                        // if cond then
                        //     e1
                        // else
                        //     e2

                        genIf synExpr.Range
                        // f.ex. if // meh
                        //           x
                        // bool expr x should be indented
                        +> ifElse hasCommentAfterIfKeyword (indent +> sepNln) sepNone
                        +> (match e1 with
                            | SynExpr.TryWith _
                            | SynExpr.TryFinally _ -> sepOpenT +> genExpr astContext e1 +> sepCloseT
                            | _ -> genExpr astContext e1)
                        +> ifElse hasCommentAfterBoolExpr sepNln sepSpace
                        +> genThen synExpr.Range
                        // f.ex if x then // meh
                        //          0
                        // 0 should be indented
                        +> ifElse
                            (hasCommentAfterIfBranchThenKeyword
                             && not hasCommentAfterIfKeyword)
                               indent
                               sepNone
                        // f.ex. if x //
                        //       then
                        //           0
                        // 0 should be indented
                        +> ifElse
                            (hasCommentAfterBoolExpr
                             && not hasCommentAfterIfKeyword)
                               indent
                               sepNone
                        // normal scenario
                        // f.ex. if (longCondition
                        //          && onMultipleLines) then
                        //           x
                        +> ifElse
                            (not hasCommentAfterIfKeyword
                             && not hasCommentAfterBoolExpr
                             && not hasCommentAfterIfBranchThenKeyword)
                               indent
                               sepNone
                        +> sepNln
                        +> genExpr astContext e2
                        +> unindent
                        +> sepNln
                        +> colPost sepNln sepNln elfis genElifMultiLine
                        +> opt id enOpt (fun e4 ->
                               let correctedElseRange =
                                   match List.tryLast elfis with
                                   | Some (_, te, _) -> mkRange "correctedElseRange" te.Range.End synExpr.Range.End
                                   | None -> synExpr.Range

                               genElse correctedElseRange
                               +> indent
                               +> sepNln
                               +> genExpr astContext e4
                               +> unindent)

                (atCurrentColumn formatIfElseExpr) ctx

        // At this stage, all symbolic operators have been handled.
        | OptVar (s, isOpt, ranges) ->
            // In case s is f.ex `onStrongDiscard.IsNone`, last range is the range of `IsNone`
            let lastRange = List.tryLast ranges

            ifElse isOpt (!- "?") sepNone -- s
            +> opt id lastRange (leaveNodeFor Ident_)
        | LongIdentSet (s, e, _) ->
            !-(sprintf "%s <- " s)
            +> autoIndentAndNlnIfExpressionExceedsPageWidth (genExpr astContext e)
        | DotIndexedGet (e, es) ->
            addParenIfAutoNln
                e
                (genExpr
                    { astContext with
                          IsInsideDotIndexed = true })
            -- "."
            +> sepOpenLFixed
            +> genIndexers astContext es
            +> sepCloseLFixed
            +> leaveNodeTokenByName synExpr.Range RBRACK
        | DotIndexedSet (e1, es, e2) ->
            addParenIfAutoNln
                e1
                (genExpr
                    { astContext with
                          IsInsideDotIndexed = true })
            -- ".["
            +> genIndexers astContext es
            -- "] <- "
            +> genExpr astContext e2
        | NamedIndexedPropertySet (ident, e1, e2) ->
            !-ident +> genExpr astContext e1 -- " <- "
            +> genExpr astContext e2
        | DotNamedIndexedPropertySet (e, ident, e1, e2) ->
            genExpr astContext e -- "." -- ident
            +> genExpr astContext e1
            -- " <- "
            +> genExpr astContext e2
        | DotGet (e, (s, _)) ->
            let shortExpr =
                genExpr
                    { astContext with
                          IsInsideDotGet = true }
                    e
                -- (sprintf ".%s" s)

            let longExpr =
                let expr =
                    match e with
                    | App (LongIdentPieces (lids), [ e2 ]) when (List.moreThanOne lids) ->
                        !-(List.head lids)
                        +> indent
                        +> sepNln
                        +> col sepNln (List.tail lids) (fun s -> !-(sprintf ".%s" s))
                        +> genExpr
                            { astContext with
                                  IsInsideDotGet = true }
                               e2
                        +> unindent
                    | App (TypeApp (LongIdentPieces (lids), ts), [ e2 ]) when (List.moreThanOne lids) ->
                        !-(List.head lids)
                        +> indent
                        +> sepNln
                        +> col sepNln (List.tail lids) (fun s -> !-(sprintf ".%s" s))
                        +> genGenericTypeParameters astContext ts
                        +> genExpr
                            { astContext with
                                  IsInsideDotGet = true }
                               e2
                        +> unindent
                    | _ -> genExpr astContext e

                expr +> indent +> sepNln -- (sprintf ".%s" s)
                +> unindent

            fun ctx -> isShortExpression ctx.Config.MaxDotGetExpressionWidth shortExpr longExpr ctx
        | DotSet (e1, s, e2) ->
            addParenIfAutoNln e1 (genExpr astContext)
            -- sprintf ".%s <- " s
            +> genExpr astContext e2

        | SynExpr.Set (e1, e2, _) ->
            addParenIfAutoNln e1 (genExpr astContext)
            -- sprintf " <- "
            +> genExpr astContext e2

        | ParsingError r ->
            raise
            <| FormatException
                (sprintf
                    "Parsing error(s) between line %i column %i and line %i column %i"
                     r.StartLine
                     (r.StartColumn + 1)
                     r.EndLine
                     (r.EndColumn + 1))
        | UnsupportedExpr r ->
            raise
            <| FormatException
                (sprintf
                    "Unsupported construct(s) between line %i column %i and line %i column %i"
                     r.StartLine
                     (r.StartColumn + 1)
                     r.EndLine
                     (r.EndColumn + 1))
        | SynExpr.InterpolatedString (parts, _) ->
            fun (ctx: Context) ->
                let stringRanges =
                    List.choose (function
                        | SynInterpolatedStringPart.String (_, r) -> Some r
                        | _ -> None) parts

                // multiline interpolated string will contain the $ and or braces in the triviaContent
                // example: $"""%s{ , } bar
                let stringsFromTrivia =
                    stringRanges
                    |> List.choose (fun range ->
                        Map.tryFindOrEmptyList SynInterpolatedStringPart_String ctx.TriviaMainNodes
                        |> List.choose (fun tn ->
                            match tn.Type, tn.ContentItself with
                            | MainNode (SynInterpolatedStringPart_String), Some (StringContent sc) when (RangeHelpers.rangeEq
                                                                                                             tn.Range
                                                                                                             range) ->
                                Some sc
                            | _ -> None)
                        |> List.tryHead
                        |> Option.map (fun sc -> range, sc))

                let genInterpolatedFillExpr expr =
                    leadingExpressionIsMultiline
                        (atCurrentColumn (autoIndentAndNlnIfExpressionExceedsPageWidth (genExpr astContext expr))) (fun isMultiline ->
                        onlyIf isMultiline sepNln)

                let expr =
                    if List.length stringRanges = List.length stringsFromTrivia then
                        colEx (fun _ -> sepNone) parts (fun part ->
                            match part with
                            | SynInterpolatedStringPart.String (_, range) ->
                                let stringFromTrivia =
                                    List.find (fun (r, _) -> RangeHelpers.rangeEq range r) stringsFromTrivia
                                    |> snd

                                !-stringFromTrivia
                                |> genTriviaFor SynInterpolatedStringPart_String range
                            | SynInterpolatedStringPart.FillExpr (expr, _ident) -> genInterpolatedFillExpr expr)
                    else
                        !- "$\""
                        +> colEx (fun _ -> sepNone) parts (fun part ->
                               match part with
                               | SynInterpolatedStringPart.String (s, r) ->
                                   !-s
                                   |> genTriviaFor SynInterpolatedStringPart_String r
                               | SynInterpolatedStringPart.FillExpr (expr, _ident) ->
                                   !- "{" +> genInterpolatedFillExpr expr +> !- "}")
                        +> !- "\""

                expr ctx
        | e -> failwithf "Unexpected expression: %O" e
        |> (match synExpr with
            | SynExpr.App _ -> genTriviaFor SynExpr_App synExpr.Range
            | SynExpr.Const _ -> genTriviaFor SynExpr_Const synExpr.Range
            | SynExpr.AnonRecd _ -> genTriviaFor SynExpr_AnonRecd synExpr.Range
            | SynExpr.Record _ -> genTriviaFor SynExpr_Record synExpr.Range
            | SynExpr.Ident _ -> genTriviaFor SynExpr_Ident synExpr.Range
            | SynExpr.IfThenElse _ -> genTriviaFor SynExpr_IfThenElse synExpr.Range
            | SynExpr.Lambda _ -> genTriviaFor SynExpr_Lambda synExpr.Range
            | SynExpr.ForEach _ -> genTriviaFor SynExpr_ForEach synExpr.Range
            | SynExpr.Match _ -> genTriviaFor SynExpr_Match synExpr.Range
            | SynExpr.YieldOrReturn _ -> genTriviaFor SynExpr_YieldOrReturn synExpr.Range
            | SynExpr.YieldOrReturnFrom _ -> genTriviaFor SynExpr_YieldOrReturnFrom synExpr.Range
            | SynExpr.TryFinally _ -> genTriviaFor SynExpr_TryFinally synExpr.Range
            | SynExpr.LongIdentSet _ -> genTriviaFor SynExpr_LongIdentSet synExpr.Range
            | SynExpr.ArrayOrListOfSeqExpr _ -> genTriviaFor SynExpr_ArrayOrListOfSeqExpr synExpr.Range
            | SynExpr.Paren _ -> genTriviaFor SynExpr_Paren synExpr.Range
            | SynExpr.InterpolatedString _ -> genTriviaFor SynExpr_InterpolatedString synExpr.Range
            | SynExpr.Tuple _ -> genTriviaFor SynExpr_Tuple synExpr.Range
            | SynExpr.DoBang _ -> genTriviaFor SynExpr_DoBang synExpr.Range
            | SynExpr.TryWith _ -> genTriviaFor SynExpr_TryWith synExpr.Range
            | SynExpr.New _ -> genTriviaFor SynExpr_New synExpr.Range
            | SynExpr.Assert _ -> genTriviaFor SynExpr_Assert synExpr.Range
            | SynExpr.While _ -> genTriviaFor SynExpr_While synExpr.Range
            | SynExpr.MatchLambda _ -> genTriviaFor SynExpr_MatchLambda synExpr.Range
            | SynExpr.LongIdent _ -> genTriviaFor SynExpr_LongIdent synExpr.Range
            | SynExpr.DotGet _ -> genTriviaFor SynExpr_DotGet synExpr.Range
            | _ -> id)

    expr ctx

and genInfixOperator operatorText (operatorExpr: SynExpr) =
    (!-operatorText
     |> genTriviaFor SynExpr_Ident operatorExpr.Range)
    +> sepNlnWhenWriteBeforeNewlineNotEmpty sepNone

and genOnelinerInfixExpr astContext e1 operatorText operatorExpr e2 =
    genExpr astContext e1
    +> sepSpace
    +> genInfixOperator operatorText operatorExpr
    +> sepSpace
    +> genExpr astContext e2

and genMultilineInfixExpr astContext e1 operatorText operatorExpr e2 =
    if noBreakInfixOps.Contains(operatorText) then
        genOnelinerInfixExpr astContext e1 operatorText operatorExpr e2
    else
        atCurrentColumn
            (genExpr astContext e1
             +> sepNln
             +> genInfixOperator operatorText operatorExpr
             +> sepSpace
             +> genExpr astContext e2)

and genGenericTypeParameters astContext ts =
    match ts with
    | [] -> sepNone
    | ts ->
        !- "<"
        +> coli sepComma ts (fun idx ->
               genType
                   { astContext with
                         IsFirstTypeParam = idx = 0 }
                   false)
        -- ">"

and genMultilineRecordInstance (inheritOpt: (SynType * SynExpr) option)
                               (xs: (RecordFieldName * SynExpr option * BlockSeparator option) list)
                               (eo: SynExpr option)
                               synExpr
                               astContext
                               (ctx: Context)
                               =
    let recordExpr =
        let fieldsExpr =
            col sepSemiNln xs (genRecordFieldName astContext)

        match eo with
        | Some e ->
            genExpr astContext e
            +> !- " with"
            +> indent
            +> sepNln
            +> fieldsExpr
            +> unindent
        | None -> fieldsExpr

    let expr =
        sepOpenS
        +> (fun (ctx: Context) ->
            { ctx with
                  RecordBraceStart = ctx.Column :: ctx.RecordBraceStart })
        +> atCurrentColumnIndent
            (leaveLeftBrace synExpr.Range
             +> opt (if xs.IsEmpty then sepNone else sepNln) inheritOpt (fun (typ, expr) ->
                    !- "inherit "
                    +> genType astContext false typ
                    +> genExpr astContext expr)
             +> recordExpr)
        +> (fun ctx ->
            match ctx.RecordBraceStart with
            | rbs :: rest ->
                if ctx.Column < rbs then
                    let offset =
                        (if ctx.Config.SpaceAroundDelimiter then 2 else 1)
                        + 1

                    let delta = Math.Max((rbs - ctx.Column) - offset, 0)
                    (!-System.String.Empty.PadRight(delta)) ({ ctx with RecordBraceStart = rest })
                else
                    sepNone ({ ctx with RecordBraceStart = rest })
            | [] -> sepNone ctx)
        +> sepNlnWhenWriteBeforeNewlineNotEmpty sepNone
        +> enterNodeTokenByName synExpr.Range RBRACE
        +> ifElseCtx lastWriteEventIsNewline sepCloseSFixed sepCloseS

    expr ctx

and genMultilineRecordInstanceAlignBrackets (inheritOpt: (SynType * SynExpr) option)
                                            (xs: (RecordFieldName * SynExpr option * BlockSeparator option) list)
                                            (eo: SynExpr option)
                                            synExpr
                                            astContext
                                            =
    let fieldsExpr =
        col sepSemiNln xs (genRecordFieldName astContext)

    let hasFields = List.isNotEmpty xs

    match inheritOpt, eo with
    | Some (inheritType, inheritExpr), None ->
        sepOpenS
        +> ifElse hasFields (indent +> sepNln) sepNone
        +> !- "inherit "
        +> genType astContext false inheritType
        +> genExpr astContext inheritExpr
        +> ifElse
            hasFields
               (sepNln
                +> fieldsExpr
                +> unindent
                +> sepNln
                +> sepCloseSFixed)
               (sepSpace +> sepCloseSFixed)

    | None, Some e ->
        sepOpenS
        +> genExpr astContext e
        +> (!- " with"
            +> indent
            +> whenShortIndent indent
            +> sepNln
            +> fieldsExpr
            +> unindent
            +> whenShortIndent unindent
            +> sepNln
            +> sepCloseSFixed)

    | _ ->
        (sepOpenSFixed
         +> indent
         +> sepNln
         +> fieldsExpr
         +> unindent
         +> enterNodeTokenByName synExpr.Range RBRACE
         +> ifElseCtx lastWriteEventIsNewline sepNone sepNln
         +> sepCloseSFixed)
    |> atCurrentColumnIndent

and genMultilineAnonRecord (isStruct: bool) fields copyInfo astContext =
    let recordExpr =
        let fieldsExpr =
            col sepSemiNln fields (genAnonRecordFieldName astContext)

        match copyInfo with
        | Some e ->
            genExpr astContext e
            +> (!- " with"
                +> indent
                +> sepNln
                +> fieldsExpr
                +> unindent)
        | None -> fieldsExpr

    onlyIf isStruct !- "struct "
    +> sepOpenAnonRecd
    +> atCurrentColumnIndent recordExpr
    +> sepCloseAnonRecd

and genMultilineAnonRecordAlignBrackets (isStruct: bool) fields copyInfo astContext =
    let fieldsExpr =
        col sepSemiNln fields (genAnonRecordFieldName astContext)

    let copyExpr fieldsExpr e =
        genExpr astContext e
        +> (!- " with"
            +> indent
            +> whenShortIndent indent
            +> sepNln
            +> fieldsExpr
            +> whenShortIndent unindent
            +> unindent)

    let genAnonRecord =
        match copyInfo with
        | Some ci ->
            sepOpenAnonRecd
            +> copyExpr fieldsExpr ci
            +> sepNln
            +> sepCloseAnonRecdFixed
        | None ->
            sepOpenAnonRecdFixed
            +> indent
            +> sepNln
            +> fieldsExpr
            +> unindent
            +> sepNln
            +> sepCloseAnonRecdFixed

    ifElse isStruct !- "struct " sepNone
    +> atCurrentColumnIndent genAnonRecord

and genObjExpr t eio bd ims range (astContext: ASTContext) =
    // Check the role of the second part of eio
    let param =
        opt sepNone (Option.map fst eio) (genExpr astContext)

    sepOpenS
    +> atCurrentColumn
        (!- "new " +> genType astContext false t +> param
         -- " with"
         +> indent
         +> sepNln
         +> genMemberBindingList
             { astContext with
                   InterfaceRange = Some range }
                bd
         +> unindent
         +> colPre sepNln sepNln ims (genInterfaceImpl astContext))
    +> sepCloseS

and genObjExprAlignBrackets t eio bd ims range (astContext: ASTContext) =
    // Check the role of the second part of eio
    let param =
        opt sepNone (Option.map fst eio) (genExpr astContext)

    let genObjExpr =
        atCurrentColumn
            (!- "new " +> genType astContext false t +> param
             -- " with"
             +> indent
             +> sepNln
             +> genMemberBindingList
                 { astContext with
                       InterfaceRange = Some range }
                    bd
             +> unindent
             +> colPre sepNln sepNln ims (genInterfaceImpl astContext))

    atCurrentColumnIndent (sepOpenS +> genObjExpr +> sepNln +> sepCloseSFixed)

and genMultiLineArrayOrList (isArray: bool) xs alNode astContext =
    ifElse isArray sepOpenA sepOpenL
    +> atCurrentColumn
        ((ifElse isArray (leaveLeftBrackBar alNode.Range) (leaveLeftBrack alNode.Range))
         +> col sepSemiNln xs (genExpr astContext))
    +> ifElse isArray (enterRightBracketBar alNode.Range) (enterRightBracket alNode.Range)
    +> ifElse isArray sepCloseA sepCloseL

and genMultiLineArrayOrListAlignBrackets (isArray: bool) xs alNode astContext =
    let isLastItem (x: SynExpr) =
        List.tryLast xs
        |> Option.map (fun i -> RangeHelpers.rangeEq i.Range x.Range)
        |> Option.defaultValue false

    fun ctx ->
        let innerExpr =
            xs
            |> List.fold (fun acc e (ctx: Context) ->
                let isLastItem = isLastItem e

                (acc
                 +> genExpr astContext e
                 +> ifElse isLastItem sepNone sepSemiNln) ctx) sepNone
            |> atCurrentColumn

        let expr =
            if isArray then
                sepOpenAFixed
                +> indent
                +> leaveNodeTokenByName alNode.Range LBRACK_BAR
                +> sepNlnUnlessLastEventIsNewline
                +> innerExpr
                +> unindent
                +> enterNodeTokenByName alNode.Range BAR_RBRACK
                +> sepNlnUnlessLastEventIsNewline
                +> sepCloseAFixed
            else
                sepOpenLFixed
                +> indent
                +> leaveNodeTokenByName alNode.Range LBRACK
                +> sepNlnUnlessLastEventIsNewline
                +> innerExpr
                +> unindent
                +> enterNodeTokenByName alNode.Range RBRACK
                +> sepNlnUnlessLastEventIsNewline
                +> sepCloseLFixed

        expr ctx

/// Use in indexed set and get only
and genIndexers astContext node =
    // helper to generate the remaining indexer expressions
    // (pulled out due to duplication)
    let inline genRest astContext (es: _ list) =
        ifElse es.IsEmpty sepNone (sepComma +> genIndexers astContext es)

    // helper to generate a single indexer expression with support for the from-end slice marker
    let inline genSingle astContext (isFromEnd: bool) (e: SynExpr) =
        ifElse isFromEnd (!- "^") sepNone
        +> genExpr astContext e

    match node with
    // list.[*]
    | Indexer (Pair ((IndexedVar None, _), (IndexedVar None, _))) :: es -> !- "*" +> genRest astContext es
    // list.[(fromEnd)<idx>..]
    | Indexer (Pair ((IndexedVar (Some e01), e1FromEnd), (IndexedVar None, _))) :: es ->
        genSingle astContext e1FromEnd e01 -- ".."
        +> genRest astContext es
    // list.[..(fromEnd)<idx>]
    | Indexer (Pair ((IndexedVar None, _), (IndexedVar (Some e2), e2FromEnd))) :: es ->
        !- ".."
        +> genSingle astContext e2FromEnd e2
        +> genRest astContext es
    // list.[(fromEnd)<idx>..(fromEnd)<idx>]
    | Indexer (Pair ((IndexedVar (Some e01), e1FromEnd), (IndexedVar (Some eo2), e2FromEnd))) :: es ->
        genSingle astContext e1FromEnd e01 -- ".."
        +> genSingle astContext e2FromEnd eo2
        +> genRest astContext es
    // list.[*]
    | Indexer (Single (IndexedVar None, _)) :: es -> !- "*" +> genRest astContext es
    // list.[(fromEnd)<idx>]
    | Indexer (Single (eo, fromEnd)) :: es ->
        genSingle astContext fromEnd eo
        +> genRest astContext es
    | _ -> sepNone

and sepNlnBetweenTypeAndMembers (ms: SynMemberDefn list) =
    sepNlnTypeAndMembers (List.tryHead ms |> Option.map (fun e -> e.Range)) SynMemberDefn_Member

and genTypeDefn astContext (TypeDef (ats, px, ao, tds, tcs, tdr, ms, s, preferPostfix) as node) =
    let typeName =
        genPreXmlDoc px
        +> ifElse
            astContext.IsFirstChild
               (genAttributes astContext ats -- "type ")
               (!- "and " +> genOnelinerAttributes astContext ats)
        +> opt sepSpace ao genAccess
        +> genTypeAndParam astContext s tds tcs preferPostfix

    match tdr with
    | Simple (TDSREnum ecs) ->
        typeName
        +> sepEq
        +> indent
        +> sepNln
        +> genTriviaFor
            SynTypeDefnSimpleRepr_Enum
               tdr.Range
               (col
                   sepNln
                    ecs
                    (genEnumCase
                        { astContext with
                              HasVerticalBar = true })
                +> onlyIf (List.isNotEmpty ms) sepNln
                +> sepNlnBetweenTypeAndMembers ms
                +> genMemberDefnList
                    { astContext with
                          InterfaceRange = None }
                       ms
                // Add newline after un-indent to be spacing-correct
                +> unindent)

    | Simple (TDSRUnion (ao', xs) as unionNode) ->
        let hasLeadingTrivia (t: TriviaNode) =
            RangeHelpers.rangeEq t.Range unionNode.Range
            && not (List.isEmpty t.ContentBefore)

        let unionCases ctx =
            match xs with
            | [] -> ctx
            | [ UnionCase (attrs, _, _, _, (UnionCaseType fields)) as x ] when List.isEmpty ms ->

                let hasVerticalBar =
                    (Option.isSome ao' && List.length fields <> 1)
                    || (Map.tryFindOrEmptyList SynTypeDefnSimpleRepr_Union ctx.TriviaMainNodes)
                       |> List.exists hasLeadingTrivia
                    || not (List.isEmpty attrs)
                    || List.isEmpty fields

                let expr =
                    genTriviaFor
                        SynTypeDefnSimpleRepr_Union
                        tdr.Range
                        (opt sepSpace ao' genAccess
                         +> genUnionCase
                             { astContext with
                                   HasVerticalBar = hasVerticalBar }
                                x)

                expressionFitsOnRestOfLine (indent +> sepSpace +> expr) (indent +> sepNln +> expr) ctx
            | xs ->
                indent
                +> sepNln
                +> genTriviaFor
                    SynTypeDefnSimpleRepr_Union
                       tdr.Range
                       (opt sepNln ao' genAccess
                        +> col
                            sepNln
                               xs
                               (genUnionCase
                                   { astContext with
                                         HasVerticalBar = true }))
                <| ctx

        typeName
        +> sepEq
        +> unionCases
        +> onlyIf (List.isNotEmpty ms) sepNln
        +> sepNlnBetweenTypeAndMembers ms
        +> genMemberDefnList
            { astContext with
                  InterfaceRange = None }
               ms
        +> unindent

    | Simple (TDSRRecord (ao', fs)) ->
        let smallExpression =
            sepSpace
            +> optSingle (fun ao -> genAccess ao +> sepSpace) ao'
            +> sepOpenS
            +> leaveLeftBrace tdr.Range
            +> col sepSemi fs (genField astContext "")
            +> sepCloseS
            +> leaveNodeTokenByName node.Range RBRACE

        let multilineExpression =
            ifAlignBrackets
                (genMultilineSimpleRecordTypeDefnAlignBrackets tdr ms ao' fs astContext)
                (genMultilineSimpleRecordTypeDefn tdr ms ao' fs astContext)

        let bodyExpr ctx =
            let size = getRecordSize ctx fs

            if (List.isEmpty ms) then
                (isSmallExpression size smallExpression multilineExpression
                 +> leaveNodeFor SynTypeDefnSimpleRepr_Record tdr.Range // this will only print something when there is trivia after } in the short expression
                // Yet it cannot be part of the short expression otherwise the multiline expression would be triggered unwillingly.
                ) ctx
            else
                multilineExpression ctx

        typeName
        +> sepEq
        +> indent
        +> enterNodeFor SynTypeDefnSimpleRepr_Record tdr.Range
        +> bodyExpr
        +> leaveNodeFor SynTypeDefnSimpleRepr_Record tdr.Range
        +> unindent

    | Simple TDSRNone -> typeName
    | Simple (TDSRTypeAbbrev t) ->
        let genTypeAbbrev =
            let needsParenthesis =
                match t with
                | SynType.Tuple (isStruct, typeNames, _) -> (isStruct && List.length typeNames > 1)
                | _ -> false

            ifElse needsParenthesis sepOpenT sepNone
            +> genType astContext false t
            +> ifElse needsParenthesis sepCloseT sepNone
            |> genTriviaFor SynTypeDefnSimpleRepr_TypeAbbrev tdr.Range

        let genMembers =
            ifElse
                (List.isEmpty ms)
                (!- "")
                (indent ++ "with"
                 +> indent
                 +> sepNln
                 +> sepNlnBetweenTypeAndMembers ms
                 +> genMemberDefnList
                     { astContext with
                           InterfaceRange = None }
                        ms
                 +> unindent
                 +> unindent)

        let genTypeBody =
            autoIndentAndNlnIfExpressionExceedsPageWidth
                (genTriviaFor SynTypeDefnRepr_ObjectModel tdr.Range genTypeAbbrev)
            +> genMembers

        typeName +> sepEq +> sepSpace +> genTypeBody
    | Simple (TDSRException (ExceptionDefRepr (ats, px, ao, uc))) -> genExceptionBody astContext ats px ao uc

    | ObjectModel (TCSimple (TCInterface
                   | TCClass) as tdk,
                   MemberDefnList (impCtor, others),
                   range) ->
        let interfaceRange =
            match tdk with
            | TCSimple TCInterface -> Some range
            | _ -> None

        let astContext =
            { astContext with
                  InterfaceRange = interfaceRange }

        typeName
        +> sepSpaceBeforeClassConstructor
        +> leadingExpressionIsMultiline (opt sepNone impCtor (genMemberDefn astContext)) (fun isMulti ctx ->
               if isMulti
                  && ctx.Config.AlternativeLongMemberDefinitions then
                   sepEqFixed ctx
               else
                   sepEq ctx)
        +> indent
        +> sepNln
        +> genTypeDefKind tdk
        +> indent
        +> onlyIf (List.isNotEmpty others) sepNln
        +> sepNlnBetweenTypeAndMembers ms
        +> genMemberDefnList astContext others
        +> unindent
        ++ "end"
        +> unindent

    | ObjectModel (TCSimple (TCStruct) as tdk, MemberDefnList (impCtor, others), _) ->
        let sepMem =
            match ms with
            | [] -> sepNone
            | _ -> sepNln

        typeName
        +> opt sepNone impCtor (genMemberDefn astContext)
        +> sepEq
        +> indent
        +> sepNln
        +> genTypeDefKind tdk
        +> indent
        +> sepNln
        +> genMemberDefnList astContext others
        +> unindent
        ++ "end"
        +> sepMem
        // Prints any members outside the struct-end construct
        +> genMemberDefnList astContext ms
        +> unindent

    | ObjectModel (TCSimple TCAugmentation, _, _) ->
        typeName -- " with"
        +> indent
        // Remember that we use MemberDefn of parent node
        +> sepNln
        +> sepNlnBetweenTypeAndMembers ms
        +> genMemberDefnList
            { astContext with
                  InterfaceRange = None }
               ms
        +> unindent

    | ObjectModel (TCDelegate (FunType ts), _, _) ->
        typeName
        +> sepEq
        +> sepSpace
        +> !- "delegate of "
        +> genTypeList astContext ts

    | ObjectModel (TCSimple TCUnspecified, MemberDefnList (impCtor, others), _) when not (List.isEmpty ms) ->
        typeName
        +> opt
            sepNone
               impCtor
               (genMemberDefn
                   { astContext with
                         InterfaceRange = None })
        +> sepEq
        +> indent
        +> sepNln
        +> genMemberDefnList
            { astContext with
                  InterfaceRange = None }
               others
        +> sepNln
        -- "with"
        +> indent
        +> sepNln
        +> genMemberDefnList
            { astContext with
                  InterfaceRange = None }
               ms
        +> unindent
        +> unindent

    | ObjectModel (_, MemberDefnList (impCtor, others), _) ->
        typeName
        +> opt sepNone impCtor (fun mdf ->
               sepSpaceBeforeClassConstructor
               +> genMemberDefn
                   { astContext with
                         InterfaceRange = None }
                      mdf)
        +> sepEq
        +> indent
        +> sepNln
        +> genMemberDefnList
            { astContext with
                  InterfaceRange = None }
               others
        +> unindent

    | ExceptionRepr (ExceptionDefRepr (ats, px, ao, uc)) -> genExceptionBody astContext ats px ao uc
    |> genTriviaFor TypeDefn_ node.Range

and genMultilineSimpleRecordTypeDefn tdr ms ao' fs astContext =
    // the typeName is already printed
    sepNlnUnlessLastEventIsNewline
    +> opt (indent +> sepNln) ao' genAccess
    +> sepOpenS
    +> atCurrentColumn
        (leaveLeftBrace tdr.Range
         +> col sepSemiNln fs (genField astContext ""))
    +> sepCloseS
    +> leaveNodeTokenByName tdr.Range RBRACE
    +> onlyIf (List.isNotEmpty ms) sepNln
    +> sepNlnBetweenTypeAndMembers ms
    +> genMemberDefnList
        { astContext with
              InterfaceRange = None }
           ms
    +> optSingle (fun _ -> unindent) ao'

and genMultilineSimpleRecordTypeDefnAlignBrackets tdr ms ao' fs astContext =
    // the typeName is already printed
    sepNlnUnlessLastEventIsNewline
    +> opt (indent +> sepNln) ao' genAccess
    +> sepOpenSFixed
    +> indent
    +> sepNln
    +> atCurrentColumn
        (leaveLeftBrace tdr.Range
         +> col sepSemiNln fs (genField astContext ""))
    +> unindent
    +> sepNln
    +> sepCloseSFixed
    +> onlyIf (List.isNotEmpty ms) sepNln
    +> sepNlnBetweenTypeAndMembers ms
    +> genMemberDefnList
        { astContext with
              InterfaceRange = None }
           ms
    +> onlyIf (Option.isSome ao') unindent

and sepNlnBetweenSigTypeAndMembers (ms: SynMemberSig list) =
    match List.tryHead ms with
    | Some m ->
        let range, mainNodeType =
            match m with
            | SynMemberSig.Interface (_, r) -> r, SynMemberSig_Interface
            | SynMemberSig.Inherit (_, r) -> r, SynMemberSig_Inherit
            | SynMemberSig.Member (_, _, r) -> r, SynMemberSig_Member
            | SynMemberSig.NestedType (_, r) -> r, SynMemberSig_NestedType
            | SynMemberSig.ValField (_, r) -> r, SynMemberSig_ValField

        sepNlnTypeAndMembers (Some range) mainNodeType
    | None -> sepNone

and genSigTypeDefn astContext (SigTypeDef (ats, px, ao, tds, tcs, tdr, ms, s, preferPostfix) as node) =
    let range =
        match node with
        | SynTypeDefnSig.TypeDefnSig (_, _, _, r) -> r

    let typeName =
        genPreXmlDoc px
        +> ifElse
            astContext.IsFirstChild
               (genAttributes astContext ats -- "type ")
               (!- "and " +> genOnelinerAttributes astContext ats)
        +> opt sepSpace ao genAccess
        +> genTypeAndParam astContext s tds tcs preferPostfix

    match tdr with
    | SigSimple (TDSREnum ecs) ->
        typeName
        +> sepEq
        +> indent
        +> sepNln
        +> col
            sepNln
               ecs
               (genEnumCase
                   { astContext with
                         HasVerticalBar = true })
        +> sepNlnBetweenSigTypeAndMembers ms
        +> colPre sepNln sepNln ms (genMemberSig astContext)
        // Add newline after un-indent to be spacing-correct
        +> unindent

    | SigSimple (TDSRUnion (ao', xs)) ->
        let unionCases =
            match xs with
            | [] -> id
            | [ UnionCase (attrs, _, _, _, (UnionCaseType fs)) as x ] when List.isEmpty ms ->
                let hasVerticalBar =
                    Option.isSome ao'
                    || not (List.isEmpty attrs)
                    || List.isEmpty fs

                let expr =
                    genTriviaFor
                        SynTypeDefnSimpleRepr_Union
                        tdr.Range
                        (opt sepSpace ao' genAccess
                         +> genUnionCase
                             { astContext with
                                   HasVerticalBar = hasVerticalBar }
                                x)

                expressionFitsOnRestOfLine (indent +> sepSpace +> expr) (indent +> sepNln +> expr)
            | xs ->
                indent
                +> sepNln
                +> opt sepNln ao' genAccess
                +> col
                    sepNln
                       xs
                       (genUnionCase
                           { astContext with
                                 HasVerticalBar = true })

        typeName
        +> sepEq
        +> unionCases
        +> sepNlnBetweenSigTypeAndMembers ms
        +> colPre sepNln sepNln ms (genMemberSig astContext)
        +> unindent

    | SigSimple (TDSRRecord (ao', fs)) ->
        let smallExpression =
            sepSpace
            +> optSingle (fun ao -> genAccess ao +> sepSpace) ao'
            +> sepOpenS
            +> leaveLeftBrace tdr.Range
            +> col sepSemi fs (genField astContext "")
            +> sepCloseS
            +> leaveNodeTokenByName tdr.Range RBRACE

        let multilineExpression =
            ifAlignBrackets
                (genSigSimpleRecordAlignBrackets tdr ms ao' fs astContext)
                (genSigSimpleRecord tdr ms ao' fs astContext)

        let bodyExpr ctx =
            let size = getRecordSize ctx fs

            if (List.isEmpty ms) then
                (isSmallExpression size smallExpression multilineExpression
                 +> leaveNodeFor SynTypeDefnSimpleRepr_Record tdr.Range // this will only print something when there is trivia after } in the short expression
                // Yet it cannot be part of the short expression otherwise the multiline expression would be triggered unwillingly.
                ) ctx
            else
                multilineExpression ctx

        typeName
        +> sepEq
        +> indent
        +> enterNodeFor SynTypeDefnSimpleRepr_Record tdr.Range
        +> bodyExpr
        +> leaveNodeFor SynTypeDefnSimpleRepr_Record tdr.Range
        +> unindent

    | SigSimple TDSRNone ->
        let genMembers =
            match ms with
            | [] -> sepNone
            | _ ->
                !- " with"
                +> indent
                +> sepNln
                +> sepNlnBetweenSigTypeAndMembers ms
                +> col sepNln ms (genMemberSig astContext)
                +> unindent

        typeName +> genMembers
    | SigSimple (TDSRTypeAbbrev t) ->
        let genTypeAbbrev =
            let needsParenthesis =
                match t with
                | SynType.Tuple (isStruct, typeNames, _) -> (isStruct && List.length typeNames > 1)
                | _ -> false

            ifElse needsParenthesis sepOpenT sepNone
            +> genType astContext false t
            +> ifElse needsParenthesis sepCloseT sepNone

        typeName +> sepEq +> sepSpace +> genTypeAbbrev
    | SigSimple (TDSRException (ExceptionDefRepr (ats, px, ao, uc))) -> genExceptionBody astContext ats px ao uc

    | SigObjectModel (TCSimple (TCStruct
                      | TCInterface
                      | TCClass) as tdk,
                      mds) ->
        typeName
        +> sepEq
        +> indent
        +> sepNln
        +> genTypeDefKind tdk
        +> indent
        +> colPre sepNln sepNln mds (genMemberSig astContext)
        +> unindent
        ++ "end"
        +> unindent

    | SigObjectModel (TCSimple TCAugmentation, _) ->
        typeName -- " with"
        +> indent
        +> sepNln
        // Remember that we use MemberSig of parent node
        +> col sepNln ms (genMemberSig astContext)
        +> unindent

    | SigObjectModel (TCDelegate (FunType ts), _) ->
        typeName +> sepEq +> sepSpace -- "delegate of "
        +> genTypeList astContext ts
    | SigObjectModel (_, mds) ->
        typeName
        +> sepEq
        +> indent
        +> sepNln
        +> col sepNln mds (genMemberSig astContext)
        +> unindent

    | SigExceptionRepr (SigExceptionDefRepr (ats, px, ao, uc)) -> genExceptionBody astContext ats px ao uc
    |> genTriviaFor TypeDefnSig_ range

and genSigSimpleRecord tdr ms ao' fs astContext =
    // the typeName is already printed
    sepNlnUnlessLastEventIsNewline
    +> opt (indent +> sepNln) ao' genAccess
    +> sepOpenS
    +> atCurrentColumn
        (leaveLeftBrace tdr.Range
         +> col sepSemiNln fs (genField astContext ""))
    +> sepCloseS
    +> sepNlnBetweenSigTypeAndMembers ms
    +> colPre sepNln sepNln ms (genMemberSig astContext)
    +> optSingle (fun _ -> unindent) ao'

and genSigSimpleRecordAlignBrackets tdr ms ao' fs astContext =
    // the typeName is already printed
    sepNlnUnlessLastEventIsNewline
    +> opt (indent +> sepNln) ao' genAccess
    +> sepOpenSFixed
    +> indent
    +> sepNln
    +> atCurrentColumn
        (leaveLeftBrace tdr.Range
         +> col sepSemiNln fs (genField astContext ""))
    +> unindent
    +> sepNln
    +> sepCloseSFixed
    +> sepNlnBetweenSigTypeAndMembers ms
    +> colPre sepNln sepNln ms (genMemberSig astContext)
    +> onlyIf (Option.isSome ao') unindent

and genMemberSig astContext node =
    let range, mainNodeName =
        match node with
        | SynMemberSig.Member (_, _, r) -> r, SynMemberSig_Member
        | SynMemberSig.Interface (_, r) -> r, SynMemberSig_Interface
        | SynMemberSig.Inherit (_, r) -> r, SynMemberSig_Inherit
        | SynMemberSig.ValField (_, r) -> r, SynMemberSig_ValField
        | SynMemberSig.NestedType (_, r) -> r, SynMemberSig_NestedType

    match node with
    | MSMember (Val (ats, px, ao, s, t, vi, _, ValTyparDecls (tds, _, tcs)), mf) ->
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
        +> atCurrentColumn
            (indent
             +> genMemberFlagsForMemberBinding
                 { astContext with
                       InterfaceRange = None }
                    mf
                    range
             +> opt sepSpace ao genAccess
             +> ifElse (s = "``new``") (!- "new") (!-s)
             +> genTypeParamPostfix astContext tds tcs
             +> sepColonX
             +> genTypeList astContext namedArgs
             +> genConstraints astContext t
             -- (genPropertyKind (not isFunctionProperty) mf.MemberKind)
             +> unindent)

    | MSInterface t -> !- "interface " +> genType astContext false t
    | MSInherit t -> !- "inherit " +> genType astContext false t
    | MSValField f -> genField astContext "val " f
    | MSNestedType _ -> invalidArg "md" "This is not implemented in F# compiler"
    |> genTriviaFor mainNodeName range

and genConstraints astContext (t: SynType) =
    match t with
    | TWithGlobalConstraints (t, tcs) ->
        genTypeByLookup astContext t
        +> sepSpaceOrNlnIfExpressionExceedsPageWidth
            (ifElse (List.isNotEmpty tcs) (!- "when ") sepSpace
             +> col wordAnd tcs (genTypeConstraint astContext))
    | _ -> sepNone

and genTyparDecl astContext (TyparDecl (ats, tp)) =
    genOnelinerAttributes astContext ats
    +> genTypar astContext tp

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

and genExceptionBody astContext ats px ao uc =
    genPreXmlDoc px +> genAttributes astContext ats
    -- "exception "
    +> opt sepSpace ao genAccess
    +> genUnionCase
        { astContext with
              HasVerticalBar = false }
           uc

and genException astContext (ExceptionDef (ats, px, ao, uc, ms) as node) =
    genExceptionBody astContext ats px ao uc
    +> ifElse
        ms.IsEmpty
           sepNone
           (!- " with"
            +> indent
            +> sepNln
            +> genMemberDefnList
                { astContext with
                      InterfaceRange = None }
                   ms
            +> unindent)
    |> genTriviaFor SynExceptionDefn_ node.Range

and genSigException astContext (SigExceptionDef (ats, px, ao, uc, ms)) =
    genExceptionBody astContext ats px ao uc
    +> colPre sepNln sepNln ms (genMemberSig astContext)

and genUnionCase astContext (UnionCase (ats, px, _, s, UnionCaseType fs) as node) =
    let shortExpr =
        colPre wordOf sepStar fs (genField { astContext with IsUnionField = true } "")

    let longExpr =
        wordOf
        +> indent
        +> sepNln
        +> atCurrentColumn (col (sepStar +> sepNln) fs (genField { astContext with IsUnionField = true } ""))
        +> unindent

    genPreXmlDoc px
    +> genTriviaBeforeClausePipe node.Range
    +> ifElse astContext.HasVerticalBar sepBar sepNone
    +> genOnelinerAttributes astContext ats
    -- s
    +> expressionFitsOnRestOfLine shortExpr longExpr
    |> genTriviaFor UnionCase_ node.Range

and genEnumCase astContext (EnumCase (ats, px, _, (_, _)) as node) =
    let genCase (ctx: Context) =
        let expr =
            match node with
            | EnumCase (_, _, identInAST, (c, r)) -> !-identInAST +> !- " = " +> genConst c r

        expr ctx

    genPreXmlDoc px
    +> genTriviaBeforeClausePipe node.Range
    +> ifElse astContext.HasVerticalBar sepBar sepNone
    +> genOnelinerAttributes astContext ats
    +> genCase

and genField astContext prefix (Field (ats, px, ao, isStatic, isMutable, t, so) as node) =
    let range =
        match node with
        | SynField.Field (_, _, _, _, _, _, _, range) -> range
    // Being protective on union case declaration
    let t =
        genType astContext astContext.IsUnionField t

    genPreXmlDoc px
    +> genAttributes astContext ats
    +> ifElse isStatic (!- "static ") sepNone
    -- prefix
    +> ifElse isMutable (!- "mutable ") sepNone
    +> opt sepSpace ao genAccess
    +> opt sepColon so (!-)
    +> t
    |> genTriviaFor Field_ range

and genTypeByLookup astContext (t: SynType) =
    getByLookup t.Range (genType astContext false) t

and genType astContext outerBracket t =
    let rec loop current =
        match current with
        | THashConstraint t ->
            let wrapInParentheses f =
                match t with
                | TApp (_, ts, isPostfix) when (isPostfix && List.isNotEmpty ts) -> sepOpenT +> f +> sepCloseT
                | _ -> f

            !- "#" +> wrapInParentheses (loop t)
        | TMeasurePower (t, n) -> loop t -- "^" +> str n
        | TMeasureDivide (t1, t2) -> loop t1 -- " / " +> loop t2
        | TStaticConstant (c, r) -> genConst c r
        | TStaticConstantExpr (e) -> genExpr astContext e
        | TStaticConstantNamed (t1, t2) -> loop t1 -- "=" +> loop t2
        | TArray (t, n) -> loop t -- " [" +> rep (n - 1) (!- ",") -- "]"
        | TAnon -> sepWild
        | TVar tp -> genTypar astContext tp
        // Drop bracket around tuples before an arrow
        | TFun (TTuple ts, t) -> loopTTupleList ts +> sepArrow +> loop t
        // Do similar for tuples after an arrow
        | TFun (t, TTuple ts) ->
            sepOpenT
            +> loop t
            +> sepArrow
            +> loopTTupleList ts
            +> sepCloseT
        | TFuns ts -> col sepArrow ts loop
        | TApp (t, ts, isPostfix) ->
            let postForm =
                match ts with
                | [] -> loop t
                | [ t' ] -> loop t' +> sepSpace +> loop t
                | ts ->
                    sepOpenT
                    +> col sepComma ts loop
                    +> sepCloseT
                    +> loop t

            ifElse isPostfix postForm (loop t +> genPrefixTypes astContext ts)

        | TLongIdentApp (t, s, ts) ->
            loop t -- sprintf ".%s" s
            +> genPrefixTypes astContext ts
        | TTuple ts -> loopTTupleList ts
        | TStructTuple ts ->
            !- "struct "
            +> sepOpenT
            +> loopTTupleList ts
            +> sepCloseT
        | TWithGlobalConstraints (TVar _, [ TyparSubtypeOfType _ as tc ]) -> genTypeConstraint astContext tc
        | TWithGlobalConstraints (TFuns ts, tcs) ->
            col sepArrow ts loop
            +> colPre (!- " when ") wordAnd tcs (genTypeConstraint astContext)
        | TWithGlobalConstraints (t, tcs) ->
            loop t
            +> colPre (!- " when ") wordAnd tcs (genTypeConstraint astContext)
        | TLongIdent s ->
            ifElseCtx (fun ctx ->
                not ctx.Config.StrictMode
                && astContext.IsCStylePattern) (genTypeByLookup astContext t) (!-s)
            |> genTriviaFor Ident_ current.Range
        | TAnonRecord (isStruct, fields) ->
            let smallExpression =
                ifElse isStruct !- "struct " sepNone
                +> sepOpenAnonRecd
                +> col sepSemi fields (genAnonRecordFieldType astContext)
                +> sepCloseAnonRecd

            let longExpression =
                ifElse isStruct !- "struct " sepNone
                +> sepOpenAnonRecd
                +> atCurrentColumn (col sepSemiNln fields (genAnonRecordFieldType astContext))
                +> sepCloseAnonRecd

            fun (ctx: Context) ->
                let size = getRecordSize ctx fields
                isSmallExpression size smallExpression longExpression ctx
        | TParen (innerT) ->
            sepOpenT
            +> loop innerT
            +> sepCloseT
            +> leaveNodeTokenByName current.Range RPAREN
        | t -> failwithf "Unexpected type: %O" t

    and loopTTupleList =
        function
        | [] -> sepNone
        | [ (_, t) ] -> loop t
        | (isDivide, t) :: ts ->
            loop t -- (if isDivide then " / " else " * ")
            +> loopTTupleList ts

    match t with
    | TFun (TTuple ts, t) ->
        ifElse
            outerBracket
            (sepOpenT
             +> loopTTupleList ts
             +> sepArrow
             +> loop t
             +> sepCloseT)
            (loopTTupleList ts +> sepArrow +> loop t)
    | TFuns ts -> ifElse outerBracket (sepOpenT +> col sepArrow ts loop +> sepCloseT) (col sepArrow ts loop)
    | TTuple ts -> ifElse outerBracket (sepOpenT +> loopTTupleList ts +> sepCloseT) (loopTTupleList ts)
    | _ -> loop t

and genAnonRecordFieldType astContext (AnonRecordFieldType (s, t)) =
    !-s +> sepColon +> (genType astContext false t)

and genPrefixTypes astContext node ctx =
    match node with
    | [] -> ctx
    // Where <  and ^ meet, we need an extra space. For example:  seq< ^a >
    | (TVar (Typar (_, true)) as t) :: ts ->
        (!- "< "
         +> col sepComma (t :: ts) (genType astContext false)
         -- " >") ctx
    | t :: _ ->
        // for example: FSharpx.Regex< @"(?<value>\d+)" >
        let firstItemHasAtSignBeforeString =
            match t with
            | TStaticConstant (_, r) ->
                TriviaHelpers.``has content itself that matches`` (function
                    | StringContent sc -> sc.StartsWith("@")
                    | _ -> false) r
                    (TriviaHelpers.getNodesForTypes
                        [ SynExpr_Const
                          SynType_StaticConstant ]
                         ctx.TriviaMainNodes)
            | _ -> false

        (!- "<"
         +> onlyIf firstItemHasAtSignBeforeString sepSpace
         +> col sepComma node (genType astContext false)
         +> onlyIf firstItemHasAtSignBeforeString sepSpace
         -- ">") ctx

and genTypeList astContext node =
    match node with
    | [] -> sepNone
    | (t, [ ArgInfo (ats, so, isOpt) ]) :: ts ->
        let gt =
            match t with
            | TTuple _ -> not ts.IsEmpty
            | TFun _ -> true // Fun is grouped by brackets inside 'genType astContext true t'
            | _ -> false
            |> fun hasBracket ->
                opt sepColonFixed so (if isOpt then (sprintf "?%s" >> (!-)) else (!-))
                +> genType astContext hasBracket t

        genOnelinerAttributes astContext ats
        +> gt
        +> ifElse ts.IsEmpty sepNone (autoNlnIfExpressionExceedsPageWidth (sepArrow +> genTypeList astContext ts))

    | (TTuple ts', argInfo) :: ts ->
        // The '/' separator shouldn't appear here
        let hasBracket = not ts.IsEmpty

        let gt sepBefore =
            col sepBefore (Seq.zip argInfo (Seq.map snd ts')) (fun ((ArgInfo (ats, so, isOpt)), t) ->
                genOnelinerAttributes astContext ats
                +> opt sepColonFixed so (if isOpt then (sprintf "?%s" >> (!-)) else (!-))
                +> genType astContext hasBracket t)

        let shortExpr =
            gt sepStar
            +> ifElse ts.IsEmpty sepNone (sepArrow +> genTypeList astContext ts)

        let longExpr =
            gt (sepNln +> sepStarFixed)
            +> ifElse
                ts.IsEmpty
                   sepNone
                   (sepNln
                    +> sepArrowFixed
                    +> genTypeList astContext ts)

        atCurrentColumn (expressionFitsOnRestOfLine shortExpr longExpr)

    | (t, _) :: ts ->
        let gt = genType astContext false t

        gt
        +> ifElse ts.IsEmpty sepNone (autoNlnIfExpressionExceedsPageWidth (sepArrow +> genTypeList astContext ts))

and genTypar astContext (Typar (s, isHead) as node) =
    ifElse isHead (ifElse astContext.IsFirstTypeParam (!- " ^") (!- "^")) (!- "'")
    -- s
    |> genTriviaFor SynType_Var node.Range

and genTypeConstraint astContext node =
    match node with
    | TyparSingle (kind, tp) ->
        genTypar astContext tp +> sepColon
        -- sprintf "%O" kind
    | TyparDefaultsToType (tp, t) ->
        !- "default "
        +> genTypar astContext tp
        +> sepColon
        +> genType astContext false t
    | TyparSubtypeOfType (tp, t) ->
        genTypar astContext tp -- " :> "
        +> genType astContext false t
    | TyparSupportsMember (tps, msg) ->
        genTyparList astContext tps
        +> sepColon
        +> sepOpenT
        +> genMemberSig astContext msg
        +> sepCloseT
    | TyparIsEnum (tp, ts) ->
        genTypar astContext tp +> sepColon -- "enum<"
        +> col sepComma ts (genType astContext false)
        -- ">"
    | TyparIsDelegate (tp, ts) ->
        genTypar astContext tp +> sepColon -- "delegate<"
        +> col sepComma ts (genType astContext false)
        -- ">"

and genInterfaceImpl astContext (InterfaceImpl (t, bs, range)) =
    match bs with
    | [] -> !- "interface " +> genType astContext false t
    | bs ->
        !- "interface " +> genType astContext false t
        -- " with"
        +> indent
        +> sepNln
        +> genMemberBindingList
            { astContext with
                  InterfaceRange = Some range }
               bs
        +> unindent

and genClause astContext hasBar (Clause (p, e, eo)) =
    let clauseBody e (ctx: Context) =
        (autoIndentAndNlnIfExpressionExceedsPageWidth (genExpr astContext e)) ctx

    let arrowRange =
        mkRange "arrowRange" p.Range.End e.Range.Start

    let pat = genPat astContext p

    let body =
        optPre (!- " when ") sepNone eo (genExpr astContext)
        +> sepArrow
        +> leaveNodeTokenByName arrowRange RARROW
        +> clauseBody e

    genTriviaBeforeClausePipe p.Range
    +> ifElse hasBar (sepBar +> atCurrentColumnWithPrepend pat body) (pat +> body)

/// Each multiline member definition has a pre and post new line.
and genMemberDefnList astContext nodes =
    let rec collectItems nodes =
        match nodes with
        | [] -> []
        | PropertyWithGetSetMemberDefn (gs, rest) ->
            let attrs =
                getRangesFromAttributesFromSynBinding (fst gs)

            let rangeOfFirstMember = List.head nodes |> fun m -> m.Range

            let expr =
                enterNodeFor SynMemberDefn_Member rangeOfFirstMember
                +> genPropertyWithGetSet astContext gs (Some rangeOfFirstMember)

            let sepNln =
                sepNlnConsideringTriviaContentBeforeWithAttributesFor SynMemberDefn_Member rangeOfFirstMember attrs

            (expr, sepNln, rangeOfFirstMember)
            :: (collectItems rest)

        | m :: rest ->
            let attrs =
                getRangesFromAttributesFromSynMemberDefinition m

            let expr = genMemberDefn astContext m

            let sepNln =
                sepNlnConsideringTriviaContentBeforeWithAttributesFor (synMemberDefnToFsAstType m) m.Range attrs

            (expr, sepNln, m.Range) :: (collectItems rest)

    collectItems nodes
    |> colWithNlnWhenItemIsMultiline

and genMemberDefn astContext node =
    match node with
    | MDNestedType _ -> invalidArg "md" "This is not implemented in F# compiler"
    | MDOpen (s) -> !-(sprintf "open %s" s)
    // What is the role of so
    | MDImplicitInherit (t, e, _) ->
        let addSpaceAfterType =
            match e with
            | SynExpr.Const (SynConst.Unit, _) -> false
            | SynExpr.Const _ -> true // string, numbers, ...
            | _ -> false

        !- "inherit "
        +> genType astContext false t
        +> ifElse addSpaceAfterType sepSpace sepNone
        +> genExpr astContext e
    | MDInherit (t, _) -> !- "inherit " +> genType astContext false t
    | MDValField f -> genField astContext "val " f
    | MDImplicitCtor (ats, ao, ps, so) ->
        let rec simplePats ps =
            match ps with
            | SynSimplePats.SimplePats (pats, _) -> pats
            | SynSimplePats.Typed (spts, _, _) -> simplePats spts

        let genCtor =
            let shortExpr =
                optPre sepSpace sepSpace ao genAccess
                +> sepOpenT
                +> col sepComma (simplePats ps) (genSimplePat astContext)
                +> sepCloseT

            let longExpr ctx =
                if ctx.Config.AlternativeLongMemberDefinitions then
                    (indent
                     +> sepNln
                     +> optSingle (fun ao -> genAccess ao +> sepNln) ao
                     +> sepOpenT
                     +> indent
                     +> sepNln
                     +> col (sepComma +> sepNln) (simplePats ps) (genSimplePat astContext)
                     +> unindent
                     +> sepNln
                     +> sepCloseT
                     +> sepNln
                     +> unindent) ctx
                else
                    (optPre sepSpace sepSpace ao genAccess
                     +> sepOpenT
                     +> atCurrentColumn (col (sepComma +> sepNln) (simplePats ps) (genSimplePat astContext))
                     +> sepCloseT) ctx

            expressionFitsOnRestOfLine shortExpr longExpr

        // In implicit constructor, attributes should come even before access qualifiers
        ifElse ats.IsEmpty sepNone (sepSpace +> genOnelinerAttributes astContext ats)
        +> genCtor
        +> optPre (!- " as ") sepNone so (!-)

    | MDMember (b) -> genMemberBinding astContext b
    | MDLetBindings (isStatic, isRec, b :: bs) ->
        let prefix =
            if isStatic && isRec then "static let rec "
            elif isStatic then "static let "
            elif isRec then "let rec "
            else "let "

        genLetBinding { astContext with IsFirstChild = true } prefix b
        +> colPre sepNln sepNln bs (genLetBinding { astContext with IsFirstChild = false } "and ")

    | MDInterface (t, mdo, range) ->
        !- "interface "
        +> genType astContext false t
        +> opt sepNone mdo (fun mds ->
               !- " with"
               +> indent
               +> sepNln
               +> genMemberDefnList
                   { astContext with
                         InterfaceRange = Some range }
                      mds
               +> unindent)

    | MDAutoProperty (ats, px, ao, mk, e, s, _isStatic, typeOpt, memberKindToMemberFlags) ->
        let isFunctionProperty =
            match typeOpt with
            | Some (TFun _) -> true
            | _ -> false

        genPreXmlDoc px
        +> genAttributes astContext ats
        +> genMemberFlags astContext (memberKindToMemberFlags mk)
        +> str "val "
        +> opt sepSpace ao genAccess
        -- s
        +> optPre sepColon sepNone typeOpt (genType astContext false)
        +> sepEq
        +> sepSpace
        +> genExpr astContext e
        -- genPropertyKind (not isFunctionProperty) mk

    | MDAbstractSlot (ats, px, ao, s, t, vi, ValTyparDecls (tds, _, tcs), MFMemberFlags mk) ->
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
        +> opt sepSpace ao genAccess
        -- sprintf "abstract %s" s
        +> genTypeParamPostfix astContext tds tcs
        +> sepColonX
        +> genTypeList astContext namedArgs
        -- genPropertyKind (not isFunctionProperty) mk
        +> genConstraints astContext t

    | md -> failwithf "Unexpected member definition: %O" md
    |> genTriviaFor (synMemberDefnToFsAstType node) node.Range

and genPropertyKind useSyntacticSugar node =
    match node with
    | PropertyGet ->
        // Try to use syntactic sugar on real properties (not methods in disguise)
        if useSyntacticSugar then "" else " with get"
    | PropertySet -> " with set"
    | PropertyGetSet -> " with get, set"
    | _ -> ""

and genSimplePat astContext node =
    match node with
    | SPId (s, isOptArg, _) -> ifElse isOptArg (!-(sprintf "?%s" s)) (!-s)
    | SPTyped (sp, t) ->
        genSimplePat astContext sp
        +> sepColon
        +> genType astContext false t
    | SPAttrib (ats, sp) ->
        genOnelinerAttributes astContext ats
        +> genSimplePat astContext sp

and genSimplePats astContext node =
    match node with
    // Remove parentheses on an extremely simple pattern
    | SimplePats [ SPId _ as sp ] -> genSimplePat astContext sp
    | SimplePats ps ->
        sepOpenT
        +> col sepComma ps (genSimplePat astContext)
        +> sepCloseT
    | SPSTyped (ps, t) ->
        genSimplePats astContext ps
        +> sepColon
        +> genType astContext false t

and genComplexPat astContext node =
    match node with
    | CPId p -> genPat astContext p
    | CPSimpleId (s, isOptArg, _) -> ifElse isOptArg (!-(sprintf "?%s" s)) (!-s)
    | CPTyped (sp, t) ->
        genComplexPat astContext sp
        +> sepColon
        +> genType astContext false t
    | CPAttrib (ats, sp) ->
        genOnelinerAttributes astContext ats
        +> genComplexPat astContext sp

and genComplexPats astContext node =
    match node with
    | ComplexPats [ CPId _ as c ]
    | ComplexPats [ CPSimpleId _ as c ] -> genComplexPat astContext c
    | ComplexPats ps ->
        sepOpenT
        +> col sepComma ps (genComplexPat astContext)
        +> sepCloseT
    | ComplexTyped (ps, t) ->
        genComplexPats astContext ps
        +> sepColon
        +> genType astContext false t

and genPatRecordFieldName astContext (PatRecordFieldName (s1, s2, p)) =
    ifElse (s1 = "") (!-(sprintf "%s = " s2)) (!-(sprintf "%s.%s = " s1 s2))
    +> genPat astContext p

and genPatWithIdent astContext (ido, p) =
    opt (sepEq +> sepSpace) ido (!-)
    +> genPat astContext p

and genPat astContext pat =
    match pat with
    | PatOptionalVal (s) -> !-(sprintf "?%s" s)
    | PatAttrib (p, ats) ->
        genOnelinerAttributes astContext ats
        +> genPat astContext p
    | PatOr (p1, p2) ->
        let barRange =
            mkRange "bar range" p1.Range.End p2.Range.Start

        genPat astContext p1
        +> sepNln
        +> enterNodeTokenByName barRange BAR
        -- "| "
        +> genPat astContext p2
    | PatAnds (ps) -> col (!- " & ") ps (genPat astContext)
    | PatNullary PatNull -> !- "null"
    | PatNullary PatWild -> sepWild
    | PatTyped (p, t) ->
        // CStyle patterns only occur on extern declaration so it doesn't escalate to expressions
        // We lookup sources to get extern types since it has quite many exceptions compared to normal F# types
        ifElse
            astContext.IsCStylePattern
            (genTypeByLookup astContext t
             +> sepSpace
             +> genPat astContext p)
            (genPat astContext p
             +> sepColon
             +> atCurrentColumnIndent (genType astContext false t))
    | PatNamed (ao, PatNullary PatWild, s) ->
        opt sepSpace ao genAccess
        +> infixOperatorFromTrivia pat.Range s
    | PatNamed (ao, p, s) ->
        opt sepSpace ao genAccess +> genPat astContext p
        -- sprintf " as %s" s
    | PatLongIdent (ao, s, ps, tpso) ->
        let aoc = opt sepSpace ao genAccess

        let tpsoc =
            opt sepNone tpso (fun (ValTyparDecls (tds, _, tcs)) -> genTypeParamPostfix astContext tds tcs)
        // Override escaped new keyword
        let s = if s = "``new``" then "new" else s

        match ps with
        | [] -> aoc -- s +> tpsoc
        | [ (_, PatTuple [ p1; p2 ]) ] when s = "(::)" ->
            aoc +> genPat astContext p1 -- " :: "
            +> genPat astContext p2
        | [ (ido, p) as ip ] ->
            aoc
            +> infixOperatorFromTrivia pat.Range s
            +> tpsoc
            +> ifElse
                (hasParenInPat p || Option.isSome ido)
                   (ifElseCtx (addSpaceBeforeParensInFunDef astContext s p) sepSpace sepNone)
                   sepSpace
            +> ifElse
                (Option.isSome ido)
                   (sepOpenT
                    +> genPatWithIdent astContext ip
                    +> sepCloseT)
                   (genPatWithIdent astContext ip)
        // This pattern is potentially long
        | ps ->
            let hasBracket =
                ps |> Seq.map fst |> Seq.exists Option.isSome

            let genName = aoc -- s +> tpsoc +> sepSpace

            let genParameters =
                expressionFitsOnRestOfLine
                    (atCurrentColumn (col (ifElse hasBracket sepSemi sepSpace) ps (genPatWithIdent astContext)))
                    (atCurrentColumn (col sepNln ps (genPatWithIdent astContext)))

            genName
            +> ifElse hasBracket sepOpenT sepNone
            +> genParameters
            +> ifElse hasBracket sepCloseT sepNone

    | PatParen (PatConst (Const "()", _)) -> !- "()"
    | PatParen (p) ->
        let shortExpression =
            sepOpenT
            +> genPat astContext p
            +> enterNodeTokenByName pat.Range RPAREN
            +> sepCloseT

        let longExpression ctx =
            if astContext.IsMemberDefinition
               && ctx.Config.AlternativeLongMemberDefinitions then
                (sepOpenT
                 +> indent
                 +> sepNln
                 +> genPat astContext p
                 +> unindent
                 +> sepNln
                 +> sepCloseT) ctx
            else
                shortExpression ctx

        expressionFitsOnRestOfLine shortExpression longExpression
    | PatTuple ps ->
        expressionFitsOnRestOfLine
            (col sepComma ps (genPat astContext))
            (atCurrentColumn (col (sepComma +> sepNln) ps (genPat astContext)))
    | PatStructTuple ps ->
        !- "struct "
        +> sepOpenT
        +> atCurrentColumn (colAutoNlnSkip0 sepComma ps (genPat astContext))
        +> sepCloseT
    | PatSeq (PatList, ps) ->
        ifElse
            ps.IsEmpty
            (sepOpenLFixed +> sepCloseLFixed)
            (sepOpenL
             +> atCurrentColumn (colAutoNlnSkip0 sepSemi ps (genPat astContext))
             +> sepCloseL)

    | PatSeq (PatArray, ps) ->
        ifElse
            ps.IsEmpty
            (sepOpenAFixed +> sepCloseAFixed)
            (sepOpenA
             +> atCurrentColumn (colAutoNlnSkip0 sepSemi ps (genPat astContext))
             +> sepCloseA)

    | PatRecord (xs) ->
        let smallRecordExpr =
            sepOpenS
            +> col sepSemi xs (genPatRecordFieldName astContext)
            +> sepCloseS

        // Note that MultilineBlockBracketsOnSameColumn is not taken into account here.
        let multilineRecordExpr =
            sepOpenS
            +> atCurrentColumn (col sepSemiNln xs (genPatRecordFieldName astContext))
            +> sepCloseS

        fun ctx ->
            let size = getRecordSize ctx xs
            isSmallExpression size smallRecordExpr multilineRecordExpr ctx
    | PatConst (c, r) -> genConst c r
    | PatIsInst (TApp (_, [ _ ], _) as t)
    | PatIsInst (TArray _ as t) ->
        // special case for things like ":? (int seq) ->"
        !- ":? "
        +> sepOpenT
        +> genType astContext false t
        +> sepCloseT
    | PatIsInst (t) ->
        // Should have brackets around in the type test patterns
        !- ":? " +> genType astContext true t
    // Quotes will be printed by inner expression
    | PatQuoteExpr e -> genExpr astContext e
    | p -> failwithf "Unexpected pattern: %O" p
    |> (match pat with
        | SynPat.Named _ -> genTriviaFor SynPat_Named pat.Range
        | SynPat.Wild _ -> genTriviaFor SynPat_Wild pat.Range
        | SynPat.LongIdent _ -> genTriviaFor SynPat_LongIdent pat.Range
        | SynPat.Paren _ -> genTriviaFor SynPat_Paren pat.Range
        | _ -> id)

and genPatWithReturnType ao s ps tpso (t: SynType option) (astContext: ASTContext) =
    let aoc = opt sepSpace ao genAccess

    let tpsoc =
        opt sepNone tpso (fun (ValTyparDecls (tds, _, tcs)) -> genTypeParamPostfix astContext tds tcs)
    // Override escaped new keyword
    let s = if s = "``new``" then "new" else s

    let hasBracket =
        ps |> Seq.map fst |> Seq.exists Option.isSome

    let genName = aoc -- s +> tpsoc +> sepSpace

    let genParametersInitial =
        colAutoNlnSkip0 (ifElse hasBracket sepSemi sepSpace) ps (genPatWithIdent astContext)

    let genReturnType, newlineBeforeReturnType =
        match t with
        | Some t ->
            enterNodeFor SynBindingReturnInfo_ t.Range
            +> genType astContext false t,
            sepNln
        | None ->
            let genReturnType = sepNone

            let newline ctx =
                if ctx.Config.AlignFunctionSignatureToIndentation then
                    ctx
                else
                    match ps with
                    | [ (_, PatTuple _) ] -> ctx
                    | _ -> sepNln ctx

            genReturnType, newline

    let genParametersWithNewlines ctx =
        if ctx.Config.AlignFunctionSignatureToIndentation then
            (indent
             +> sepNln
             +> col sepNln ps (genPatWithIdent astContext)
             +> newlineBeforeReturnType
             +> unindent) ctx
        else
            atCurrentColumn
                (col sepNln ps (genPatWithIdent astContext)
                 +> newlineBeforeReturnType)
                ctx

    let isLongFunctionSignature (ctx: Context) =
        let space = 1

        let colon =
            if ctx.Config.SpaceBeforeColon then 3 else 2

        let lengthByAST =
            getSynAccessLength ao
            + lengthWhenSome (fun _ -> space) ao
            + s.Length
            + space
            + List.sumBy (snd >> getSynPatLength >> (+) space) ps
            + lengthWhenSome (fun _ -> colon) t
            + lengthWhenSome getSynTypeLength t

        (ctx.Column + lengthByAST > ctx.Config.MaxLineLength)
        || futureNlnCheck (genName +> genParametersInitial +> genReturnType) ctx

    fun ctx ->
        let isLong = isLongFunctionSignature ctx

        let expr =
            genName
            +> ifElse hasBracket sepOpenT sepNone
            +> ifElse isLong genParametersWithNewlines genParametersInitial
            +> ifElse hasBracket sepCloseT sepNone

        expr ctx

and genConst (c: SynConst) (r: range) =
    match c with
    | SynConst.Unit ->
        enterNodeTokenByName r LPAREN
        +> !- "("
        +> leaveNodeTokenByName r LPAREN
        +> enterNodeTokenByName r RPAREN
        +> !- ")"
        +> leaveNodeTokenByName r RPAREN
    | SynConst.Bool (b) -> !-(if b then "true" else "false")
    | SynConst.Byte _
    | SynConst.SByte _
    | SynConst.Int16 _
    | SynConst.Int32 _
    | SynConst.Int64 _
    | SynConst.UInt16 _
    | SynConst.UInt16s _
    | SynConst.UInt32 _
    | SynConst.UInt64 _
    | SynConst.Double _
    | SynConst.Single _
    | SynConst.Decimal _
    | SynConst.IntPtr _
    | SynConst.UInt64 _
    | SynConst.UIntPtr _
    | SynConst.UserNum _ -> genConstNumber c r
    | SynConst.String (s, _) ->
        fun (ctx: Context) ->
            let trivia =
                TriviaHelpers.getNodesForTypes
                    [ SynExpr_Const
                      SynType_StaticConstant
                      SynPat_Const ]
                    ctx.TriviaMainNodes
                |> List.tryFind (fun tv -> RangeHelpers.rangeEq tv.Range r)

            let triviaStringContent =
                trivia
                |> Option.bind (fun tv ->
                    match tv.ContentItself with
                    | Some (StringContent (sc)) -> Some sc
                    | _ -> None)

            match triviaStringContent, trivia with
            | Some stringContent, Some _ -> !-stringContent
            | None,
              Some ({ ContentBefore = [ Keyword ({ TokenInfo = { TokenName = "KEYWORD_STRING" }; Content = kw }) ] }) ->
                !-kw
            | None,
              Some ({ ContentBefore = [ Keyword ({ TokenInfo = { TokenName = "QMARK" } }) ];
                      ContentItself = Some (IdentBetweenTicks ibt) }) -> !-ibt
            | None, Some ({ ContentBefore = [ Keyword ({ TokenInfo = { TokenName = "QMARK" } }) ] }) -> !-s
            | _ ->
                let escaped = Regex.Replace(s, "\"{1}", "\\\"")
                !-(sprintf "\"%s\"" escaped)
            <| ctx
    | SynConst.Char (c) ->
        fun (ctx: Context) ->
            let charContentFromTrivia =
                TriviaHelpers.``get CharContent`` r ctx.TriviaMainNodes

            let expr =
                match charContentFromTrivia with
                | Some content -> !-content
                | None ->
                    let escapedChar = Char.escape c
                    !-(sprintf "\'%s\'" escapedChar)

            expr ctx
    | SynConst.Bytes (bytes, _) -> genConstBytes bytes r
    | SynConst.Measure (c, m) ->
        let measure =
            match m with
            | Measure m -> !-m

        genConstNumber c r
        +> measure
        +> leaveNodeTokenByName r GREATER

and genConstNumber (c: SynConst) (r: range) =
    fun (ctx: Context) ->
        TriviaHelpers.getNodesForTypes
            [ SynExpr_Const
              SynPat_Const
              EnumCase_ ]
            ctx.TriviaMainNodes
        |> List.tryFind (fun t -> RangeHelpers.rangeEq t.Range r)
        |> Option.bind (fun tn ->
            match tn.ContentItself with
            | Some (Number (n)) -> Some n
            | _ -> None)
        |> fun n ->
            match n with
            | Some n -> !-n
            | None ->
                match c with
                | SynConst.Byte (v) -> !-(sprintf "%A" v)
                | SynConst.SByte (v) -> !-(sprintf "%A" v)
                | SynConst.Int16 (v) -> !-(sprintf "%A" v)
                | SynConst.Int32 (v) -> !-(sprintf "%A" v)
                | SynConst.Int64 (v) -> !-(sprintf "%A" v)
                | SynConst.UInt16 (v) -> !-(sprintf "%A" v)
                | SynConst.UInt16s (v) -> !-(sprintf "%A" v)
                | SynConst.UInt32 (v) -> !-(sprintf "%A" v)
                | SynConst.UInt64 (v) -> !-(sprintf "%A" v)
                | SynConst.Double (v) -> !-(sprintf "%A" v)
                | SynConst.Single (v) -> !-(sprintf "%A" v)
                | SynConst.Decimal (v) -> !-(sprintf "%A" v)
                | SynConst.IntPtr (v) -> !-(sprintf "%A" v)
                | SynConst.UIntPtr (v) -> !-(sprintf "%A" v)
                | SynConst.UserNum (v, s) -> !-(sprintf "%s%s" v s)
                | _ -> failwithf "Cannot generating Const number for %A" c
        <| ctx

and genConstBytes (bytes: byte []) (r: range) =
    fun (ctx: Context) ->
        let trivia =
            Map.tryFindOrEmptyList SynExpr_Const ctx.TriviaMainNodes
            |> List.tryFind (fun t -> RangeHelpers.rangeEq t.Range r)
            |> Option.bind (fun tv ->
                match tv.ContentItself with
                | Some (StringContent (content)) -> Some content
                | _ -> None)

        match trivia with
        | Some t -> !-t
        | None -> !-(sprintf "%A" bytes)
        <| ctx

and genTriviaFor (mainNodeName: FsAstType) (range: range) f ctx =
    (enterNodeFor mainNodeName range
     +> f
     +> leaveNodeFor mainNodeName range) ctx

and infixOperatorFromTrivia range fallback (ctx: Context) =
    // by specs, section 3.4 https://fsharp.org/specs/language-spec/4.1/FSharpSpec-4.1-latest.pdf#page=24&zoom=auto,-137,312
    let validIdentRegex =
        """^(_|\p{L}|\p{Nl})([_'0-9]|\p{L}|\p{Nl}\p{Pc}|\p{Mn}|\p{Mc}|\p{Cf})*$"""

    let isValidIdent x = Regex.Match(x, validIdentRegex).Success

    TriviaHelpers.getNodesForTypes [ SynPat_LongIdent; SynPat_Named ] ctx.TriviaMainNodes
    |> List.choose (fun t ->
        match t.Range = range with
        | true ->
            match t.ContentItself with
            | Some (IdentOperatorAsWord (iiw)) -> Some iiw
            | Some (IdentBetweenTicks (iiw)) when not (isValidIdent fallback) -> Some iiw // Used when value between ``...``
            | _ -> None
        | _ -> None)
    |> List.tryHead
    |> fun iiw ->
        match iiw with
        | Some iiw -> !- iiw ctx
        | None -> !- fallback ctx
