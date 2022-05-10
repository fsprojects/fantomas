module internal Fantomas.Core.CodePrinter

open System
open System.Text.RegularExpressions
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Xml
open Fantomas
open Fantomas.Core.AstExtensions
open Fantomas.Core.FormatConfig
open Fantomas.Core.SourceParser
open Fantomas.Core.SourceTransformer
open Fantomas.Core.Context
open Fantomas.Core.TriviaTypes

/// This type consists of contextual information which is important for formatting
/// Please avoid using this record as it can be the cause of unexpected behavior when used incorrectly
type ASTContext =
    { /// Current node is a subnode deep down in an interface
      InterfaceRange: Range option
      /// This pattern matters for formatting extern declarations
      IsCStylePattern: bool
      /// Range operators are naked in 'for..in..do' constructs
      IsNakedRange: bool
      /// A field is rendered as union field or not
      IsUnionField: bool
      /// First type param might need extra spaces to avoid parsing errors on `<^`, `<'`, etc.
      IsFirstTypeParam: bool
      /// Inside a SynPat of MatchClause
      IsInsideMatchClausePattern: bool }
    static member Default =
        { InterfaceRange = None
          IsCStylePattern = false
          IsNakedRange = false
          IsUnionField = false
          IsFirstTypeParam = false
          IsInsideMatchClausePattern = false }

let rec addSpaceBeforeParensInFunCall functionOrMethod arg (ctx: Context) =
    match functionOrMethod, arg with
    | SynExpr.TypeApp (e, _, _, _, _, _, _), _ -> addSpaceBeforeParensInFunCall e arg ctx
    | SynExpr.Paren _, _ -> true
    | SynExpr.Const _, _ -> true
    | UppercaseSynExpr, ConstUnitExpr -> ctx.Config.SpaceBeforeUppercaseInvocation
    | LowercaseSynExpr, ConstUnitExpr -> ctx.Config.SpaceBeforeLowercaseInvocation
    | SynExpr.Ident _, SynExpr.Ident _ -> true
    | UppercaseSynExpr, Paren _ -> ctx.Config.SpaceBeforeUppercaseInvocation
    | LowercaseSynExpr, Paren _ -> ctx.Config.SpaceBeforeLowercaseInvocation
    | _ -> true

let addSpaceBeforeParensInFunDef (spaceBeforeSetting: bool) (functionOrMethod: SynLongIdent) args =
    match functionOrMethod, args with
    | SynLongIdent(id = [ newIdent ]), _ when newIdent.idText = "new" -> false
    | _, PatParen _ -> spaceBeforeSetting
    | _, PatNamed _
    | _, SynPat.Wild _ -> true
    | SynLongIdent (id = lid), _ ->
        match List.tryLast lid with
        | None -> false
        | Some ident -> not (Char.IsUpper ident.idText.[0])
    | _ -> true

let rec genParsedInput astContext ast =
    match ast with
    | ImplFile im -> genImpFile astContext im
    | SigFile si -> genSigFile astContext si
    +> addFinalNewline

/// Respect insert_final_newline setting
and addFinalNewline ctx =
    let lastEvent = ctx.WriterEvents.TryHead

    match lastEvent with
    | Some WriteLineBecauseOfTrivia ->
        if ctx.Config.InsertFinalNewline then
            ctx
        else
            // Due to trivia the last event is a newline, if insert_final_newline is false, we need to remove it.
            { ctx with
                WriterEvents = ctx.WriterEvents.Tail
                WriterModel = { ctx.WriterModel with Lines = List.tail ctx.WriterModel.Lines } }
    | _ -> onlyIf ctx.Config.InsertFinalNewline sepNln ctx

(*
    See https://github.com/fsharp/FSharp.Compiler.Service/blob/master/src/fsharp/ast.fs#L1518
    hs = hashDirectives : ParsedHashDirective list
    mns = modules : SynModuleOrNamespace list
*)
and genImpFile astContext (ParsedImplFileInput (hs, mns, _, _)) =
    col sepNln hs genParsedHashDirective
    +> (if hs.IsEmpty then sepNone else sepNln)
    +> col sepNln mns (genModuleOrNamespace astContext)

and genSigFile astContext (ParsedSigFileInput (hs, mns, _, _)) =
    col sepNone hs genParsedHashDirective
    +> (if hs.IsEmpty then sepNone else sepNln)
    +> col sepNln mns (genSigModuleOrNamespace astContext)

and genParsedHashDirective (ParsedHashDirective (h, args, r)) =
    let genArg (arg: ParsedHashDirectiveArgument) =
        match arg with
        | ParsedHashDirectiveArgument.String (value, stringKind, range) ->
            genConstString stringKind value
            |> genTriviaFor ParsedHashDirectiveArgument_String range
        | ParsedHashDirectiveArgument.SourceIdentifier (identifier, _, range) ->
            !-identifier
            |> genTriviaFor ParsedHashDirectiveArgument_String range

    !- "#" -- h
    +> sepSpace
    +> col sepSpace args genArg
    |> genTriviaFor ParsedHashDirective_ r

and genModuleOrNamespaceKind
    (moduleRange: range option)
    (namespaceRange: range option)
    (kind: SynModuleOrNamespaceKind)
    =
    match kind with
    | SynModuleOrNamespaceKind.DeclaredNamespace ->
        genTriviaForOption SynModuleOrNamespace_Namespace namespaceRange !- "namespace "
    | SynModuleOrNamespaceKind.NamedModule -> genTriviaForOption SynModuleOrNamespace_Module moduleRange !- "module "
    | SynModuleOrNamespaceKind.GlobalNamespace ->
        genTriviaForOption SynModuleOrNamespace_Namespace namespaceRange !- "namespace"
        +> !- " global"
    | SynModuleOrNamespaceKind.AnonModule -> sepNone

and genModuleOrNamespace
    astContext
    (ModuleOrNamespace (ats, px, moduleRange, namespaceRange, ao, lids, mds, isRecursive, moduleKind, range))
    =
    let sepModuleAndFirstDecl =
        let firstDecl = List.tryHead mds

        match firstDecl with
        | None -> sepNone
        | Some mdl ->
            sepNln
            +> sepNlnConsideringTriviaContentBeforeForMainNode (synModuleDeclToFsAstType mdl) mdl.Range

    let moduleOrNamespace =
        genModuleOrNamespaceKind moduleRange namespaceRange moduleKind
        +> opt sepSpace ao genAccess
        +> ifElse isRecursive (!- "rec ") sepNone
        +> genLongIdent lids

    // Anonymous module do have a single (fixed) ident in the LongIdent
    // We don't print the ident but it could have trivia assigned to it.
    let genTriviaForAnonModuleIdent =
        match lids with
        | [ ident ] ->
            genTriviaFor Ident_ ident.idRange sepNone
            |> genTriviaFor LongIdent_ ident.idRange
        | _ -> sepNone


    genPreXmlDoc px
    +> genAttributes astContext ats
    +> ifElse (moduleKind = SynModuleOrNamespaceKind.AnonModule) genTriviaForAnonModuleIdent moduleOrNamespace
    +> sepModuleAndFirstDecl
    +> genModuleDeclList astContext mds
    |> (match moduleKind with
        | SynModuleOrNamespaceKind.AnonModule -> id
        | SynModuleOrNamespaceKind.DeclaredNamespace -> genTriviaFor SynModuleOrNamespace_DeclaredNamespace range
        | SynModuleOrNamespaceKind.GlobalNamespace -> genTriviaFor SynModuleOrNamespace_GlobalNamespace range
        | SynModuleOrNamespaceKind.NamedModule -> genTriviaFor SynModuleOrNamespace_NamedModule range)

and genSigModuleOrNamespace
    astContext
    (SigModuleOrNamespace (ats, px, moduleRange, namespaceRange, ao, lids, mds, isRecursive, moduleKind, range))
    =
    let sepModuleAndFirstDecl =
        let firstDecl = List.tryHead mds

        match firstDecl with
        | None -> sepNone
        | Some mdl ->
            match mdl with
            | SynModuleSigDecl.Types _ ->
                sepNlnConsideringTriviaContentBeforeForMainNode SynModuleSigDecl_Types mdl.Range
            | SynModuleSigDecl.Val _ -> sepNlnConsideringTriviaContentBeforeForMainNode SynValSig_ mdl.Range
            | _ -> sepNone
            +> sepNln

    let moduleOrNamespace =
        genModuleOrNamespaceKind moduleRange namespaceRange moduleKind
        +> opt sepSpace ao genAccess
        +> ifElse isRecursive (!- "rec ") sepNone
        +> genLongIdent lids

    genPreXmlDoc px
    +> genAttributes astContext ats
    +> ifElse (moduleKind = SynModuleOrNamespaceKind.AnonModule) sepNone moduleOrNamespace
    +> sepModuleAndFirstDecl
    +> genSigModuleDeclList astContext mds
    |> (match moduleKind with
        | SynModuleOrNamespaceKind.DeclaredNamespace -> genTriviaFor SynModuleOrNamespaceSig_DeclaredNamespace range
        | SynModuleOrNamespaceKind.GlobalNamespace -> genTriviaFor SynModuleOrNamespaceSig_GlobalNamespace range
        | SynModuleOrNamespaceKind.NamedModule -> genTriviaFor SynModuleOrNamespaceSig_NamedModule range
        | _ -> id)

and genModuleDeclList astContext e =
    let rec collectItems
        (e: SynModuleDecl list)
        (finalContinuation: ColMultilineItem list -> ColMultilineItem list)
        : ColMultilineItem list =
        match e with
        | [] -> finalContinuation []
        | OpenL (xs, ys) ->
            let expr = col sepNln xs (genModuleDecl astContext)

            let r = List.head xs |> fun mdl -> mdl.Range
            // SynModuleDecl.Open cannot have attributes
            let sepNln = sepNlnConsideringTriviaContentBeforeForMainNode SynModuleDecl_Open r

            collectItems ys (fun ysItems ->
                ColMultilineItem(expr, sepNln) :: ysItems
                |> finalContinuation)

        | HashDirectiveL (xs, ys) ->
            let expr = col sepNln xs (genModuleDecl astContext)

            let r = List.head xs |> fun mdl -> mdl.Range
            // SynModuleDecl.HashDirective cannot have attributes
            let sepNln =
                sepNlnConsideringTriviaContentBeforeForMainNode SynModuleDecl_HashDirective r

            collectItems ys (fun ysItems ->
                ColMultilineItem(expr, sepNln) :: ysItems
                |> finalContinuation)

        | AttributesL (xs, y :: rest) ->
            let expr =
                col sepNln xs (genModuleDecl astContext)
                +> sepNlnConsideringTriviaContentBeforeForMainNode (synModuleDeclToFsAstType y) y.Range
                +> genModuleDecl astContext y

            let r = List.head xs |> fun mdl -> mdl.Range

            let sepNln =
                sepNlnConsideringTriviaContentBeforeForMainNode SynModuleDecl_Attributes r

            collectItems rest (fun restItems ->
                ColMultilineItem(expr, sepNln) :: restItems
                |> finalContinuation)

        | m :: rest ->
            let sepNln =
                sepNlnConsideringTriviaContentBeforeForMainNode (synModuleDeclToFsAstType m) m.Range

            let expr = genModuleDecl astContext m

            collectItems rest (fun restItems ->
                ColMultilineItem(expr, sepNln) :: restItems
                |> finalContinuation)

    collectItems e id |> colWithNlnWhenItemIsMultiline

and genSigModuleDeclList astContext (e: SynModuleSigDecl list) =
    let rec collectItems
        (e: SynModuleSigDecl list)
        (finalContinuation: ColMultilineItem list -> ColMultilineItem list)
        : ColMultilineItem list =
        match e with
        | [] -> finalContinuation []
        | SigOpenL (xs, ys) ->
            let expr = col sepNln xs (genSigModuleDecl astContext)

            let r = List.head xs |> fun mdl -> mdl.Range
            // SynModuleSigDecl.Open cannot have attributes
            let sepNln = sepNlnConsideringTriviaContentBeforeForMainNode SynModuleSigDecl_Open r

            collectItems ys (fun ysItems ->
                ColMultilineItem(expr, sepNln) :: ysItems
                |> finalContinuation)
        | s :: rest ->
            let sepNln =
                sepNlnConsideringTriviaContentBeforeForMainNode (synModuleSigDeclToFsAstType s) s.Range

            let expr = genSigModuleDecl astContext s

            collectItems rest (fun restItems ->
                ColMultilineItem(expr, sepNln) :: restItems
                |> finalContinuation)

    collectItems e id |> colWithNlnWhenItemIsMultiline

and genModuleDecl astContext (node: SynModuleDecl) =
    match node with
    | Attributes ats ->
        fun ctx ->
            let attributesExpr =
                // attributes can have trivia content before or after
                // we do extra detection to ensure no additional newline is introduced
                // first attribute should not have a newline anyway
                List.fold
                    (fun (prevContentAfterPresent, prevExpr) (a: SynAttributeList) ->
                        let expr =
                            ifElse
                                prevContentAfterPresent
                                sepNone
                                (sepNlnConsideringTriviaContentBeforeForMainNode SynModuleDecl_Attributes a.Range)
                            +> ((col sepNln a.Attributes (genAttribute astContext))
                                |> genTriviaFor SynAttributeList_ a.Range)

                        let hasContentAfter =
                            TriviaHelpers.``has content after after that matches``
                                (fun tn -> RangeHelpers.rangeEq tn.Range a.Range)
                                (function
                                | Newline
                                | Comment (LineCommentOnSingleLine _)
                                | Directive _ -> true
                                | _ -> false)
                                (Map.tryFindOrEmptyList SynAttributeList_ ctx.TriviaNodes)

                        (hasContentAfter, prevExpr +> expr))
                    (true, sepNone)
                    ats
                |> snd

            attributesExpr ctx
    | DeclExpr e -> genExprKeepIndentInBranch astContext e
    | Exception ex -> genException astContext ex
    | HashDirective p -> genParsedHashDirective p
    | Extern (ats, px, ao, t, sli, ps) ->
        genPreXmlDoc px +> genAttributes astContext ats
        -- "extern "
        +> genType { astContext with IsCStylePattern = true } false t
        +> sepSpace
        +> opt sepSpace ao genAccess
        +> genSynLongIdent false sli
        +> sepOpenT
        +> col sepComma ps (genPat { astContext with IsCStylePattern = true })
        +> sepCloseT
    // Add a new line after module-level let bindings
    | Let b -> genLetBinding astContext "let " b
    | LetRec (b :: bs) ->
        let sepBAndBs =
            match List.tryHead bs with
            | Some b' ->
                let r = b'.RangeOfBindingWithRhs

                sepNln
                +> sepNlnConsideringTriviaContentBeforeForMainNode (synBindingToFsAstType b) r
            | None -> id

        genLetBinding astContext "let rec " b
        +> sepBAndBs
        +> colEx
            (fun (b': SynBinding) ->
                let r = b'.RangeOfBindingWithRhs

                sepNln
                +> sepNlnConsideringTriviaContentBeforeForMainNode (synBindingToFsAstType b) r)
            bs
            (fun andBinding ->
                enterNodeFor (synBindingToFsAstType b) andBinding.RangeOfBindingWithRhs
                +> genLetBinding astContext "and " andBinding)

    | ModuleAbbrev (ident, lid) ->
        !- "module "
        +> genIdent ident
        +> sepEq
        +> sepSpace
        +> genLongIdent lid
    | NamespaceFragment m -> failwithf "NamespaceFragment hasn't been implemented yet: %O" m
    | NestedModule (ats, px, moduleKeyword, ao, lid, isRecursive, equalsRange, mds) ->
        genPreXmlDoc px
        +> genAttributes astContext ats
        +> genTriviaForOption SynModuleDecl_NestedModule_Module moduleKeyword (!- "module ")
        +> opt sepSpace ao genAccess
        +> ifElse isRecursive (!- "rec ") sepNone
        +> genLongIdent lid
        +> genEq SynModuleDecl_NestedModule_Equals equalsRange
        +> indent
        +> sepNln
        +> genModuleDeclList astContext mds
        +> unindent

    | Open lid -> !- "open " +> genLongIdent lid
    | OpenType lid -> !- "open type " +> genSynLongIdent false lid
    // There is no nested types and they are recursive if there are more than one definition
    | Types (t :: ts) ->
        let items =
            ColMultilineItem(genTypeDefn astContext true t, sepNone)
            :: (List.map
                    (fun t ->
                        ColMultilineItem(
                            genTypeDefn astContext false t,
                            sepNlnConsideringTriviaContentBeforeForMainNode SynTypeDefn_ t.Range
                        ))
                    ts)

        colWithNlnWhenItemIsMultilineUsingConfig items
    | md -> failwithf "Unexpected module declaration: %O" md
    |> genTriviaFor (synModuleDeclToFsAstType node) node.Range

and genSigModuleDecl astContext node =
    match node with
    | SigException ex -> genSigException astContext ex
    | SigHashDirective p -> genParsedHashDirective p
    | SigVal v -> genVal astContext v
    | SigModuleAbbrev (ident, lid) ->
        !- "module "
        +> genIdent ident
        +> sepEq
        +> sepSpace
        +> genLongIdent lid
    | SigNamespaceFragment m -> failwithf "NamespaceFragment is not supported yet: %O" m
    | SigNestedModule (ats, px, moduleKeyword, ao, lid, equalsRange, mds) ->
        genPreXmlDoc px
        +> genAttributes astContext ats
        +> genTriviaForOption SynModuleSigDecl_NestedModule_Module moduleKeyword !- "module "
        +> opt sepSpace ao genAccess
        +> genLongIdent lid
        +> genEq SynModuleSigDecl_NestedModule_Equals equalsRange
        +> indent
        +> sepNln
        +> genSigModuleDeclList astContext mds
        +> unindent

    | SigOpen lid -> !- "open " +> genLongIdent lid
    | SigOpenType sli -> !- "open type " +> genSynLongIdent false sli
    | SigTypes (t :: ts) ->
        let items =
            ColMultilineItem(genSigTypeDefn astContext true t, sepNone)
            :: (List.map
                    (fun (t: SynTypeDefnSig) ->
                        let sepNln = sepNlnConsideringTriviaContentBeforeForMainNode SynTypeDefnSig_ t.Range

                        ColMultilineItem(genSigTypeDefn astContext false t, sepNln))
                    ts)

        colWithNlnWhenItemIsMultilineUsingConfig items
    | md -> failwithf "Unexpected module signature declaration: %O" md
    |> (match node with
        | SynModuleSigDecl.Types _ -> genTriviaFor SynModuleSigDecl_Types node.Range
        | SynModuleSigDecl.NestedModule _ -> genTriviaFor SynModuleSigDecl_NestedModule node.Range
        | SynModuleSigDecl.Open (SynOpenDeclTarget.ModuleOrNamespace _, _) ->
            genTriviaFor SynModuleSigDecl_Open node.Range
        | SynModuleSigDecl.Open (SynOpenDeclTarget.Type _, _) -> genTriviaFor SynModuleSigDecl_OpenType node.Range
        | SynModuleSigDecl.Exception _ -> genTriviaFor SynModuleSigDecl_Exception node.Range
        | _ -> id)

and genIdent (ident: Ident) =
    let width =
        ident.idRange.EndColumn
        - ident.idRange.StartColumn

    let genIdent =
        if ident.idText.Length + 4 = width then
            // add backticks
            !- $"``{ident.idText}``"
        else
            !-ident.idText

    genTriviaFor Ident_ ident.idRange genIdent

and genLongIdent (lid: LongIdent) =
    col sepDot lid genIdent
    |> genTriviaFor LongIdent_ (longIdentFullRange lid)

and genSynIdent (addDot: bool) (synIdent: SynIdent) =
    let (SynIdent (ident, trivia)) = synIdent

    match trivia with
    | Some (IdentTrivia.OriginalNotation text) -> !-text
    | Some (IdentTrivia.OriginalNotationWithParen (_, text, _)) -> !- $"({text})"
    | Some (IdentTrivia.HasParenthesis _) -> !- $"({ident.idText})"
    | None -> genIdent ident

    |> fun genSy -> genTriviaFor SynIdent_ synIdent.FullRange (onlyIf addDot sepDot +> genSy)

and genSynLongIdent (addLeadingDot: bool) (longIdent: SynLongIdent) =
    let lastIndex = longIdent.IdentsWithTrivia.Length - 1

    coli sepNone longIdent.IdentsWithTrivia (fun idx si ->
        genSynIdent (addLeadingDot || idx > 0) si
        +> onlyIf (idx < lastIndex) (sepNlnWhenWriteBeforeNewlineNotEmpty sepNone))
    |> genTriviaFor SynLongIdent_ longIdent.FullRange

and genSynLongIdentMultiline (addLeadingDot: bool) (longIdent: SynLongIdent) =
    coli sepNln longIdent.IdentsWithTrivia (fun idx -> genSynIdent (idx > 0 || addLeadingDot))
    |> genTriviaFor SynLongIdent_ longIdent.FullRange

and genAccess (Access s) = !-s

and genAttribute astContext (Attribute (sli, e, target)) =
    match e with
    // Special treatment for function application on attributes
    | ConstUnitExpr ->
        !- "[<"
        +> opt sepColon target genIdent
        +> genSynLongIdent false sli
        +> !- ">]"
    | e ->
        let argSpacing =
            if hasParenthesis e then
                id
            else
                sepSpace

        !- "[<"
        +> opt sepColon target genIdent
        +> genSynLongIdent false sli
        +> argSpacing
        +> genExpr astContext e
        -- ">]"

and genAttributesCore astContext (ats: SynAttribute seq) =
    let genAttributeExpr astContext (Attribute (sli, e, target) as attr) =
        match e with
        | ConstUnitExpr ->
            opt sepColon target genIdent
            +> genSynLongIdent false sli
        | e ->
            let argSpacing =
                if hasParenthesis e then
                    id
                else
                    sepSpace

            opt sepColon target genIdent
            +> genSynLongIdent false sli
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
    let ats = List.collect (fun (a: SynAttributeList) -> a.Attributes) ats

    ifElse (Seq.isEmpty ats) sepNone (genAttributesCore astContext ats +> sepSpace)

/// Try to group attributes if they are on the same line
/// Separate same-line attributes by ';'
/// Each bucket is printed in a different line
and genAttributes astContext (ats: SynAttributes) =
    ats
    |> List.fold
        (fun acc a (ctx: Context) ->
            let dontAddNewline =
                TriviaHelpers.``has content after that ends with``
                    (fun t -> RangeHelpers.rangeEq t.Range a.Range)
                    (function
                    | Directive _
                    | Newline
                    | Comment (LineCommentOnSingleLine _) -> true
                    | _ -> false)
                    (Map.tryFindOrEmptyList SynAttributeList_ ctx.TriviaNodes)

            let chain =
                acc
                +> (genAttributesCore astContext a.Attributes
                    |> genTriviaFor SynAttributeList_ a.Range)
                +> ifElse dontAddNewline sepNone sepNln

            chain ctx)
        sepNone

and genPreXmlDoc (PreXmlDoc (lines, _)) =
    colPost sepNln sepNln lines (sprintf "///%s" >> (!-))

and genExprSepEqPrependType
    (astContext: ASTContext)
    (equalsAstType: FsAstType)
    (equalsRange: range option)
    (e: SynExpr)
    =
    match e with
    | TypedExpr (Typed, e, t) ->
        sepColon
        +> genType astContext false t
        +> genEq equalsAstType equalsRange
        +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (genExpr astContext e)
    | _ ->
        genEq equalsAstType equalsRange
        +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (genExpr astContext e)

and genTyparList astContext tps =
    colSurr sepOpenT sepCloseT wordOr tps (genTypar astContext)

and genTypeSupportMember astContext st =
    match st with
    | SynType.Var (td, _) -> genTypar astContext td
    | TLongIdent sli -> genSynLongIdent false sli
    | _ -> !- ""

and genTypeSupportMemberList astContext tps =
    colSurr sepOpenT sepCloseT wordOr tps (genTypeSupportMember astContext)

and genTypeAndParam astContext (typeName: Context -> Context) (tds: SynTyparDecls option) tcs =
    let types openSep tds tcs closeSep =
        (!-openSep
         +> coli sepComma tds (fun i -> genTyparDecl { astContext with IsFirstTypeParam = i = 0 })
         +> colPre (!- " when ") wordAnd tcs (genTypeConstraint astContext)
         -- closeSep)

    match tds with
    | None -> typeName
    | Some (SynTyparDecls.PostfixList (tds, tcs, _range)) -> typeName +> types "<" tds tcs ">"
    | Some (SynTyparDecls.PrefixList (tds, _range)) -> types "(" tds [] ")" -- " " +> typeName
    | Some (SynTyparDecls.SinglePrefix (td, _range)) ->
        genTyparDecl { astContext with IsFirstTypeParam = true } td
        +> sepSpace
        +> typeName
    +> colPre (!- " when ") wordAnd tcs (genTypeConstraint astContext)

and genTypeParamPostfix astContext tds =
    match tds with
    | Some (SynTyparDecls.PostfixList (tds, tcs, _range)) ->
        (!- "<"
         +> coli sepComma tds (fun i -> genTyparDecl { astContext with IsFirstTypeParam = i = 0 })
         +> colPre (!- " when ") wordAnd tcs (genTypeConstraint astContext)
         -- ">")
    | _ -> sepNone

and genLetBinding astContext pref b =
    let genPref letKeyword =
        genTriviaForOption SynBinding_Let letKeyword !-pref

    let isRecursiveLetOrUseFunction = (pref = "and ")

    match b with
    | LetBinding (ats, px, letKeyword, ao, isInline, isMutable, p, equalsRange, e, valInfo) ->
        match e, p with
        | TypedExpr (Typed, e, t), PatLongIdent (ao, sli, _, ps, tpso) when (List.isNotEmpty ps) ->
            genSynBindingFunctionWithReturnType
                astContext
                false
                isRecursiveLetOrUseFunction
                px
                ats
                (genPref letKeyword)
                ao
                isInline
                isMutable
                sli
                p.Range
                ps
                tpso
                t
                valInfo
                equalsRange
                e
        | e, PatLongIdent (ao, sli, _, ps, tpso) when (List.isNotEmpty ps) ->
            genSynBindingFunction
                astContext
                false
                isRecursiveLetOrUseFunction
                px
                ats
                (genPref letKeyword)
                ao
                isInline
                isMutable
                sli
                p.Range
                ps
                tpso
                equalsRange
                e
        | TypedExpr (Typed, e, t), pat ->
            genSynBindingValue
                astContext
                isRecursiveLetOrUseFunction
                px
                ats
                (genPref letKeyword)
                ao
                isInline
                isMutable
                pat
                (Some t)
                equalsRange
                e
        | _, PatTuple _ ->
            genLetBindingDestructedTuple
                astContext
                isRecursiveLetOrUseFunction
                px
                ats
                pref
                ao
                isInline
                isMutable
                p
                equalsRange
                e
        | _, pat ->
            genSynBindingValue
                astContext
                isRecursiveLetOrUseFunction
                px
                ats
                (genPref letKeyword)
                ao
                isInline
                isMutable
                pat
                None
                equalsRange
                e
        | _ -> sepNone
    | DoBinding (ats, px, e) ->
        let prefix =
            if pref.Contains("let") then
                pref.Replace("let", "do")
            else
                "do "

        genPreXmlDoc px +> genAttributes astContext ats
        -- prefix
        +> autoIndentAndNlnIfExpressionExceedsPageWidth (genExpr astContext e)

    | b -> failwithf "%O isn't a let binding" b
    +> leaveNodeFor (synBindingToFsAstType b) b.RangeOfBindingWithRhs

and genProperty
    astContext
    (prefix: Context -> Context)
    ao
    propertyKind
    ps
    (equalsAstType: FsAstType)
    (equalsRange: range option)
    e
    =
    let tuplerize ps =
        let rec loop acc =
            function
            | [ p ] -> (List.rev acc, p)
            | p1 :: ps -> loop (p1 :: acc) ps
            | [] -> invalidArg "p" "Patterns should not be empty"

        loop [] ps

    match ps with
    | [ PatTuple ps ] ->
        let ps, p = tuplerize ps

        prefix +> opt sepSpace ao genAccess
        -- propertyKind
        +> ifElse
            (List.atMostOne ps)
            (col sepComma ps (genPat astContext) +> sepSpace)
            (sepOpenT
             +> col sepComma ps (genPat astContext)
             +> sepCloseT
             +> sepSpace)
        +> genPat astContext p
        +> genExprSepEqPrependType astContext equalsAstType equalsRange e

    | ps ->
        prefix +> opt sepSpace ao genAccess
        -- propertyKind
        +> col sepSpace ps (genPat astContext)
        +> genExprSepEqPrependType astContext equalsAstType equalsRange e

and genPropertyWithGetSet astContext (b1, b2) =
    match b1, b2 with
    | PropertyBinding (ats, px, ao, isInline, mf1, PatLongIdent (ao1, s1, pk1, ps1, _), eqR1, e1, _),
      PropertyBinding (_, _, _, _, _, PatLongIdent (ao2, _, pk2, ps2, _), eqR2, e2, _) ->
        let prefix =
            genPreXmlDoc px
            +> genAttributes astContext ats
            +> genMemberFlags mf1
            +> ifElse isInline (!- "inline ") sepNone
            +> opt sepSpace ao genAccess

        assert (ps1 |> Seq.map fst |> Seq.forall Option.isNone)
        assert (ps2 |> Seq.map fst |> Seq.forall Option.isNone)
        let ps1 = List.map snd ps1
        let ps2 = List.map snd ps2

        let genGet okw ikw =
            genProperty astContext (genPropertyKeyword (okw, ikw)) ao1 "get " ps1 SynBinding_Equals eqR1 e1

        let genSet okw ikw =
            genProperty astContext (genPropertyKeyword (okw, ikw)) ao2 "set " ps2 SynBinding_Equals eqR2 e2

        let w = "with"
        let a = "and"

        let genGetSet =
            // regardless of get/set ordering, the second member needs to be rendered as keyword "and", not keyword "with".
            // therefore, the genGet and genSet helper functions have to take the desired keyword as a parameter.
            match pk2 with
            | Some (PropertyKeyword.With _) -> genSet w pk1 +> sepNln +> genGet a pk2
            | _ -> genGet w pk1 +> sepNln +> genSet a pk2

        prefix
        +> genSynLongIdent false s1
        +> indent
        +> sepNln
        +> genGetSet
        +> unindent
    | _ -> sepNone

/// <summary>Generate the keyword <code>and</code> or <code>with</code>, along with any matching syntax trivia, for a given keyword</summary>
/// <param name="outputKeyword">the keyword that the user wants for the property after writing.</param>
/// <param name="inputKeyword">the parsed keyword range for the property from the AST. this is used to lookup trivia based on its range, since this range can differ from the output keyword's range.</param>
/// <param name="ctx">the writing context context, not used inside this function</param>
/// <remarks>The output keyword and input keyword can be different in the case of a property where the getter and setter are defined separately.
/// Fantomas will combine the definitions, each of which are defined as <code>member blah with get</code>, <code>member blah with get</code>,
/// into a combined getter and setter on a single member. This means that one of the <code>with</code> must be rewritten as an <code>and</code>,
/// but we need to preserve the trivia.</remarks>
/// <returns>A function that will transform and rewrite the member property keywords.</returns>
and genPropertyKeyword (outputKeyword: string, inputKeyword: PropertyKeyword option) (ctx: Context) =
    let start = outputKeyword + " "

    match inputKeyword with
    | None -> ctx
    | Some (PropertyKeyword.And r) -> (!-start |> genTriviaFor SynPat_LongIdent_And r) ctx
    | Some (PropertyKeyword.With r) -> (!-start |> genTriviaFor SynPat_LongIdent_With r) ctx

and genMemberBindingList astContext node =
    let rec collectItems
        (node: SynBinding list)
        (finalContinuation: ColMultilineItem list -> ColMultilineItem list)
        : ColMultilineItem list =
        match node with
        | [] -> finalContinuation []
        | mb :: rest ->
            let expr = genMemberBinding astContext mb
            let r = mb.RangeOfBindingWithRhs

            let sepNln =
                sepNlnConsideringTriviaContentBeforeForMainNode (synBindingToFsAstType mb) r

            collectItems rest (fun restItems ->
                ColMultilineItem(expr, sepNln) :: restItems
                |> finalContinuation)

    collectItems node id
    |> colWithNlnWhenItemIsMultiline

and genMemberBinding astContext b =
    match b with
    | PropertyBinding (ats, px, ao, isInline, mf, p, equalsRange, e, synValInfo) ->
        let prefix =
            genPreXmlDoc px
            +> genAttributes astContext ats
            +> genMemberFlags mf
            +> ifElse isInline (!- "inline ") sepNone
            +> opt sepSpace ao genAccess

        let propertyKind =
            match mf with
            | MFProperty PropertyGet -> "get "
            | MFProperty PropertySet -> "set "
            | mf -> failwithf "Unexpected member flags: %O" mf

        match p with
        | PatLongIdent (ao, sli, propertyKeyword, ps, _) ->
            assert (ps |> Seq.map fst |> Seq.forall Option.isNone)

            match ao, propertyKind, ps with
            | None, "get ", [ _, PatParen (_, PatUnitConst, _) ] ->
                // Provide short-hand notation `x.Member = ...` for `x.Member with get()` getters
                let pat =
                    match p with
                    | SynPat.LongIdent (lid, propertyKeyword, extraId, typarDecls, _, accessibility, range) ->
                        SynPat.LongIdent(
                            lid,
                            propertyKeyword,
                            extraId,
                            typarDecls,
                            SynArgPats.Pats([]),
                            accessibility,
                            range
                        )
                    | _ -> p

                let prefix =
                    (onlyIfNot mf.IsInstance (!- "static ")
                     +> !- "member ")

                genMemberBindingImpl astContext prefix ats px ao isInline pat equalsRange e synValInfo
            | _ ->
                let ps = List.map snd ps

                let genPropertyKeyword ctx =
                    match propertyKeyword with
                    | None -> ctx
                    | Some (PropertyKeyword.And r) ->
                        // even if the keyword was `and` in the original source, due to transformations we always want to use `with` here.
                        (!- "with " |> genTriviaFor SynPat_LongIdent_And r) ctx
                    | Some (PropertyKeyword.With r) -> (!- "with " |> genTriviaFor SynPat_LongIdent_With r) ctx

                prefix
                +> genSynLongIdent false sli
                +> indent
                +> sepNln
                +> genProperty astContext genPropertyKeyword ao propertyKind ps SynBinding_Equals equalsRange e
                +> unindent
        | p -> failwithf "Unexpected pattern: %O" p

    | MemberBinding (ats, px, ao, isInline, mf, p, equalsRange, e, synValInfo) ->
        let prefix = genMemberFlags mf

        genMemberBindingImpl astContext prefix ats px ao isInline p equalsRange e synValInfo

    | ExplicitCtor (ats, px, ao, p, equalsRange, e, io) ->
        let prefix =
            let genPat ctx =
                match p with
                | PatExplicitCtor (ao, pat) ->
                    (opt sepSpace ao genAccess
                     +> !- "new"
                     +> sepSpaceBeforeClassConstructor
                     +> genPat astContext pat)
                        ctx
                | _ -> genPat astContext p ctx

            genPreXmlDoc px
            +> genAttributes astContext ats
            +> opt sepSpace ao genAccess
            +> genPat
            +> optSingle (fun ident -> !- " as " +> genIdent ident) io

        match e with
        // Handle special "then" block i.e. fake sequential expressions in constructors
        | Sequential (e1, e2, false) ->
            prefix
            +> genEq SynBinding_Equals equalsRange
            +> indent
            +> sepNln
            +> genExpr astContext e1
            ++ "then "
            +> autoIndentAndNlnIfExpressionExceedsPageWidth (genExpr astContext e2)
            +> unindent

        | e ->
            prefix
            +> genEq SynBinding_Equals equalsRange
            +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (genExpr astContext e)

    | b -> failwithf "%O isn't a member binding" b
    |> genTriviaFor (synBindingToFsAstType b) b.RangeOfBindingWithRhs

and genMemberBindingImpl
    (astContext: ASTContext)
    (prefix: Context -> Context)
    (ats: SynAttributes)
    (px: PreXmlDoc)
    (ao: SynAccess option)
    (isInline: bool)
    (p: SynPat)
    (equalsRange: range option)
    (e: SynExpr)
    (synValInfo: SynValInfo)
    =
    match e, p with
    | TypedExpr (Typed, e, t), PatLongIdent (ao, s, _, ps, tpso) when (List.isNotEmpty ps) ->
        genSynBindingFunctionWithReturnType
            astContext
            true
            false
            px
            ats
            prefix
            ao
            isInline
            false
            s
            p.Range
            ps
            tpso
            t
            synValInfo
            equalsRange
            e
    | e, PatLongIdent (ao, s, _, ps, tpso) when (List.isNotEmpty ps) ->
        genSynBindingFunction astContext true false px ats prefix ao isInline false s p.Range ps tpso equalsRange e
    | TypedExpr (Typed, e, t), pat ->
        genSynBindingValue astContext false px ats prefix ao isInline false pat (Some t) equalsRange e
    | _, pat -> genSynBindingValue astContext false px ats prefix ao isInline false pat None equalsRange e

and genMemberFlags (mf: SynMemberFlags) =
    match mf.Trivia with
    | { StaticRange = Some s
        MemberRange = Some _m } ->
        genTriviaFor SynValData_Static s !- "static"
        +> sepSpace
        +> !- "member "
    | { OverrideRange = Some _o } -> !- "override "
    | { DefaultRange = Some _d } -> !- "default "
    | { AbstractRange = Some _a
        MemberRange = Some _m } -> !- "abstract member "
    | { MemberRange = Some m } -> genTriviaFor SynValData_Member m !- "member "
    | { AbstractRange = Some _a } -> !- "abstract "
    | _ -> sepNone

and genVal astContext (Val (ats, px, valKeyword, ao, si, t, vi, isInline, isMutable, tds, eo, range)) =
    let typeName = genTypeAndParam astContext (genSynIdent false si) tds []

    let (FunType namedArgs) = (t, vi)
    let hasGenerics = Option.isSome tds

    genPreXmlDoc px
    +> genAttributes astContext ats
    +> (genTriviaForOption SynValSig_Val valKeyword !- "val "
        +> onlyIf isInline (!- "inline ")
        +> onlyIf isMutable (!- "mutable ")
        +> opt sepSpace ao genAccess
        +> typeName)
    +> ifElse hasGenerics sepColonWithSpacesFixed sepColon
    +> ifElse
        (List.isNotEmpty namedArgs)
        (autoIndentAndNlnIfExpressionExceedsPageWidth (genTypeList astContext namedArgs))
        (genConstraints astContext t vi)
    +> optSingle (fun e -> sepEq +> sepSpace +> genExpr astContext e) eo
    |> genTriviaFor SynValSig_ range

and genRecordFieldName astContext (SynExprRecordField ((rfn, _), equalsRange, eo, _blockSeparator) as rf) =
    opt sepNone eo (fun e ->
        let expr = sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (genExpr astContext e)

        genSynLongIdent false rfn
        +> genEq SynExprRecordField_Equals equalsRange
        +> expr)
    |> genTriviaFor SynExprRecordField_ rf.FullRange

and genAnonRecordFieldName astContext (AnonRecordFieldName (ident, equalsRange, e)) =
    let expr = sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (genExpr astContext e)

    genIdent ident
    +> genEq SynExpr_AnonRecd_Field_Equals equalsRange
    +> expr

and genTuple astContext es =
    let genShortExpr astContext e =
        addParenForTupleWhen (genExpr astContext) e

    let shortExpression = col sepComma es (genShortExpr astContext)

    let longExpression =
        let containsLambdaOrMatchExpr =
            es
            |> List.pairwise
            |> List.exists (function
                | SynExpr.Match _, _
                | SynExpr.Lambda _, _
                | InfixApp (_, _, _, SynExpr.Lambda _, _), _ -> true
                | _ -> false)

        let sep =
            if containsLambdaOrMatchExpr then
                (sepNln +> sepComma)
            else
                (sepComma +> sepNln)

        let lastIndex = List.length es - 1

        let genExpr astContext idx e =
            match e with
            | SynExpr.IfThenElse _ when (idx < lastIndex) ->
                autoParenthesisIfExpressionExceedsPageWidth (genExpr astContext e)
            | _ -> genExpr astContext e

        coli sep es (genExpr astContext)

    atCurrentColumn (expressionFitsOnRestOfLine shortExpression longExpression)

and genNamedArgumentExpr (astContext: ASTContext) (operatorSli: SynLongIdent) e1 e2 appRange =
    let short =
        genExpr astContext e1
        +> sepSpace
        +> genSynLongIdent false operatorSli
        +> sepSpace
        +> genExpr astContext e2

    let long =
        genExpr astContext e1
        +> sepSpace
        +> genSynLongIdent false operatorSli
        +> autoIndentAndNlnExpressUnlessRagnarok (fun e -> sepSpace +> genExpr astContext e) e2

    expressionFitsOnRestOfLine short long
    |> genTriviaFor SynExpr_App appRange

and genExpr astContext synExpr ctx =
    let expr =
        match synExpr with
        | EndsWithSingleListAppExpr (e, es, a) when ctx.Config.Ragnarok ->
            // check if everything else beside the last array/list fits on one line
            let singleLineTestExpr =
                genExpr astContext e
                +> sepSpace
                +> col sepSpace es (genExpr astContext)

            let short =
                genExpr astContext e
                +> sepSpace
                +> col sepSpace es (genExpr astContext)
                +> onlyIfNot es.IsEmpty sepSpace
                +> genExpr astContext a

            let long =
                genExpr astContext e
                +> indent
                +> sepNln
                +> col sepNln es (genExpr astContext)
                +> onlyIfNot es.IsEmpty sepNln
                +> genExpr astContext a
                +> unindent

            if futureNlnCheck singleLineTestExpr ctx then
                long
            else
                short

        | EndsWithDualListAppExpr (e, es, props, children) ->
            // check if everything else beside the last array/list fits on one line
            let singleLineTestExpr =
                genExpr astContext e
                +> sepSpace
                +> col sepSpace es (genExpr astContext)
                +> sepSpace
                +> genExpr astContext props

            let short =
                genExpr astContext e
                +> sepSpace
                +> col sepSpace es (genExpr astContext)
                +> onlyIfNot es.IsEmpty sepSpace
                +> genExpr astContext props
                +> sepSpace
                +> genExpr astContext children

            let long =
                // check if everything besides both lists fits on one line
                let singleLineTestExpr =
                    genExpr astContext e
                    +> sepSpace
                    +> col sepSpace es (genExpr astContext)

                if futureNlnCheck singleLineTestExpr ctx then
                    genExpr astContext e
                    +> indent
                    +> sepNln
                    +> col sepNln es (genExpr astContext)
                    +> sepSpace
                    +> genExpr astContext props
                    +> sepSpace
                    +> genExpr astContext children
                    +> unindent
                else
                    genExpr astContext e
                    +> sepSpace
                    +> col sepSpace es (genExpr astContext)
                    +> genExpr astContext props
                    +> sepSpace
                    +> genExpr astContext children

            if futureNlnCheck singleLineTestExpr ctx then
                long
            else
                short
        | LazyExpr (lazyKeyword, e) ->
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
                autoIndentAndNlnIfExpressionExceedsPageWidth (genExpr astContext e)

            genTriviaFor SynExpr_Lazy_Lazy lazyKeyword !- "lazy "
            +> ifElse isInfixExpr genInfixExpr genNonInfixExpr

        | SingleExpr (kind, e) ->
            let mapping =
                (match kind with
                 | YieldFrom _
                 | Yield _
                 | Return _
                 | ReturnFrom _
                 | Do _
                 | DoBang _ -> autoIndentAndNlnIfExpressionExceedsPageWidthUnlessRagnarok (genExpr astContext) e
                 | _ -> autoIndentAndNlnIfExpressionExceedsPageWidth (genExpr astContext e))

            match kind with
            | InferredDowncast downcastKeyword ->
                genTriviaFor SynExpr_InferredDowncast_Downcast downcastKeyword !- "downcast "
            | InferredUpcast upcastKeyword -> genTriviaFor SynExpr_InferredUpcast_Upcast upcastKeyword !- "upcast "
            | Assert assertKeyword -> genTriviaFor SynExpr_Assert_Assert assertKeyword !- "assert "
            | AddressOfSingle ampersandToken -> genTriviaFor SynExpr_AddressOf_SingleAmpersand ampersandToken !- "&"
            | AddressOfDouble ampersandToken -> genTriviaFor SynExpr_AddressOf_DoubleAmpersand ampersandToken !- "&&"
            | Yield yieldKeyword -> genTriviaFor SynExpr_YieldOrReturn_Yield yieldKeyword !- "yield "
            | Return returnKeyword -> genTriviaFor SynExpr_YieldOrReturn_Return returnKeyword !- "return "
            | YieldFrom yieldBangKeyword ->
                genTriviaFor SynExpr_YieldOrReturnFrom_YieldBang yieldBangKeyword !- "yield! "
            | ReturnFrom returnBangKeyword ->
                genTriviaFor SynExpr_YieldOrReturnFrom_ReturnBang returnBangKeyword !- "return! "
            | Do doKeyword -> genTriviaFor SynExpr_Do_Do doKeyword !- "do "
            | DoBang doBangKeyword -> genTriviaFor SynExpr_DoBang_DoBang doBangKeyword !- "do! "
            | Fixed fixedKeyword -> genTriviaFor SynExpr_Fixed_Fixed fixedKeyword !- "fixed "
            +> mapping

        | ConstExpr (c, r) -> genConst c r
        | NullExpr -> !- "null"
        // Not sure about the role of e1
        | Quote (_, e2, isRaw) ->
            let e =
                expressionFitsOnRestOfLine
                    (genExpr astContext e2)
                    (indent
                     +> sepNln
                     +> genExpr astContext e2
                     +> unindent
                     +> sepNln)

            ifElse
                isRaw
                (!- "<@@" +> sepSpace +> e +> sepSpace +> !- "@@>")
                (!- "<@" +> sepSpace +> e +> sepSpace +> !- "@>")
        | TypedExpr (TypeTest, e, t) ->
            genExpr astContext e -- " :? "
            +> genType astContext false t
        | TypedExpr (Downcast, e, t) ->
            let shortExpr =
                genExpr astContext e -- " :?> "
                +> genType astContext false t

            let longExpr =
                genExpr astContext e +> sepNln -- ":?> "
                +> genType astContext false t

            expressionFitsOnRestOfLine shortExpr longExpr
        | TypedExpr (Upcast, e, t) ->
            let shortExpr =
                genExpr astContext e -- " :> "
                +> genType astContext false t

            let longExpr =
                genExpr astContext e +> sepNln -- ":> "
                +> genType astContext false t

            expressionFitsOnRestOfLine shortExpr longExpr
        | TypedExpr (Typed, e, t) ->
            genExpr astContext e
            +> sepColon
            +> genType astContext false t
        | NewTuple (t, px) ->
            let sepSpace (ctx: Context) =
                match t with
                | UppercaseSynType -> onlyIf ctx.Config.SpaceBeforeUppercaseInvocation sepSpace ctx
                | LowercaseSynType -> onlyIf ctx.Config.SpaceBeforeLowercaseInvocation sepSpace ctx

            let short =
                !- "new "
                +> genType astContext false t
                +> sepSpace
                +> genExpr astContext px

            let long =
                !- "new "
                +> genType astContext false t
                +> sepSpace
                +> genMultilineFunctionApplicationArguments astContext px

            expressionFitsOnRestOfLine short long
        | SynExpr.New (_, t, e, _) ->
            !- "new "
            +> genType astContext false t
            +> sepSpace
            +> genExpr astContext e
        | Tuple (es, _) -> genTuple astContext es
        | StructTuple es ->
            !- "struct "
            +> sepOpenT
            +> genTuple astContext es
            +> sepCloseT
        | ArrayOrList (sr, isArray, [], er, _) ->
            ifElse
                isArray
                (genTriviaFor SynExpr_ArrayOrList_OpeningDelimiter sr sepOpenAFixed
                 +> genTriviaFor SynExpr_ArrayOrList_ClosingDelimiter er sepCloseAFixed)
                (genTriviaFor SynExpr_ArrayOrList_OpeningDelimiter sr sepOpenLFixed
                 +> genTriviaFor SynExpr_ArrayOrList_ClosingDelimiter er sepCloseLFixed)
        | ArrayOrList (openingTokenRange, isArray, xs, closingTokenRange, _) ->
            let smallExpression =
                ifElse
                    isArray
                    (genTriviaFor SynExpr_ArrayOrList_OpeningDelimiter openingTokenRange sepOpenA)
                    (genTriviaFor SynExpr_ArrayOrList_OpeningDelimiter openingTokenRange sepOpenL)
                +> col sepSemi xs (genExpr astContext)
                +> ifElse
                    isArray
                    (genTriviaFor SynExpr_ArrayOrList_ClosingDelimiter closingTokenRange sepCloseA)
                    (genTriviaFor SynExpr_ArrayOrList_ClosingDelimiter closingTokenRange sepCloseL)

            let multilineExpression =
                ifAlignBrackets
                    (genMultiLineArrayOrListAlignBrackets isArray xs openingTokenRange closingTokenRange astContext)
                    (genMultiLineArrayOrList isArray xs openingTokenRange closingTokenRange astContext)

            fun ctx ->
                if List.exists isIfThenElseWithYieldReturn xs
                   || List.forall isSynExprLambdaOrIfThenElse xs then
                    multilineExpression ctx
                else
                    let size = getListOrArrayExprSize ctx ctx.Config.MaxArrayOrListWidth xs

                    isSmallExpression size smallExpression multilineExpression ctx

        | Record (openingBrace, inheritOpt, xs, eo, closingBrace) ->
            let smallRecordExpr =
                genTriviaFor SynExpr_Record_OpeningBrace openingBrace sepOpenS
                +> optSingle
                    (fun (inheritType, inheritExpr) ->
                        !- "inherit "
                        +> genType astContext false inheritType
                        +> addSpaceBeforeClassConstructor inheritExpr
                        +> genExpr astContext inheritExpr
                        +> onlyIf (List.isNotEmpty xs) sepSemi)
                    inheritOpt
                +> optSingle (fun e -> genExpr astContext e +> !- " with ") eo
                +> col sepSemi xs (genRecordFieldName astContext)
                +> genTriviaFor SynExpr_Record_ClosingBrace closingBrace sepCloseS

            let multilineRecordExpr =
                ifAlignBrackets
                    (genMultilineRecordInstanceAlignBrackets astContext openingBrace inheritOpt xs eo closingBrace)
                    (genMultilineRecordInstance astContext openingBrace inheritOpt xs eo closingBrace)

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

        | ObjExpr (t, eio, withKeyword, bd, members, ims, range) ->
            if List.isEmpty bd && List.isEmpty members then
                // Check the role of the second part of eio
                let param = opt sepNone (Option.map fst eio) (genExpr astContext)

                // See https://devblogs.microsoft.com/dotnet/announcing-f-5/#default-interface-member-consumption
                sepOpenS
                +> !- "new "
                +> genType astContext false t
                +> param
                +> sepCloseS
            else
                ifAlignBrackets
                    (genObjExprAlignBrackets t eio withKeyword bd members ims range astContext)
                    (genObjExpr t eio withKeyword bd members ims range astContext)

        | While (e1, e2) ->
            atCurrentColumn (
                !- "while " +> genExpr astContext e1 -- " do"
                +> indent
                +> sepNln
                +> genExpr astContext e2
                +> unindent
            )

        | For (ident, equalsRange, e1, e2, e3, isUp) ->
            atCurrentColumn (
                !- "for "
                +> genIdent ident
                +> genEq SynExpr_For_Equals equalsRange
                +> sepSpace
                +> genExpr astContext e1
                +> ifElse isUp (!- " to ") (!- " downto ")
                +> genExpr astContext e2
                -- " do"
                +> indent
                +> sepNln
                +> genExpr astContext e3
                +> unindent
            )

        // Handle the form 'for i in e1 -> e2'
        | ForEach (p, e1, e2, isArrow) ->
            atCurrentColumn (
                !- "for " +> genPat astContext p -- " in "
                +> autoIndentAndNlnIfExpressionExceedsPageWidth (genExpr { astContext with IsNakedRange = true } e1)
                +> ifElse
                    isArrow
                    (sepArrow
                     +> autoIndentAndNlnIfExpressionExceedsPageWidth (genExpr astContext e2))
                    (!- " do"
                     +> indent
                     +> sepNln
                     +> genExpr astContext e2
                     +> unindent)
            )

        | NamedComputationExpr (nameExpr, openingBrace, bodyExpr, closingBrace) ->
            fun ctx ->
                let short =
                    genExpr astContext nameExpr
                    +> sepSpace
                    +> genTriviaFor SynExpr_ComputationExpr_OpeningBrace openingBrace sepOpenS
                    +> genExpr astContext bodyExpr
                    +> genTriviaFor SynExpr_ComputationExpr_ClosingBrace closingBrace sepCloseS

                let long =
                    genExpr astContext nameExpr
                    +> sepSpace
                    +> genTriviaFor SynExpr_ComputationExpr_OpeningBrace openingBrace sepOpenS
                    +> indent
                    +> sepNln
                    +> genExpr astContext bodyExpr
                    +> unindent
                    +> sepNln
                    +> genTriviaFor SynExpr_ComputationExpr_ClosingBrace closingBrace sepCloseSFixed

                expressionFitsOnRestOfLine short long ctx
        | ComputationExpr (openingBrace, e, closingBrace) ->
            expressionFitsOnRestOfLine
                (genTriviaFor SynExpr_ComputationExpr_OpeningBrace openingBrace sepOpenS
                 +> genExpr astContext e
                 +> genTriviaFor SynExpr_ComputationExpr_ClosingBrace closingBrace sepCloseS)
                (genTriviaFor SynExpr_ComputationExpr_OpeningBrace openingBrace sepOpenS
                 +> genExpr astContext e
                 +> unindent
                 +> genTriviaFor
                     SynExpr_ComputationExpr_ClosingBrace
                     closingBrace
                     (sepNlnUnlessLastEventIsNewline +> sepCloseSFixed))

        | CompExprBody statements ->
            let genCompExprStatement astContext ces =
                match ces with
                | LetOrUseStatement (prefix, binding, inKeyword) ->
                    enterNodeFor (synBindingToFsAstType binding) binding.RangeOfBindingWithRhs
                    +> genLetBinding astContext prefix binding
                    +> genTriviaForOption SynExpr_LetOrUse_In inKeyword !- " in "
                | LetOrUseBangStatement (isUse, pat, equalsRange, expr, r) ->
                    enterNodeFor SynExpr_LetOrUseBang r // print Trivia before entire LetBang expression
                    +> ifElse isUse (!- "use! ") (!- "let! ")
                    +> genPat astContext pat
                    +> genEq SynExpr_LetOrUseBang_Equals equalsRange
                    +> sepSpace
                    +> autoIndentAndNlnIfExpressionExceedsPageWidthUnlessRagnarok (genExpr astContext) expr
                | AndBangStatement (pat, equalsRange, expr, range) ->
                    !- "and! "
                    +> genPat astContext pat
                    +> genEq SynExprAndBang_Equals (Some equalsRange)
                    +> sepSpace
                    +> autoIndentAndNlnIfExpressionExceedsPageWidthUnlessRagnarok (genExpr astContext) expr
                    |> genTriviaFor SynExprAndBang_ range
                | OtherStatement expr -> genExpr astContext expr

            let getRangeOfCompExprStatement ces =
                match ces with
                | LetOrUseStatement (_, binding, _) -> binding.RangeOfBindingWithRhs
                | LetOrUseBangStatement (range = r) -> r
                | AndBangStatement (range = r) -> r
                | OtherStatement expr -> expr.Range

            let getSepNln ces r =
                match ces with
                | LetOrUseStatement (_, b, _) ->
                    sepNlnConsideringTriviaContentBeforeForMainNode (synBindingToFsAstType b) r
                | LetOrUseBangStatement _ -> sepNlnConsideringTriviaContentBeforeForMainNode SynExpr_LetOrUseBang r
                | AndBangStatement (_, _, _, r) -> sepNlnConsideringTriviaContentBeforeForMainNode SynExprAndBang_ r
                | OtherStatement e ->
                    let t, r = synExprToFsAstType e
                    sepNlnConsideringTriviaContentBeforeForMainNode t r

            statements
            |> List.map (fun ces ->
                let expr = genCompExprStatement astContext ces
                let r = getRangeOfCompExprStatement ces
                let sepNln = getSepNln ces r
                ColMultilineItem(expr, sepNln))
            |> colWithNlnWhenItemIsMultilineUsingConfig

        | JoinIn (e1, e2) ->
            genExpr astContext e1 -- " in "
            +> genExpr astContext e2
        | Paren (lpr, Lambda (pats, arrowRange, expr, lambdaRange), rpr, pr) ->
            fun (ctx: Context) ->
                let body = genExprKeepIndentInBranch astContext

                let expr =
                    let triviaOfLambda f (ctx: Context) =
                        (Map.tryFindOrEmptyList SynExpr_Lambda ctx.TriviaNodes
                         |> List.tryFind (fun tn -> RangeHelpers.rangeEq tn.Range lambdaRange)
                         |> optSingle f)
                            ctx

                    sepOpenTFor lpr
                    +> triviaOfLambda printContentBefore
                    -- "fun "
                    +> col sepSpace pats (genPat astContext)
                    +> (fun ctx ->
                        if not ctx.Config.MultiLineLambdaClosingNewline then
                            genLambdaArrowWithTrivia
                                (fun e ->
                                    body e
                                    +> triviaOfLambda printContentAfter
                                    +> sepNlnWhenWriteBeforeNewlineNotEmpty id
                                    +> sepCloseTFor rpr)
                                expr
                                arrowRange
                                ctx
                        else
                            leadingExpressionIsMultiline
                                (genLambdaArrowWithTrivia
                                    (fun e ->
                                        body e
                                        +> triviaOfLambda printContentAfter
                                        +> sepNlnWhenWriteBeforeNewlineNotEmpty id)
                                    expr
                                    arrowRange)
                                (fun isMultiline -> onlyIf isMultiline sepNln +> sepCloseTFor rpr)
                                ctx)

                expr ctx

        // When there are parentheses, most likely lambda will appear in function application
        | Lambda (pats, arrowRange, expr, _range) ->
            atCurrentColumn (
                !- "fun "
                +> col sepSpace pats (genPat astContext)
                +> optSingle
                    (fun arrowRange ->
                        sepArrow
                        |> genTriviaFor SynExpr_Lambda_Arrow arrowRange)
                    arrowRange
                +> autoIndentAndNlnIfExpressionExceedsPageWidthUnlessRagnarok (genExpr astContext) expr
            )
        | MatchLambda (keywordRange, cs) ->
            (!- "function "
             |> genTriviaFor SynExpr_MatchLambda_Function keywordRange)
            +> sepNln
            +> genClauses astContext cs
        | Match (matchRange, e, withRange, cs) ->
            let genMatchExpr =
                genTriviaFor SynExpr_Match_Match matchRange !- "match "
                +> autoIndentAndNlnWhenWriteBeforeNewlineNotEmpty (
                    expressionFitsOnRestOfLine
                        (genExpr astContext e
                         +> genWithAfterMatch SynExpr_Match_With withRange)
                        (genExprInIfOrMatch astContext e
                         +> (sepNlnUnlessLastEventIsNewline
                             +> (genWithAfterMatch SynExpr_Match_With withRange)))
                )

            atCurrentColumn (genMatchExpr +> sepNln +> genClauses astContext cs)
        | MatchBang (matchRange, e, withRange, cs) ->
            let genMatchExpr =
                genTriviaFor SynExpr_MatchBang_Match matchRange !- "match! "
                +> autoIndentAndNlnWhenWriteBeforeNewlineNotEmpty (
                    expressionFitsOnRestOfLine
                        (genExpr astContext e
                         +> genWithAfterMatch SynExpr_MatchBang_With withRange)
                        (genExprInIfOrMatch astContext e
                         +> (sepNlnUnlessLastEventIsNewline
                             +> (genWithAfterMatch SynExpr_MatchBang_With withRange)))
                )

            atCurrentColumn (genMatchExpr +> sepNln +> genClauses astContext cs)
        | TraitCall (tps, msg, e) ->
            genTyparList astContext tps
            +> sepColon
            +> sepOpenT
            +> genMemberSig astContext msg
            +> sepCloseT
            +> sepSpace
            +> genExpr astContext e

        | Paren (_, ILEmbedded r, rpr, _) ->
            fun ctx ->
                let expr =
                    match ctx.FromSourceText r with
                    | None -> sepNone
                    | Some eil -> !-eil

                (expr
                 +> optSingle (leaveNodeFor SynExpr_Paren_ClosingParenthesis) rpr)
                    ctx
        | ParenFunctionNameWithStar (lpr, originalNotation, rpr) ->
            sepOpenTFor lpr
            +> !- $" {originalNotation} "
            +> sepCloseTFor rpr
        | Paren (lpr, e, rpr, _pr) ->
            match e with
            | LetOrUses _
            | Sequential _ ->
                sepOpenTFor lpr
                +> atCurrentColumn (genExpr astContext e)
                +> sepCloseTFor rpr
            | _ ->
                sepOpenTFor lpr
                +> genExpr astContext e
                +> sepCloseTFor rpr

        // This supposes to be an infix function, but for some reason it isn't picked up by InfixApps
        // TODO: improve when https://github.com/dotnet/fsharp/issues/12755 is implemented.
        | App (OptVar (_, SynLongIdent ([ _ ], [], [ Some (IdentTrivia.OriginalNotation "?") ]), _), e :: es) ->
            match es with
            | SynExpr.Const (SynConst.String (text = text), _) :: rest ->
                let genText =
                    !-(if text = "checked" then
                           "``checked``"
                       else
                           text)

                genExpr astContext e -- "?"
                +> genText
                +> onlyIfNot rest.IsEmpty sepSpace
                +> col sepSpace rest (genExpr astContext)
            | _ ->
                genExpr astContext e -- "?"
                +> sepOpenT
                +> col sepSpace es (genExpr astContext)
                +> sepCloseT

        // Separate two prefix ops by spaces
        | PrefixApp (s1, PrefixApp (s2, e)) -> !-(sprintf "%s %s" s1 s2) +> genExpr astContext e
        | PrefixApp (s, App (e, [ Paren _ as p ]))
        | PrefixApp (s, App (e, [ ConstExpr (SynConst.Unit _, _) as p ])) ->
            !-s
            +> sepSpace
            +> genExpr astContext e
            +> genExpr astContext p
        | PrefixApp (s, e) ->
            let extraSpaceBeforeString =
                match e with
                | SynExpr.Const _
                | SynExpr.InterpolatedString _ -> sepSpace
                | _ -> sepNone

            !-s
            +> extraSpaceBeforeString
            +> genExpr astContext e

        | NewlineInfixApp (operatorText, operatorExpr, (Lambda _ as e1), e2)
        | NewlineInfixApp (operatorText, operatorExpr, (IfThenElse _ as e1), e2) ->
            genMultilineInfixExpr astContext e1 operatorText operatorExpr e2

        | NewlineInfixApps (e, es) ->
            let shortExpr =
                genExpr astContext e
                +> sepSpace
                +> col sepSpace es (fun (s, oe, e) ->
                    genSynLongIdent false oe
                    +> sepSpace
                    +> onlyIf (isSynExprLambdaOrIfThenElse e) sepOpenT
                    +> genExpr astContext e
                    +> onlyIf (isSynExprLambdaOrIfThenElse e) sepCloseT)

            let multilineExpr =
                match es with
                | [] -> genExpr astContext e
                | (s, oe, e2) :: es ->
                    genMultilineInfixExpr astContext e s oe e2
                    +> sepNln
                    +> col sepNln es (fun (s, oe, e) ->
                        genSynLongIdent false oe
                        +> sepSpace
                        +> genExprInMultilineInfixExpr astContext e)

            fun ctx ->
                atCurrentColumn (isShortExpression ctx.Config.MaxInfixOperatorExpression shortExpr multilineExpr) ctx

        | SameInfixApps (e, es) ->
            let shortExpr =
                genExpr astContext e
                +> sepSpace
                +> col sepSpace es (fun (s, oe, e) ->
                    genSynLongIdent false oe
                    +> sepSpace
                    +> genExpr astContext e)

            let multilineExpr =
                genExpr astContext e
                +> sepNln
                +> col sepNln es (fun (s, oe, e) ->
                    genSynLongIdent false oe
                    +> sepSpace
                    +> genExprInMultilineInfixExpr astContext e)

            fun ctx ->
                atCurrentColumn (isShortExpression ctx.Config.MaxInfixOperatorExpression shortExpr multilineExpr) ctx

        | InfixApp (operatorText, operatorSli, e1, e2, _) ->
            fun ctx ->
                isShortExpression
                    ctx.Config.MaxInfixOperatorExpression
                    (genOnelinerInfixExpr astContext e1 operatorSli e2)
                    (ifElse
                        (noBreakInfixOps.Contains(operatorText))
                        (genOnelinerInfixExpr astContext e1 operatorSli e2)
                        (genMultilineInfixExpr astContext e1 operatorText operatorSli e2))
                    ctx

        | TernaryApp (e1, e2, e3) ->
            atCurrentColumn (
                genExpr astContext e1
                +> !- "?"
                +> genExpr astContext e2
                +> sepSpace
                +> !- "<-"
                +> sepSpace
                +> genExpr astContext e3
            )

        | IndexWithoutDotExpr (identifierExpr, indexExpr) ->
            genExpr astContext identifierExpr
            +> sepOpenLFixed
            +> genExpr astContext indexExpr
            +> sepCloseLFixed

        // Result<int, string>.Ok 42
        | App (DotGet (TypeApp (e, lt, ts, gt), sli), es) ->
            genExpr astContext e
            +> genGenericTypeParameters astContext lt ts gt
            +> genSynLongIdent true sli
            +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (col sepSpace es (genExpr astContext))

        // Foo(fun x -> x).Bar().Meh
        | DotGetAppDotGetAppParenLambda (e, px, appLids, es, lids) ->
            let short =
                genExpr astContext e
                +> genExpr astContext px
                +> genSynLongIdent true appLids
                +> col sepComma es (genExpr astContext)
                +> genSynLongIdent true lids

            let long =
                let functionName =
                    match e with
                    | LongIdentExprWithMoreThanOneIdent lids -> genFunctionNameWithMultilineLids id lids
                    | TypeApp (LongIdentExprWithMoreThanOneIdent lids, lt, ts, gt) ->
                        genFunctionNameWithMultilineLids (genGenericTypeParameters astContext lt ts gt) lids
                    | _ -> genExpr astContext e

                functionName
                +> indent
                +> genExpr astContext px
                +> sepNln
                +> genSynLongIdentMultiline true appLids
                +> col sepComma es (genExpr astContext)
                +> sepNln
                +> genSynLongIdentMultiline true lids
                +> unindent

            fun ctx -> isShortExpression ctx.Config.MaxDotGetExpressionWidth short long ctx

        // Foo().Bar
        | DotGetAppParen (e, px, lids) ->
            let shortAppExpr = genExpr astContext e +> genExpr astContext px

            let longAppExpr =
                let functionName argFn =
                    match e with
                    | LongIdentExprWithMoreThanOneIdent lids -> genFunctionNameWithMultilineLids argFn lids
                    | TypeApp (LongIdentExprWithMoreThanOneIdent lids, lt, ts, gt) ->
                        genFunctionNameWithMultilineLids
                            (genGenericTypeParameters astContext lt ts gt
                             +> argFn)
                            lids
                    | DotGetAppDotGetAppParenLambda _ ->
                        leadingExpressionIsMultiline (genExpr astContext e) (fun isMultiline ->
                            if isMultiline then
                                indent +> argFn +> unindent
                            else
                                argFn)
                    | _ -> genExpr astContext e +> argFn

                let arguments = genMultilineFunctionApplicationArguments astContext px

                functionName arguments

            let shortDotGetExpr = genSynLongIdent true lids

            let longDotGetExpr =
                indent
                +> sepNln
                +> genSynLongIdentMultiline true lids
                +> unindent

            fun ctx ->
                isShortExpression
                    ctx.Config.MaxDotGetExpressionWidth
                    (shortAppExpr +> shortDotGetExpr)
                    (longAppExpr +> longDotGetExpr)
                    ctx

        // Foo(fun x -> x).Bar()
        | DotGetApp (App (e, [ Paren (_, Lambda _, _, _) as px ]), es) ->
            let genLongFunctionName f =
                match e with
                | LongIdentExprWithMoreThanOneIdent lids -> genFunctionNameWithMultilineLids f lids
                | TypeApp (LongIdentExprWithMoreThanOneIdent lids, lt, ts, gt) ->
                    genFunctionNameWithMultilineLids (genGenericTypeParameters astContext lt ts gt +> f) lids
                | _ -> genExpr astContext e +> f

            let lastEsIndex = es.Length - 1

            let genApp (idx: int) (lids, e, t) : Context -> Context =
                let short =
                    genSynLongIdent true lids
                    +> optSingle (fun (lt, ts, gt) -> genGenericTypeParameters astContext lt ts gt) t
                    +> genSpaceBeforeLids idx lastEsIndex lids e
                    +> genExpr astContext e

                let long =
                    genSynLongIdentMultiline true lids
                    +> optSingle (fun (lt, ts, gt) -> genGenericTypeParameters astContext lt ts gt) t
                    +> genSpaceBeforeLids idx lastEsIndex lids e
                    +> genMultilineFunctionApplicationArguments astContext e

                expressionFitsOnRestOfLine short long

            let short =
                genExpr astContext e
                +> genExpr astContext px
                +> coli sepNone es (fun idx (lids, e, t) ->
                    genSynLongIdent true lids
                    +> optSingle (fun (lt, ts, gt) -> genGenericTypeParameters astContext lt ts gt) t
                    +> genSpaceBeforeLids idx lastEsIndex lids e
                    +> genExpr astContext e)

            let long =
                genLongFunctionName (genExpr astContext px)
                +> indent
                +> sepNln
                +> coli sepNln es genApp
                +> unindent

            fun ctx -> isShortExpression ctx.Config.MaxDotGetExpressionWidth short long ctx

        // Foo().Bar().Meh()
        | DotGetApp (e, es) ->
            let genLongFunctionName =
                match e with
                | AppOrTypeApp (LongIdentExprWithMoreThanOneIdent lids, t, [ Paren _ as px ]) ->
                    genFunctionNameWithMultilineLids
                        (optSingle (fun (lt, ts, gt) -> genGenericTypeParameters astContext lt ts gt) t
                         +> expressionFitsOnRestOfLine
                             (genExpr astContext px)
                             (genMultilineFunctionApplicationArguments astContext px))
                        lids
                | AppOrTypeApp (LongIdentExprWithMoreThanOneIdent lids, t, [ e2 ]) ->
                    genFunctionNameWithMultilineLids
                        (optSingle (fun (lt, ts, gt) -> genGenericTypeParameters astContext lt ts gt) t
                         +> genExpr astContext e2)
                        lids
                | AppOrTypeApp (SimpleExpr e, t, [ ConstExpr (SynConst.Unit, r) ]) ->
                    genExpr astContext e
                    +> optSingle (fun (lt, ts, gt) -> genGenericTypeParameters astContext lt ts gt) t
                    +> genConst SynConst.Unit r
                | AppOrTypeApp (SimpleExpr e, t, [ Paren _ as px ]) ->
                    let short =
                        genExpr astContext e
                        +> optSingle (fun (lt, ts, gt) -> genGenericTypeParameters astContext lt ts gt) t
                        +> genExpr astContext px

                    let long =
                        genExpr astContext e
                        +> optSingle (fun (lt, ts, gt) -> genGenericTypeParameters astContext lt ts gt) t
                        +> genMultilineFunctionApplicationArguments astContext px

                    expressionFitsOnRestOfLine short long
                | _ -> genExpr astContext e

            let lastEsIndex = es.Length - 1

            let genApp (idx: int) (lids, e, t) : Context -> Context =
                let short =
                    genSynLongIdent true lids
                    +> optSingle (fun (lt, ts, gt) -> genGenericTypeParameters astContext lt ts gt) t
                    +> genSpaceBeforeLids idx lastEsIndex lids e
                    +> genExpr astContext e

                let long =
                    genSynLongIdentMultiline true lids
                    +> optSingle (fun (lt, ts, gt) -> genGenericTypeParameters astContext lt ts gt) t
                    +> genSpaceBeforeLids idx lastEsIndex lids e
                    +> genMultilineFunctionApplicationArguments astContext e

                expressionFitsOnRestOfLine short long

            let short =
                match e with
                | App (e, [ px ]) when (hasParenthesis px || isArrayOrList px) ->
                    genExpr astContext e +> genExpr astContext px
                | _ -> genExpr astContext e
                +> coli sepNone es genApp

            let long =
                genLongFunctionName
                +> indent
                +> sepNln
                +> coli sepNln es genApp
                +> unindent

            fun ctx -> isShortExpression ctx.Config.MaxDotGetExpressionWidth short long ctx

        // (*) (60. * 1.1515 * 1.609344)
        // function is wrapped in parenthesis
        | AppParenArg (Choice1Of2 (Paren _, _, _, _, _, _) as app)
        | AppParenArg (Choice2Of2 (Paren _, _, _, _, _) as app) ->
            let short = genAppWithParenthesis app astContext

            let long = genAlternativeAppWithParenthesis app astContext

            expressionFitsOnRestOfLine short long

        // path.Replace("../../../", "....")
        | AppSingleParenArg (LongIdentExpr lids as functionOrMethod, px) ->
            let addSpace =
                onlyIfCtx (addSpaceBeforeParensInFunCall functionOrMethod px) sepSpace

            let shortLids = genSynLongIdent false lids

            let short = shortLids +> addSpace +> genExpr astContext px

            let long =
                let args =
                    addSpace
                    +> expressionFitsOnRestOfLine
                        (genExpr astContext px)
                        (genMultilineFunctionApplicationArguments astContext px)

                ifElseCtx (futureNlnCheck shortLids) (genFunctionNameWithMultilineLids args lids) (shortLids +> args)

            expressionFitsOnRestOfLine short long

        | AppSingleParenArg (e, px) ->
            let sepSpace (ctx: Context) =
                match e with
                | Paren _ -> sepSpace ctx
                | UppercaseSynExpr -> onlyIf ctx.Config.SpaceBeforeUppercaseInvocation sepSpace ctx
                | LowercaseSynExpr -> onlyIf ctx.Config.SpaceBeforeLowercaseInvocation sepSpace ctx

            let short =
                genExpr astContext e
                +> sepSpace
                +> genExpr astContext px

            let long =
                genExpr astContext e
                +> sepSpace
                +> genMultilineFunctionApplicationArguments astContext px

            expressionFitsOnRestOfLine short long

        // functionName arg1 arg2 (fun x y z -> ...)
        | AppWithLambda (e, es, lpr, lambda, rpr, pr) ->
            let sepSpaceAfterFunctionName =
                let sepSpaceBasedOnSetting e =
                    match e with
                    | Paren _ -> sepSpace
                    | UppercaseSynExpr -> (fun ctx -> onlyIf ctx.Config.SpaceBeforeUppercaseInvocation sepSpace ctx)
                    | LowercaseSynExpr -> (fun ctx -> onlyIf ctx.Config.SpaceBeforeLowercaseInvocation sepSpace ctx)

                match es with
                | [] -> sepSpaceBasedOnSetting e
                | _ -> sepSpace

            let short =
                genExpr astContext e
                +> sepSpaceAfterFunctionName
                +> col sepSpace es (genExpr astContext)
                +> onlyIf (List.isNotEmpty es) sepSpace
                +> (sepOpenTFor lpr
                    +> (match lambda with
                        | Choice1Of2 (pats, arrowRange, body, lambdaRange) ->
                            !- "fun "
                            +> col sepSpace pats (genPat astContext)
                            +> optSingle
                                (fun arrowRange ->
                                    sepArrow
                                    |> genTriviaFor SynExpr_Lambda_Arrow arrowRange)
                                arrowRange
                            +> genExprKeepIndentInBranch astContext body
                            |> genTriviaFor SynExpr_Lambda lambdaRange
                        | Choice2Of2 (keywordRange, cs, range) ->
                            (!- "function "
                             |> genTriviaFor SynExpr_MatchLambda_Function keywordRange)
                            +> indent
                            +> sepNln
                            +> genClauses astContext cs
                            +> unindent
                            |> genTriviaFor SynExpr_MatchLambda range)
                    +> sepNlnWhenWriteBeforeNewlineNotEmpty sepNone
                    +> sepCloseTFor rpr
                    |> genTriviaFor SynExpr_Paren pr)

            let long (ctx: Context) : Context =
                if ctx.Config.MultiLineLambdaClosingNewline then
                    let genArguments =
                        match es with
                        | [] ->
                            match lambda with
                            | Choice1Of2 (pats, arrowRange, bodyExpr, range) ->
                                sepOpenTFor lpr
                                +> (!- "fun "
                                    +> col sepSpace pats (genPat astContext)
                                    +> genLambdaArrowWithTrivia
                                        (genExprKeepIndentInBranch astContext)
                                        bodyExpr
                                        arrowRange
                                    |> genTriviaFor SynExpr_Lambda range)
                                +> sepNln
                                +> sepCloseTFor rpr
                                |> genTriviaFor SynExpr_Paren pr
                            | Choice2Of2 (keywordRange, cs, matchLambdaRange) ->
                                sepOpenTFor lpr
                                +> indent
                                +> sepNln
                                +> ((!- "function "
                                     |> genTriviaFor SynExpr_MatchLambda_Function keywordRange)
                                    +> sepNln
                                    +> genClauses astContext cs
                                    |> genTriviaFor SynExpr_MatchLambda matchLambdaRange)
                                +> unindent
                                +> sepNln
                                +> sepCloseTFor rpr
                                |> genTriviaFor SynExpr_Paren pr

                        | es ->
                            col sepNln es (genExpr astContext)
                            +> sepNln
                            +> (match lambda with
                                | Choice1Of2 (pats, arrowRange, bodyExpr, range) ->
                                    genLambdaMultiLineClosingNewline
                                        astContext
                                        lpr
                                        pats
                                        arrowRange
                                        bodyExpr
                                        range
                                        rpr
                                        pr
                                | Choice2Of2 (keywordRange, cs, matchLambdaRange) ->
                                    (sepOpenTFor lpr
                                     +> ((!- "function "
                                          |> genTriviaFor SynExpr_MatchLambda_Function keywordRange)
                                         +> sepNln
                                         +> genClauses astContext cs
                                         |> genTriviaFor SynExpr_MatchLambda matchLambdaRange)
                                     +> sepNln
                                     +> sepCloseTFor rpr)
                                    |> genTriviaFor SynExpr_Paren pr)
                            +> unindent

                    (genExpr astContext e
                     +> ifElse (List.isEmpty es) sepSpaceAfterFunctionName (indent +> sepNln)
                     +> genArguments)
                        ctx
                else
                    match lambda with
                    | Choice1Of2 (pats, arrowRange, body, lambdaRange) ->
                        let singleLineTestExpr =
                            genExpr astContext e
                            +> sepSpaceAfterFunctionName
                            +> col sepSpace es (genExpr astContext)
                            +> sepSpace
                            +> enterNodeFor SynExpr_Paren pr
                            +> sepOpenTFor lpr
                            +> enterNodeFor SynExpr_Lambda lambdaRange
                            +> !- "fun "
                            +> col sepSpace pats (genPat astContext)
                            +> optSingle
                                (fun arrowRange ->
                                    sepArrow
                                    |> genTriviaFor SynExpr_Lambda_Arrow arrowRange)
                                arrowRange

                        let singleLine =
                            genExpr astContext e
                            +> sepSpaceAfterFunctionName
                            +> col sepSpace es (genExpr astContext)
                            +> sepSpace
                            +> (sepOpenTFor lpr
                                +> (!- "fun "
                                    +> col sepSpace pats (genPat astContext)
                                    +> genLambdaArrowWithTrivia (genExprKeepIndentInBranch astContext) body arrowRange
                                    |> genTriviaFor SynExpr_Lambda lambdaRange)
                                +> sepNlnWhenWriteBeforeNewlineNotEmpty sepNone
                                +> sepCloseTFor rpr
                                |> genTriviaFor SynExpr_Paren pr)

                        let multiLine =
                            genExpr astContext e
                            +> indent
                            +> sepNln
                            +> col sepNln es (genExpr astContext)
                            +> onlyIfNot (List.isEmpty es) sepNln
                            +> (sepOpenTFor lpr
                                +> (!- "fun "
                                    +> col sepSpace pats (genPat astContext)
                                    +> genLambdaArrowWithTrivia (genExprKeepIndentInBranch astContext) body arrowRange
                                    |> genTriviaFor SynExpr_Lambda lambdaRange)
                                +> sepCloseTFor rpr
                                |> genTriviaFor SynExpr_Paren pr)
                            +> unindent

                        if futureNlnCheck singleLineTestExpr ctx then
                            multiLine ctx
                        else
                            singleLine ctx

                    | Choice2Of2 (keywordRange, cs, matchLambdaRange) ->
                        let singleLineTestExpr =
                            genExpr astContext e
                            +> sepSpaceAfterFunctionName
                            +> col sepSpace es (genExpr astContext)
                            +> enterNodeFor SynExpr_Paren pr
                            +> sepOpenTFor lpr
                            +> enterNodeFor SynExpr_MatchLambda matchLambdaRange
                            +> enterNodeFor SynExpr_MatchLambda_Function keywordRange
                            +> !- "function "

                        let singleLine =
                            genExpr astContext e
                            +> sepSpaceAfterFunctionName
                            +> col sepSpace es (genExpr astContext)
                            +> sepSpace
                            +> (sepOpenTFor lpr
                                +> ((!- "function "
                                     |> genTriviaFor SynExpr_MatchLambda_Function keywordRange)
                                    +> indent
                                    +> sepNln
                                    +> genClauses astContext cs
                                    +> unindent
                                    |> genTriviaFor SynExpr_MatchLambda matchLambdaRange)
                                +> sepNlnWhenWriteBeforeNewlineNotEmpty id
                                +> sepCloseTFor rpr)
                            |> genTriviaFor SynExpr_Paren pr

                        let multiLine =
                            genExpr astContext e
                            +> indent
                            +> sepNln
                            +> col sepNln es (genExpr astContext)
                            +> sepNln
                            +> (sepOpenTFor lpr
                                +> atCurrentColumn (
                                    (!- "function "
                                     |> genTriviaFor SynExpr_MatchLambda_Function keywordRange)
                                    +> sepNln
                                    +> genClauses astContext cs
                                    |> genTriviaFor SynExpr_MatchLambda matchLambdaRange
                                )
                                +> sepCloseTFor rpr
                                |> genTriviaFor SynExpr_Paren pr)
                            +> unindent

                        if futureNlnCheck singleLineTestExpr ctx then
                            multiLine ctx
                        else
                            singleLine ctx

            expressionFitsOnRestOfLine short long

        | NestedIndexWithoutDotExpr (identifierExpr, indexExpr, argExpr) ->
            genExpr astContext identifierExpr
            +> sepOpenLFixed
            +> genExpr astContext indexExpr
            +> sepCloseLFixed
            +> genExpr astContext argExpr

        // Always spacing in multiple arguments
        | App (e, es) -> genApp astContext e es
        | TypeApp (e, lt, ts, gt) ->
            genExpr astContext e
            +> genGenericTypeParameters astContext lt ts gt
        | LetOrUses (bs, e) ->
            let items =
                collectMultilineItemForLetOrUses astContext bs (collectMultilineItemForSynExpr astContext e)

            atCurrentColumn (colWithNlnWhenItemIsMultilineUsingConfig items)
        // Could customize a bit if e is single line
        | TryWith (tryKeyword, e, withKeyword, cs) ->
            atCurrentColumn (
                genTriviaFor SynExpr_TryWith_Try tryKeyword !- "try "
                +> indent
                +> sepNln
                +> genExpr astContext e
                +> unindent
                +> sepNln // unless trivia?
                +> genTriviaFor SynExpr_TryWith_With withKeyword (!- "with")
                +> indentOnWith
                +> sepNln
                +> (fun ctx ->
                    let hasMultipleClausesWhereOneHasRagnarok =
                        hasMultipleClausesWhereOneHasRagnarok ctx.Config.Ragnarok cs

                    col sepNln cs (genClause astContext true hasMultipleClausesWhereOneHasRagnarok) ctx)
                +> unindentOnWith
            )

        | TryFinally (tryKeyword, e1, finallyKeyword, e2) ->
            atCurrentColumn (
                genTriviaFor SynExpr_TryFinally_Try tryKeyword !- "try "
                +> indent
                +> sepNln
                +> genExpr astContext e1
                +> unindent
                +> genTriviaFor SynExpr_TryFinally_Finally finallyKeyword !+~ "finally"
                +> indent
                +> sepNln
                +> genExpr astContext e2
                +> unindent
            )

        | Sequentials es ->
            let items = List.collect (collectMultilineItemForSynExpr astContext) es
            atCurrentColumn (colWithNlnWhenItemIsMultilineUsingConfig items)
        // A generalization of IfThenElse
        | ElIf ((_, ifKw, isElif, e1, thenKw, e2) :: es, (elseKw, elseOpt), _) ->
            // https://docs.microsoft.com/en-us/dotnet/fsharp/style-guide/formatting#formatting-if-expressions
            let hasElfis = not (List.isEmpty es)
            let hasElse = Option.isSome elseOpt

            let genIf ifKeywordRange isElif =
                (ifElse isElif (!- "elif ") (!- "if ")
                 |> genTriviaFor
                     (if isElif then
                          SynExpr_IfThenElse_Elif
                      else
                          SynExpr_IfThenElse_If)
                     ifKeywordRange)
                +> sepSpace

            let genThen thenRange =
                !- "then "
                |> genTriviaFor SynExpr_IfThenElse_Then thenRange

            let genElse elseRange =
                !- "else "
                |> genTriviaFor SynExpr_IfThenElse_Else elseRange

            let genElifOneliner (elseKw, ifKw, isElif, e1, thenKw, e2) =
                optSingle genElse elseKw
                +> sepNlnWhenWriteBeforeNewlineNotEmpty sepSpace
                +> genIf ifKw isElif
                +> sepNlnWhenWriteBeforeNewlineNotEmpty sepSpace
                +> genExpr astContext e1
                +> sepNlnWhenWriteBeforeNewlineNotEmpty sepSpace
                +> genThen thenKw
                +> sepNlnWhenWriteBeforeNewlineNotEmpty sepSpace
                +> genExpr astContext e2

            let genElifMultiLine (elseKw, ifKw, isElif, e1, thenKw, e2) =
                optSingle genElse elseKw
                +> sepNlnWhenWriteBeforeNewlineNotEmpty sepSpace
                +> genIf ifKw isElif
                +> autoIndentAndNlnWhenWriteBeforeNewlineNotEmpty (genExprInIfOrMatch astContext e1)
                +> sepNlnWhenWriteBeforeNewlineNotEmpty sepSpace
                +> genThen thenKw
                +> indent
                +> sepNln
                +> genExpr astContext e2
                +> unindent

            let genShortElse e elseRange =
                optSingle
                    (fun e ->
                        sepSpace
                        +> optSingle genElse elseRange
                        +> genExpr astContext e)
                    e

            let genOneliner elseOpt =
                genIf ifKw isElif
                +> genExpr astContext e1
                +> sepNlnWhenWriteBeforeNewlineNotEmpty sepSpace
                +> genThen thenKw
                +> genExpr astContext e2
                +> genShortElse elseOpt elseKw

            let isIfThenElse =
                function
                | SynExpr.IfThenElse _ -> true
                | _ -> false

            let longIfThenElse =
                genIf ifKw isElif
                // f.ex. if // meh
                //           x
                // bool expr x should be indented
                +> autoIndentAndNlnWhenWriteBeforeNewlineNotEmpty (
                    genExprInIfOrMatch astContext e1
                    +> sepNlnWhenWriteBeforeNewlineNotEmpty sepSpace
                )
                +> genThen thenKw
                +> indent
                +> sepNln
                +> genExpr astContext e2
                +> unindent
                +> onlyIf (hasElfis || hasElse) sepNln
                +> col sepNln es genElifMultiLine
                +> opt id elseOpt (fun e4 ->
                    onlyIf (List.isNotEmpty es) sepNln
                    +> optSingle genElse elseKw
                    +> indent
                    +> sepNln
                    +> genExpr astContext e4
                    +> unindent)

            let shortIfThenElif (ctx: Context) =
                // Try and format if each conditional follow the one-liner rules
                // Abort if something is too long
                let shortCtx, isShort =
                    let exprs =
                        [ yield genOneliner None
                          yield! (List.map genElifOneliner es)
                          yield!
                              (Option.map (fun _ -> genShortElse elseOpt elseKw) elseOpt
                               |> Option.toList) ]

                    let lastIndex = List.length exprs - 1

                    exprs
                    |> List.indexed
                    |> List.fold
                        (fun (acc, allLinesShort) (idx, expr) ->
                            if allLinesShort then
                                let lastLine, lastColumn = acc.WriterModel.Lines.Length, acc.Column

                                let nextCtx = expr acc

                                let currentLine, currentColumn = nextCtx.WriterModel.Lines.Length, nextCtx.Column

                                let isStillShort =
                                    lastLine = currentLine
                                    && (currentColumn - lastColumn
                                        <= acc.Config.MaxIfThenElseShortWidth)

                                (ifElse (lastIndex > idx) sepNln sepNone nextCtx, isStillShort)
                            else
                                ctx, false)
                        (ctx, true)

                if isShort then
                    shortCtx
                else
                    longIfThenElse ctx

            let expr =
                if hasElfis && not (isIfThenElse e2) then
                    shortIfThenElif
                elif isIfThenElse e2 then
                    // If branch expression is an if/then/else expressions.
                    // Always go with long version in this case
                    longIfThenElse
                else
                    let shortExpression = genOneliner elseOpt
                    let longExpression = longIfThenElse
                    (fun ctx -> isShortExpression ctx.Config.MaxIfThenElseShortWidth shortExpression longExpression ctx)

            atCurrentColumnIndent expr

        | IdentExpr ident -> genIdent ident

        // At this stage, all symbolic operators have been handled.
        | OptVar (isOpt, sli, _) ->
            ifElse isOpt (!- "?") sepNone
            +> genSynLongIdent false sli
        | LongIdentSet (sli, e, _) ->
            genSynLongIdent false sli
            +> !- " <- "
            +> autoIndentAndNlnIfExpressionExceedsPageWidthUnlessRagnarok (genExpr astContext) e
        | DotIndexedGet (App (e, [ ConstExpr (SynConst.Unit, _) as ux ]), indexArgs) ->
            genExpr astContext e
            +> genExpr astContext ux
            +> !- "."
            +> sepOpenLFixed
            +> genExpr astContext indexArgs
            +> sepCloseLFixed
        | DotIndexedGet (AppSingleParenArg (e, px), indexArgs) ->
            let short = genExpr astContext e +> genExpr astContext px

            let long =
                genExpr astContext e
                +> genMultilineFunctionApplicationArguments astContext px

            let idx =
                !- "."
                +> sepOpenLFixed
                +> genExpr astContext indexArgs
                +> sepCloseLFixed

            expressionFitsOnRestOfLine (short +> idx) (long +> idx)
        | DotIndexedGet (objectExpr, indexArgs) ->
            let isParen =
                match objectExpr with
                | Paren _ -> true
                | _ -> false

            ifElse isParen (genExpr astContext objectExpr) (addParenIfAutoNln objectExpr (genExpr astContext))
            -- "."
            +> sepOpenLFixed
            +> genExpr astContext indexArgs
            +> sepCloseLFixed
        | DotIndexedSet (App (e, [ ConstExpr (SynConst.Unit, _) as ux ]), indexArgs, valueExpr) ->
            let appExpr = genExpr astContext e +> genExpr astContext ux

            let idx =
                !- "."
                +> sepOpenLFixed
                +> genExpr astContext indexArgs
                +> sepCloseLFixed
                +> sepArrowRev

            expressionFitsOnRestOfLine
                (appExpr +> idx +> genExpr astContext valueExpr)
                (appExpr
                 +> idx
                 +> autoIndentAndNlnIfExpressionExceedsPageWidthUnlessRagnarok (genExpr astContext) valueExpr)
        | DotIndexedSet (AppSingleParenArg (a, px), indexArgs, valueExpr) ->
            let short = genExpr astContext a +> genExpr astContext px

            let long =
                genExpr astContext a
                +> genMultilineFunctionApplicationArguments astContext px

            let idx =
                !- "."
                +> sepOpenLFixed
                +> genExpr astContext indexArgs
                +> sepCloseLFixed
                +> sepArrowRev

            expressionFitsOnRestOfLine
                (short +> idx +> genExpr astContext valueExpr)
                (long
                 +> idx
                 +> autoIndentAndNlnIfExpressionExceedsPageWidthUnlessRagnarok (genExpr astContext) valueExpr)

        | DotIndexedSet (objectExpr, indexArgs, valueExpr) ->
            addParenIfAutoNln objectExpr (genExpr astContext)
            -- ".["
            +> genExpr astContext indexArgs
            -- "] <- "
            +> autoIndentAndNlnIfExpressionExceedsPageWidthUnlessRagnarok (genExpr astContext) valueExpr
        | NamedIndexedPropertySet (sli, e1, e2) ->
            genSynLongIdent false sli +> genExpr astContext e1
            -- " <- "
            +> autoIndentAndNlnIfExpressionExceedsPageWidth (genExpr astContext e2)
        | DotNamedIndexedPropertySet (e, sli, e1, e2) ->
            genExpr astContext e
            +> sepDot
            +> genSynLongIdent false sli
            +> genExpr astContext e1
            -- " <- "
            +> autoIndentAndNlnIfExpressionExceedsPageWidth (genExpr astContext e2)

        // typeof<System.Collections.IEnumerable>.FullName
        | DotGet (e, sli) ->
            let shortExpr = genExpr astContext e +> genSynLongIdent true sli

            let longExpr =
                //genLongIdentWithMultipleFragmentsMultiline astContext e
                genExpr astContext e
                +> indent
                +> sepNln
                +> genSynLongIdentMultiline true sli
                +> unindent

            fun ctx -> isShortExpression ctx.Config.MaxDotGetExpressionWidth shortExpr longExpr ctx
        | DotSet (e1, sli, e2) ->
            addParenIfAutoNln e1 (genExpr astContext)
            +> sepDot
            +> genSynLongIdent false sli
            +> !- " <- "
            +> autoIndentAndNlnIfExpressionExceedsPageWidthUnlessRagnarok (genExpr astContext) e2

        | SynExpr.Set (e1, e2, _) ->
            addParenIfAutoNln e1 (genExpr astContext)
            -- sprintf " <- "
            +> autoIndentAndNlnIfExpressionExceedsPageWidthUnlessRagnarok (genExpr astContext) e2

        | ParsingError r ->
            raise
            <| FormatException(
                sprintf
                    "Parsing error(s) between line %i column %i and line %i column %i"
                    r.StartLine
                    (r.StartColumn + 1)
                    r.EndLine
                    (r.EndColumn + 1)
            )

        | LibraryOnlyStaticOptimization (optExpr, constraints, e) ->
            genExpr astContext optExpr
            +> genSynStaticOptimizationConstraint astContext constraints
            +> sepEq
            +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (genExpr astContext e)

        | UnsupportedExpr r ->
            raise
            <| FormatException(
                sprintf
                    "Unsupported construct(s) between line %i column %i and line %i column %i"
                    r.StartLine
                    (r.StartColumn + 1)
                    r.EndLine
                    (r.EndColumn + 1)
            )
        | InterpolatedStringExpr (parts, _stringKind) ->
            let genInterpolatedFillExpr expr =
                fun ctx ->
                    let currentConfig = ctx.Config

                    let interpolatedConfig =
                        { currentConfig with
                            // override the max line length for the interpolated expression.
                            // this is to avoid scenarios where the long / multiline format of the expresion will be used
                            // where the construct is this short
                            // see unit test ``construct url with Fable``
                            MaxLineLength = ctx.WriterModel.Column + ctx.Config.MaxLineLength }

                    genExpr astContext expr { ctx with Config = interpolatedConfig }
                    // Restore the existing configuration after printing the interpolated expression
                    |> fun ctx -> { ctx with Config = currentConfig }
                |> atCurrentColumnIndent

            let withoutSourceText s ctx =
                match ctx.SourceText with
                | Some _ -> ctx
                | None -> !- s ctx

            withoutSourceText "$\""
            +> col sepNone parts (fun part ->
                match part with
                | SynInterpolatedStringPart.String (s, r) ->
                    fun ctx ->
                        let expr =
                            match ctx.FromSourceText r with
                            | None -> !-s
                            | Some s -> !-s

                        genTriviaFor SynInterpolatedStringPart_String r expr ctx
                | SynInterpolatedStringPart.FillExpr (expr, ident) ->
                    fun ctx ->
                        let genFill =
                            genInterpolatedFillExpr expr
                            +> optSingle (fun format -> sepColonFixed +> genIdent format) ident

                        if ctx.Config.StrictMode then
                            (!- "{" +> genFill +> !- "}") ctx
                        else
                            genFill ctx)
            +> withoutSourceText "\""

        | IndexRangeExpr (None, None) -> !- "*"
        | IndexRangeExpr (Some (IndexRangeExpr (Some (ConstNumberExpr e1), Some (ConstNumberExpr e2))),
                          Some (ConstNumberExpr e3)) ->
            let hasOmittedTrailingZero (fromSourceText: range -> string option) r =
                match fromSourceText r with
                | None -> false
                | Some sourceText -> sourceText.EndsWith(".")

            let dots (ctx: Context) =
                if hasOmittedTrailingZero ctx.FromSourceText e1.Range
                   || hasOmittedTrailingZero ctx.FromSourceText e2.Range then
                    !- " .. " ctx
                else
                    !- ".." ctx

            genExpr astContext e1
            +> dots
            +> genExpr astContext e2
            +> dots
            +> genExpr astContext e3
        | IndexRangeExpr (e1, e2) ->
            let hasSpaces =
                let rec (|AtomicExpr|_|) e =
                    match e with
                    | NegativeNumber _ -> None
                    | SynExpr.Ident _
                    | SynExpr.Const (SynConst.Int32 _, _)
                    | IndexRangeExpr (Some (AtomicExpr _), Some (AtomicExpr _))
                    | IndexFromEndExpr (AtomicExpr _) -> Some e
                    | _ -> None

                match e1, e2 with
                | Some (AtomicExpr _), None
                | None, Some (AtomicExpr _)
                | Some (AtomicExpr _), Some (AtomicExpr _) -> false
                | _ -> true

            optSingle (fun e -> genExpr astContext e +> onlyIf hasSpaces sepSpace) e1
            +> !- ".."
            +> optSingle (fun e -> onlyIf hasSpaces sepSpace +> genExpr astContext e) e2
        | IndexFromEndExpr e -> !- "^" +> genExpr astContext e
        | e -> failwithf "Unexpected expression: %O" e
        |> (match synExpr with
            | SynExpr.App _ -> genTriviaFor SynExpr_App synExpr.Range
            | SynExpr.AnonRecd _ -> genTriviaFor SynExpr_AnonRecd synExpr.Range
            | SynExpr.Record _ -> genTriviaFor SynExpr_Record synExpr.Range
            | SynExpr.Ident _ -> genTriviaFor SynExpr_Ident synExpr.Range
            | SynExpr.IfThenElse _ -> genTriviaFor SynExpr_IfThenElse synExpr.Range
            | SynExpr.Lambda _ -> genTriviaFor SynExpr_Lambda synExpr.Range
            | SynExpr.ForEach _ -> genTriviaFor SynExpr_ForEach synExpr.Range
            | SynExpr.For _ -> genTriviaFor SynExpr_For synExpr.Range
            | SynExpr.Match _ -> genTriviaFor SynExpr_Match synExpr.Range
            | SynExpr.MatchBang _ -> genTriviaFor SynExpr_MatchBang synExpr.Range
            | SynExpr.YieldOrReturn _ -> genTriviaFor SynExpr_YieldOrReturn synExpr.Range
            | SynExpr.YieldOrReturnFrom _ -> genTriviaFor SynExpr_YieldOrReturnFrom synExpr.Range
            | SynExpr.TryFinally _ -> genTriviaFor SynExpr_TryFinally synExpr.Range
            | SynExpr.LongIdentSet _ -> genTriviaFor SynExpr_LongIdentSet synExpr.Range
            | SynExpr.ArrayOrList _ -> genTriviaFor SynExpr_ArrayOrList synExpr.Range
            | SynExpr.ArrayOrListComputed _ -> genTriviaFor SynExpr_ArrayOrList synExpr.Range
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
            | SynExpr.Upcast _ -> genTriviaFor SynExpr_Upcast synExpr.Range
            | SynExpr.Downcast _ -> genTriviaFor SynExpr_Downcast synExpr.Range
            | SynExpr.DotIndexedGet _ -> genTriviaFor SynExpr_DotIndexedGet synExpr.Range
            | SynExpr.DotIndexedSet _ -> genTriviaFor SynExpr_DotIndexedSet synExpr.Range
            | SynExpr.ObjExpr _ -> genTriviaFor SynExpr_ObjExpr synExpr.Range
            | SynExpr.JoinIn _ -> genTriviaFor SynExpr_JoinIn synExpr.Range
            | SynExpr.Do _ -> genTriviaFor SynExpr_Do synExpr.Range
            | SynExpr.TypeApp _ -> genTriviaFor SynExpr_TypeApp synExpr.Range
            | SynExpr.Lazy _ -> genTriviaFor SynExpr_Lazy synExpr.Range
            | SynExpr.InferredUpcast _ -> genTriviaFor SynExpr_InferredUpcast synExpr.Range
            | SynExpr.InferredDowncast _ -> genTriviaFor SynExpr_InferredDowncast synExpr.Range
            | SynExpr.AddressOf _ -> genTriviaFor SynExpr_AddressOf synExpr.Range
            | SynExpr.Null _ -> genTriviaFor SynExpr_Null synExpr.Range
            | SynExpr.TraitCall _ -> genTriviaFor SynExpr_TraitCall synExpr.Range
            | SynExpr.DotNamedIndexedPropertySet _ -> genTriviaFor SynExpr_DotNamedIndexedPropertySet synExpr.Range
            | SynExpr.NamedIndexedPropertySet _ -> genTriviaFor SynExpr_NamedIndexedPropertySet synExpr.Range
            | SynExpr.Set _ -> genTriviaFor SynExpr_Set synExpr.Range
            | SynExpr.Quote _ -> genTriviaFor SynExpr_Quote synExpr.Range
            | SynExpr.ArbitraryAfterError _ -> genTriviaFor SynExpr_ArbitraryAfterError synExpr.Range
            | SynExpr.DiscardAfterMissingQualificationAfterDot _ ->
                genTriviaFor SynExpr_DiscardAfterMissingQualificationAfterDot synExpr.Range
            | SynExpr.DotSet _ -> genTriviaFor SynExpr_DotSet synExpr.Range
            | SynExpr.Fixed _ -> genTriviaFor SynExpr_Fixed synExpr.Range
            | SynExpr.FromParseError _ -> genTriviaFor SynExpr_FromParseError synExpr.Range
            | SynExpr.ImplicitZero _ -> genTriviaFor SynExpr_ImplicitZero synExpr.Range
            | SynExpr.LibraryOnlyStaticOptimization _ ->
                genTriviaFor SynExpr_LibraryOnlyStaticOptimization synExpr.Range
            | SynExpr.LibraryOnlyILAssembly _ -> genTriviaFor SynExpr_LibraryOnlyILAssembly synExpr.Range
            | SynExpr.LibraryOnlyUnionCaseFieldGet _ -> genTriviaFor SynExpr_LibraryOnlyUnionCaseFieldGet synExpr.Range
            | SynExpr.LibraryOnlyUnionCaseFieldSet _ -> genTriviaFor SynExpr_LibraryOnlyUnionCaseFieldSet synExpr.Range
            | SynExpr.SequentialOrImplicitYield _ -> genTriviaFor SynExpr_SequentialOrImplicitYield synExpr.Range
            | SynExpr.TypeTest _ -> genTriviaFor SynExpr_TypeTest synExpr.Range
            | SynExpr.IndexRange _ -> genTriviaFor SynExpr_IndexRange synExpr.Range
            | SynExpr.IndexFromEnd _ -> genTriviaFor SynExpr_IndexFromEnd synExpr.Range
            | SynExpr.Const _ ->
                // SynConst has trivia attached to it
                id
            | SynExpr.LetOrUse _
            | SynExpr.Sequential _
            | SynExpr.ComputationExpr _ ->
                // first and last nested node has trivia attached to it
                id
            | SynExpr.LetOrUseBang _ ->
                // printed as part of CompBody
                id
            | SynExpr.Typed _ ->
                // child nodes contain trivia
                id
            | SynExpr.DebugPoint _ ->
                // I don't believe the parser will ever return this node
                id)

    expr ctx

and genOnelinerInfixExpr astContext e1 operatorSli e2 =
    genExpr astContext e1
    +> sepSpace
    +> genSynLongIdent false operatorSli
    +> sepNlnWhenWriteBeforeNewlineNotEmpty sepNone
    +> sepSpace
    +> genExpr astContext e2

and genMultilineInfixExpr astContext e1 operatorText operatorSli e2 =
    let genE1 (ctx: Context) =
        match e1 with
        | SynExpr.IfThenElse _ when (ctx.Config.IndentSize - 1 <= operatorText.Length) ->
            autoParenthesisIfExpressionExceedsPageWidth (genExpr astContext e1) ctx
        | SynExpr.Match _ when (ctx.Config.IndentSize <= operatorText.Length) ->
            let ctxAfterMatch = genExpr astContext e1 ctx

            let lastClauseIsSingleLine =
                Queue.rev ctxAfterMatch.WriterEvents
                |> Seq.skipWhile (fun e ->
                    match e with
                    | RestoreIndent _
                    | RestoreAtColumn _ -> true
                    | _ -> false)
                // In case the last clause was multiline an UnIndent event should follow
                |> Seq.tryHead
                |> fun e ->
                    match e with
                    | Some (UnIndentBy _) -> false
                    | _ -> true

            if lastClauseIsSingleLine then
                ctxAfterMatch
            else
                autoParenthesisIfExpressionExceedsPageWidth (genExpr astContext e1) ctx
        | _ -> genExpr astContext e1 ctx

    atCurrentColumn (
        genE1
        +> sepNln
        +> genSynLongIdent false operatorSli
        +> sepNlnWhenWriteBeforeNewlineNotEmpty sepNone
        +> sepSpace
        +> genExprInMultilineInfixExpr astContext e2
    )

and genExprInMultilineInfixExpr astContext e =
    match e with
    | LetOrUses (xs, e) ->
        atCurrentColumn (
            col sepNln xs (fun (pref, lb, inKeyword) ->
                genLetBinding astContext pref lb
                +> (match inKeyword with
                    | Some inKw -> genTriviaFor SynExpr_LetOrUse_In inKw !- " in"
                    | None -> !- " in"))
            +> sepNln
            +> expressionFitsOnRestOfLine
                (genExpr astContext e)
                (let t, r = synExprToFsAstType e in

                 sepNlnConsideringTriviaContentBeforeForMainNode t r
                 +> genExpr astContext e)
        )
    | Paren (lpr, (Match _ as mex), rpr, pr) ->
        fun ctx ->
            if ctx.Config.MultiLineLambdaClosingNewline then
                (sepOpenTFor lpr
                 +> indent
                 +> sepNln
                 +> genExpr astContext mex
                 +> unindent
                 +> sepNln
                 +> sepCloseTFor rpr
                 |> genTriviaFor SynExpr_Paren pr)
                    ctx
            else
                (sepOpenTFor lpr
                 +> atCurrentColumnIndent (genExpr astContext mex)
                 +> sepCloseTFor rpr
                 |> genTriviaFor SynExpr_Paren pr)
                    ctx
    | Paren (_, InfixApp (_, _, DotGet _, _, _), _, _)
    | Paren (_, DotGetApp _, _, _) -> atCurrentColumnIndent (genExpr astContext e)
    | MatchLambda (keywordRange, cs) ->
        (!- "function "
         |> genTriviaFor SynExpr_MatchLambda_Function keywordRange)
        +> indent
        +> sepNln
        +> genClauses astContext cs
        +> unindent
    | _ -> genExpr astContext e

//and genLidsWithDots (lids: (string * range) list) =
//    col sepNone lids (fun (s, r) -> genTriviaFor Ident_ r !- $".{s}")
//
//and genLidsWithDotsAndNewlines (lids: (string * range) list) =
//    col sepNln lids (fun (s, r) -> genTriviaFor Ident_ r !- $".{s}")

and genSpaceBeforeLids
    (currentIndex: int)
    (lastEsIndex: int)
    (lids: SynLongIdent)
    (arg: SynExpr)
    (ctx: Context)
    : Context =
    let config =
        match lids with
        | SynLongIdent (id = (h :: _)) ->
            let s = h.idText

            if Char.IsUpper(s.[0]) then
                ctx.Config.SpaceBeforeUppercaseInvocation
            else
                ctx.Config.SpaceBeforeLowercaseInvocation
        | _ -> ctx.Config.SpaceBeforeUppercaseInvocation

    if (lastEsIndex = currentIndex)
       && (not (hasParenthesis arg) || config) then
        sepSpace ctx
    else
        ctx

and genFunctionNameWithMultilineLids f (synLongIdent: SynLongIdent) =
    match synLongIdent.IdentsWithTrivia with
    | h :: t ->
        genSynIdent false h
        +> indent
        +> sepNln
        +> col sepNln t (genSynIdent true)
        +> f
        +> unindent
    | _ -> sepNone
    |> genTriviaFor SynLongIdent_ synLongIdent.FullRange

and genMultilineFunctionApplicationArguments astContext argExpr =
    let argsInsideParenthesis lpr rpr pr f =
        sepOpenTFor lpr
        +> indent
        +> sepNln
        +> f
        +> unindent
        +> sepNln
        +> sepCloseTFor rpr
        |> genTriviaFor SynExpr_Paren pr

    let genExpr astContext e =
        match e with
        | InfixApp (equal, operatorSli, e1, e2, range) when (equal = "=") ->
            genNamedArgumentExpr astContext operatorSli e1 e2 range
        | _ -> genExpr astContext e

    match argExpr with
    | Paren (lpr, Lambda (pats, arrowRange, body, range), rpr, _pr) ->
        fun ctx ->
            if ctx.Config.MultiLineLambdaClosingNewline then
                (sepOpenTFor lpr
                 +> (!- "fun "
                     +> col sepSpace pats (genPat astContext)
                     +> genLambdaArrowWithTrivia (genExprKeepIndentInBranch astContext) body arrowRange
                     |> genTriviaFor SynExpr_Lambda range)
                 +> sepNln
                 +> sepCloseTFor rpr)
                    ctx
            else
                genExpr astContext argExpr ctx
    | Paren (lpr, Tuple (args, tupleRange), rpr, pr) ->
        (col (sepCommaFixed +> sepNln) args (genExpr astContext))
        |> genTriviaFor SynExpr_Tuple tupleRange
        |> argsInsideParenthesis lpr rpr pr
    | Paren (lpr, singleExpr, rpr, pr) ->
        genExpr astContext singleExpr
        |> argsInsideParenthesis lpr rpr pr
    | _ -> genExpr astContext argExpr

and genGenericTypeParameters astContext lt ts gt =
    match ts with
    | [] -> sepNone
    | ts ->
        genTriviaFor SynExpr_TypeApp_Less lt !- "<"
        +> coli sepComma ts (fun idx -> genType { astContext with IsFirstTypeParam = idx = 0 } false)
        +> indentIfNeeded sepNone
        +> genTriviaFor SynExpr_TypeApp_Greater gt !- ">"

and genMultilineRecordInstance
    (astContext: ASTContext)
    (openingBrace: Range)
    (inheritOpt: (SynType * SynExpr) option)
    (xs: SynExprRecordField list)
    (eo: SynExpr option)
    (closingBrace: Range)
    (ctx: Context)
    =
    let ifIndentLesserThan size lesser greater ctx =
        if ctx.Config.IndentSize < size then
            lesser ctx
        else
            greater ctx

    let expressionStartColumn = ctx.Column

    let fieldsExpr = col sepSemiNln xs (genRecordFieldName astContext)

    let expr =
        match inheritOpt with
        | Some (t, e) ->
            genTriviaFor SynExpr_Record_OpeningBrace openingBrace sepOpenS
            +> atCurrentColumn (
                !- "inherit "
                +> genType astContext false t
                +> addSpaceBeforeClassConstructor e
                +> genExpr astContext e
                +> onlyIf (List.isNotEmpty xs) sepNln
                +> fieldsExpr
                +> genTriviaFor SynExpr_Record_ClosingBrace closingBrace sepCloseS
            )
        | None ->
            match eo with
            | None ->
                fun (ctx: Context) ->
                    // position after `{ ` or `{`
                    let targetColumn =
                        ctx.Column
                        + (if ctx.Config.SpaceAroundDelimiter then
                               2
                           else
                               1)

                    atCurrentColumn
                        (genTriviaFor SynExpr_Record_OpeningBrace openingBrace sepOpenS
                         +> sepNlnWhenWriteBeforeNewlineNotEmpty sepNone // comment after curly brace
                         +> col sepSemiNln xs (fun e ->
                             // Add spaces to ensure the record field (incl trivia) starts at the right column.
                             addFixedSpaces targetColumn
                             // Lock the start of the record field, however keep potential indentations in relation to the opening curly brace
                             +> atCurrentColumn (genRecordFieldName astContext e))
                         +> genTriviaFor
                             SynExpr_Record_ClosingBrace
                             closingBrace
                             (sepNlnWhenWriteBeforeNewlineNotEmpty sepNone // comment after last record field
                              +> (fun ctx ->
                                  // Edge case scenario to make sure that the closing brace is not before the opening one
                                  // See unit test "multiline string before closing brace"
                                  let delta = expressionStartColumn - ctx.Column

                                  if delta > 0 then
                                      ((rep delta (!- " ")) +> sepCloseSFixed) ctx
                                  else
                                      ifElseCtx lastWriteEventIsNewline sepCloseSFixed sepCloseS ctx)))
                        ctx
            | Some e ->
                genTriviaFor SynExpr_Record_OpeningBrace openingBrace sepOpenS
                +> atCurrentColumnIndent (genExpr astContext e)
                +> !- " with"
                +> ifIndentLesserThan
                    3
                    (sepSpaceOrDoubleIndentAndNlnIfExpressionExceedsPageWidth fieldsExpr)
                    (sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth fieldsExpr)
                +> genTriviaFor SynExpr_Record_ClosingBrace closingBrace sepCloseS

    expr ctx

and genMultilineRecordInstanceAlignBrackets
    (astContext: ASTContext)
    (openingBrace: Range)
    (inheritOpt: (SynType * SynExpr) option)
    (xs: SynExprRecordField list)
    (eo: SynExpr option)
    (closingBrace: Range)
    =
    let fieldsExpr = col sepSemiNln xs (genRecordFieldName astContext)

    let hasFields = List.isNotEmpty xs

    match inheritOpt, eo with
    | Some (inheritType, inheritExpr), None ->
        genTriviaFor SynExpr_Record_OpeningBrace openingBrace sepOpenS
        +> ifElse hasFields (indent +> sepNln) sepNone
        +> !- "inherit "
        +> genType astContext false inheritType
        +> addSpaceBeforeClassConstructor inheritExpr
        +> genExpr astContext inheritExpr
        +> ifElse
            hasFields
            (sepNln
             +> fieldsExpr
             +> unindent
             +> sepNln
             +> genTriviaFor SynExpr_Record_ClosingBrace closingBrace sepCloseSFixed)
            (sepSpace
             +> genTriviaFor SynExpr_Record_ClosingBrace closingBrace sepCloseSFixed)

    | None, Some e ->
        genTriviaFor SynExpr_Record_OpeningBrace openingBrace sepOpenS
        +> atCurrentColumnIndent (genExpr astContext e)
        +> (!- " with"
            +> indent
            +> whenShortIndent indent
            +> sepNln
            +> fieldsExpr
            +> unindent
            +> whenShortIndent unindent
            +> sepNln
            +> genTriviaFor SynExpr_Record_ClosingBrace closingBrace sepCloseSFixed)

    | _ ->
        (genTriviaFor SynExpr_Record_OpeningBrace openingBrace sepOpenSFixed
         +> indent
         +> sepNln
         +> fieldsExpr
         +> unindent
         +> ifElseCtx lastWriteEventIsNewline sepNone sepNln
         +> genTriviaFor SynExpr_Record_ClosingBrace closingBrace sepCloseSFixed)

and genMultilineAnonRecord (isStruct: bool) fields copyInfo (astContext: ASTContext) =
    let recordExpr =
        match copyInfo with
        | Some e ->
            sepOpenAnonRecd
            +> atCurrentColumn (
                genExpr astContext e
                +> (!- " with"
                    +> indent
                    +> sepNln
                    +> col sepSemiNln fields (genAnonRecordFieldName astContext)
                    +> unindent)
            )
            +> sepCloseAnonRecd
        | None ->
            fun ctx ->
                // position after `{| ` or `{|`
                let targetColumn =
                    ctx.Column
                    + (if ctx.Config.SpaceAroundDelimiter then
                           3
                       else
                           2)

                atCurrentColumn
                    (sepOpenAnonRecd
                     +> col sepSemiNln fields (fun (AnonRecordFieldName (ident, eq, e)) ->
                         let expr =
                             if ctx.Config.IndentSize < 3 then
                                 sepSpaceOrDoubleIndentAndNlnIfExpressionExceedsPageWidth (genExpr astContext e)
                             else
                                 sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (genExpr astContext e)

                         // Add enough spaces to start at the right column but indent from the opening curly brace.
                         // Use a double indent when using a small indent size to avoid offset warnings.
                         addFixedSpaces targetColumn
                         +> atCurrentColumn (genIdent ident)
                         +> genEq SynExpr_AnonRecd_Field_Equals eq
                         +> expr)
                     +> sepCloseAnonRecd)
                    ctx

    onlyIf isStruct !- "struct " +> recordExpr

and genMultilineAnonRecordAlignBrackets (isStruct: bool) fields copyInfo astContext =
    let fieldsExpr = col sepSemiNln fields (genAnonRecordFieldName astContext)

    let copyExpr fieldsExpr e =
        atCurrentColumnIndent (genExpr astContext e)
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
    +> genAnonRecord

and genObjExpr t eio withKeyword bd members ims range (astContext: ASTContext) =
    // Check the role of the second part of eio
    let param = opt sepNone (Option.map fst eio) (genExpr astContext)

    sepOpenS
    +> atCurrentColumn (
        !- "new "
        +> genType astContext false t
        +> param
        +> genTriviaForOption SynExpr_ObjExpr_With withKeyword !- " with"
        +> indent
        +> sepNln
        +> genMemberBindingList { astContext with InterfaceRange = Some range } bd
        +> genMemberDefnList astContext members
        +> unindent
        +> colPre sepNln sepNln ims (genInterfaceImpl astContext)
    )
    +> sepCloseS

and genObjExprAlignBrackets t eio withKeyword bd members ims range (astContext: ASTContext) =
    // Check the role of the second part of eio
    let param = opt sepNone (Option.map fst eio) (genExpr astContext)

    let genObjExpr =
        atCurrentColumn (
            !- "new "
            +> genType astContext false t
            +> param
            +> genTriviaForOption SynExpr_ObjExpr_With withKeyword !- " with"
            +> indent
            +> sepNln
            +> genMemberBindingList { astContext with InterfaceRange = Some range } bd
            +> genMemberDefnList astContext members
            +> unindent
            +> colPre sepNln sepNln ims (genInterfaceImpl astContext)
        )

    atCurrentColumnIndent (sepOpenS +> genObjExpr +> sepNln +> sepCloseSFixed)

and genMultiLineArrayOrList
    (isArray: bool)
    xs
    (openingTokenRange: Range)
    (closingTokenRange: Range)
    (astContext: ASTContext)
    ctx
    =
    if isArray then
        (genTriviaFor SynExpr_ArrayOrList_OpeningDelimiter openingTokenRange sepOpenA
         +> atCurrentColumnIndent (
             sepNlnWhenWriteBeforeNewlineNotEmpty sepNone
             +> col sepSemiNln xs (genExpr astContext)
             +> genTriviaFor
                 SynExpr_ArrayOrList_ClosingDelimiter
                 closingTokenRange
                 (ifElseCtx lastWriteEventIsNewline sepCloseAFixed sepCloseA)
         ))
            ctx
    else
        (genTriviaFor SynExpr_ArrayOrList_OpeningDelimiter openingTokenRange sepOpenL
         +> atCurrentColumnIndent (
             sepNlnWhenWriteBeforeNewlineNotEmpty sepNone
             +> col sepSemiNln xs (genExpr astContext)
             +> genTriviaFor
                 SynExpr_ArrayOrList_ClosingDelimiter
                 closingTokenRange
                 (ifElseCtx lastWriteEventIsNewline sepCloseLFixed sepCloseL)
         ))
            ctx

and genMultiLineArrayOrListAlignBrackets (isArray: bool) xs openingTokenRange closingTokenRange astContext =
    if isArray then
        genTriviaFor SynExpr_ArrayOrList_OpeningDelimiter openingTokenRange sepOpenAFixed
        +> indent
        +> sepNlnUnlessLastEventIsNewline
        +> col sepNln xs (genExpr astContext)
        +> unindent
        +> sepNlnUnlessLastEventIsNewline
        +> genTriviaFor SynExpr_ArrayOrList_ClosingDelimiter closingTokenRange sepCloseAFixed
    else
        genTriviaFor SynExpr_ArrayOrList_OpeningDelimiter openingTokenRange sepOpenLFixed
        +> indent
        +> sepNlnUnlessLastEventIsNewline
        +> col sepNln xs (genExpr astContext)
        +> unindent
        +> sepNlnUnlessLastEventIsNewline
        +> genTriviaFor SynExpr_ArrayOrList_ClosingDelimiter closingTokenRange sepCloseLFixed

and genApp astContext e es ctx =
    let shortExpression =
        let addFirstSpace =
            ifElseCtx
                (fun ctx ->
                    match es with
                    | [] -> false
                    | [ h ] -> addSpaceBeforeParensInFunCall e h ctx
                    | _ -> true)
                sepSpace
                sepNone

        atCurrentColumn (
            genExpr astContext e
            +> addFirstSpace
            +> col sepSpace es (genExpr astContext)
        )

    let isParenLambda =
        (function
        | Paren (_, Lambda _, _, _)
        | Paren (_, MatchLambda _, _, _) -> true
        | _ -> false)

    let shouldHaveAlternativeLambdaStyle =
        let hasLambdas = List.exists isParenLambda es

        ctx.Config.MultiLineLambdaClosingNewline
        && hasLambdas

    let longExpression =
        if shouldHaveAlternativeLambdaStyle then
            // sample:
            // myFunction
            //    argumentOne
            //    (fun x ->
            //        // foo
            //        x = 12
            //    )
            let argExpr =
                col sepNln es (fun e ->
                    let genLambda
                        (pats: Context -> Context)
                        (bodyExpr: SynExpr)
                        (lpr: Range)
                        (rpr: Range option)
                        (arrowRange: Range option)
                        (pr: Range)
                        : Context -> Context =
                        leadingExpressionIsMultiline
                            (sepOpenTFor lpr -- "fun "
                             +> pats
                             +> genLambdaArrowWithTrivia (genExprKeepIndentInBranch astContext) bodyExpr arrowRange)
                            (fun isMultiline -> onlyIf isMultiline sepNln +> sepCloseTFor rpr)
                        |> genTriviaFor SynExpr_Paren pr

                    match e with
                    | Paren (lpr, Lambda (pats, arrowRange, expr, _range), rpr, pr) ->
                        genLambda (col sepSpace pats (genPat astContext)) expr lpr rpr arrowRange pr
                    | _ -> genExpr astContext e)

            genExpr astContext e
            +> indent
            +> sepNln
            +> argExpr
            +> unindent
        else
            atCurrentColumn (
                genExpr astContext e
                +> indent
                +> sepNln
                +> col sepNln es (genExpr astContext)
                +> unindent
            )

    if
        List.exists
            (function
            | ComputationExpr _ -> true
            | _ -> false)
            es then
        shortExpression ctx
    else
        expressionFitsOnRestOfLine shortExpression longExpression ctx

and genLambdaMultiLineClosingNewline
    (astContext: ASTContext)
    (lpr: Range)
    (pats: SynPat list)
    (arrowRange: Range option)
    (bodyExpr: SynExpr)
    (lambdaRange: Range)
    (rpr: Range option)
    (pr: Range)
    : Context -> Context =
    leadingExpressionIsMultiline
        (sepOpenTFor lpr -- "fun "
         +> col sepSpace pats (genPat astContext)
         +> genLambdaArrowWithTrivia (genExprKeepIndentInBranch astContext) bodyExpr arrowRange
         |> genTriviaFor SynExpr_Lambda lambdaRange)
        (fun isMultiline -> onlyIf isMultiline sepNln +> sepCloseTFor rpr)
    |> genTriviaFor SynExpr_Paren pr

and genAppWithTupledArgument (e, lpr, ts, tr, rpr, _pr) astContext =
    genExpr astContext e
    +> sepSpace
    +> sepOpenTFor lpr
    +> (col sepComma ts (genExpr astContext)
        |> genTriviaFor SynExpr_Tuple tr)
    +> sepCloseTFor rpr

and genAlternativeAppWithTupledArgument (e, lpr, ts, tr, rpr, _pr) astContext =
    genExpr astContext e
    +> indent
    +> sepNln
    +> sepOpenTFor lpr
    +> indent
    +> sepNln
    +> (col (sepComma +> sepNln) ts (genExpr astContext)
        |> genTriviaFor SynExpr_Tuple tr)
    +> unindent
    +> sepNln
    +> sepCloseTFor rpr
    +> unindent

and genAlternativeAppWithSingleParenthesisArgument (e, lpr, a, rpr, _pr) astContext =
    genExpr astContext e
    +> sepSpaceWhenOrIndentAndNlnIfExpressionExceedsPageWidth
        (fun ctx ->
            match e with
            | Paren _ -> true
            | UppercaseSynExpr _ -> ctx.Config.SpaceBeforeUppercaseInvocation
            | LowercaseSynExpr _ -> ctx.Config.SpaceBeforeLowercaseInvocation)
        (sepOpenTFor lpr
         +> expressionFitsOnRestOfLine
             (genExpr astContext a)
             (indent
              +> sepNln
              +> genExpr astContext a
              +> unindent
              +> sepNln)
         +> sepCloseTFor rpr)

and genAppWithSingleParenthesisArgument (e, lpr, a, rpr, _pr) astContext =
    genExpr astContext e
    +> sepSpace
    +> sepOpenTFor lpr
    +> (genExpr astContext a)
    +> sepCloseTFor rpr

and genExprInIfOrMatch astContext (e: SynExpr) (ctx: Context) : Context =
    let short =
        sepNlnWhenWriteBeforeNewlineNotEmpty sepSpace
        +> genExpr astContext e

    let long =
        let hasCommentBeforeExpr (e: SynExpr) =
            TriviaHelpers.``has content before that matches``
                (fun tn -> RangeHelpers.rangeEq tn.Range e.Range)
                (function
                | Comment (LineCommentOnSingleLine _) -> true
                | _ -> false)
                (Map.tryFindOrEmptyList (synExprToFsAstType e |> fst) ctx.TriviaNodes)

        let indentNlnUnindentNln f =
            indent +> sepNln +> f +> unindent +> sepNln

        let fallback =
            if hasCommentBeforeExpr e then
                genExpr astContext e |> indentNlnUnindentNln
            else
                sepNlnWhenWriteBeforeNewlineNotEmpty sepNone
                +> genExpr astContext e

        match e with
        | App (SynExpr.DotGet _, [ (Paren _) ]) -> atCurrentColumn (genExpr astContext e)
        | Paren (lpr, (AppSingleParenArg _ as ate), rpr, _pr) ->
            sepOpenTFor lpr
            +> atCurrentColumnIndent (genExpr astContext ate)
            +> sepCloseTFor rpr
        | AppParenArg app ->
            genAlternativeAppWithParenthesis app astContext
            |> indentNlnUnindentNln
        | InfixApp (s, sli, (AppParenArg app as e1), e2, _) ->
            (expressionFitsOnRestOfLine (genExpr astContext e1) (genAlternativeAppWithParenthesis app astContext)
             +> ifElse (noBreakInfixOps.Contains(s)) sepSpace sepNln
             +> genSynLongIdent false sli
             +> sepSpace
             +> genExpr astContext e2)
            |> indentNlnUnindentNln
        | InfixApp (s, sli, e1, (AppParenArg app as e2), _) ->
            (genExpr astContext e1
             +> sepNln
             +> genSynLongIdent false sli
             +> sepSpace
             +> expressionFitsOnRestOfLine (genExpr astContext e2) (genAlternativeAppWithParenthesis app astContext))
            |> indentNlnUnindentNln
        // very specific fix for 1380
        | SameInfixApps (Paren (lpr, AppParenArg e, rpr, _pr), es) ->
            (sepOpenTFor lpr
             +> genAlternativeAppWithParenthesis e astContext
             +> sepCloseTFor rpr
             +> sepNln
             +> col sepNln es (fun (opText, opExpr, e) ->
                 genSynLongIdent false opExpr
                 +> sepSpace
                 +> (match e with
                     | Paren (lpr, AppParenArg app, rpr, _pr) ->
                         sepOpenTFor lpr
                         +> genAlternativeAppWithParenthesis app astContext
                         +> sepCloseTFor rpr
                     | _ -> genExpr astContext e)))
            |> indentNlnUnindentNln
        | InfixApp _ -> fallback
        | App (SynExpr.Ident _, _)
        | App (SynExpr.LongIdent _, _) ->
            indent
            +> sepNln
            +> genExpr astContext e
            +> unindent
        | SynExpr.Match _
        | SynExpr.MatchBang _
        | SynExpr.TryWith _
        | SynExpr.TryFinally _ -> genExpr astContext e |> indentNlnUnindentNln
        | DotGetAppParen (DotGetAppParen (e1, px1, lids1), px2, lids2) ->
            genExpr astContext e1
            +> genExpr astContext px1
            +> indent
            +> sepNln
            +> genSynLongIdentMultiline true lids1
            +> genExpr astContext px2
            +> sepNln
            +> genSynLongIdentMultiline true lids2
            +> unindent
            |> genTriviaFor SynExpr_DotGet e.Range
            |> indentNlnUnindentNln
        | _ -> fallback

    expressionFitsOnRestOfLine short long ctx

and genWithAfterMatch (astType: FsAstType) (withRange: Range) =
    genTriviaFor astType withRange (fun ctx ->
        let hasContentOnLastLine =
            List.tryHead ctx.WriterModel.Lines
            |> Option.map String.isNotNullOrWhitespace
            |> Option.defaultValue false

        if hasContentOnLastLine then
            // add a space if there is no newline right after the expression
            (!- " with") ctx
        else
            // add the indentation in spaces if there is no content on the current line
            (rep ctx.Config.IndentSize (!- " ") +> !- "with") ctx)

and genAlternativeAppWithParenthesis app astContext =
    match app with
    | Choice1Of2 t -> genAlternativeAppWithTupledArgument t astContext
    | Choice2Of2 s -> genAlternativeAppWithSingleParenthesisArgument s astContext

and genAppWithParenthesis app astContext =
    match app with
    | Choice1Of2 t -> genAppWithTupledArgument t astContext
    | Choice2Of2 s -> genAppWithSingleParenthesisArgument s astContext

and collectMultilineItemForSynExpr (astContext: ASTContext) (e: SynExpr) : ColMultilineItem list =
    match e with
    | LetOrUses (bs, e) -> collectMultilineItemForLetOrUses astContext bs (collectMultilineItemForSynExpr astContext e)
    | Sequentials s ->
        s
        |> List.collect (collectMultilineItemForSynExpr astContext)
    | _ ->
        let t, r = synExprToFsAstType e
        [ ColMultilineItem(genExpr astContext e, sepNlnConsideringTriviaContentBeforeForMainNode t r) ]

and collectMultilineItemForLetOrUses
    (astContext: ASTContext)
    (bs: (string * SynBinding * range option) list)
    (itemsForExpr: ColMultilineItem list)
    : ColMultilineItem list =

    let multilineBinding p x inKw =
        let expr =
            enterNodeFor (synBindingToFsAstType x) x.RangeOfBindingWithRhs
            +> genLetBinding astContext p x
            +> genTriviaForOption SynExpr_LetOrUse_In inKw !- " in "

        let range = x.RangeOfBindingWithRhs

        let sepNln =
            sepNlnConsideringTriviaContentBeforeForMainNode (synBindingToFsAstType x) range

        ColMultilineItem(expr, sepNln)

    let multipleOrLongBs bs =
        bs
        |> List.map (fun (p, x, inKw) -> multilineBinding p x inKw)

    match bs, itemsForExpr with
    | [], _ -> itemsForExpr
    | [ p, b, inKeyword ], [ ColMultilineItem (expr, sepNlnForExpr) ] ->
        // This is a trickier case
        // maybe the let binding and expression are short so they form one ColMultilineItem
        // Something like: let a = 1 in ()

        let range = b.RangeOfBindingWithRhs

        let sepNlnForBinding =
            sepNlnConsideringTriviaContentBeforeForMainNode (synBindingToFsAstType b) range

        match inKeyword with
        | Some inKw ->
            // single multiline item
            let expr =
                enterNodeFor (synBindingToFsAstType b) b.RangeOfBindingWithRhs
                +> genLetBinding astContext p b
                +> genTriviaFor SynExpr_LetOrUse_In inKw !- " in "
                +> expressionFitsOnRestOfLine expr (sepNln +> sepNlnForExpr +> expr)

            [ ColMultilineItem(expr, sepNlnForBinding) ]
        | None -> multipleOrLongBs bs @ itemsForExpr
    | bs, _ -> multipleOrLongBs bs @ itemsForExpr

and sepNlnBetweenTypeAndMembers (withKeywordRange: range option) (ms: SynMemberDefn list) =
    match List.tryHead ms with
    | Some m -> sepNlnTypeAndMembers SynTypeDefn_With withKeywordRange m.Range (synMemberDefnToFsAstType m)
    | None -> sepNone

and genTypeDefn
    astContext
    (isFirstTypeDefn: bool)
    (TypeDef (ats, px, typeKeyword, ao, tds, tcs, equalsRange, tdr, withKeyword, ms, lids, preferPostfix) as node)
    =
    let typeName =
        genPreXmlDoc px
        +> ifElse
            isFirstTypeDefn
            (genAttributes astContext ats
             +> optSingle (enterNodeFor SynTypeDefn_Type) typeKeyword
             -- "type ")
            (!- "and " +> genOnelinerAttributes astContext ats)
        +> opt sepSpace ao genAccess
        +> genTypeAndParam astContext (genLongIdent lids) tds tcs

    match tdr with
    | Simple (TDSREnum ecs) ->
        typeName
        +> genEq SynTypeDefn_Equals equalsRange
        +> indent
        +> sepNln
        +> genTriviaFor
            SynTypeDefnSimpleRepr_Enum
            tdr.Range
            (col sepNln ecs (genEnumCase astContext)
             +> onlyIf (List.isNotEmpty ms) sepNln
             +> sepNlnBetweenTypeAndMembers withKeyword ms
             +> genMemberDefnList { astContext with InterfaceRange = None } ms
             // Add newline after un-indent to be spacing-correct
             +> unindent)

    | Simple (TDSRUnion (ao', xs)) ->
        let unionCases (ctx: Context) =
            match xs with
            | [] -> ctx
            | [ UnionCase (attrs, _, _, _, _, UnionCaseType fields, _) as x ] when List.isEmpty ms ->
                let hasVerticalBar =
                    ctx.Config.BarBeforeDiscriminatedUnionDeclaration
                    || List.isNotEmpty attrs
                    || List.isEmpty fields

                let short =
                    genTriviaFor
                        SynTypeDefnSimpleRepr_Union
                        tdr.Range
                        (opt sepSpace ao' genAccess
                         +> genUnionCase astContext hasVerticalBar x)

                let long =
                    genTriviaFor
                        SynTypeDefnSimpleRepr_Union
                        tdr.Range
                        (opt sepSpace ao' genAccess
                         +> genUnionCase astContext true x)

                expressionFitsOnRestOfLine (indent +> sepSpace +> short) (indent +> sepNln +> long) ctx
            | xs ->
                indent
                +> sepNln
                +> genTriviaFor
                    SynTypeDefnSimpleRepr_Union
                    tdr.Range
                    (opt sepNln ao' genAccess
                     +> col sepNln xs (genUnionCase astContext true))
                <| ctx

        typeName
        +> genEq SynTypeDefn_Equals equalsRange
        +> unionCases
        +> onlyIf (List.isNotEmpty ms) sepNln
        +> sepNlnBetweenTypeAndMembers withKeyword ms
        +> genMemberDefnList { astContext with InterfaceRange = None } ms
        +> unindent

    | Simple (TDSRRecord (openingBrace, ao', fs, closingBrace)) ->
        let smallExpression =
            sepSpace
            +> optSingle (fun ao -> genAccess ao +> sepSpace) ao'
            +> genTriviaFor SynTypeDefnSimpleRepr_Record_OpeningBrace openingBrace sepOpenS
            +> col sepSemi fs (genField astContext "")
            +> genTriviaFor SynTypeDefnSimpleRepr_Record_ClosingBrace closingBrace sepCloseS

        let multilineExpression =
            ifAlignBrackets
                (genMultilineSimpleRecordTypeDefnAlignBrackets
                    astContext
                    openingBrace
                    withKeyword
                    ms
                    ao'
                    fs
                    closingBrace)
                (genMultilineSimpleRecordTypeDefn astContext openingBrace withKeyword ms ao' fs closingBrace)

        let bodyExpr size ctx =
            if (List.isEmpty ms) then
                (isSmallExpression size smallExpression multilineExpression
                 +> leaveNodeFor SynTypeDefnSimpleRepr_Record tdr.Range // this will only print something when there is trivia after } in the short expression
                // Yet it cannot be part of the short expression otherwise the multiline expression would be triggered unwillingly.
                )
                    ctx
            else
                multilineExpression ctx

        let genTypeDefinition (ctx: Context) =
            let size = getRecordSize ctx fs

            let short =
                enterNodeFor SynTypeDefnSimpleRepr_Record tdr.Range
                +> bodyExpr size
                +> leaveNodeFor SynTypeDefnSimpleRepr_Record tdr.Range

            if ctx.Config.Ragnarok && ms.IsEmpty then
                (sepSpace +> short) ctx
            else
                isSmallExpression size short (indent +> sepNln +> short +> unindent) ctx

        typeName
        +> genEq SynTypeDefn_Equals equalsRange
        +> genTypeDefinition

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
                 +> sepNlnBetweenTypeAndMembers withKeyword ms
                 +> genMemberDefnList { astContext with InterfaceRange = None } ms
                 +> unindent
                 +> unindent)

        let genTypeBody =
            autoIndentAndNlnIfExpressionExceedsPageWidth genTypeAbbrev
            +> genMembers

        typeName
        +> genEq SynTypeDefn_Equals equalsRange
        +> sepSpace
        +> genTypeBody
    | Simple (TDSRException (ExceptionDefRepr (ats, px, ao, uc))) -> genExceptionBody astContext ats px ao uc

    | ObjectModel (TCSimple (TCInterface
                   | TCClass) as tdk,
                   MemberDefnList (impCtor, others),
                   range) ->
        let interfaceRange =
            match tdk with
            | TCSimple TCInterface -> Some range
            | _ -> None

        let astContext = { astContext with InterfaceRange = interfaceRange }

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
        +> sepNlnBetweenTypeAndMembers withKeyword ms
        +> genMemberDefnList astContext others
        +> unindent
        ++ "end"
        +> (fun ctx ->
            match ms with
            | [] -> ctx
            | h :: _ ->
                (sepNln
                 +> sepNlnConsideringTriviaContentBeforeForMainNode (synMemberDefnToFsAstType h) h.Range
                 +> genMemberDefnList astContext ms)
                    ctx)
        +> unindent

    | ObjectModel (TCSimple TCStruct as tdk, MemberDefnList (impCtor, others), _) ->
        let sepMem =
            match ms with
            | [] -> sepNone
            | _ -> sepNln

        typeName
        +> opt sepNone impCtor (genMemberDefn astContext)
        +> genEq SynTypeDefn_Equals equalsRange
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

    | ObjectModel (TCSimple (TCAugmentation withKeywordAug), _, _) ->
        typeName
        +> genTriviaFor SynTypeDefnKind_Augmentation_With withKeywordAug !- " with"
        +> indent
        // Remember that we use MemberDefn of parent node
        +> sepNln
        +> sepNlnBetweenTypeAndMembers withKeyword ms
        +> genMemberDefnList { astContext with InterfaceRange = None } ms
        +> unindent

    | ObjectModel (TCDelegate (FunType ts), _, _) ->
        typeName
        +> genEq SynTypeDefn_Equals equalsRange
        +> sepSpace
        +> !- "delegate of "
        +> genTypeList astContext ts

    | ObjectModel (TCSimple TCUnspecified, MemberDefnList (impCtor, others), _) when not (List.isEmpty ms) ->
        typeName
        +> opt sepNone impCtor (genMemberDefn { astContext with InterfaceRange = None })
        +> genEq SynTypeDefn_Equals equalsRange
        +> indent
        +> sepNln
        +> genMemberDefnList { astContext with InterfaceRange = None } others
        +> sepNln
        +> genTriviaForOption SynTypeDefn_With withKeyword !- "with"
        +> indent
        +> sepNln
        +> genMemberDefnList { astContext with InterfaceRange = None } ms
        +> unindent
        +> unindent

    | ObjectModel (_, MemberDefnList (impCtor, others), _) ->
        typeName
        +> opt sepNone impCtor (fun mdf ->
            sepSpaceBeforeClassConstructor
            +> genMemberDefn { astContext with InterfaceRange = None } mdf)
        +> genEq SynTypeDefn_Equals equalsRange
        +> indent
        +> sepNln
        +> genMemberDefnList { astContext with InterfaceRange = None } others
        +> unindent

    | ExceptionRepr (ExceptionDefRepr (ats, px, ao, uc)) -> genExceptionBody astContext ats px ao uc
    |> genTriviaFor SynTypeDefn_ node.Range

and genMultilineSimpleRecordTypeDefn astContext openingBrace withKeyword ms ao' fs closingBrace =
    // the typeName is already printed
    sepNlnUnlessLastEventIsNewline
    +> opt (indent +> sepNln) ao' genAccess
    +> enterNodeFor SynTypeDefnSimpleRepr_Record_OpeningBrace openingBrace
    +> sepOpenS
    +> atCurrentColumn (
        leaveNodeFor SynTypeDefnSimpleRepr_Record_OpeningBrace openingBrace
        +> sepNlnWhenWriteBeforeNewlineNotEmpty sepNone
        +> col sepSemiNln fs (genField astContext "")
    )
    +> genTriviaFor SynTypeDefnSimpleRepr_Record_ClosingBrace closingBrace sepCloseS
    +> optSingle (fun _ -> unindent) ao'
    +> onlyIf (List.isNotEmpty ms) sepNln
    +> sepNlnBetweenTypeAndMembers withKeyword ms
    +> genMemberDefnList { astContext with InterfaceRange = None } ms

and genMultilineSimpleRecordTypeDefnAlignBrackets astContext openingBrace withKeyword ms ao' fs closingBrace =
    // the typeName is already printed
    opt (indent +> sepNln) ao' genAccess
    +> enterNodeFor SynTypeDefnSimpleRepr_Record_OpeningBrace openingBrace
    +> sepOpenSFixed
    +> indent
    +> sepNln
    +> atCurrentColumn (
        leaveNodeFor SynTypeDefnSimpleRepr_Record_OpeningBrace openingBrace
        +> col sepSemiNln fs (genField astContext "")
    )
    +> unindent
    +> sepNln
    +> genTriviaFor SynTypeDefnSimpleRepr_Record_ClosingBrace closingBrace sepCloseSFixed
    +> optSingle (fun _ -> unindent) ao'
    +> onlyIf (List.isNotEmpty ms) sepNln
    +> sepNlnBetweenTypeAndMembers withKeyword ms
    +> genMemberDefnList { astContext with InterfaceRange = None } ms

and sepNlnBetweenSigTypeAndMembers (withKeyword: range option) (ms: SynMemberSig list) : Context -> Context =
    match List.tryHead ms with
    | Some m -> sepNlnTypeAndMembers SynTypeDefnSig_With withKeyword m.Range (synMemberSigToFsAstType m)
    | None -> sepNone

and genSigTypeDefn
    astContext
    (isFirstSigTypeDefn: bool)
    (SigTypeDef (ats, px, ao, tds, tcs, equalsRange, tdr, withKeyword, ms, lid, _preferPostfix, fullRange))
    =
    let genTriviaForOnelinerAttributes f (ctx: Context) =
        match ats with
        | [] -> f ctx
        | h :: _ ->
            (enterNodeFor SynAttributeList_ h.Range
             +> f
             +> leaveNodeFor SynAttributeList_ h.Range)
                ctx

    let genXmlTypeKeywordAttrsAccess =
        genPreXmlDoc px
        +> ifElse
            isFirstSigTypeDefn
            (genAttributes astContext ats -- "type ")
            ((!- "and " +> genOnelinerAttributes astContext ats)
             |> genTriviaForOnelinerAttributes)
        +> opt sepSpace ao genAccess

    let typeName =
        genXmlTypeKeywordAttrsAccess
        +> genTypeAndParam astContext (genLongIdent lid) tds tcs

    match tdr with
    | SigSimple (TDSREnum ecs) ->
        typeName
        +> genEq SynTypeDefnSig_Equals equalsRange
        +> indent
        +> sepNln
        +> col sepNln ecs (genEnumCase astContext)
        +> sepNlnBetweenSigTypeAndMembers withKeyword ms
        +> colPre sepNln sepNln ms (genMemberSig astContext)
        // Add newline after un-indent to be spacing-correct
        +> unindent

    | SigSimple (TDSRUnion (ao', xs)) ->
        let unionCases (ctx: Context) =
            match xs with
            | [] -> ctx
            | [ UnionCase (attrs, _, _, _, _, UnionCaseType fields, _) as x ] when List.isEmpty ms ->
                let hasVerticalBar =
                    ctx.Config.BarBeforeDiscriminatedUnionDeclaration
                    || List.isNotEmpty attrs
                    || List.isEmpty fields

                let short =
                    genTriviaFor
                        SynTypeDefnSimpleRepr_Union
                        tdr.Range
                        (opt sepSpace ao' genAccess
                         +> genUnionCase astContext hasVerticalBar x)

                let long =
                    genTriviaFor
                        SynTypeDefnSimpleRepr_Union
                        tdr.Range
                        (opt sepSpace ao' genAccess
                         +> genUnionCase astContext true x)

                expressionFitsOnRestOfLine (indent +> sepSpace +> short) (indent +> sepNln +> long) ctx
            | xs ->
                (indent
                 +> sepNln
                 +> (opt sepNln ao' genAccess
                     +> col sepNln xs (genUnionCase astContext true)
                     |> genTriviaFor SynTypeDefnSimpleRepr_Union tdr.Range))
                    ctx

        typeName
        +> genEq SynTypeDefnSig_Equals equalsRange
        +> unionCases
        +> sepNlnBetweenSigTypeAndMembers withKeyword ms
        +> colPre sepNln sepNln ms (genMemberSig astContext)
        +> unindent

    | SigSimple (TDSRRecord (openingBrace, ao', fs, closingBrace)) ->
        let smallExpression =
            sepSpace
            +> optSingle (fun ao -> genAccess ao +> sepSpace) ao'
            +> genTriviaFor SynExpr_Record_OpeningBrace openingBrace sepOpenS
            +> col sepSemi fs (genField astContext "")
            +> genTriviaFor SynExpr_Record_ClosingBrace closingBrace sepCloseS

        let multilineExpression =
            ifAlignBrackets
                (genSigSimpleRecordAlignBrackets astContext openingBrace withKeyword ms ao' fs closingBrace)
                (genSigSimpleRecord astContext openingBrace withKeyword ms ao' fs closingBrace)

        let bodyExpr size ctx =
            if (List.isEmpty ms) then
                (isSmallExpression size smallExpression multilineExpression
                 +> leaveNodeFor SynTypeDefnSimpleRepr_Record tdr.Range // this will only print something when there is trivia after } in the short expression
                // Yet it cannot be part of the short expression otherwise the multiline expression would be triggered unwillingly.
                )
                    ctx
            else
                multilineExpression ctx

        let genTypeDefinition (ctx: Context) =
            let size = getRecordSize ctx fs

            let short =
                enterNodeFor SynTypeDefnSimpleRepr_Record tdr.Range
                +> bodyExpr size
                +> leaveNodeFor SynTypeDefnSimpleRepr_Record tdr.Range

            if ctx.Config.Ragnarok && ms.IsEmpty then
                (sepSpace +> short) ctx
            else
                isSmallExpression size short (indent +> sepNln +> short +> unindent) ctx

        typeName +> sepEq +> genTypeDefinition

    | SigSimple TDSRNone ->
        let genMembers =
            match ms with
            | [] -> sepNone
            | _ ->
                !- " with"
                +> indent
                +> sepNln
                +> sepNlnBetweenSigTypeAndMembers withKeyword ms
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

        let short =
            genTypeAndParam astContext (genLongIdent lid) tds tcs
            +> genEq SynTypeDefnSig_Equals equalsRange
            +> sepSpace
            +> genTypeAbbrev

        let long =
            genTypeAndParam astContext (genLongIdent lid) tds tcs
            +> sepSpace
            +> genEqFixed SynTypeDefnSig_Equals equalsRange
            +> indent
            +> sepNln
            +> genTypeAbbrev
            +> unindent

        genXmlTypeKeywordAttrsAccess
        +> expressionFitsOnRestOfLine short long
    | SigSimple (TDSRException (ExceptionDefRepr (ats, px, ao, uc))) -> genExceptionBody astContext ats px ao uc

    | SigObjectModel (TCSimple (TCStruct
                      | TCInterface
                      | TCClass) as tdk,
                      mds) ->
        typeName
        +> genEq SynTypeDefnSig_Equals equalsRange
        +> indent
        +> sepNln
        +> genTypeDefKind tdk
        +> indent
        +> colPre sepNln sepNln mds (genMemberSig astContext)
        +> unindent
        ++ "end"
        +> unindent

    | SigObjectModel (TCSimple (TCAugmentation withKeyword), _) ->
        typeName
        +> genTriviaFor SynTypeDefnKind_Augmentation_With withKeyword !- " with"
        +> indent
        +> sepNln
        // Remember that we use MemberSig of parent node
        +> col sepNln ms (genMemberSig astContext)
        +> unindent

    | SigObjectModel (TCDelegate (FunType ts), _) ->
        typeName
        +> genEq SynTypeDefnSig_Equals equalsRange
        +> sepSpace
        -- "delegate of "
        +> genTypeList astContext ts
    | SigObjectModel (_, mds) ->
        typeName
        +> genEq SynTypeDefnSig_Equals equalsRange
        +> indent
        +> sepNln
        +> col sepNln mds (genMemberSig astContext)
        +> unindent

    | SigExceptionRepr (SigExceptionDefRepr (ats, px, ao, uc)) -> genExceptionBody astContext ats px ao uc
    |> genTriviaFor SynTypeDefnSig_ fullRange

and genSigSimpleRecord astContext openingBrace withKeyword ms ao' fs closingBrace =
    opt (indent +> sepNln) ao' genAccess
    +> sepOpenS
    +> atCurrentColumn (
        leaveNodeFor SynTypeDefnSimpleRepr_Record_OpeningBrace openingBrace
        +> sepNlnWhenWriteBeforeNewlineNotEmpty sepNone
        +> col sepSemiNln fs (genField astContext "")
    )
    +> genTriviaFor SynTypeDefnSimpleRepr_Record_ClosingBrace closingBrace sepCloseS
    +> optSingle (fun _ -> unindent) ao'
    +> sepNlnBetweenSigTypeAndMembers withKeyword ms
    +> colPre sepNln sepNln ms (genMemberSig astContext)

and genSigSimpleRecordAlignBrackets astContext openingBrace withKeyword ms ao' fs closingBrace =
    opt (indent +> sepNln) ao' genAccess
    +> sepOpenSFixed
    +> indent
    +> sepNln
    +> atCurrentColumn (
        leaveNodeFor SynTypeDefnSimpleRepr_Record_OpeningBrace closingBrace
        +> col sepSemiNln fs (genField astContext "")
    )
    +> unindent
    +> sepNln
    +> genTriviaFor SynTypeDefnSimpleRepr_Record_ClosingBrace closingBrace sepCloseSFixed
    +> optSingle (fun _ -> unindent) ao'
    +> sepNlnBetweenSigTypeAndMembers withKeyword ms
    +> colPre sepNln sepNln ms (genMemberSig astContext)

and genMemberSig astContext node =
    let range, mainNodeName =
        match node with
        | SynMemberSig.Member (_, _, r) -> r, SynMemberSig_Member
        | SynMemberSig.Interface (_, r) -> r, SynMemberSig_Interface
        | SynMemberSig.Inherit (_, r) -> r, SynMemberSig_Inherit
        | SynMemberSig.ValField (_, r) -> r, SynMemberSig_ValField
        | SynMemberSig.NestedType (_, r) -> r, SynMemberSig_NestedType

    match node with
    | MSMember (Val (ats, px, _, ao, si, t, vi, isInline, _, tds, eo, _), mf) ->
        let (FunType namedArgs) = (t, vi)

        let isFunctionProperty =
            match t with
            | TFun _ -> true
            | _ -> false

        let hasGenerics = Option.isSome tds

        genPreXmlDoc px
        +> genAttributes astContext ats
        +> genMemberFlags mf
        +> ifElse isInline (!- "inline ") sepNone
        +> opt sepSpace ao genAccess
        +> genTypeAndParam astContext (genSynIdent false si) tds [] // (if s = "``new``" then "new" else s)
        +> ifElse hasGenerics sepColonWithSpacesFixed sepColon
        +> ifElse
            (List.isNotEmpty namedArgs)
            (autoIndentAndNlnIfExpressionExceedsPageWidth (genTypeList astContext namedArgs))
            (genConstraints astContext t vi)
        -- (genPropertyKind (not isFunctionProperty) mf.MemberKind)
        +> optSingle (fun e -> sepEq +> sepSpace +> genExpr astContext e) eo


    | MSInterface t -> !- "interface " +> genType astContext false t
    | MSInherit t -> !- "inherit " +> genType astContext false t
    | MSValField f -> genField astContext "val " f
    | MSNestedType _ -> invalidArg "md" "This is not implemented in F# compiler"
    |> genTriviaFor mainNodeName range

and genConstraints astContext (t: SynType) (vi: SynValInfo) =
    match t with
    | TWithGlobalConstraints (ti, tcs) ->
        let genType =
            match ti, vi with
            | TFuns ts, SynValInfo (curriedArgInfos, returnType) ->
                let namedArgInfos =
                    [ yield! curriedArgInfos
                      yield [ returnType ] ]

                let args = List.zip namedArgInfos ts

                col sepArrow args (fun (argInfo, t) ->
                    match argInfo, t with
                    | [], _ -> genType astContext false t
                    | [ SynArgInfo (_, isOptional, Some ident) ], _ ->
                        onlyIf isOptional (!- "?")
                        +> genIdent ident
                        +> sepColon
                        +> genType astContext false t
                    | [ SynArgInfo _ ], _ -> genType astContext false t
                    | multipleArgInfo, TTuple ts ->
                        let combined = List.zip multipleArgInfo ts

                        col sepStar combined (fun (argInfo, (_, t)) ->
                            let genNamed =
                                match argInfo with
                                | SynArgInfo (_, isOptional, Some ident) ->
                                    onlyIf isOptional (!- "?")
                                    +> genIdent ident
                                    +> sepColon
                                | _ -> sepNone

                            genNamed +> genType astContext false t)
                    | _ -> sepNone)
            | _ -> genType astContext false ti

        genType
        +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (
            ifElse (List.isNotEmpty tcs) (!- "when ") sepSpace
            +> col wordAnd tcs (genTypeConstraint astContext)
        )
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
    | TCSimple TCOpaque -> sepNone
    | TCSimple (TCAugmentation _) -> sepNone
    | TCSimple TCIL -> sepNone
    | TCDelegate _ -> sepNone

and genExceptionBody astContext ats px ao uc =
    genPreXmlDoc px +> genAttributes astContext ats
    -- "exception "
    +> opt sepSpace ao genAccess
    +> genUnionCase astContext false uc

and genException astContext (ExceptionDef (ats, px, ao, uc, withKeyword, ms) as node) =
    genExceptionBody astContext ats px ao uc
    +> ifElse
        ms.IsEmpty
        sepNone
        (genTriviaForOption SynExceptionDefn_With withKeyword (!- " with")
         +> indent
         +> sepNln
         +> genMemberDefnList { astContext with InterfaceRange = None } ms
         +> unindent)
    |> genTriviaFor SynExceptionDefn_ node.Range

and genSigException astContext (SigExceptionDef (ats, px, ao, uc, withKeyword, ms)) =
    genExceptionBody astContext ats px ao uc
    +> onlyIfNot
        ms.IsEmpty
        (genTriviaForOption SynExceptionSig_With withKeyword (!- " with")
         +> indent
         +> sepNln
         +> col sepNln ms (genMemberSig astContext)
         +> unindent)

and genUnionCase astContext (hasVerticalBar: bool) (UnionCase (ats, px, barRange, _, si, UnionCaseType fs, range)) =
    let shortExpr = col sepStar fs (genField { astContext with IsUnionField = true } "")

    let longExpr =
        indent
        +> sepNln
        +> atCurrentColumn (col (sepStar +> sepNln) fs (genField { astContext with IsUnionField = true } ""))
        +> unindent

    genPreXmlDoc px
    +> genTriviaForOptionOr SynUnionCase_Bar barRange (ifElse hasVerticalBar sepBar sepNone)
    +> atCurrentColumn (
        // If the bar has a comment after, add a newline and print the identifier on the same column on the next line.
        sepNlnWhenWriteBeforeNewlineNotEmpty sepNone
        +> genOnelinerAttributes astContext ats
        +> genSynIdent false si
        +> onlyIf (List.isNotEmpty fs) wordOf
    )
    +> onlyIf (List.isNotEmpty fs) (expressionFitsOnRestOfLine shortExpr longExpr)
    |> genTriviaFor SynUnionCase_ range

and genEnumCase astContext (EnumCase (ats, barRange, px, si, equalsRange, c, cr, r)) =
    let genCase =
        (genSynIdent false si
         +> genEq SynEnumCase_Equals (Some equalsRange)
         +> autoIndentAndNlnWhenWriteBeforeNewlineNotEmpty (sepSpace +> genConst c cr))

    genPreXmlDoc px
    +> (match barRange with
        | None -> sepBar
        | Some barRange -> genTriviaFor SynEnumCase_Bar barRange sepBar)
    +> genOnelinerAttributes astContext ats
    +> genCase
    |> genTriviaFor SynEnumCase_ r

and genField astContext prefix (Field (ats, px, ao, isStatic, isMutable, t, so, innerRange, range)) =
    // Being protective on union case declaration
    let t = genType astContext astContext.IsUnionField t

    genPreXmlDoc px
    +> genAttributes astContext ats
    +> ifElse isStatic (!- "static ") sepNone
    -- prefix
    +> ifElse isMutable (!- "mutable ") sepNone
    +> opt sepSpace ao genAccess
    +> (opt sepColon so genIdent +> t
        |> optSingle (genTriviaFor SynField_IdentifierAndType) innerRange)
    |> genTriviaFor SynField_ range

and genType astContext outerBracket t =
    let rec loop current =
        match current with
        | THashConstraint t ->
            let wrapInParentheses f =
                match t with
                | TApp (_, _, ts, _, isPostfix, range) when (isPostfix && List.isNotEmpty ts) ->
                    sepOpenT +> f +> sepCloseT
                    |> genTriviaFor SynType_App range
                | _ -> f

            !- "#" +> wrapInParentheses (loop t)
        | TMeasurePower (t, n) -> loop t -- "^" +> str n
        | TMeasureDivide (t1, t2) -> loop t1 -- " / " +> loop t2
        | TStaticConstant (c, r) -> genConst c r
        | TStaticConstantExpr e -> !- "const" +> genExpr astContext e
        | TStaticConstantNamed (t1, t2) ->
            loop t1 -- "="
            +> addSpaceIfSynTypeStaticConstantHasAtSignBeforeString t2
            +> loop t2
        | TArray (t, n, r) ->
            loop t -- "[" +> rep (n - 1) (!- ",") -- "]"
            |> genTriviaFor SynType_Array r
        | TAnon -> sepWild
        | TVar (tp, r) ->
            genTypar astContext tp
            |> genTriviaFor SynType_Var r
        // Drop bracket around tuples before an arrow
        | TFun (TTuple ts, t) -> loopTTupleList ts +> sepArrow +> loop t
        // Do similar for tuples after an arrow
        | TFun (t, TTuple ts) -> loop t +> sepArrow +> loopTTupleList ts
        | TFuns ts -> col sepArrow ts loop
        | TApp (TLongIdent (SynLongIdent ([ _ ], [], [ Some (IdentTrivia.OriginalNotation "*") ])),
                _,
                [ t ],
                _,
                true,
                range) when astContext.IsCStylePattern -> loop t -- "*" |> genTriviaFor SynType_App range
        | TApp (TLongIdent (SynLongIdent ([ _ ], [], [ Some (IdentTrivia.OriginalNotation "&") ])),
                _,
                [ t ],
                _,
                true,
                range) when astContext.IsCStylePattern -> loop t -- "&" |> genTriviaFor SynType_App range
        | TApp (t, lessRange, ts, greaterRange, isPostfix, range) ->
            let postForm =
                match ts with
                | [] -> loop t
                | [ t' ] -> loop t' +> sepSpace +> loop t
                | ts ->
                    sepOpenT
                    +> col sepComma ts loop
                    +> sepCloseT
                    +> loop t

            ifElse
                isPostfix
                postForm
                (loop t
                 +> genPrefixTypes astContext SynType_App_Less lessRange ts SynType_App_Greater greaterRange)
            |> genTriviaFor SynType_App range

        | TLongIdentApp (t, sli, lessRange, ts, greaterRange) ->
            loop t
            +> sepDot
            +> genSynLongIdent false sli
            +> genPrefixTypes
                astContext
                SynType_LongIdentApp_Less
                lessRange
                ts
                SynType_LongIdentApp_Greater
                greaterRange
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
        | SynType.LongIdent (SynLongIdent(id = [ lid ])) when (astContext.IsCStylePattern && lid.idText = "[]") ->
            !- "[]"
        | TLongIdent sli -> genSynLongIdent false sli
        //            ifElseCtx
        //                (fun ctx ->
        //                    not ctx.Config.StrictMode
        //                    && astContext.IsCStylePattern)
        //                (!-(if s = "unit" then "void" else s))
        //                (!-s)
        //            |> genTriviaFor Ident_ current.Range
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
        | TParen (lpr, innerT, rpr) ->
            genTriviaFor SynType_Paren_OpeningParenthesis lpr sepOpenT
            +> loop innerT
            +> genTriviaFor SynType_Paren_ClosingParenthesis rpr sepCloseT
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
    | TFuns ts ->
        let short = col sepArrow ts loop

        let long =
            match ts with
            | [] -> sepNone
            | h :: rest ->
                loop h
                +> indent
                +> sepNln
                +> sepArrowFixed
                +> sepSpace
                +> col (sepNln +> sepArrowFixed +> sepSpace) rest loop
                +> unindent

        let genTs = expressionFitsOnRestOfLine short long

        ifElse outerBracket (sepOpenT +> genTs +> sepCloseT) genTs
    | TTuple ts -> ifElse outerBracket (sepOpenT +> loopTTupleList ts +> sepCloseT) (loopTTupleList ts)
    | _ -> loop t

// for example: FSharpx.Regex< @"(?<value>\d+)" >
and addSpaceIfSynTypeStaticConstantHasAtSignBeforeString (t: SynType) (ctx: Context) =
    let hasAtSign =
        match t with
        | TStaticConstant (SynConst.String (_, SynStringKind.Verbatim, _), _) -> true
        | _ -> false

    onlyIf hasAtSign sepSpace ctx

and genAnonRecordFieldType astContext (AnonRecordFieldType (ident, t)) =
    genIdent ident
    +> sepColon
    +> (genType astContext false t)

and genPrefixTypes
    astContext
    (lessNodeType: FsAstType)
    (lessRange: range option)
    ts
    (greaterNodeType: FsAstType)
    (greaterRange: range option)
    ctx
    =
    match ts with
    | [] -> ctx
    // Where <  and ^ meet, we need an extra space. For example:  seq< ^a >
    | TVar (Typar (_, true), _r) as t :: ts ->
        (genTriviaForOption lessNodeType lessRange !- "< "
         +> col sepComma (t :: ts) (genType astContext false)
         +> genTriviaForOption greaterNodeType greaterRange !- " >")
            ctx
    | t :: _ ->
        (genTriviaForOption lessNodeType lessRange !- "<"
         +> atCurrentColumnIndent (
             addSpaceIfSynTypeStaticConstantHasAtSignBeforeString t
             +> col sepComma ts (genType astContext false)
             +> addSpaceIfSynTypeStaticConstantHasAtSignBeforeString t
         )
         +> genTriviaForOption greaterNodeType greaterRange !- ">")
            ctx

and genTypeList astContext node =
    let gt (t, args: SynArgInfo list) =
        match t, args with
        | TTuple ts', _ ->
            let hasBracket = not node.IsEmpty

            let gt sepBefore =
                if args.Length = ts'.Length then
                    col sepBefore (Seq.zip args (Seq.map snd ts')) (fun (ArgInfo (ats, so, isOpt), t) ->
                        genOnelinerAttributes astContext ats
                        +> opt sepColon so (fun ident -> onlyIf isOpt (!- "?") +> genIdent ident)
                        +> genType astContext hasBracket t)
                else
                    col sepBefore ts' (snd >> genType astContext hasBracket)

            let shortExpr = gt sepStar
            let longExpr = gt (sepSpace +> sepStarFixed +> sepNln)
            expressionFitsOnRestOfLine shortExpr longExpr

        | _, [ ArgInfo (ats, so, isOpt) ] ->
            match t with
            | TTuple _ -> not node.IsEmpty
            | TFun _ -> true // Fun is grouped by brackets inside 'genType astContext true t'
            | _ -> false
            |> fun hasBracket ->
                genOnelinerAttributes astContext ats
                +> opt sepColon so (fun ident -> onlyIf isOpt (!- "?") +> genIdent ident)
                +> genType astContext hasBracket t
        | _ -> genType astContext false t

    let shortExpr = col sepArrow node gt

    let longExpr =
        let lastIndex = node.Length - 1

        let isTupleOrLastIndex index =
            index = lastIndex
            || match List.tryItem (index - 1) node with
               | Some (TTuple _, _) -> true
               | _ -> false

        let resetIndent =
            if lastIndex < 0 then
                id
            else
                [ 0..lastIndex ]
                |> List.choose (fun idx ->
                    if isTupleOrLastIndex idx then
                        Some unindent
                    else
                        None)
                |> List.reduce (+>)

        colii
            (fun idx ->
                sepSpace
                +> sepArrowFixed
                +> onlyIf (isTupleOrLastIndex idx) indent
                +> sepNln)
            node
            (fun _ -> gt)
        +> resetIndent

    expressionFitsOnRestOfLine shortExpr longExpr

and genTypar astContext (Typar (ident, isHead)) =
    ifElse isHead (ifElse astContext.IsFirstTypeParam (!- " ^") (!- "^")) (!- "'")
    +> genIdent ident

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
        genTypeSupportMemberList astContext tps
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

and genInterfaceImpl astContext (InterfaceImpl (t, withKeywordRange, bs, members, range)) =
    if bs.IsEmpty && members.IsEmpty then
        !- "interface " +> genType astContext false t
    else
        !- "interface "
        +> genType astContext false t
        +> genTriviaForOption SynInterfaceImpl_With withKeywordRange !- " with"
        +> indent
        +> sepNln
        +> genMemberBindingList { astContext with InterfaceRange = Some range } bs
        +> genMemberDefnList astContext members
        +> unindent

and genClause
    (astContext: ASTContext)
    (hasBar: bool)
    (hasMultipleClausesWhereOneHasRagnarok: bool)
    (Clause (p, eo, arrowRange, e) as ce)
    =
    let astCtx = { astContext with IsInsideMatchClausePattern = true }

    let patAndBody =
        genPat astCtx p
        +> leadingExpressionIsMultiline
            (optPre (!- " when") sepNone eo (fun e ->
                let short = sepSpace +> genExpr astContext e

                let long =
                    match e with
                    | AppParenArg app ->
                        indent
                        +> sepNln
                        +> genAlternativeAppWithParenthesis app astContext
                        +> unindent
                    | e ->
                        indent
                        +> sepNln
                        +> (genExpr astContext e)
                        +> unindent

                expressionFitsOnRestOfLine short long))
            (fun isMultiline ctx ->
                if isMultiline then
                    (indent
                     +> sepNln
                     +> optSingle
                         (fun arrowRange ->
                             sepArrowFixed
                             |> genTriviaFor SynMatchClause_Arrow arrowRange)
                         arrowRange
                     +> sepNln
                     +> genExpr astContext e
                     +> unindent)
                        ctx
                else
                    (optSingle
                        (fun arrowRange ->
                            sepArrow
                            |> genTriviaFor SynMatchClause_Arrow arrowRange)
                        arrowRange
                     +> autoIndentAndNlnIfExpressionExceedsPageWidthUnlessRagnarok (genExpr astContext) e)
                        ctx)

    (onlyIf hasBar sepBar
     +> (fun ctx ->
         if hasMultipleClausesWhereOneHasRagnarok then
             // avoid edge case
             (*
                match x with
                | y -> [
                    1
                    2
                    3
                ]
                | z -> [
                    4
                    5
                    6
                ]
            *)
             // ] and | cannot align, otherwise you get a parser error
             atCurrentColumn patAndBody ctx
         else
             patAndBody ctx)
     |> genTriviaFor SynMatchClause_ ce.Range)

and genClauses astContext cs (ctx: Context) =
    col sepNln cs (genClause astContext true (hasMultipleClausesWhereOneHasRagnarok ctx.Config.Ragnarok cs)) ctx

/// Each multiline member definition has a pre and post new line.
and genMemberDefnList astContext nodes =
    let rec collectItems
        (nodes: SynMemberDefn list)
        (finalContinuation: ColMultilineItem list -> ColMultilineItem list)
        : ColMultilineItem list =
        match nodes with
        | [] -> finalContinuation []
        | PropertyWithGetSetMemberDefn (gs, rest) ->
            let rangeOfFirstMember = List.head nodes |> fun m -> m.Range

            let expr =
                enterNodeFor SynMemberDefn_Member rangeOfFirstMember
                +> genPropertyWithGetSet astContext gs

            let sepNln =
                sepNlnConsideringTriviaContentBeforeForMainNode SynMemberDefn_Member rangeOfFirstMember

            collectItems rest (fun restItems ->
                ColMultilineItem(expr, sepNln) :: restItems
                |> finalContinuation)
        | m :: rest ->
            let expr = genMemberDefn astContext m

            let sepNln =
                sepNlnConsideringTriviaContentBeforeForMainNode (synMemberDefnToFsAstType m) m.Range

            collectItems rest (fun restItems ->
                ColMultilineItem(expr, sepNln) :: restItems
                |> finalContinuation)

    collectItems nodes id
    |> colWithNlnWhenItemIsMultilineUsingConfig

and genMemberDefn astContext node =
    match node with
    | MDNestedType _ -> invalidArg "md" "This is not implemented in F# compiler"
    | MDOpen lid -> !- "open " +> genLongIdent lid
    // What is the role of so
    | MDImplicitInherit (t, e, _) ->
        let genBasecall =
            let shortExpr = genExpr astContext e

            let longExpr =
                match e with
                | Paren (lpr, Tuple (es, tr), rpr, pr) ->
                    indent
                    +> sepNln
                    +> indent
                    +> sepOpenTFor lpr
                    +> sepNln
                    +> (col (sepComma +> sepNln) es (genExpr astContext)
                        |> genTriviaFor SynExpr_Tuple tr)
                    +> unindent
                    +> sepNln
                    +> unindent
                    +> sepCloseTFor rpr
                    |> genTriviaFor SynExpr_Paren pr
                | _ -> genExpr astContext e

            expressionFitsOnRestOfLine shortExpr longExpr

        !- "inherit "
        +> genType astContext false t
        +> addSpaceBeforeClassConstructor e
        +> genBasecall

    | MDInherit (t, _) -> !- "inherit " +> genType astContext false t
    | MDValField f -> genField astContext "val " f
    | MDImplicitCtor ((PreXmlDoc (xmlDoc, _) as preXmlDoc), ats, ao, ps, so) ->
        let rec simplePats ps =
            match ps with
            | SynSimplePats.SimplePats (pats, _) -> pats
            | SynSimplePats.Typed (spts, _, _) -> simplePats spts

        let genCtor =
            let shortExpr =
                optPre sepSpace sepSpace ao genAccess
                +> ((sepOpenT
                     +> col sepComma (simplePats ps) (genSimplePat astContext)
                     +> sepCloseT)
                    |> genTriviaFor SynSimplePats_SimplePats ps.Range)

            let emptyPats =
                let rec isEmpty ps =
                    match ps with
                    | SynSimplePats.SimplePats ([], _) -> true
                    | SynSimplePats.SimplePats _ -> false
                    | SynSimplePats.Typed (spts, _, _) -> isEmpty spts

                isEmpty ps

            let hasXmlDocComment = xmlDoc.Length > 0

            let longExpr ctx =
                (indent
                 +> sepNln
                 +> genPreXmlDoc preXmlDoc
                 +> optSingle (fun ao -> genAccess ao +> sepNln) ao
                 +> ifElse emptyPats (sepOpenT +> sepCloseT) (fun ctx ->
                     let shortPats =
                         sepOpenT
                         +> col sepComma (simplePats ps) (genSimplePat astContext)
                         +> sepCloseT

                     let longPats =
                         sepOpenT
                         +> indent
                         +> sepNln
                         +> col (sepComma +> sepNln) (simplePats ps) (genSimplePat astContext)
                         +> unindent
                         +> sepNln
                         +> sepCloseT

                     let triviaBeforePats =
                         Map.tryFindOrEmptyList SynSimplePats_SimplePats ctx.TriviaNodes
                         |> List.tryFind (fun tn -> RangeHelpers.rangeEq tn.Range ps.Range)

                     match triviaBeforePats with
                     | Some tn ->
                         (printContentBefore tn
                          +> expressionFitsOnRestOfLine shortPats longPats
                          +> printContentAfter tn)
                             ctx
                     | None when hasXmlDocComment -> expressionFitsOnRestOfLine shortPats longPats ctx
                     | _ -> longPats ctx)
                 +> onlyIf ctx.Config.AlternativeLongMemberDefinitions sepNln
                 +> unindent)
                    ctx

            if hasXmlDocComment then
                longExpr
            else
                expressionFitsOnRestOfLine shortExpr longExpr

        // In implicit constructor, attributes should come even before access qualifiers
        ifElse ats.IsEmpty sepNone (sepSpace +> genOnelinerAttributes astContext ats)
        +> genCtor
        +> optPre (!- " as ") sepNone so genIdent

    | MDMember b -> genMemberBinding astContext b
    | MDLetBindings (isStatic, isRec, b :: bs) ->
        let prefix =
            if isStatic && isRec then
                "static let rec "
            elif isStatic then
                "static let "
            elif isRec then
                "let rec "
            else
                "let "

        let items =
            let bsItems =
                bs
                |> List.map (fun andBinding ->
                    let expr =
                        enterNodeFor (synBindingToFsAstType b) andBinding.RangeOfBindingWithRhs
                        +> genLetBinding astContext "and " andBinding

                    ColMultilineItem(
                        expr,
                        sepNlnConsideringTriviaContentBeforeForMainNode
                            SynBindingKind_Normal
                            andBinding.RangeOfBindingWithRhs
                    ))

            ColMultilineItem(genLetBinding astContext prefix b, sepNone)
            :: bsItems

        colWithNlnWhenItemIsMultilineUsingConfig items

    | MDInterface (t, withKeyword, mdo, range) ->
        !- "interface "
        +> genType astContext false t
        +> opt sepNone mdo (fun mds ->
            genTriviaForOption SynMemberDefn_Interface_With withKeyword !- " with"
            +> indent
            +> sepNln
            +> genMemberDefnList { astContext with InterfaceRange = Some range } mds
            +> unindent)

    | MDAutoProperty (ats, px, ao, mk, equalsRange, e, _withKeyword, ident, _isStatic, typeOpt, memberKindToMemberFlags) ->
        let isFunctionProperty =
            match typeOpt with
            | Some (TFun _) -> true
            | _ -> false

        genPreXmlDoc px
        +> genAttributes astContext ats
        +> genMemberFlags (memberKindToMemberFlags mk)
        +> str "val "
        +> opt sepSpace ao genAccess
        +> genIdent ident
        +> optPre sepColon sepNone typeOpt (genType astContext false)
        +> genEq SynMemberDefn_AutoProperty_Equals (Some equalsRange)
        +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (
            genExpr astContext e
            -- genPropertyKind (not isFunctionProperty) mk
        )

    | MDAbstractSlot (ats, px, ao, si, t, vi, ValTyparDecls (tds, _), mf) ->
        let (FunType namedArgs) = (t, vi)

        let isFunctionProperty =
            match t with
            | TFun _ -> true
            | _ -> false

        let hasGenerics = Option.isSome tds

        genPreXmlDoc px
        +> genAttributes astContext ats
        +> opt sepSpace ao genAccess
        +> genMemberFlags mf
        +> genSynIdent false si
        +> genTypeParamPostfix astContext tds
        +> ifElse hasGenerics sepColonWithSpacesFixed sepColon
        +> autoIndentAndNlnIfExpressionExceedsPageWidth (genTypeList astContext namedArgs)
        -- genPropertyKind (not isFunctionProperty) mf.MemberKind
        +> onlyIf
            (match t with
             | TWithGlobalConstraints _ -> true
             | _ -> false)
            autoIndentAndNlnIfExpressionExceedsPageWidth
            (genConstraints astContext t vi)

    | md -> failwithf "Unexpected member definition: %O" md
    |> genTriviaFor (synMemberDefnToFsAstType node) node.Range

and genPropertyKind useSyntacticSugar node =
    match node with
    | PropertyGet ->
        // Try to use syntactic sugar on real properties (not methods in disguise)
        if useSyntacticSugar then
            ""
        else
            " with get"
    | PropertySet -> " with set"
    | PropertyGetSet -> " with get, set"
    | _ -> ""

and genSimplePat astContext node =
    match node with
    | SPId (ident, isOptArg, _) -> onlyIf isOptArg (!- "?") +> genIdent ident
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

and genPatRecordFieldName astContext ((lid: LongIdent, ident: Ident), _: range, p: SynPat) =
    ifElse
        lid.IsEmpty
        (genIdent ident +> sepEq +> sepSpace)
        (genLongIdent lid
         +> sepDot
         +> genIdent ident
         +> sepEq
         +> sepSpace)
    +> genPat { astContext with IsInsideMatchClausePattern = false } p // see issue 1252.

and genPatWithIdent astContext (ido, p) =
    optSingle
        (fun (ident, eqR) ->
            genIdent ident
            +> genEq SynArgPats_NamePatPairs_Equals (Some eqR)
            +> sepSpace)
        ido
    +> genPat astContext p

and genPat astContext pat =
    match pat with
    | PatOptionalVal ident -> !- "?" +> genIdent ident
    | PatAttrib (p, ats) ->
        genOnelinerAttributes astContext ats
        +> genPat astContext p
    | PatOr (p1, barRange, p2) ->
        genPat astContext p1
        +> ifElse astContext.IsInsideMatchClausePattern sepNln sepSpace
        +> genTriviaFor SynPat_Or_Bar barRange !- "| "
        +> genPat astContext p2
    | PatAnds ps -> col (!- " & ") ps (genPat astContext)
    | PatNullary PatNull -> !- "null"
    | PatNullary PatWild -> sepWild
    | PatTyped (p, t) ->
        // CStyle patterns only occur on extern declaration so it doesn't escalate to expressions
        // We lookup sources to get extern types since it has quite many exceptions compared to normal F# types
        ifElse
            astContext.IsCStylePattern
            (genType astContext false t
             +> sepSpace
             +> genPat astContext p)
            (genPat astContext p
             +> sepColon
             +> atCurrentColumnIndent (genType astContext false t))

    | PatNamed (ao, si) -> opt sepSpace ao genAccess +> genSynIdent false si
    | PatAs (p1, p2, r) ->
        genPat astContext p1 -- " as "
        +> genPat astContext p2
        |> genTriviaFor SynPat_As r
    | PatLongIdent (ao, sli, _, ps, tpso) ->
        let aoc = opt sepSpace ao genAccess

        let tpsoc =
            opt sepNone tpso (fun (ValTyparDecls (tds, _)) -> genTypeParamPostfix astContext tds)

        match ps, sli with
        | [], _ -> aoc +> genSynLongIdent false sli +> tpsoc
        | [ (_, PatTuple [ p1; p2 ]) ], SynLongIdent(trivia = [ Some (IdentTrivia.OriginalNotation "::") ]) ->
            aoc +> genPat astContext p1 -- " :: "
            +> genPat astContext p2
        | [ ido, p as ip ], _ ->
            aoc
            +> genSynLongIdent false sli
            +> tpsoc
            +> ifElse
                (hasParenInPat p || Option.isSome ido)
                (ifElseCtx
                    (fun ctx -> addSpaceBeforeParensInFunDef ctx.Config.SpaceBeforeParameter sli p)
                    sepSpace
                    sepNone)
                sepSpace
            +> ifElse
                (Option.isSome ido)
                (sepOpenT
                 +> genPatWithIdent astContext ip
                 +> sepCloseT)
                (genPatWithIdent astContext ip)
        // This pattern is potentially long
        | ps, _ ->
            let hasBracket = ps |> Seq.map fst |> Seq.exists Option.isSome

            let genName =
                aoc
                +> genSynLongIdent false sli
                +> tpsoc
                +> sepSpace

            let genParameters =
                expressionFitsOnRestOfLine
                    (atCurrentColumn (col (ifElse hasBracket sepSemi sepSpace) ps (genPatWithIdent astContext)))
                    (atCurrentColumn (col sepNln ps (genPatWithIdent astContext)))

            genName
            +> ifElse hasBracket sepOpenT sepNone
            +> genParameters
            +> ifElse hasBracket sepCloseT sepNone

    | PatParen (_, PatUnitConst, _) -> !- "()"
    | PatParen (lpr, p, rpr) ->
        genTriviaFor SynPat_Paren_OpeningParenthesis lpr sepOpenT
        +> genPat astContext p
        +> genTriviaFor SynPat_Paren_ClosingParenthesis rpr sepCloseT
    | PatTuple ps ->
        expressionFitsOnRestOfLine
            (col sepComma ps (genPat astContext))
            (atCurrentColumn (col (sepComma +> sepNln) ps (genPat astContext)))
    | PatStructTuple ps ->
        !- "struct "
        +> sepOpenT
        +> atCurrentColumn (colAutoNlnSkip0 sepComma ps (genPat astContext))
        +> sepCloseT
    | PatSeq (patListType, [ PatOrs (patOr, patOrs) ]) ->
        let sepOpen, sepClose =
            match patListType with
            | PatArray -> sepOpenA, sepCloseA
            | PatList -> sepOpenL, sepCloseL

        let short =
            sepOpen
            +> genPat astContext patOr
            +> sepSpace
            +> col sepSpace patOrs (fun (barRange, p) -> sepBar +> genPat astContext p)
            +> sepClose

        let long =
            sepOpen
            +> atCurrentColumnIndent (
                match patOrs with
                | [] -> sepNone
                | pats ->
                    genPat astContext patOr +> sepNln -- " "
                    +> atCurrentColumn (
                        sepBar
                        +> col (sepNln +> sepBar) pats (fun (barRange, p) -> genPat astContext p)
                    )
            )
            +> sepClose

        expressionFitsOnRestOfLine short long
    | PatSeq (PatList, ps) ->
        let genPats =
            let short = colAutoNlnSkip0 sepSemi ps (genPat astContext)

            let long = col sepSemiNln ps (genPat astContext)
            expressionFitsOnRestOfLine short long

        ifElse ps.IsEmpty (sepOpenLFixed +> sepCloseLFixed) (sepOpenL +> atCurrentColumn genPats +> sepCloseL)

    | PatSeq (PatArray, ps) ->
        let genPats =
            let short = colAutoNlnSkip0 sepSemi ps (genPat astContext)

            let long = col sepSemiNln ps (genPat astContext)
            expressionFitsOnRestOfLine short long

        ifElse ps.IsEmpty (sepOpenAFixed +> sepCloseAFixed) (sepOpenA +> atCurrentColumn genPats +> sepCloseA)

    | PatRecord xs ->
        let smallRecordExpr =
            sepOpenS
            +> col sepSemi xs (genPatRecordFieldName astContext)
            +> sepCloseS

        // Note that MultilineBlockBracketsOnSameColumn is not taken into account here.
        let multilineRecordExpr =
            sepOpenS
            +> atCurrentColumn (col sepSemiNln xs (genPatRecordFieldName astContext))
            +> sepCloseS

        let multilineRecordExprAlignBrackets =
            sepOpenSFixed
            +> indent
            +> sepNln
            +> atCurrentColumn (col sepSemiNln xs (genPatRecordFieldName astContext))
            +> unindent
            +> sepNln
            +> sepCloseSFixed
            |> atCurrentColumnIndent

        let multilineExpressionIfAlignBrackets =
            ifAlignBrackets multilineRecordExprAlignBrackets multilineRecordExpr

        fun ctx ->
            let size = getRecordSize ctx xs
            isSmallExpression size smallRecordExpr multilineExpressionIfAlignBrackets ctx
    | PatConst (c, r) -> genConst c r
    | PatIsInst t -> !- ":? " +> genType astContext false t
    // Quotes will be printed by inner expression
    | PatQuoteExpr e -> genExpr astContext e
    | p -> failwithf "Unexpected pattern: %O" p
    |> (match pat with
        | SynPat.Named _ -> genTriviaFor SynPat_Named pat.Range
        | SynPat.Wild _ -> genTriviaFor SynPat_Wild pat.Range
        | SynPat.LongIdent _ -> genTriviaFor SynPat_LongIdent pat.Range
        | SynPat.Paren _ -> genTriviaFor SynPat_Paren pat.Range
        | _ -> id)

and genSynBindingFunction
    (astContext: ASTContext)
    (isMemberDefinition: bool)
    (isRecursiveLetOrUseFunction: bool)
    (px: PreXmlDoc)
    (ats: SynAttributes)
    (pref: Context -> Context)
    (ao: SynAccess option)
    (isInline: bool)
    (isMutable: bool)
    (functionName: SynLongIdent)
    (patRange: Range)
    (parameters: ((Ident * range) option * SynPat) list)
    (genericTypeParameters: SynValTyparDecls option)
    (equalsRange: range option)
    (e: SynExpr)
    (ctx: Context)
    =
    let spaceBefore, alternativeSyntax =
        if isMemberDefinition then
            ctx.Config.SpaceBeforeMember, ctx.Config.AlternativeLongMemberDefinitions
        else
            ctx.Config.SpaceBeforeParameter, ctx.Config.AlignFunctionSignatureToIndentation

    let genAttrIsFirstChild =
        onlyIf (not isRecursiveLetOrUseFunction) (genAttributes astContext ats)

    let genPref =
        if not isRecursiveLetOrUseFunction then
            pref
        else
            (pref +> genOnelinerAttributes astContext ats)

    let afterLetKeyword =
        ifElse isMutable (!- "mutable ") sepNone
        +> ifElse isInline (!- "inline ") sepNone
        +> opt sepSpace ao genAccess

    let genFunctionName =
        genSynBindingFunctionName functionName
        +> opt sepNone genericTypeParameters (fun (ValTyparDecls (tds, _)) -> genTypeParamPostfix astContext tds)

    let genSignature =
        let spaceBeforeParameters =
            match parameters with
            | [] -> sepNone
            | [ (_, p) ] -> ifElse (addSpaceBeforeParensInFunDef spaceBefore functionName p) sepSpace sepNone
            | _ -> sepSpace

        let short =
            afterLetKeyword
            +> genFunctionName
            +> spaceBeforeParameters
            +> col sepSpace parameters (genPatWithIdent astContext)
            +> genEq SynBinding_Equals equalsRange

        let long (ctx: Context) =
            let genParameters, hasSingleTupledArg =
                match parameters with
                | [ _, (PatParen (lpr, PatTuple ps, rpr) as pp) ] ->
                    genParenTupleWithIndentAndNewlines astContext lpr ps rpr pp.Range, true
                | _ -> col sepNln parameters (genPatWithIdent astContext), false

            (afterLetKeyword
             +> sepSpace
             +> genFunctionName
             +> indent
             +> sepNln
             +> genParameters
             +> ifElse (hasSingleTupledArg && not alternativeSyntax) sepSpace sepNln
             +> genEqFixed SynBinding_Equals equalsRange
             +> unindent)
                ctx

        expressionFitsOnRestOfLine short long

    let body (ctx: Context) =
        genExprKeepIndentInBranch astContext e ctx

    let genExpr isMultiline =
        if isMultiline then
            (indent +> sepNln +> body +> unindent)
        else
            let short = sepSpace +> body

            let long =
                autoIndentAndNlnExpressUnlessRagnarok (fun e -> sepSpace +> genExprKeepIndentInBranch astContext e) e

            isShortExpression ctx.Config.MaxFunctionBindingWidth short long

    (genPreXmlDoc px
     +> genAttrIsFirstChild
     +> genPref
     +> leadingExpressionIsMultiline genSignature genExpr)
        ctx

and genSynBindingFunctionWithReturnType
    (astContext: ASTContext)
    (isMemberDefinition: bool)
    (isRecursiveLetOrUseFunction: bool)
    (px: PreXmlDoc)
    (ats: SynAttributes)
    (pref: Context -> Context)
    (ao: SynAccess option)
    (isInline: bool)
    (isMutable: bool)
    (functionName: SynLongIdent)
    (patRange: Range)
    (parameters: ((Ident * range) option * SynPat) list)
    (genericTypeParameters: SynValTyparDecls option)
    (returnType: SynType)
    (valInfo: SynValInfo)
    (equalsRange: range option)
    (e: SynExpr)
    (ctx: Context)
    =
    let spaceBefore, alternativeSyntax =
        if isMemberDefinition then
            ctx.Config.SpaceBeforeMember, ctx.Config.AlternativeLongMemberDefinitions
        else
            ctx.Config.SpaceBeforeParameter, ctx.Config.AlignFunctionSignatureToIndentation

    let genAttrIsFirstChild =
        onlyIf (not isRecursiveLetOrUseFunction) (genAttributes astContext ats)

    let genPref =
        if not isRecursiveLetOrUseFunction then
            pref
        else
            pref +> genOnelinerAttributes astContext ats

    let afterLetKeyword =
        ifElse isMutable (!- "mutable ") sepNone
        +> ifElse isInline (!- "inline ") sepNone
        +> opt sepSpace ao genAccess

    let genFunctionName =
        genSynBindingFunctionName functionName
        +> opt sepNone genericTypeParameters (fun (ValTyparDecls (tds, _)) -> genTypeParamPostfix astContext tds)

    let genReturnType isFixed =
        let genMetadataAttributes =
            match valInfo with
            | SynValInfo (_, SynArgInfo (attributes, _, _)) -> genOnelinerAttributes astContext attributes

        enterNodeFor SynBindingReturnInfo_ returnType.Range
        +> ifElse isFixed (sepColonFixed +> sepSpace) sepColonWithSpacesFixed
        +> genMetadataAttributes
        +> genType astContext false returnType

    let genSignature =
        let spaceBeforeParameters =
            match parameters with
            | [] -> sepNone
            | [ (_, p) ] -> ifElse (addSpaceBeforeParensInFunDef spaceBefore functionName p) sepSpace sepNone
            | _ -> sepSpace

        let short =
            afterLetKeyword
            +> sepSpace
            +> genFunctionName
            +> spaceBeforeParameters
            +> col sepSpace parameters (genPatWithIdent astContext)
            +> genReturnType false
            +> genEq SynBinding_Equals equalsRange

        let long (ctx: Context) =
            let genParameters, hasSingleTupledArg =
                match parameters with
                | [ _, (PatParen (lpr, PatTuple ps, rpr) as pp) ] ->
                    genParenTupleWithIndentAndNewlines astContext lpr ps rpr pp.Range, true
                | _ -> col sepNln parameters (genPatWithIdent astContext), false

            (afterLetKeyword
             +> sepSpace
             +> genFunctionName
             +> indent
             +> sepNln
             +> genParameters
             +> onlyIf (not hasSingleTupledArg || alternativeSyntax) sepNln
             +> genReturnType (not hasSingleTupledArg || alternativeSyntax)
             +> ifElse alternativeSyntax (sepNln +> genEqFixed SynBinding_Equals equalsRange) sepEq
             +> unindent)
                ctx

        expressionFitsOnRestOfLine short long

    let body = genExprKeepIndentInBranch astContext e

    let genExpr isMultiline =
        if isMultiline then
            (indent +> sepNln +> body +> unindent)
        else
            let short = sepSpace +> body

            let long =
                autoIndentAndNlnExpressUnlessRagnarok (fun e -> sepSpace +> genExprKeepIndentInBranch astContext e) e

            isShortExpression ctx.Config.MaxFunctionBindingWidth short long

    (genPreXmlDoc px
     +> genAttrIsFirstChild
     +> genPref
     +> leadingExpressionIsMultiline genSignature genExpr)
        ctx

and genLetBindingDestructedTuple
    (astContext: ASTContext)
    (isRecursiveLetOrUseFunction: bool)
    (px: PreXmlDoc)
    (ats: SynAttributes)
    (pref: string)
    (ao: SynAccess option)
    (isInline: bool)
    (isMutable: bool)
    (pat: SynPat)
    (equalsRange: range option)
    (e: SynExpr)
    =
    let genAttrAndPref =
        if not isRecursiveLetOrUseFunction then
            (genAttributes astContext ats -- pref)
        else
            (!-pref +> genOnelinerAttributes astContext ats)

    let afterLetKeyword =
        opt sepSpace ao genAccess
        +> ifElse isMutable (!- "mutable ") sepNone
        +> ifElse isInline (!- "inline ") sepNone

    let genDestructedTuples =
        expressionFitsOnRestOfLine (genPat astContext pat) (sepOpenT +> genPat astContext pat +> sepCloseT)

    genPreXmlDoc px
    +> genAttrAndPref
    +> (fun ctx ->
        let prefix =
            afterLetKeyword
            +> sepSpace
            +> genDestructedTuples
            +> genEq SynBinding_Equals equalsRange

        let long =
            prefix
            +> indent
            +> sepNln
            +> genExpr astContext e
            +> unindent

        let short = prefix +> sepSpace +> genExpr astContext e

        isShortExpression ctx.Config.MaxValueBindingWidth short long ctx)

and genSynBindingValue
    (astContext: ASTContext)
    (isRecursiveLetOrUseFunction: bool)
    (px: PreXmlDoc)
    (ats: SynAttributes)
    (pref: Context -> Context)
    (ao: SynAccess option)
    (isInline: bool)
    (isMutable: bool)
    (valueName: SynPat)
    (returnType: SynType option)
    (equalsRange: range option)
    (e: SynExpr)
    =
    let genAttrIsFirstChild =
        onlyIf (not isRecursiveLetOrUseFunction) (genAttributes astContext ats)

    let genPref =
        if not isRecursiveLetOrUseFunction then
            pref
        else
            (pref +> genOnelinerAttributes astContext ats)

    let afterLetKeyword =
        opt sepSpace ao genAccess
        +> ifElse isMutable (!- "mutable ") sepNone
        +> ifElse isInline (!- "inline ") sepNone

    let genValueName = genPat astContext valueName

    let genEqualsInBinding (ctx: Context) =
        (genEqFixed SynBinding_Equals equalsRange
         +> sepSpaceUnlessWriteBeforeNewlineNotEmpty)
            ctx

    let genReturnType =
        match returnType with
        | Some rt ->
            let hasGenerics =
                match valueName with
                | SynPat.LongIdent (_, _, _, Some _, _, _, _) -> true
                | _ -> false

            ifElse hasGenerics sepColonWithSpacesFixed sepColon
            +> (genType astContext false rt
                |> genTriviaFor SynBindingReturnInfo_ rt.Range)
            +> sepSpaceUnlessWriteBeforeNewlineNotEmpty
            +> autoIndentAndNlnWhenWriteBeforeNewlineNotEmpty genEqualsInBinding
        | None -> sepSpace +> genEqualsInBinding

    genPreXmlDoc px
    +> genAttrIsFirstChild
    +> genPref
    +> (fun ctx ->
        let prefix =
            afterLetKeyword
            +> sepSpace
            +> genValueName
            +> genReturnType

        let short = prefix +> genExprKeepIndentInBranch astContext e

        let long =
            prefix
            +> autoIndentAndNlnExpressUnlessRagnarok (genExprKeepIndentInBranch astContext) e

        isShortExpression ctx.Config.MaxValueBindingWidth short long ctx)

// Example case: let ( *= ) a b = ()
// There must be spaces due to the *
// The idea is to solve this only where this can occur and not at the SynIdent level.
and genSynBindingFunctionName (functionName: SynLongIdent) =
    match functionName with
    | OperatorNameWithStar (text, synIdentRange, synLongIdentRange) ->
        !- $"( {text} )"
        |> genTriviaFor SynIdent_ synIdentRange
        |> genTriviaFor SynLongIdent_ synLongIdentRange
    | PrefixedOperatorNameWithStar (prefix, text, synIdentRange, synLongIdentRange) ->
        genSynIdent false prefix
        +> sepDot
        +> !- $"( {text} )"
        |> genTriviaFor SynIdent_ synIdentRange
        |> genTriviaFor SynLongIdent_ synLongIdentRange
    | _ -> genSynLongIdent false functionName

and genParenTupleWithIndentAndNewlines
    (astContext: ASTContext)
    (lpr: range)
    (ps: SynPat list)
    (rpr: range)
    (pr: range)
    : Context -> Context =
    genTriviaFor SynPat_Paren_OpeningParenthesis lpr sepOpenT
    +> indent
    +> sepNln
    +> col (sepComma +> sepNln) ps (genPat astContext)
    +> unindent
    +> sepNln
    +> genTriviaFor SynPat_Paren_ClosingParenthesis rpr sepCloseT
    |> genTriviaFor SynPat_Paren pr

and collectMultilineItemForSynExprKeepIndent (astContext: ASTContext) (e: SynExpr) : ColMultilineItem list =
    match e with
    | LetOrUses (bs, e) ->
        collectMultilineItemForLetOrUses astContext bs (collectMultilineItemForSynExprKeepIndent astContext e)
    | Sequentials es ->
        let lastIndex = es.Length - 1

        es
        |> List.mapi (fun idx e ->
            if idx = lastIndex then
                collectMultilineItemForSynExprKeepIndent astContext e
            else
                collectMultilineItemForSynExpr astContext e)
        |> List.collect id
    | KeepIndentMatch (matchKeywordRange, me, withRange, clauses, range, matchTriviaType) ->
        ColMultilineItem(
            genKeepIndentMatch astContext matchKeywordRange me withRange clauses range matchTriviaType,
            sepNlnConsideringTriviaContentBeforeForMainNode matchTriviaType range
        )
        |> List.singleton
    | KeepIndentIfThenElse (branches, elseBranch, ifElseRange) ->
        ColMultilineItem(
            genKeepIdentIf astContext branches elseBranch ifElseRange,
            sepNlnConsideringTriviaContentBeforeForMainNode SynExpr_IfThenElse ifElseRange
        )
        |> List.singleton
    | _ ->
        let t, r = synExprToFsAstType e
        [ ColMultilineItem(genExpr astContext e, sepNlnConsideringTriviaContentBeforeForMainNode t r) ]

and genExprKeepIndentInBranch (astContext: ASTContext) (e: SynExpr) : Context -> Context =
    let keepIndentExpr (ctx: Context) =
        let items = collectMultilineItemForSynExprKeepIndent astContext e

        colWithNlnWhenItemIsMultilineUsingConfig items ctx

    ifElseCtx (fun ctx -> ctx.Config.KeepIndentInBranch) keepIndentExpr (genExpr astContext e)

and genKeepIndentMatch
    (astContext: ASTContext)
    (matchKeyword: range)
    (e: SynExpr)
    (withRange: range)
    (clauses: SynMatchClause list)
    (range: Range)
    (triviaType: FsAstType)
    : Context -> Context =
    let lastClauseIndex = clauses.Length - 1
    let isMatchBang = triviaType = SynExpr_MatchBang

    ifElse
        isMatchBang
        (genTriviaFor SynExpr_MatchBang_Match matchKeyword !- "match! ")
        (genTriviaFor SynExpr_Match_Match matchKeyword !- "match ")
    +> autoIndentAndNlnWhenWriteBeforeNewlineNotEmpty (
        genExprInIfOrMatch astContext e
        +> genWithAfterMatch
            (if isMatchBang then
                 SynExpr_MatchBang_With
             else
                 SynExpr_Match_With)
            withRange
    )
    +> sepNln
    +> (fun ctx ->
        let hasMultipleClausesWhereOneHasRagnarok =
            hasMultipleClausesWhereOneHasRagnarok ctx.Config.Ragnarok clauses

        coli
            sepNln
            clauses
            (fun idx ->
                if idx < lastClauseIndex then
                    genClause astContext true hasMultipleClausesWhereOneHasRagnarok
                else
                    genLastClauseKeepIdent astContext)
            ctx)
    |> genTriviaFor triviaType range

and genLastClauseKeepIdent (astContext: ASTContext) (Clause (pat, whenExpr, arrowRange, expr)) =
    sepBar
    +> genPat astContext pat
    +> sepSpace
    +> optSingle (genExpr astContext) whenExpr
    +> optSingle
        (fun arrowRange ->
            sepArrowFixed
            |> genTriviaFor SynMatchClause_Arrow arrowRange)
        arrowRange
    +> sepNln
    +> (let t, r = synExprToFsAstType expr in sepNlnConsideringTriviaContentBeforeForMainNode t r)
    +> genExprKeepIndentInBranch astContext expr

and genKeepIdentIf
    (astContext: ASTContext)
    (branches: (range option * range * bool * SynExpr * range * SynExpr) list)
    (elseExpr: SynExpr)
    (ifElseRange: Range)
    =
    col sepNln branches (fun (elseKw, ifKw, isElif, ifExpr, thenKw, thenExpr) ->
        let genIf =
            let genKeywordStart =
                optSingle
                    (fun r ->
                        !- "else "
                        |> genTriviaFor SynExpr_IfThenElse_Else r)
                    elseKw
                +> (ifElse isElif (!- "elif ") (!- "if ")
                    |> genTriviaFor
                        (if isElif then
                             SynExpr_IfThenElse_Elif
                         else
                             SynExpr_IfThenElse_If)
                        ifKw)

            let short =
                genKeywordStart
                +> genExpr astContext ifExpr
                +> (!- " then"
                    |> genTriviaFor SynExpr_IfThenElse_Then thenKw)

            let long =
                genKeywordStart
                +> genExprInIfOrMatch astContext ifExpr
                +> sepSpace
                +> !- "then"

            expressionFitsOnRestOfLine short long

        genIf
        +> indent
        +> sepNln
        +> genExpr astContext thenExpr
        +> unindent)
    +> sepNln
    +> !- "else"
    +> sepNln
    +> (let t, r = synExprToFsAstType elseExpr in sepNlnConsideringTriviaContentBeforeForMainNode t r)
    +> genExprKeepIndentInBranch astContext elseExpr
    |> genTriviaFor SynExpr_IfThenElse ifElseRange

and genConst (c: SynConst) (r: Range) =
    match c with
    | SynConst.Unit ->
        let lpr, rpr = RangeHelpers.mkStartEndRange 1 r

        genTriviaFor SynConst_Unit_OpeningParenthesis lpr sepOpenT
        +> genTriviaFor SynConst_Unit_ClosingParenthesis rpr sepCloseT
        |> genTriviaFor SynConst_Unit r
    | SynConst.Bool b ->
        !-(if b then "true" else "false")
        |> genTriviaFor SynConst_Bool r
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
    | SynConst.String (s, kind, r) ->
        fun (ctx: Context) ->
            let expr =
                match ctx.FromSourceText r with
                | None -> genConstString kind s
                | Some s -> !-s

            genTriviaFor SynConst_String r expr ctx

    | SynConst.Char c ->
        fun (ctx: Context) ->
            let expr =
                match ctx.FromSourceText r with
                | Some c -> !-c
                | None ->
                    let escapedChar = Char.escape c
                    !- $"\'%s{escapedChar}\'"

            genTriviaFor SynConst_Char r expr ctx
    | SynConst.Bytes (bytes, _, r) ->
        // TODO: take kind into account
        genConstBytes bytes r
        |> genTriviaFor SynConst_Bytes r
    | SynConst.Measure (c, numberRange, m) ->
        let genNumber (ctx: Context) = genConstNumber c numberRange ctx

        genNumber +> genMeasure m
    | SynConst.SourceIdentifier (c, _, r) -> !-c |> genTriviaFor SynConst_SourceIdentifier r

and genConstNumber (c: SynConst) (r: Range) =
    fun (ctx: Context) ->
        let findNumberFromSourceText (fallback: Context -> Context) (nodeType: FsAstType) =
            let expr =
                match ctx.FromSourceText r with
                | None -> fallback
                | Some n -> !-n

            genTriviaFor nodeType r expr

        let expr =
            match c with
            | SynConst.Byte v -> findNumberFromSourceText (!- $"%A{v}") SynConst_Byte
            | SynConst.SByte v -> findNumberFromSourceText (!- $"%A{v}") SynConst_SByte
            | SynConst.Int16 v -> findNumberFromSourceText (!- $"%A{v}") SynConst_Int16
            | SynConst.Int32 v -> findNumberFromSourceText (!- $"%A{v}") SynConst_Int32
            | SynConst.Int64 v -> findNumberFromSourceText (!- $"%A{v}") SynConst_Int64
            | SynConst.UInt16 v -> findNumberFromSourceText (!- $"%A{v}") SynConst_UInt16
            | SynConst.UInt16s v -> findNumberFromSourceText (!- $"%A{v}") SynConst_UInt16s
            | SynConst.UInt32 v -> findNumberFromSourceText (!- $"%A{v}") SynConst_UInt32
            | SynConst.UInt64 v -> findNumberFromSourceText (!- $"%A{v}") SynConst_UInt64
            | SynConst.Double v -> findNumberFromSourceText (!- $"%A{v}") SynConst_Double
            | SynConst.Single v -> findNumberFromSourceText (!- $"%A{v}") SynConst_Single
            | SynConst.Decimal v -> findNumberFromSourceText (!- $"%A{v}") SynConst_Decimal
            | SynConst.IntPtr v -> findNumberFromSourceText (!- $"%A{v}") SynConst_IntPtr
            | SynConst.UIntPtr v -> findNumberFromSourceText (!- $"%A{v}") SynConst_UIntPtr
            | SynConst.UserNum (v, s) -> findNumberFromSourceText (!- $"%s{v}%s{s}") SynConst_UserNum
            | _ -> failwithf $"Cannot generating Const number for %A{c}"

        expr ctx

and genConstBytes (bytes: byte[]) (r: Range) =
    fun (ctx: Context) ->
        let expr =
            match ctx.FromSourceText r with
            | Some s -> !-s
            | None ->
                let content =
                    System.String(Array.map (fun (byte: byte) -> Convert.ToChar(byte)) bytes)

                !- $"\"{content}\"B"

        genTriviaFor SynConst_Bytes r expr ctx

and genConstString (stringKind: SynStringKind) (value: string) =
    let escaped = Regex.Replace(value, "\"{1}", "\\\"")

    let content =
        match stringKind with
        | SynStringKind.Regular -> String.Concat("\"", escaped, "\"")
        | SynStringKind.Verbatim -> String.Concat("@\"", value, "\"")
        | SynStringKind.TripleQuote -> String.Concat("\"\"\"", value, "\"\"\"")

    !-content

and genSynStaticOptimizationConstraint
    (astContext: ASTContext)
    (constraints: SynStaticOptimizationConstraint list)
    : Context -> Context =
    let genConstraint astContext con =
        match con with
        | SynStaticOptimizationConstraint.WhenTyparTyconEqualsTycon (t1, t2, _) ->
            genTypar astContext t1
            +> sepColon
            +> sepSpace
            +> genType astContext false t2
        | SynStaticOptimizationConstraint.WhenTyparIsStruct (t, _) -> genTypar astContext t

    !- " when "
    +> col sepSpace constraints (genConstraint astContext)

and genTriviaFor (mainNodeName: FsAstType) (range: Range) f ctx =
    (enterNodeFor mainNodeName range
     +> f
     +> leaveNodeFor mainNodeName range)
        ctx

and genTriviaForOption (mainNodeName: FsAstType) (range: range option) f ctx =
    match range with
    | None -> ctx
    | Some range -> genTriviaFor mainNodeName range f ctx

and genTriviaForOptionOr (mainNodeName: FsAstType) (range: range option) f ctx =
    match range with
    | None -> f ctx
    | Some range -> genTriviaFor mainNodeName range f ctx

and genLambdaArrowWithTrivia
    (bodyExpr: SynExpr -> Context -> Context)
    (body: SynExpr)
    (arrowRange: Range option)
    : Context -> Context =
    optSingle
        (fun arrowRange ->
            sepArrow
            |> genTriviaFor SynExpr_Lambda_Arrow arrowRange)
        arrowRange
    +> (fun ctx ->
        if String.isNotNullOrEmpty ctx.WriterModel.WriteBeforeNewline then
            (indent +> sepNln +> (bodyExpr body) +> unindent) ctx
        else
            autoIndentAndNlnIfExpressionExceedsPageWidthUnlessRagnarok bodyExpr body ctx)

and addSpaceBeforeClassConstructor expr =
    match expr with
    | Paren _
    | ConstExpr (SynConst.Unit, _) -> sepSpaceBeforeClassConstructor
    | _ -> sepSpace

and sepOpenTFor r =
    genTriviaFor SynExpr_Paren_OpeningParenthesis r sepOpenT

and sepCloseTFor (rpr: range option) (ctx: Context) =
    match rpr with
    | None -> ctx
    | Some rpr -> genTriviaFor SynExpr_Paren_ClosingParenthesis rpr sepCloseT ctx

and genEq (nodeType: FsAstType) (range: range option) =
    match range with
    | None -> sepEq
    | Some r -> genTriviaFor nodeType r sepEq

and genEqFixed (nodeType: FsAstType) (range: range option) =
    match range with
    | None -> sepEqFixed
    | Some r -> genTriviaFor nodeType r sepEqFixed

and genMeasure (measure: SynMeasure) =
    let rec loop measure =
        match measure with
        | SynMeasure.Var (Typar (s, _), _) -> !-s.idText
        | SynMeasure.Anon _ -> !- "_"
        | SynMeasure.One -> !- "1"
        | SynMeasure.Product (m1, m2, _) -> loop m1 +> !- "*" +> loop m2
        | SynMeasure.Divide (m1, m2, _) -> loop m1 +> !- "/" +> loop m2
        | SynMeasure.Power (m, RationalConst n, _) -> loop m +> !- $"^{n}"
        | SynMeasure.Seq (ms, _) -> col sepSpace ms loop
        | SynMeasure.Named (lid, _) -> genLongIdent lid
        | SynMeasure.Paren (m, _) -> sepOpenT +> loop m +> sepCloseT

    !- "<" +> loop measure +> !- ">"
