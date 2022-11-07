module internal Fantomas.Core.CodePrinter

open System
open System.Text.RegularExpressions
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Xml
open Fantomas.Core.AstExtensions
open Fantomas.Core.FormatConfig
open Fantomas.Core.SourceParser
open Fantomas.Core.SourceTransformer
open Fantomas.Core.Context
open Fantomas.Core.TriviaTypes

let rec addSpaceBeforeParensInFunCall functionOrMethod arg (ctx: Context) =
    match functionOrMethod, arg with
    | SynExpr.TypeApp(e, _, _, _, _, _, _), _ -> addSpaceBeforeParensInFunCall e arg ctx
    | SynExpr.Paren _, _ -> true
    | SynExpr.Const _, _ -> true
    | UppercaseSynExpr, ConstUnitExpr -> ctx.Config.SpaceBeforeUppercaseInvocation
    | LowercaseSynExpr, ConstUnitExpr -> ctx.Config.SpaceBeforeLowercaseInvocation
    | SynExpr.Ident _, SynExpr.Ident _ -> true
    | UppercaseSynExpr, Paren _ -> ctx.Config.SpaceBeforeUppercaseInvocation
    | LowercaseSynExpr, Paren _ -> ctx.Config.SpaceBeforeLowercaseInvocation
    | _ -> true

let addSpaceBeforeParenInPattern (sli: SynLongIdent) (ctx: Context) =
    match List.tryLast sli.LongIdent with
    | None -> sepSpace ctx
    | Some ident when String.IsNullOrWhiteSpace ident.idText -> sepSpace ctx
    | Some ident ->
        let parameterValue =
            if Char.IsUpper ident.idText.[0] then
                ctx.Config.SpaceBeforeUppercaseInvocation
            else
                ctx.Config.SpaceBeforeLowercaseInvocation

        onlyIf parameterValue sepSpace ctx

let addSpaceBeforeParensInFunDef (spaceBeforeSetting: bool) (functionOrMethod: SynLongIdent) args =
    match functionOrMethod, args with
    | SynLongIdent(id = [ newIdent ]), _ when newIdent.idText = "new" -> false
    | _, PatParen _ -> spaceBeforeSetting
    | _, PatNamed _
    | _, SynPat.Wild _ -> true
    | SynLongIdent(id = lid), _ ->
        match List.tryLast lid with
        | None -> false
        | Some ident -> not (Char.IsUpper ident.idText.[0])
    | _ -> true

let rec genParsedInput ast =
    let genParsedInput =
        match ast with
        | ImplFile im -> genImpFile im
        | SigFile si -> genSigFile si

    genTriviaFor ParsedInput_ ast.FullRange genParsedInput +> addFinalNewline

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
    See https://github.com/dotnet/fsharp/blob/main/src/Compiler/SyntaxTree/SyntaxTree.fs
    hs = hashDirectives : ParsedHashDirective list
    mns = modules : SynModuleOrNamespace list
*)
and genImpFile (ParsedImplFileInput(hs, mns, _, _)) =
    col sepNln hs genParsedHashDirective
    +> (if hs.IsEmpty then sepNone else sepNln)
    +> col sepNln mns genModuleOrNamespace

and genSigFile (ParsedSigFileInput(hs, mns, _, _)) =
    col sepNone hs genParsedHashDirective
    +> (if hs.IsEmpty then sepNone else sepNln)
    +> col sepNln mns genSigModuleOrNamespace

and genParsedHashDirective (ParsedHashDirective(h, args, r)) =
    let genArg (arg: ParsedHashDirectiveArgument) =
        match arg with
        | ParsedHashDirectiveArgument.String(value, stringKind, range) ->
            genConstString stringKind value
            |> genTriviaFor ParsedHashDirectiveArgument_String range
        | ParsedHashDirectiveArgument.SourceIdentifier(identifier, _, range) ->
            !-identifier |> genTriviaFor ParsedHashDirectiveArgument_String range

    !- "#" +> !-h +> sepSpace +> col sepSpace args genArg
    |> genTriviaFor ParsedHashDirective_ r

and genModuleOrNamespaceKind (leadingKeyword: SynModuleOrNamespaceLeadingKeyword) (kind: SynModuleOrNamespaceKind) =
    match leadingKeyword with
    | SynModuleOrNamespaceLeadingKeyword.Namespace range ->
        if kind = SynModuleOrNamespaceKind.GlobalNamespace then
            genTriviaFor SynModuleOrNamespace_Namespace range !- "namespace" +> !- " global"
        else
            genTriviaFor SynModuleOrNamespace_Namespace range !- "namespace "
    | SynModuleOrNamespaceLeadingKeyword.Module range -> genTriviaFor SynModuleOrNamespace_Module range !- "module "
    | SynModuleOrNamespaceLeadingKeyword.None -> sepNone

and genModuleOrNamespace (ModuleOrNamespace(ats, px, leadingKeyword, ao, lids, mds, isRecursive, moduleKind, range)) =
    let sepModuleAndFirstDecl =
        let firstDecl = List.tryHead mds

        match firstDecl with
        | None -> sepNone
        | Some mdl ->
            sepNln
            +> sepNlnConsideringTriviaContentBeforeFor (synModuleDeclToFsAstType mdl) mdl.Range

    let moduleOrNamespace =
        genModuleOrNamespaceKind leadingKeyword moduleKind
        +> genAccessOpt ao
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
    +> genAttributes ats
    +> ifElse
        (moduleKind = SynModuleOrNamespaceKind.AnonModule)
        genTriviaForAnonModuleIdent
        (moduleOrNamespace +> sepModuleAndFirstDecl)
    +> genModuleDeclList mds
    |> (match moduleKind with
        | SynModuleOrNamespaceKind.AnonModule -> genTriviaFor SynModuleOrNamespace_AnonModule range
        | SynModuleOrNamespaceKind.DeclaredNamespace -> genTriviaFor SynModuleOrNamespace_DeclaredNamespace range
        | SynModuleOrNamespaceKind.GlobalNamespace -> genTriviaFor SynModuleOrNamespace_GlobalNamespace range
        | SynModuleOrNamespaceKind.NamedModule -> genTriviaFor SynModuleOrNamespace_NamedModule range)

and genSigModuleOrNamespace
    (SigModuleOrNamespace(ats, px, leadingKeyword, ao, lids, mds, isRecursive, moduleKind, range))
    =
    let sepModuleAndFirstDecl =
        let firstDecl = List.tryHead mds

        match firstDecl with
        | None -> sepNone
        | Some mdl ->
            sepNln
            +> sepNlnConsideringTriviaContentBeforeFor (synModuleSigDeclToFsAstType mdl) mdl.Range

    let moduleOrNamespace =
        genModuleOrNamespaceKind leadingKeyword moduleKind
        +> genAccessOpt ao
        +> ifElse isRecursive (!- "rec ") sepNone
        +> genLongIdent lids

    genPreXmlDoc px
    +> genAttributes ats
    +> ifElse (moduleKind = SynModuleOrNamespaceKind.AnonModule) sepNone (moduleOrNamespace +> sepModuleAndFirstDecl)
    +> genSigModuleDeclList mds
    |> (match moduleKind with
        | SynModuleOrNamespaceKind.AnonModule -> genTriviaFor SynModuleOrNamespaceSig_AnonModule range
        | SynModuleOrNamespaceKind.DeclaredNamespace -> genTriviaFor SynModuleOrNamespaceSig_DeclaredNamespace range
        | SynModuleOrNamespaceKind.GlobalNamespace -> genTriviaFor SynModuleOrNamespaceSig_GlobalNamespace range
        | SynModuleOrNamespaceKind.NamedModule -> genTriviaFor SynModuleOrNamespaceSig_NamedModule range)

and genModuleDeclList e =
    let rec collectItems
        (e: SynModuleDecl list)
        (finalContinuation: ColMultilineItem list -> ColMultilineItem list)
        : ColMultilineItem list =
        match e with
        | [] -> finalContinuation []
        | OpenL(xs, ys) ->
            let expr = col sepNln xs genModuleDecl

            let r, triviaType =
                List.head xs |> fun mdl -> mdl.Range, synModuleDeclToFsAstType mdl
            // SynModuleDecl.Open cannot have attributes
            let sepNln = sepNlnConsideringTriviaContentBeforeFor triviaType r

            collectItems ys (fun ysItems -> ColMultilineItem(expr, sepNln) :: ysItems |> finalContinuation)

        | HashDirectiveL(xs, ys) ->
            let expr = col sepNln xs genModuleDecl
            let r = List.head xs |> fun mdl -> mdl.Range
            // SynModuleDecl.HashDirective cannot have attributes
            let sepNln = sepNlnConsideringTriviaContentBeforeFor SynModuleDecl_HashDirective r
            collectItems ys (fun ysItems -> ColMultilineItem(expr, sepNln) :: ysItems |> finalContinuation)

        | AttributesL(xs, y :: rest) ->
            let expr =
                col sepNln xs genModuleDecl
                +> sepNlnConsideringTriviaContentBeforeFor (synModuleDeclToFsAstType y) y.Range
                +> genModuleDecl y

            let r = List.head xs |> fun mdl -> mdl.Range

            let sepNln = sepNlnConsideringTriviaContentBeforeFor SynModuleDecl_Attributes r

            collectItems rest (fun restItems -> ColMultilineItem(expr, sepNln) :: restItems |> finalContinuation)

        | m :: rest ->
            let sepNln =
                sepNlnConsideringTriviaContentBeforeFor (synModuleDeclToFsAstType m) m.Range

            let expr = genModuleDecl m

            collectItems rest (fun restItems -> ColMultilineItem(expr, sepNln) :: restItems |> finalContinuation)

    collectItems e id |> colWithNlnWhenItemIsMultiline

and genSigModuleDeclList (e: SynModuleSigDecl list) =
    let rec collectItems
        (e: SynModuleSigDecl list)
        (finalContinuation: ColMultilineItem list -> ColMultilineItem list)
        : ColMultilineItem list =
        match e with
        | [] -> finalContinuation []
        | SigOpenL(xs, ys) ->
            let expr = col sepNln xs genSigModuleDecl

            let r, triviaType =
                List.head xs |> fun mdl -> mdl.Range, synModuleSigDeclToFsAstType mdl
            // SynModuleSigDecl.Open cannot have attributes
            let sepNln = sepNlnConsideringTriviaContentBeforeFor triviaType r

            collectItems ys (fun ysItems -> ColMultilineItem(expr, sepNln) :: ysItems |> finalContinuation)

        | SigHashDirectiveL(xs, ys) ->
            let expr = col sepNln xs genSigModuleDecl
            let r = List.head xs |> fun mdl -> mdl.Range

            let sepNln =
                sepNlnConsideringTriviaContentBeforeFor SynModuleSigDecl_HashDirective r

            collectItems ys (fun ysItems -> ColMultilineItem(expr, sepNln) :: ysItems |> finalContinuation)

        | s :: rest ->
            let sepNln =
                sepNlnConsideringTriviaContentBeforeFor (synModuleSigDeclToFsAstType s) s.Range

            let expr = genSigModuleDecl s
            collectItems rest (fun restItems -> ColMultilineItem(expr, sepNln) :: restItems |> finalContinuation)

    collectItems e id |> colWithNlnWhenItemIsMultiline

and genModuleDecl (node: SynModuleDecl) =
    match node with
    | Attributes ats ->
        fun (ctx: Context) ->
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
                                (sepNlnConsideringTriviaContentBeforeFor SynModuleDecl_Attributes a.Range)
                            +> ((col sepNln a.Attributes genAttribute) |> genTriviaFor SynAttributeList_ a.Range)

                        let hasContentAfter = ctx.HasContentAfter(SynAttributeList_, a.Range)
                        (hasContentAfter, prevExpr +> expr))
                    (true, sepNone)
                    ats
                |> snd

            attributesExpr ctx
    | DeclExpr e -> genExpr e
    | Exception ex -> genException ex
    | HashDirective p -> genParsedHashDirective p
    | Let(ExternBinding eb) -> genExternBinding eb
    | Let b -> genSynBinding b
    | LetRec bs -> genSynBindings bs false
    | ModuleAbbrev(ident, lid) -> !- "module " +> genIdent ident +> sepEq +> sepSpace +> genLongIdent lid
    | NamespaceFragment m -> failwithf "NamespaceFragment hasn't been implemented yet: %O" m
    | NestedModule(ats, px, moduleKeyword, ao, lid, isRecursive, equalsRange, mds) ->
        genPreXmlDoc px
        +> genAttributes ats
        +> genTriviaForOption SynModuleDecl_NestedModule_Module moduleKeyword (!- "module ")
        +> genAccessOpt ao
        +> ifElse isRecursive (!- "rec ") sepNone
        +> genLongIdent lid
        +> genEq SynModuleDecl_NestedModule_Equals equalsRange
        +> indent
        +> sepNln
        +> genModuleDeclList mds
        +> unindent

    | Open lid -> !- "open " +> genSynLongIdent false lid
    | OpenType lid -> !- "open type " +> genSynLongIdent false lid
    // There is no nested types and they are recursive if there are more than one definition
    | Types ts ->
        let items =
            List.map
                (fun (t: SynTypeDefn) ->
                    ColMultilineItem(genTypeDefn t, sepNlnConsideringTriviaContentBeforeFor SynTypeDefn_ t.Range))
                ts

        colWithNlnWhenItemIsMultilineUsingConfig items
    | md -> failwithf "Unexpected module declaration: %O" md
    |> genTriviaFor (synModuleDeclToFsAstType node) node.Range

and genSigModuleDecl node =
    match node with
    | SigException ex -> genSigException ex
    | SigHashDirective p -> genParsedHashDirective p
    | SigVal v -> genVal v None
    | SigModuleAbbrev(ident, lid) -> !- "module " +> genIdent ident +> sepEq +> sepSpace +> genLongIdent lid
    | SigNamespaceFragment m -> failwithf "NamespaceFragment is not supported yet: %O" m
    | SigNestedModule(ats, px, moduleKeyword, ao, lid, equalsRange, mds) ->
        genPreXmlDoc px
        +> genAttributes ats
        +> genTriviaForOption SynModuleSigDecl_NestedModule_Module moduleKeyword !- "module "
        +> genAccessOpt ao
        +> genLongIdent lid
        +> genEq SynModuleSigDecl_NestedModule_Equals equalsRange
        +> indent
        +> sepNln
        +> genSigModuleDeclList mds
        +> unindent

    | SigOpen lid -> !- "open " +> genSynLongIdent false lid
    | SigOpenType sli -> !- "open type " +> genSynLongIdent false sli
    | SigTypes ts ->
        let items =
            List.map
                (fun (t: SynTypeDefnSig) ->
                    let sepNln = sepNlnConsideringTriviaContentBeforeFor SynTypeDefnSig_ t.Range
                    ColMultilineItem(genSigTypeDefn t, sepNln))
                ts

        colWithNlnWhenItemIsMultilineUsingConfig items
    | md -> failwithf "Unexpected module signature declaration: %O" md
    |> genTriviaFor (synModuleSigDeclToFsAstType node) node.Range

and genIdent (ident: Ident) =
    let width = ident.idRange.EndColumn - ident.idRange.StartColumn

    let genIdent =
        if ident.idText.Length + 4 = width then
            // add backticks
            !- $"``{ident.idText}``"
        else
            !-ident.idText

    genTriviaFor Ident_ ident.idRange genIdent

and genLongIdent (lid: LongIdent) =
    col sepDot lid genIdent |> genTriviaFor LongIdent_ (longIdentFullRange lid)

and genSynIdent (addDot: bool) (synIdent: SynIdent) =
    let (SynIdent(ident, trivia)) = synIdent

    match trivia with
    | Some(IdentTrivia.OriginalNotation text) -> !-text
    | Some(IdentTrivia.OriginalNotationWithParen(_, text, _)) -> !- $"({text})"
    | Some(IdentTrivia.HasParenthesis _) -> !- $"({ident.idText})"
    | None -> genIdent ident

    |> fun genSy -> genTriviaFor SynIdent_ synIdent.FullRange (onlyIf addDot sepDot +> genSy)

and genSynLongIdent (addLeadingDot: bool) (longIdent: SynLongIdent) =
    let lastIndex = longIdent.IdentsWithTrivia.Length - 1

    coli sepNone longIdent.IdentsWithTrivia (fun idx si ->
        genSynIdent (addLeadingDot || idx > 0) si
        +> onlyIf (idx < lastIndex) sepNlnWhenWriteBeforeNewlineNotEmpty)
    |> genTriviaFor SynLongIdent_ longIdent.FullRange

and genSynLongIdentMultiline (addLeadingDot: bool) (longIdent: SynLongIdent) =
    coli sepNln longIdent.IdentsWithTrivia (fun idx -> genSynIdent (idx > 0 || addLeadingDot))
    |> genTriviaFor SynLongIdent_ longIdent.FullRange

// TODO: multiple keyword should be split up!
and genSynLeadingKeyword (lk: SynLeadingKeyword) =
    match lk with
    | SynLeadingKeyword.Let _ -> genTriviaFor SynLeadingKeyword_Let lk.Range !- "let "
    | SynLeadingKeyword.LetRec(_letRange, _recRange) -> !- "let rec " |> genTriviaFor SynLeadingKeyword_LetRec lk.Range
    | SynLeadingKeyword.And _ -> genTriviaFor SynLeadingKeyword_And lk.Range !- "and "
    | SynLeadingKeyword.Use _ -> genTriviaFor SynLeadingKeyword_Use lk.Range !- "use "
    | SynLeadingKeyword.UseRec(_useRange, _recRange) -> genTriviaFor SynLeadingKeyword_UseRec lk.Range !- "use rec "
    | SynLeadingKeyword.Extern _ -> genTriviaFor SynLeadingKeyword_Extern lk.Range !- "extern "
    | SynLeadingKeyword.Member _ -> genTriviaFor SynLeadingKeyword_Member lk.Range !- "member "
    | SynLeadingKeyword.MemberVal(_memberRange, _valRange) ->
        genTriviaFor SynLeadingKeyword_MemberVal lk.Range !- "member val "
    | SynLeadingKeyword.Override _ -> genTriviaFor SynLeadingKeyword_Override lk.Range !- "override "
    | SynLeadingKeyword.OverrideVal(_overrideRange, _valRange) ->
        genTriviaFor SynLeadingKeyword_OverrideVal lk.Range !- "override val "
    | SynLeadingKeyword.Abstract _ -> genTriviaFor SynLeadingKeyword_Abstract lk.Range !- "abstract "
    | SynLeadingKeyword.AbstractMember(_abstractRange, _memberRange) ->
        genTriviaFor SynLeadingKeyword_AbstractMember lk.Range !- "abstract member "
    | SynLeadingKeyword.StaticMember(_staticRange, _memberRange) ->
        genTriviaFor SynLeadingKeyword_StaticMember lk.Range !- "static member "
    | SynLeadingKeyword.StaticMemberVal(_staticRange, _memberRange, _valRange) ->
        genTriviaFor SynLeadingKeyword_StaticMemberVal lk.Range !- "static member val "
    | SynLeadingKeyword.StaticAbstract(_staticRange, _abstractRange) ->
        genTriviaFor SynLeadingKeyword_StaticAbstract lk.Range !- "static abstract "
    | SynLeadingKeyword.StaticAbstractMember(_staticRange, _abstractMember, _memberRange) ->
        genTriviaFor SynLeadingKeyword_StaticAbstractMember lk.Range !- "static abstract member "
    | SynLeadingKeyword.StaticVal(_staticRange, _valRange) ->
        genTriviaFor SynLeadingKeyword_StaticVal lk.Range !- "static val "
    | SynLeadingKeyword.StaticLet(_staticRange, _letRange) ->
        genTriviaFor SynLeadingKeyword_StaticLet lk.Range !- "static let "
    | SynLeadingKeyword.StaticLetRec(_staticRange, _letRange, _recRange) ->
        genTriviaFor SynLeadingKeyword_StaticLetRec lk.Range !- "static let rec "
    | SynLeadingKeyword.StaticDo(_staticRange, _doRange) ->
        genTriviaFor SynLeadingKeyword_StaticLetRec lk.Range !- "static do "
    | SynLeadingKeyword.Default _ -> genTriviaFor SynLeadingKeyword_Default lk.Range !- "default "
    | SynLeadingKeyword.DefaultVal(_defaultRange, _valRange) ->
        genTriviaFor SynLeadingKeyword_DefaultVal lk.Range !- "default val "
    | SynLeadingKeyword.Val _ -> genTriviaFor SynLeadingKeyword_Val lk.Range !- "val "
    | SynLeadingKeyword.New _ -> genTriviaFor SynLeadingKeyword_New lk.Range !- "new"
    | SynLeadingKeyword.Do _ -> genTriviaFor SynLeadingKeyword_Do lk.Range !- "do "
    | SynLeadingKeyword.Synthetic -> sepNone

and genAccess (vis: SynAccess) =
    match vis with
    | SynAccess.Public r -> genTriviaFor SynAccess_Public r !- "public"
    | SynAccess.Internal r -> genTriviaFor SynAccess_Internal r !- "internal"
    | SynAccess.Private r -> genTriviaFor SynAccess_Private r !- "private"

and genAccessOpt (ao: SynAccess option) =
    optSingle (fun ao -> genAccess ao +> sepSpace) ao

and genAttribute (Attribute(sli, e, target)) =
    match e with
    // Special treatment for function application on attributes
    | ConstUnitExpr -> !- "[<" +> opt sepColon target genIdent +> genSynLongIdent false sli +> !- ">]"
    | e ->
        let argSpacing = if hasParenthesis e then sepNone else sepSpace

        !- "[<"
        +> opt sepColon target genIdent
        +> genSynLongIdent false sli
        +> argSpacing
        +> genExpr e
        +> !- ">]"

and genAttributesCore (ats: SynAttribute seq) =
    let genAttributeExpr (Attribute(sli, e, target) as attr) =
        match e with
        | ConstUnitExpr -> opt sepColon target genIdent +> genSynLongIdent false sli
        | e ->
            let argSpacing = if hasParenthesis e then sepNone else sepSpace

            opt sepColon target genIdent
            +> genSynLongIdent false sli
            +> argSpacing
            +> genExpr e
        |> genTriviaFor SynAttribute_ attr.Range

    let shortExpression =
        !- "[<" +> atCurrentColumn (col sepSemi ats genAttributeExpr) +> !- ">]"

    let longExpression =
        !- "[<"
        +> atCurrentColumn (col (sepSemi +> sepNln) ats genAttributeExpr)
        +> !- ">]"

    ifElse (Seq.isEmpty ats) sepNone (expressionFitsOnRestOfLine shortExpression longExpression)

and genOnelinerAttributes ats =
    let ats = List.collect (fun (a: SynAttributeList) -> a.Attributes) ats
    ifElse (Seq.isEmpty ats) sepNone (genAttributesCore ats +> sepSpace)

/// Try to group attributes if they are on the same line
/// Separate same-line attributes by ';'
/// Each bucket is printed in a different line
and genAttributes (ats: SynAttributes) =
    colPost sepNlnUnlessLastEventIsNewline sepNln ats (fun a ->
        (genAttributesCore a.Attributes |> genTriviaFor SynAttributeList_ a.Range)
        +> sepNlnWhenWriteBeforeNewlineNotEmpty)

and genPreXmlDoc (PreXmlDoc(lines, _)) =
    colPost sepNln sepNln lines (sprintf "///%s" >> (!-))

and genExprSepEqPrependType (equalsAstType: FsAstType) (equalsRange: range option) (e: SynExpr) =
    match e with
    | TypedExpr(Typed, e, t) ->
        sepColon
        +> genType t
        +> genEq equalsAstType equalsRange
        +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (genExpr e)
    | _ ->
        genEq equalsAstType equalsRange
        +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (genExpr e)

and genTypeSupportMember st =
    match st with
    | SynType.Var(td, _) -> genTypar td
    | TLongIdent sli -> genSynLongIdent false sli
    | _ -> !- ""

and genTypeAndParam (typeName: Context -> Context) (tds: SynTyparDecls option) tcs =
    let types openSep tds tcs closeSep =
        (openSep
         +> coli sepComma tds (fun i -> genTyparDecl (i = 0))
         +> genSynTypeConstraintList tcs
         +> closeSep)

    match tds with
    | None -> typeName
    | Some(PostfixList(gt, tds, tcs, lt, _)) ->
        typeName
        +> types
            (genTriviaFor SynTyparDecls_PostfixList_Greater gt !- "<")
            tds
            tcs
            (genTriviaFor SynTyparDecls_PostfixList_Lesser lt !- ">")
    | Some(SynTyparDecls.PostfixList _) -> sepNone // captured above
    | Some(SynTyparDecls.PrefixList(tds, _range)) -> types (!- "(") tds [] (!- ")") +> !- " " +> typeName
    | Some(SynTyparDecls.SinglePrefix(td, _range)) -> genTyparDecl true td +> sepSpace +> typeName
    +> colPre (!- " when ") wordAnd tcs genTypeConstraint

and genTypeParamPostfix tds =
    match tds with
    | Some(PostfixList(gt, tds, tcs, lt, _range)) ->
        (genTriviaFor SynTyparDecls_PostfixList_Greater gt !- "<")
        +> coli sepComma tds (fun i -> genTyparDecl (i = 0))
        +> genSynTypeConstraintList tcs
        +> (genTriviaFor SynTyparDecls_PostfixList_Lesser lt !- ">")
    | _ -> sepNone

and genSynTypeConstraintList tcs =
    match tcs with
    | [] -> sepNone
    | _ ->
        let short = colPre (sepSpace +> !- "when ") wordAnd tcs genTypeConstraint

        let long =
            colPre (!- "when ") (sepNln +> wordAndFixed +> sepSpace) tcs genTypeConstraint

        autoIndentAndNlnIfExpressionExceedsPageWidth (expressionFitsOnRestOfLine short long)

and genExternBinding (leadingKeyword, ao, attrs, px, pat, rt) =
    let rec genTypeForExtern t =
        match t with
        | TApp(TLongIdent(SynLongIdent([ _ ], [], [ Some(IdentTrivia.OriginalNotation "*") ])), _, [ t ], _, true, range) ->
            genType t +> !- "*" |> genTriviaFor SynType_App range
        | TApp(TLongIdent(SynLongIdent([ _ ], [], [ Some(IdentTrivia.OriginalNotation "&") ])), _, [ t ], _, true, range) ->
            genType t +> !- "&" |> genTriviaFor SynType_App range
        | TApp(TArrayInExtern arrayText, None, [ TApp(t, _, _, _, _, _) ], None, true, _) ->
            genTypeForExtern t +> !-arrayText
        | TArrayInExtern arrayText -> !-arrayText
        | _ -> genType t

    let rec genPatForExtern pat =
        match pat with
        | PatAttrib(PatTyped(PatNullary _, t), attrs) -> genOnelinerAttributes attrs +> genTypeForExtern t
        | PatAttrib(PatTyped(p, t), attrs) -> genOnelinerAttributes attrs +> genTypeForExtern t +> sepSpace +> genPat p
        | PatLongIdent(_, sli, [ PatTuple ps ], _) ->
            let short =
                genSynLongIdent false sli
                +> sepOpenT
                +> col sepComma ps genPatForExtern
                +> sepCloseT

            let long =
                genSynLongIdent false sli
                +> sepOpenT
                +> indentSepNlnUnindent (col (sepComma +> sepNln) ps genPatForExtern)
                +> sepNln
                +> sepCloseT

            expressionFitsOnRestOfLine short long
        | _ -> genPat pat

    genPreXmlDoc px
    +> genAttributes attrs
    +> genSynLeadingKeyword leadingKeyword
    +> optSingle (fun (a, t) -> genOnelinerAttributes a +> genType t +> sepSpace) rt
    +> genAccessOpt ao
    +> genPatForExtern pat
    +> sepSpace

and genProperty (getOrSetType: FsAstType, getOrSetRange: range, binding: SynBinding) =
    let genGetOrSet =
        let getOrSetText =
            match getOrSetType with
            | SynMemberDefn_GetSetMember_Get -> "get"
            | SynMemberDefn_GetSetMember_Set -> "set"
            | _ -> failwith "expected \"get\" or \"set\""

        genTriviaFor getOrSetType getOrSetRange !-getOrSetText +> sepSpace

    match binding with
    | SynBinding(headPat = PatLongIdent(ao, _, ps, _); expr = e; trivia = { EqualsRange = equalsRange }) ->
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

            genAccessOpt ao
            +> genGetOrSet
            +> ifElse
                (List.atMostOne ps)
                (col sepComma ps genPat +> sepSpace)
                (sepOpenT +> col sepComma ps genPat +> sepCloseT +> sepSpace)
            +> genPat p
            +> genExprSepEqPrependType SynBinding_Equals equalsRange e

        | ps ->
            genAccessOpt ao
            +> genGetOrSet
            +> col sepSpace ps genPat
            +> genExprSepEqPrependType SynBinding_Equals equalsRange e
    | _ -> sepNone

and genMemberBinding b =
    match b with
    | ExplicitCtor(ats, px, ao, p, equalsRange, e, io) ->
        let prefix =
            let genPat ctx =
                match p with
                | PatExplicitCtor(ao, pat) ->
                    (genAccessOpt ao +> !- "new" +> sepSpaceBeforeClassConstructor +> genPat pat) ctx
                | _ -> genPat p ctx

            genPreXmlDoc px
            +> genAttributes ats
            +> genAccessOpt ao
            +> genPat
            +> optSingle (fun ident -> !- " as " +> genIdent ident) io

        match e with
        // Handle special "then" block i.e. fake sequential expressions in constructors
        | Sequential(e1, e2, false) ->
            prefix
            +> genEq SynBinding_Equals equalsRange
            +> indent
            +> sepNln
            +> genExpr e1
            +> sepNln
            +> !- "then "
            +> autoIndentAndNlnIfExpressionExceedsPageWidth (genExpr e2)
            +> unindent

        | e ->
            prefix
            +> genEq SynBinding_Equals equalsRange
            +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (genExpr e)

    | _ -> genSynBinding b

and genVal
    (Val(ats, px, leadingKeyword, ao, si, t, _, isInline, isMutable, tds, eo, range))
    (optGetSet: string option)
    =
    let typeName = genTypeAndParam (genSynIdent false si) tds []
    let hasGenerics = Option.isSome tds

    let lk =
        match leadingKeyword with
        | SynLeadingKeyword.New _ ->
            // new is also the name of the type
            sepNone
        | _ -> genSynLeadingKeyword leadingKeyword

    genPreXmlDoc px
    +> genAttributes ats
    +> (lk
        +> onlyIf isInline (!- "inline ")
        +> onlyIf isMutable (!- "mutable ")
        +> genAccessOpt ao
        +> typeName)
    +> ifElse hasGenerics sepColonWithSpacesFixed sepColon
    +> genTypeInSignature t
    +> optSingle (!-) optGetSet
    +> optSingle (fun e -> sepEq +> sepSpace +> genExpr e) eo
    |> genTriviaFor SynValSig_ range

and genTypeInSignature (FunType ts) =
    match ts with
    | [ TWithGlobalConstraints(t, tcs), None ] -> genConstraints t tcs
    | ts -> autoIndentAndNlnIfExpressionExceedsPageWidth (genTypeList ts)

and genRecordFieldName (SynExprRecordField((rfn, _), equalsRange, eo, _blockSeparator) as rf) =
    opt sepNone eo (fun e ->
        let expr = sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (genExpr e)

        genSynLongIdent false rfn +> genEq SynExprRecordField_Equals equalsRange +> expr)
    |> genTriviaFor SynExprRecordField_ rf.FullRange

and genAnonRecordFieldName (AnonRecordFieldName(ident, equalsRange, e, range)) =
    let expr = sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (genExpr e)

    genIdent ident +> genEq SynExpr_AnonRecd_Field_Equals equalsRange +> expr
    |> genTriviaFor SynExpr_AnonRecd_Field range

and genTuple es =
    let genShortExpr e = addParenForTupleWhen genExpr e

    let shortExpression = col sepComma es genShortExpr

    let longExpression = genTupleMultiline es

    atCurrentColumn (expressionFitsOnRestOfLine shortExpression longExpression)

and genTupleMultiline es =
    let containsLambdaOrMatchExpr =
        es
        |> List.pairwise
        |> List.exists (function
            | SynExpr.Match _, _
            | SynExpr.Lambda _, _
            | InfixApp(_, _, _, SynExpr.Lambda _, _), _ -> true
            | _ -> false)

    let sep =
        if containsLambdaOrMatchExpr then
            (sepNln +> sepComma)
        else
            (sepCommaFixed +> sepNln)

    let lastIndex = List.length es - 1

    let genExpr idx e =
        match e with
        | SynExpr.IfThenElse _ when (idx < lastIndex) -> autoParenthesisIfExpressionExceedsPageWidth (genExpr e)
        | InfixApp(equal, operatorSli, e1, e2, range) when (equal = "=") -> genNamedArgumentExpr operatorSli e1 e2 range
        | _ -> genExpr e

    coli sep es genExpr

and genNamedArgumentExpr (operatorSli: SynLongIdent) e1 e2 appRange =
    let short =
        genExpr e1
        +> sepSpace
        +> genSynLongIdent false operatorSli
        +> sepSpace
        +> genExpr e2

    let long =
        genExpr e1
        +> sepSpace
        +> genSynLongIdent false operatorSli
        +> autoIndentAndNlnExpressUnlessStroustrup (fun e -> sepSpace +> genExpr e) e2

    expressionFitsOnRestOfLine short long |> genTriviaFor SynExpr_App appRange

and genExpr synExpr ctx =
    let expr =
        match synExpr with
        | LazyExpr(lazyKeyword, e) ->
            let isInfixExpr =
                match e with
                | InfixApp _ -> true
                | _ -> false

            let genInfixExpr (ctx: Context) =
                isShortExpression
                    ctx.Config.MaxInfixOperatorExpression
                    // if this fits on the rest of line right after the lazy keyword, it should be wrapped in parenthesis.
                    (sepOpenT +> genExpr e +> sepCloseT)
                    // if it is multiline there is no need for parenthesis, because of the indentation
                    (indent +> sepNln +> genExpr e +> unindent)
                    ctx

            let genNonInfixExpr = autoIndentAndNlnIfExpressionExceedsPageWidth (genExpr e)

            genTriviaFor SynExpr_Lazy_Lazy lazyKeyword !- "lazy "
            +> ifElse isInfixExpr genInfixExpr genNonInfixExpr

        | SingleExpr(kind, e) ->
            let mapping =
                (match kind with
                 | YieldFrom _
                 | Yield _
                 | Return _
                 | ReturnFrom _
                 | Do _
                 | DoBang _ -> autoIndentAndNlnIfExpressionExceedsPageWidthUnlessStroustrup genExpr e
                 | _ -> autoIndentAndNlnIfExpressionExceedsPageWidth (genExpr e))

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

        | ConstExpr(c, r) -> genConst c r
        | NullExpr -> !- "null"
        // Not sure about the role of e1
        | Quote(_, e2, isRaw) ->
            let e =
                expressionFitsOnRestOfLine (genExpr e2) (indent +> sepNln +> genExpr e2 +> unindent +> sepNln)

            ifElse
                isRaw
                (!- "<@@" +> sepSpace +> e +> sepSpace +> !- "@@>")
                (!- "<@" +> sepSpace +> e +> sepSpace +> !- "@>")
        | TypedExpr(TypeTest, e, t) -> genExpr e +> !- " :? " +> genType t
        | TypedExpr(Downcast, e, t) ->
            let shortExpr = genExpr e +> !- " :?> " +> genType t

            let longExpr = genExpr e +> sepNln +> !- ":?> " +> genType t

            expressionFitsOnRestOfLine shortExpr longExpr
        | TypedExpr(Upcast, e, t) ->
            let shortExpr = genExpr e +> !- " :> " +> genType t

            let longExpr = genExpr e +> sepNln +> !- ":> " +> genType t

            expressionFitsOnRestOfLine shortExpr longExpr
        | TypedExpr(Typed, (Lambda _ as e), t) -> genExpr e +> sepNln +> sepColon +> genType t
        | TypedExpr(Typed, e, t) -> genExpr e +> sepColon +> genType t
        | NewTuple(t, px) ->
            let sepSpace (ctx: Context) =
                match t with
                | UppercaseSynType -> onlyIf ctx.Config.SpaceBeforeUppercaseInvocation sepSpace ctx
                | LowercaseSynType -> onlyIf ctx.Config.SpaceBeforeLowercaseInvocation sepSpace ctx

            let short = !- "new " +> genType t +> sepSpace +> genExpr px

            let long =
                !- "new "
                +> genType t
                +> sepSpace
                +> genMultilineFunctionApplicationArguments px

            expressionFitsOnRestOfLine short long
        | SynExpr.New(_, t, e, _) -> !- "new " +> genType t +> sepSpace +> genExpr e
        | Tuple(es, _) -> genTuple es
        | StructTuple es -> !- "struct " +> sepOpenT +> genTuple es +> sepCloseT
        | ArrayOrList(sr, isArray, [], er, _) ->
            genArrayOrListOpeningToken isArray true sr
            +> genArrayOrListClosingToken isArray true er
        | ArrayOrList(openingTokenRange, isArray, xs, closingTokenRange, _) ->
            let smallExpression =
                genArrayOrListOpeningToken isArray false openingTokenRange
                +> col sepSemi xs genExpr
                +> genArrayOrListClosingToken isArray false closingTokenRange

            let multilineExpression =
                ifAlignBrackets
                    (genMultiLineArrayOrListAlignBrackets isArray xs openingTokenRange closingTokenRange)
                    (genMultiLineArrayOrList isArray xs openingTokenRange closingTokenRange)

            fun ctx ->
                if
                    List.exists isIfThenElseWithYieldReturn xs
                    || List.forall isSynExprLambdaOrIfThenElse xs
                then
                    multilineExpression ctx
                else
                    let size = getListOrArrayExprSize ctx ctx.Config.MaxArrayOrListWidth xs

                    isSmallExpression size smallExpression multilineExpression ctx

        | Record(openingBrace, inheritOpt, xs, eo, closingBrace) ->
            let smallRecordExpr =
                genTriviaFor SynExpr_Record_OpeningBrace openingBrace sepOpenS
                +> optSingle
                    (fun inheritCtor ->
                        !- "inherit "
                        +> genInheritConstructor inheritCtor
                        +> onlyIf (List.isNotEmpty xs) sepSemi)
                    inheritOpt
                +> optSingle (fun e -> genExpr e +> !- " with ") eo
                +> col sepSemi xs genRecordFieldName
                +> genTriviaFor SynExpr_Record_ClosingBrace closingBrace sepCloseS

            let multilineRecordExpr =
                ifAlignBrackets
                    (genMultilineRecordInstanceAlignBrackets openingBrace inheritOpt xs eo closingBrace)
                    (genMultilineRecordInstance openingBrace inheritOpt xs eo closingBrace)

            fun ctx ->
                let size = getRecordSize ctx xs
                isSmallExpression size smallRecordExpr multilineRecordExpr ctx

        | AnonRecord(openingBrace, isStruct, fields, copyInfo, closingBrace) ->
            let smallExpression =
                onlyIf isStruct !- "struct "
                +> genTriviaFor SynExpr_AnonRecd_OpeningBrace openingBrace sepOpenAnonRecd
                +> optSingle (fun e -> genExpr e +> !- " with ") copyInfo
                +> col sepSemi fields genAnonRecordFieldName
                +> genTriviaFor SynExpr_AnonRecd_ClosingBrace closingBrace sepCloseAnonRecd

            let longExpression =
                ifAlignBrackets
                    (genMultilineAnonRecordAlignBrackets openingBrace isStruct fields copyInfo closingBrace)
                    (genMultilineAnonRecord openingBrace isStruct fields copyInfo closingBrace)

            fun (ctx: Context) ->
                let size = getRecordSize ctx fields
                isSmallExpression size smallExpression longExpression ctx

        | ObjExpr(t, eio, withKeyword, bd, members, ims) ->
            if List.isEmpty bd && List.isEmpty members && List.isEmpty ims then
                // Check the role of the second part of eio
                let param = opt sepNone (Option.map fst eio) genExpr

                // See https://devblogs.microsoft.com/dotnet/announcing-f-5/#default-interface-member-consumption
                sepOpenS +> !- "new " +> genType t +> param +> sepCloseS
            else
                ifAlignBrackets
                    (genObjExprAlignBrackets t eio withKeyword bd members ims)
                    (genObjExpr t eio withKeyword bd members ims)

        | While(e1, e2) ->
            atCurrentColumn (
                !- "while "
                +> genExpr e1
                +> !- " do"
                +> indent
                +> sepNln
                +> genExpr e2
                +> unindent
            )

        | For(ident, equalsRange, e1, e2, e3, isUp) ->
            atCurrentColumn (
                !- "for "
                +> genIdent ident
                +> genEq SynExpr_For_Equals equalsRange
                +> sepSpace
                +> genExpr e1
                +> ifElse isUp (!- " to ") (!- " downto ")
                +> genExpr e2
                +> !- " do"
                +> indent
                +> sepNln
                +> genExpr e3
                +> unindent
            )

        // Handle the form 'for i in e1 -> e2'
        | ForEach(p, e1, e2, isArrow) ->
            atCurrentColumn (
                !- "for "
                +> genPat p
                +> !- " in "
                +> autoIndentAndNlnIfExpressionExceedsPageWidth (genExpr e1)
                +> ifElse
                    isArrow
                    (sepArrow +> autoIndentAndNlnIfExpressionExceedsPageWidth (genExpr e2))
                    (!- " do" +> indent +> sepNln +> genExpr e2 +> unindent)
            )

        | NamedComputationExpr(nameExpr, openingBrace, bodyExpr, closingBrace, computationExprRange) ->
            fun ctx ->
                let short =
                    genExpr nameExpr
                    +> sepSpace
                    +> (genTriviaFor SynExpr_ComputationExpr_OpeningBrace openingBrace sepOpenS
                        +> genExpr bodyExpr
                        +> genTriviaFor SynExpr_ComputationExpr_ClosingBrace closingBrace sepCloseS
                        |> genTriviaFor SynExpr_ComputationExpr computationExprRange)

                let long =
                    genExpr nameExpr
                    +> sepSpace
                    +> (genTriviaFor SynExpr_ComputationExpr_OpeningBrace openingBrace sepOpenS
                        +> indent
                        +> sepNln
                        +> genExpr bodyExpr
                        +> unindent
                        +> sepNln
                        +> genTriviaFor SynExpr_ComputationExpr_ClosingBrace closingBrace sepCloseSFixed
                        |> genTriviaFor SynExpr_ComputationExpr computationExprRange)

                expressionFitsOnRestOfLine short long ctx
        | ComputationExpr(openingBrace, e, closingBrace) ->
            expressionFitsOnRestOfLine
                (genTriviaFor SynExpr_ComputationExpr_OpeningBrace openingBrace sepOpenS
                 +> genExpr e
                 +> genTriviaFor SynExpr_ComputationExpr_ClosingBrace closingBrace sepCloseS)
                (genTriviaFor SynExpr_ComputationExpr_OpeningBrace openingBrace sepOpenS
                 +> genExpr e
                 +> unindent
                 +> genTriviaFor
                     SynExpr_ComputationExpr_ClosingBrace
                     closingBrace
                     (sepNlnUnlessLastEventIsNewline +> sepCloseSFixed))

        | CompExprBody statements ->
            let genCompExprStatement ces =
                match ces with
                | LetOrUseStatement(binding, inKeyword) ->
                    genSynBinding binding
                    +> genTriviaForOption SynExpr_LetOrUse_In inKeyword !- " in "
                | LetOrUseBangStatement(isUse, pat, equalsRange, expr, r) ->
                    enterNodeFor SynExpr_LetOrUseBang r // print Trivia before entire LetBang expression
                    +> ifElse isUse (!- "use! ") (!- "let! ")
                    +> genPat pat
                    +> genEq SynExpr_LetOrUseBang_Equals equalsRange
                    +> sepSpace
                    +> autoIndentAndNlnIfExpressionExceedsPageWidthUnlessStroustrup genExpr expr
                | AndBangStatement(pat, equalsRange, expr, range) ->
                    !- "and! "
                    +> genPat pat
                    +> genEq SynExprAndBang_Equals (Some equalsRange)
                    +> sepSpace
                    +> autoIndentAndNlnIfExpressionExceedsPageWidthUnlessStroustrup genExpr expr
                    |> genTriviaFor SynExprAndBang_ range
                | OtherStatement expr -> genExpr expr

            let getRangeOfCompExprStatement ces =
                match ces with
                | LetOrUseStatement(binding, _) -> binding.RangeOfBindingWithRhs
                | LetOrUseBangStatement(range = r) -> r
                | AndBangStatement(range = r) -> r
                | OtherStatement expr -> expr.Range

            let getSepNln ces r =
                match ces with
                | LetOrUseStatement(b, _) -> sepNlnConsideringTriviaContentBeforeFor SynBinding_ b.FullRange
                | LetOrUseBangStatement _ -> sepNlnConsideringTriviaContentBeforeFor SynExpr_LetOrUseBang r
                | AndBangStatement(_, _, _, r) -> sepNlnConsideringTriviaContentBeforeFor SynExprAndBang_ r
                | OtherStatement e ->
                    let t, r = synExprToFsAstType e
                    sepNlnConsideringTriviaContentBeforeFor t r

            statements
            |> List.map (fun ces ->
                let expr = genCompExprStatement ces
                let r = getRangeOfCompExprStatement ces
                let sepNln = getSepNln ces r
                ColMultilineItem(expr, sepNln))
            |> colWithNlnWhenItemIsMultilineUsingConfig

        | JoinIn(e1, e2) -> genExpr e1 +> !- " in " +> genExpr e2
        | Paren(lpr, Lambda(pats, arrowRange, expr, lambdaRange), rpr, _) ->
            fun (ctx: Context) ->
                let body = genExpr

                let expr =
                    sepOpenTFor lpr
                    +> enterNodeFor SynExpr_Lambda lambdaRange
                    +> !- "fun "
                    +> genLambdaPats pats
                    +> (fun ctx ->
                        if not ctx.Config.MultiLineLambdaClosingNewline then
                            genLambdaArrowWithTrivia
                                (fun e ->
                                    body e
                                    +> leaveNodeFor SynExpr_Lambda lambdaRange
                                    +> sepNlnWhenWriteBeforeNewlineNotEmpty
                                    +> sepCloseTFor rpr)
                                expr
                                arrowRange
                                ctx
                        else
                            leadingExpressionIsMultiline
                                (genLambdaArrowWithTrivia
                                    (fun e ->
                                        body e
                                        +> leaveNodeFor SynExpr_Lambda lambdaRange
                                        +> sepNlnWhenWriteBeforeNewlineNotEmpty)
                                    expr
                                    arrowRange)
                                (fun isMultiline -> onlyIf isMultiline sepNln +> sepCloseTFor rpr)
                                ctx)

                expr ctx

        // When there are parentheses, most likely lambda will appear in function application
        | Lambda(pats, arrowRange, expr, _range) ->
            atCurrentColumn (
                !- "fun "
                +> genLambdaPats pats
                +> optSingle (fun arrowRange -> sepArrow |> genTriviaFor SynExpr_Lambda_Arrow arrowRange) arrowRange
                +> autoIndentAndNlnIfExpressionExceedsPageWidthUnlessStroustrup genExpr expr
            )
        | MatchLambda(keywordRange, cs) ->
            (!- "function " |> genTriviaFor SynExpr_MatchLambda_Function keywordRange)
            +> sepNln
            +> genClauses cs
        | Match(matchRange, e, withRange, cs) ->
            let genMatchExpr = genMatchWith matchRange e withRange
            atCurrentColumn (genMatchExpr +> sepNln +> genClauses cs)
        | MatchBang(matchRange, e, withRange, cs) ->
            let genMatchExpr = genMatchBangWith matchRange e withRange
            atCurrentColumn (genMatchExpr +> sepNln +> genClauses cs)
        | TraitCall(tps, msg, e) ->
            genType tps
            +> sepColon
            +> sepOpenT
            +> genMemberSig msg
            +> sepCloseT
            +> sepSpace
            +> genExpr e
        | Paren(_, ILEmbedded r, rpr, _) ->
            fun ctx ->
                let expr =
                    match ctx.FromSourceText r with
                    | None -> sepNone
                    | Some eil -> !-eil

                (expr +> optSingle (leaveNodeFor SynExpr_Paren_ClosingParenthesis) rpr) ctx
        | ParenFunctionNameWithStar(lpr, originalNotation, rpr) ->
            sepOpenTFor lpr +> !- $" {originalNotation} " +> sepCloseTFor (Some rpr)
        | Paren(lpr, e, rpr, _pr) ->
            match e with
            | LetOrUses _
            | Sequential _ -> sepOpenTFor lpr +> atCurrentColumn (genExpr e) +> sepCloseTFor rpr
            | _ -> sepOpenTFor lpr +> genExpr e +> sepCloseTFor rpr

        | DynamicExpr(func, arg) -> genExpr func +> !- "?" +> genExpr arg

        // Separate two prefix ops by spaces
        | PrefixApp(s1, PrefixApp(s2, e)) -> !-(sprintf "%s %s" s1 s2) +> genExpr e
        | PrefixApp(s, App(e, [ Paren _ as p ]))
        | PrefixApp(s, App(e, [ ConstExpr(SynConst.Unit _, _) as p ])) -> !-s +> sepSpace +> genExpr e +> genExpr p
        | PrefixApp(s, e) ->
            let extraSpaceBeforeString =
                match e with
                | SynExpr.Const _
                | SynExpr.InterpolatedString _ -> sepSpace
                | _ -> sepNone

            !-s +> extraSpaceBeforeString +> genExpr e

        | NewlineInfixApp(operatorText, operatorExpr, (Lambda _ as e1), e2)
        | NewlineInfixApp(operatorText, operatorExpr, (IfThenElse _ as e1), e2) ->
            genMultilineInfixExpr e1 operatorText operatorExpr e2

        | NewlineInfixApps(e, es) ->
            let shortExpr =
                onlyIf (isSynExprLambdaOrIfThenElse e) sepOpenT
                +> genExpr e
                +> onlyIf (isSynExprLambdaOrIfThenElse e) sepCloseT
                +> sepSpace
                +> col sepSpace es (fun (_s, oe, e) ->
                    genSynLongIdent false oe
                    +> sepSpace
                    +> onlyIf (isSynExprLambdaOrIfThenElse e) sepOpenT
                    +> genExpr e
                    +> onlyIf (isSynExprLambdaOrIfThenElse e) sepCloseT)

            let multilineExpr =
                match es with
                | [] -> genExpr e
                | (s, oe, e2) :: es ->
                    genMultilineInfixExpr e s oe e2
                    +> sepNln
                    +> col sepNln es (fun (_s, oe, e) ->
                        genSynLongIdent false oe +> sepSpace +> genExprInMultilineInfixExpr e)

            fun ctx ->
                atCurrentColumn (isShortExpression ctx.Config.MaxInfixOperatorExpression shortExpr multilineExpr) ctx

        | SameInfixApps(e, es) ->
            let shortExpr =
                onlyIf (isSynExprLambdaOrIfThenElse e) sepOpenT
                +> genExpr e
                +> onlyIf (isSynExprLambdaOrIfThenElse e) sepCloseT
                +> sepSpace
                +> col sepSpace es (fun (_s, oe, e) ->
                    genSynLongIdent false oe
                    +> sepSpace
                    +> onlyIf (isSynExprLambdaOrIfThenElse e) sepOpenT
                    +> genExpr e
                    +> onlyIf (isSynExprLambdaOrIfThenElse e) sepCloseT)

            let multilineExpr =
                genExpr e
                +> sepNln
                +> col sepNln es (fun (_s, oe, e) ->
                    genSynLongIdent false oe +> sepSpace +> genExprInMultilineInfixExpr e)

            fun ctx ->
                atCurrentColumn (isShortExpression ctx.Config.MaxInfixOperatorExpression shortExpr multilineExpr) ctx

        | InfixApp(operatorText, operatorSli, e1, e2, _) ->
            fun ctx ->
                isShortExpression
                    ctx.Config.MaxInfixOperatorExpression
                    (genOnelinerInfixExpr e1 operatorSli e2)
                    (ifElse
                        (noBreakInfixOps.Contains(operatorText))
                        (genOnelinerInfixExpr e1 operatorSli e2)
                        (genMultilineInfixExpr e1 operatorText operatorSli e2))
                    ctx

        | TernaryApp(e1, e2, e3) ->
            atCurrentColumn (
                genExpr e1
                +> !- "?"
                +> genExpr e2
                +> sepSpace
                +> !- "<-"
                +> sepSpace
                +> genExpr e3
            )

        | IndexWithoutDotExpr(identifierExpr, indexExpr, arrayOrListRange) ->
            let genIndexExpr = genExpr indexExpr

            genExpr identifierExpr
            +> (sepOpenLFixed
                +> expressionFitsOnRestOfLine genIndexExpr (atCurrentColumnIndent genIndexExpr)
                +> sepCloseLFixed
                |> genTriviaFor SynExpr_ArrayOrList arrayOrListRange)

        // Result<int, string>.Ok 42
        | App(DotGet(TypeApp(e, lt, ts, gt), sli), es) ->
            genExpr e
            +> genGenericTypeParameters lt ts gt
            +> genSynLongIdent true sli
            +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (col sepSpace es genExpr)

        // Foo(fun x -> x).Bar().Meh
        | DotGetAppDotGetAppParenLambda(e, px, appLids, es, lids) ->
            let short =
                genExpr e
                +> genExpr px
                +> genSynLongIdent true appLids
                +> col sepComma es genExpr
                +> genSynLongIdent true lids

            let long =
                let functionName =
                    match e with
                    | LongIdentExprWithMoreThanOneIdent lids -> genFunctionNameWithMultilineLids id lids
                    | TypeApp(LongIdentExprWithMoreThanOneIdent lids, lt, ts, gt) ->
                        genFunctionNameWithMultilineLids (genGenericTypeParameters lt ts gt) lids
                    | _ -> genExpr e

                functionName
                +> indent
                +> genExpr px
                +> sepNln
                +> genSynLongIdentMultiline true appLids
                +> col sepComma es genExpr
                +> sepNln
                +> genSynLongIdentMultiline true lids
                +> unindent

            fun ctx -> isShortExpression ctx.Config.MaxDotGetExpressionWidth short long ctx

        // Foo().Bar
        | DotGetAppParen(e, px, lids) ->
            let shortAppExpr = genExpr e +> genExpr px

            let longAppExpr =
                let functionName argFn =
                    match e with
                    | LongIdentExprWithMoreThanOneIdent lids -> genFunctionNameWithMultilineLids argFn lids
                    | TypeApp(LongIdentExprWithMoreThanOneIdent lids, lt, ts, gt) ->
                        genFunctionNameWithMultilineLids (genGenericTypeParameters lt ts gt +> argFn) lids
                    | DotGetAppDotGetAppParenLambda _ ->
                        leadingExpressionIsMultiline (genExpr e) (fun isMultiline ->
                            if isMultiline then indent +> argFn +> unindent else argFn)
                    | _ -> genExpr e +> argFn

                let arguments = genMultilineFunctionApplicationArguments px

                functionName arguments

            let shortDotGetExpr = genSynLongIdent true lids

            let longDotGetExpr =
                indent +> sepNln +> genSynLongIdentMultiline true lids +> unindent

            fun ctx ->
                isShortExpression
                    ctx.Config.MaxDotGetExpressionWidth
                    (shortAppExpr +> shortDotGetExpr)
                    (longAppExpr +> longDotGetExpr)
                    ctx

        // Foo(fun x -> x).Bar()
        | DotGetApp(App(e, [ Paren(_, Lambda _, _, _) as px ]), es) ->
            let genLongFunctionName f =
                match e with
                | LongIdentExprWithMoreThanOneIdent lids -> genFunctionNameWithMultilineLids f lids
                | TypeApp(LongIdentExprWithMoreThanOneIdent lids, lt, ts, gt) ->
                    genFunctionNameWithMultilineLids (genGenericTypeParameters lt ts gt +> f) lids
                | _ -> genExpr e +> f

            let lastEsIndex = es.Length - 1

            let genApp (idx: int) (lids, e, t) : Context -> Context =
                let short =
                    genSynLongIdent true lids
                    +> optSingle (fun (lt, ts, gt) -> genGenericTypeParameters lt ts gt) t
                    +> genSpaceBeforeLids idx lastEsIndex lids e
                    +> genExpr e

                let long =
                    genSynLongIdentMultiline true lids
                    +> optSingle (fun (lt, ts, gt) -> genGenericTypeParameters lt ts gt) t
                    +> genSpaceBeforeLids idx lastEsIndex lids e
                    +> genMultilineFunctionApplicationArguments e

                expressionFitsOnRestOfLine short long

            let short =
                genExpr e
                +> genExpr px
                +> coli sepNone es (fun idx (lids, e, t) ->
                    genSynLongIdent true lids
                    +> optSingle (fun (lt, ts, gt) -> genGenericTypeParameters lt ts gt) t
                    +> genSpaceBeforeLids idx lastEsIndex lids e
                    +> genExpr e)

            let long =
                genLongFunctionName (genExpr px)
                +> indent
                +> sepNln
                +> coli sepNln es genApp
                +> unindent

            fun ctx -> isShortExpression ctx.Config.MaxDotGetExpressionWidth short long ctx

        // Foo().Bar().Meh()
        | DotGetApp(e, es) ->
            let genLongFunctionName =
                match e with
                | AppOrTypeApp(LongIdentExprWithMoreThanOneIdent lids, t, [ Paren _ as px ]) ->
                    genFunctionNameWithMultilineLids
                        (optSingle (fun (lt, ts, gt) -> genGenericTypeParameters lt ts gt) t
                         +> expressionFitsOnRestOfLine (genExpr px) (genMultilineFunctionApplicationArguments px))
                        lids
                | AppOrTypeApp(LongIdentExprWithMoreThanOneIdent lids, t, [ e2 ]) ->
                    genFunctionNameWithMultilineLids
                        (optSingle (fun (lt, ts, gt) -> genGenericTypeParameters lt ts gt) t
                         +> genExpr e2)
                        lids
                | AppOrTypeApp(SimpleExpr e, t, [ ConstExpr(SynConst.Unit, r) ]) ->
                    genExpr e
                    +> optSingle (fun (lt, ts, gt) -> genGenericTypeParameters lt ts gt) t
                    +> genTriviaFor SynExpr_Const r (genConst SynConst.Unit r)
                | AppOrTypeApp(SimpleExpr e, t, [ Paren _ as px ]) ->
                    let short =
                        genExpr e
                        +> optSingle (fun (lt, ts, gt) -> genGenericTypeParameters lt ts gt) t
                        +> genExpr px

                    let long =
                        genExpr e
                        +> optSingle (fun (lt, ts, gt) -> genGenericTypeParameters lt ts gt) t
                        +> genMultilineFunctionApplicationArguments px

                    expressionFitsOnRestOfLine short long
                | _ -> genExpr e

            let lastEsIndex = es.Length - 1

            let genApp (idx: int) (lids, e, t) : Context -> Context =
                let short =
                    genSynLongIdent true lids
                    +> optSingle (fun (lt, ts, gt) -> genGenericTypeParameters lt ts gt) t
                    +> genSpaceBeforeLids idx lastEsIndex lids e
                    +> genExpr e

                let long =
                    genSynLongIdentMultiline true lids
                    +> optSingle (fun (lt, ts, gt) -> genGenericTypeParameters lt ts gt) t
                    +> genSpaceBeforeLids idx lastEsIndex lids e
                    +> genMultilineFunctionApplicationArguments e

                expressionFitsOnRestOfLine short long

            let short =
                match e with
                | App(e, [ px ]) when (hasParenthesis px || isArrayOrList px) -> genExpr e +> genExpr px
                | _ -> genExpr e
                +> coli sepNone es genApp

            let long =
                genLongFunctionName +> indent +> sepNln +> coli sepNln es genApp +> unindent

            fun ctx -> isShortExpression ctx.Config.MaxDotGetExpressionWidth short long ctx

        // path.Replace("../../../", "....")
        | AppSingleParenArg(LongIdentExpr lids as functionOrMethod, px) ->
            let addSpace =
                onlyIfCtx (addSpaceBeforeParensInFunCall functionOrMethod px) sepSpace

            let shortLids = genSynLongIdent false lids

            let short = shortLids +> addSpace +> genExpr px

            let long =
                let args =
                    addSpace
                    +> expressionFitsOnRestOfLine (genExpr px) (genMultilineFunctionApplicationArguments px)

                ifElseCtx (futureNlnCheck shortLids) (genFunctionNameWithMultilineLids args lids) (shortLids +> args)

            expressionFitsOnRestOfLine short long

        | AppSingleParenArg(e, px) ->
            let sepSpace (ctx: Context) =
                match e with
                | Paren _ -> sepSpace ctx
                | UppercaseSynExpr -> onlyIf ctx.Config.SpaceBeforeUppercaseInvocation sepSpace ctx
                | LowercaseSynExpr -> onlyIf ctx.Config.SpaceBeforeLowercaseInvocation sepSpace ctx

            let short = genExpr e +> sepSpace +> genExpr px
            let long = genExpr e +> sepSpace +> genMultilineFunctionApplicationArguments px

            expressionFitsOnRestOfLine short long

        | DotGetAppWithLambda((e, es, lpr, lambda, rpr, pr), lids) ->
            leadingExpressionIsMultiline (genAppWithLambda sepNone (e, es, lpr, lambda, rpr, pr)) (fun isMultiline ->
                if isMultiline then
                    (indent +> sepNln +> genSynLongIdent true lids +> unindent)
                else
                    genSynLongIdent true lids)

        // functionName arg1 arg2 (fun x y z -> ...)
        | AppWithLambda(e, es, lpr, lambda, rpr, pr) ->
            let sepSpaceAfterFunctionName =
                let sepSpaceBasedOnSetting e =
                    match e with
                    | Paren _ -> sepSpace
                    | UppercaseSynExpr -> (fun ctx -> onlyIf ctx.Config.SpaceBeforeUppercaseInvocation sepSpace ctx)
                    | LowercaseSynExpr -> (fun ctx -> onlyIf ctx.Config.SpaceBeforeLowercaseInvocation sepSpace ctx)

                match es with
                | [] -> sepSpaceBasedOnSetting e
                | _ -> sepSpace

            genAppWithLambda sepSpaceAfterFunctionName (e, es, lpr, lambda, rpr, pr)

        | NestedIndexWithoutDotExpr(identifierExpr, indexExpr, argExpr) ->
            genExpr identifierExpr
            +> sepOpenLFixed
            +> genExpr indexExpr
            +> sepCloseLFixed
            +> genExpr argExpr
        | EndsWithDualListAppExpr ctx.Config.ExperimentalStroustrupStyle (e, es, props, children) ->
            // check if everything else beside the last array/list fits on one line
            let singleLineTestExpr =
                genExpr e +> sepSpace +> col sepSpace es genExpr +> sepSpace +> genExpr props

            let short =
                genExpr e
                +> sepSpace
                +> col sepSpace es genExpr
                +> onlyIfNot es.IsEmpty sepSpace
                +> genExpr props
                +> sepSpace
                +> genExpr children

            let long =
                // check if everything besides both lists fits on one line
                let singleLineTestExpr = genExpr e +> sepSpace +> col sepSpace es genExpr

                if futureNlnCheck singleLineTestExpr ctx then
                    genExpr e
                    +> indent
                    +> sepNln
                    +> col sepNln es genExpr
                    +> sepSpace
                    +> genExpr props
                    +> sepSpace
                    +> genExpr children
                    +> unindent
                else
                    genExpr e
                    +> sepSpace
                    +> col sepSpace es genExpr
                    +> genExpr props
                    +> sepSpace
                    +> genExpr children

            if futureNlnCheck singleLineTestExpr ctx then
                long
            else
                short

        | EndsWithSingleListAppExpr ctx.Config.ExperimentalStroustrupStyle (e, es, a) ->
            // check if everything else beside the last array/list fits on one line
            let singleLineTestExpr = genExpr e +> sepSpace +> col sepSpace es genExpr

            let short =
                genExpr e
                +> sepSpace
                +> col sepSpace es genExpr
                +> onlyIfNot es.IsEmpty sepSpace
                +> genExpr a

            let long =
                genExpr e
                +> indent
                +> sepNln
                +> col sepNln es genExpr
                +> onlyIfNot es.IsEmpty sepNln
                +> genExpr a
                +> unindent

            if futureNlnCheck singleLineTestExpr ctx then
                long
            else
                short

        // Always spacing in multiple arguments
        | App(e, es) -> genApp e es
        | TypeApp(e, lt, ts, gt) -> genExpr e +> genGenericTypeParameters lt ts gt
        | LetOrUses(bs, e) ->
            let items = collectMultilineItemForLetOrUses bs (collectMultilineItemForSynExpr e)

            atCurrentColumn (colWithNlnWhenItemIsMultilineUsingConfig items)

        | TryWithSingleClause(tryKeyword, e, withKeyword, barRange, p, eo, arrowRange, catchExpr, clauseRange) ->
            let genClause =
                leadingExpressionResult
                    (enterNodeFor SynMatchClause_ clauseRange
                     +> genTriviaForOption SynMatchClause_Bar barRange sepNone)
                    (fun ((linesBefore, _), (linesAfter, _)) ->
                        onlyIfCtx (fun ctx -> linesAfter > linesBefore || hasWriteBeforeNewlineContent ctx) sepBar)
                +> genPatInClause p
                +> optSingle (fun e -> !- " when" +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (genExpr e)) eo
                +> genTriviaForOption SynMatchClause_Arrow arrowRange sepArrow
                +> autoIndentAndNlnExpressUnlessStroustrup genExpr catchExpr
                +> leaveNodeFor SynMatchClause_ clauseRange

            atCurrentColumn (
                genTriviaFor SynExpr_TryWith_Try tryKeyword !- "try"
                +> indent
                +> sepNln
                +> genExpr e
                +> unindent
                +> sepNln
                +> genTriviaFor SynExpr_TryWith_With withKeyword (!- "with")
                +> sepSpace
                +> genClause
            )

        | TryWith(tryKeyword, e, withKeyword, cs) ->
            atCurrentColumn (
                genTriviaFor SynExpr_TryWith_Try tryKeyword !- "try"
                +> indent
                +> sepNln
                +> genExpr e
                +> unindent
                +> sepNln // unless trivia?
                +> genTriviaFor SynExpr_TryWith_With withKeyword (!- "with")
                +> sepNln
                +> (fun ctx ->
                    let hasMultipleClausesWhereOneHasStroustrup =
                        hasMultipleClausesWhereOneHasStroustrup ctx.Config.ExperimentalStroustrupStyle cs

                    col sepNln cs (genClause false hasMultipleClausesWhereOneHasStroustrup) ctx)
            )

        | TryFinally(tryKeyword, e1, finallyKeyword, e2) ->
            atCurrentColumn (
                genTriviaFor SynExpr_TryFinally_Try tryKeyword !- "try "
                +> indent
                +> sepNln
                +> genExpr e1
                +> unindent
                +> genTriviaFor SynExpr_TryFinally_Finally finallyKeyword !+~ "finally"
                +> indent
                +> sepNln
                +> genExpr e2
                +> unindent
            )

        | Sequentials es ->
            let items = List.collect collectMultilineItemForSynExpr es
            atCurrentColumn (colWithNlnWhenItemIsMultilineUsingConfig items)

        // if condExpr then thenExpr
        | ElIf([ None, ifKw, false, ifExpr, thenKw, thenExpr ], None, _) ->
            leadingExpressionResult
                (genIfThen ifKw ifExpr thenKw)
                (fun ((lineCountBefore, columnBefore), (lineCountAfter, columnAfter)) ctx ->
                    // Check if the `if expr then` is already multiline or cross the max_line_length.
                    let isMultiline =
                        lineCountAfter > lineCountBefore || columnAfter > ctx.Config.MaxLineLength

                    if isMultiline then
                        indentSepNlnUnindent (genExpr thenExpr) ctx
                    else
                        // Check if the entire expression is will still fit on one line, respecting MaxIfThenShortWidth
                        let remainingMaxLength =
                            ctx.Config.MaxIfThenShortWidth - (columnAfter - columnBefore)

                        isShortExpression
                            remainingMaxLength
                            (sepSpace +> genExpr thenExpr)
                            (indentSepNlnUnindent (genExpr thenExpr))
                            ctx)
            |> atCurrentColumnIndent

        // if condExpr then thenExpr else elseExpr
        | ElIf([ None, ifKw, false, ifExpr, thenKw, thenExpr ], Some(elseKw, elseExpr), _) ->
            let genElse = genTriviaFor SynExpr_IfThenElse_Else elseKw !- "else"

            leadingExpressionResult
                (genIfThen ifKw ifExpr thenKw)
                (fun ((lineCountBefore, columnBefore), (lineCountAfter, columnAfter)) ctx ->
                    let long =
                        indentSepNlnUnindent (genExpr thenExpr)
                        +> sepNln
                        +> genElse
                        +> genKeepIdent elseKw elseExpr
                        +> sepNln
                        +> genExpr elseExpr
                        +> unindent

                    // Check if the `if expr then` is already multiline or cross the max_line_length.
                    let isMultiline =
                        lineCountAfter > lineCountBefore || columnAfter > ctx.Config.MaxLineLength

                    // If the `thenExpr` is also an SynExpr.IfThenElse, it will not be valid code if put on one line.
                    // ex: if cond then if a then b else c else e2
                    let thenExprIsIfThenElse =
                        match thenExpr with
                        | IfThenElse _ -> true
                        | _ -> false

                    if isMultiline || thenExprIsIfThenElse then
                        long ctx
                    else
                        // Check if the entire expression is will still fit on one line, respecting MaxIfThenShortWidth
                        let remainingMaxLength =
                            ctx.Config.MaxIfThenElseShortWidth - (columnAfter - columnBefore)

                        isShortExpression
                            remainingMaxLength
                            (sepSpace
                             +> genExpr thenExpr
                             +> sepSpace
                             +> genElse
                             +> sepSpace
                             +> genExpr elseExpr)
                            long
                            ctx)
            |> atCurrentColumnIndent

        // At least one `elif` or `else if` is present
        // Optional else branch
        | ElIf(branches, elseInfo, _) ->
            // multiple branches but no else expr
            // use the same threshold check as for if-then
            // Everything should fit on one line
            let areAllShort ctx =
                let anyThenExprIsIfThenElse =
                    branches
                    |> List.exists (fun (_, _, _, _, _, thenExpr) ->
                        match thenExpr with
                        | IfThenElse _ -> true
                        | _ -> false)

                let checkIfLine (elseKwOpt, ifKw, isElif, condExpr, thenKw, thenExpr) =
                    genIfOrElseIfOrElifThen elseKwOpt ifKw isElif condExpr thenKw
                    +> sepSpace
                    +> genExpr thenExpr

                let linesToCheck =
                    match elseInfo with
                    | None -> List.map checkIfLine branches
                    | Some(elseKw, elseExpr) ->
                        // This may appear a bit odd that we are adding the `else elseExpr` before the `if expr then expr` lines but purely for this check this doesn't matter.
                        // Each lines needs to fit on one line in order for us to format the short way
                        (genTriviaFor SynExpr_IfThenElse_Else elseKw !- "else"
                         +> sepSpace
                         +> genExpr elseExpr)
                        :: (List.map checkIfLine branches)

                let lineCheck () =
                    linesToCheck
                    |> List.forall (fun lineCheck ->
                        let maxWidth =
                            if elseInfo.IsSome then
                                ctx.Config.MaxIfThenElseShortWidth
                            else
                                ctx.Config.MaxIfThenShortWidth

                        not (exceedsWidth maxWidth lineCheck ctx))

                not anyThenExprIsIfThenElse && lineCheck ()

            let shortExpr =
                col sepNln branches (fun (elseKwOpt, ifKw, isElif, condExpr, thenKw, thenExpr) ->
                    genIfOrElseIfOrElifThen elseKwOpt ifKw isElif condExpr thenKw
                    +> sepSpace
                    +> genExpr thenExpr)
                +> optSingle
                    (fun (elseKw, elseExpr) ->
                        sepNln
                        +> genTriviaFor SynExpr_IfThenElse_Else elseKw !- "else"
                        +> sepSpace
                        +> genExpr elseExpr)
                    elseInfo

            let longExpr =
                col sepNln branches (fun (elseKwOpt, ifKw, isElif, condExpr, thenKw, thenExpr) ->
                    genIfOrElseIfOrElifThen elseKwOpt ifKw isElif condExpr thenKw
                    +> indentSepNlnUnindent (genExpr thenExpr))
                +> optSingle
                    (fun (elseKw, elseExpr) ->
                        sepNln
                        +> genTriviaFor SynExpr_IfThenElse_Else elseKw !- "else"
                        +> genKeepIdent elseKw elseExpr
                        +> sepNln
                        +> genExpr elseExpr
                        +> unindent)
                    elseInfo

            ifElseCtx areAllShort shortExpr longExpr |> atCurrentColumnIndent

        | IdentExpr ident -> genIdent ident

        // At this stage, all symbolic operators have been handled.
        | OptVar(isOpt, sli, _) -> ifElse isOpt (!- "?") sepNone +> genSynLongIdent false sli
        | LongIdentSet(sli, e, _) ->
            genSynLongIdent false sli
            +> sepArrowRev
            +> autoIndentAndNlnIfExpressionExceedsPageWidthUnlessStroustrup genExpr e
        | DotIndexedGet(App(e, [ ConstExpr(SynConst.Unit, _) as ux ]), indexArgs) ->
            genExpr e
            +> genExpr ux
            +> !- "."
            +> sepOpenLFixed
            +> genExpr indexArgs
            +> sepCloseLFixed
        | DotIndexedGet(AppSingleParenArg(e, px), indexArgs) ->
            let short = genExpr e +> genExpr px
            let long = genExpr e +> genMultilineFunctionApplicationArguments px
            let idx = !- "." +> sepOpenLFixed +> genExpr indexArgs +> sepCloseLFixed

            expressionFitsOnRestOfLine (short +> idx) (long +> idx)
        | DotIndexedGet(objectExpr, indexArgs) ->
            let isParen =
                match objectExpr with
                | Paren _ -> true
                | _ -> false

            ifElse isParen (genExpr objectExpr) (addParenIfAutoNln objectExpr genExpr)
            +> !- "."
            +> sepOpenLFixed
            +> genExpr indexArgs
            +> sepCloseLFixed
        | DotIndexedSet(App(e, [ ConstExpr(SynConst.Unit, _) as ux ]), indexArgs, valueExpr) ->
            let appExpr = genExpr e +> genExpr ux

            let idx =
                !- "." +> sepOpenLFixed +> genExpr indexArgs +> sepCloseLFixed +> sepArrowRev

            expressionFitsOnRestOfLine
                (appExpr +> idx +> genExpr valueExpr)
                (appExpr
                 +> idx
                 +> autoIndentAndNlnIfExpressionExceedsPageWidthUnlessStroustrup genExpr valueExpr)
        | DotIndexedSet(AppSingleParenArg(a, px), indexArgs, valueExpr) ->
            let short = genExpr a +> genExpr px
            let long = genExpr a +> genMultilineFunctionApplicationArguments px

            let idx =
                !- "." +> sepOpenLFixed +> genExpr indexArgs +> sepCloseLFixed +> sepArrowRev

            expressionFitsOnRestOfLine
                (short +> idx +> genExpr valueExpr)
                (long
                 +> idx
                 +> autoIndentAndNlnIfExpressionExceedsPageWidthUnlessStroustrup genExpr valueExpr)

        | DotIndexedSet(objectExpr, indexArgs, valueExpr) ->
            addParenIfAutoNln objectExpr genExpr
            +> !- ".["
            +> genExpr indexArgs
            +> !- "] <- "
            +> autoIndentAndNlnIfExpressionExceedsPageWidthUnlessStroustrup genExpr valueExpr

        // a.b[c] <- d
        | NamedIndexedPropertySet(sli, ArrayOrList(sr, isArray, [ e1 ], er, _), e2) ->
            genSynLongIdent false sli
            +> genArrayOrListOpeningToken isArray true sr
            +> genExpr e1
            +> genArrayOrListClosingToken isArray true er
            +> sepArrowRev
            +> autoIndentAndNlnIfExpressionExceedsPageWidth (genExpr e2)

        | NamedIndexedPropertySet(sli, e1, e2) ->
            let sep =
                match e1 with
                | SynExpr.Const _
                | SynExpr.Ident _ -> sepSpace
                | _ -> sepNone

            genSynLongIdent false sli
            +> sep
            +> genExpr e1
            +> sepArrowRev
            +> autoIndentAndNlnIfExpressionExceedsPageWidth (genExpr e2)

        | DotNamedIndexedPropertySet(e, sli, e1, e2) ->
            genExpr e
            +> sepDot
            +> genSynLongIdent false sli
            +> genExpr e1
            +> sepArrowRev
            +> autoIndentAndNlnIfExpressionExceedsPageWidth (genExpr e2)

        // typeof<System.Collections.IEnumerable>.FullName
        | DotGet(e, sli) ->
            let shortExpr = genExpr e +> genSynLongIdent true sli
            let longExpr = genExpr e +> indentSepNlnUnindent (genSynLongIdentMultiline true sli)

            fun ctx -> isShortExpression ctx.Config.MaxDotGetExpressionWidth shortExpr longExpr ctx

        | DotSet(AppSingleParenArg(e1, px), sli, e2) ->
            genExpr e1
            +> genExpr px
            +> sepDot
            +> genSynLongIdent false sli
            +> !- " <- "
            +> autoIndentAndNlnIfExpressionExceedsPageWidthUnlessStroustrup genExpr e2

        | DotSet(e1, sli, e2) ->
            addParenIfAutoNln e1 genExpr
            +> sepDot
            +> genSynLongIdent false sli
            +> sepArrowRev
            +> autoIndentAndNlnIfExpressionExceedsPageWidthUnlessStroustrup genExpr e2

        | SynExpr.Set(e1, e2, _) ->
            addParenIfAutoNln e1 genExpr
            +> sepArrowRev
            +> autoIndentAndNlnIfExpressionExceedsPageWidthUnlessStroustrup genExpr e2

        | ParsingError r ->
            raise
            <| FormatException
                $"Parsing error(s) between line %i{r.StartLine} column %i{r.StartColumn + 1} and line %i{r.EndLine} column %i{r.EndColumn + 1}"

        | LibraryOnlyStaticOptimization(optExpr, constraints, e) ->
            genExpr optExpr
            +> genSynStaticOptimizationConstraint constraints
            +> sepEq
            +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (genExpr e)

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
        | InterpolatedStringExpr(parts, _stringKind) ->
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

                    genExpr expr { ctx with Config = interpolatedConfig }
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
                | SynInterpolatedStringPart.String(s, r) ->
                    fun ctx ->
                        let expr =
                            match ctx.FromSourceText r with
                            | None -> !-s
                            | Some s -> !-s

                        genTriviaFor SynInterpolatedStringPart_String r expr ctx
                | SynInterpolatedStringPart.FillExpr(expr, ident) ->
                    fun ctx ->
                        let genFill =
                            genInterpolatedFillExpr expr
                            +> optSingle (fun format -> sepColonFixed +> genIdent format) ident

                        if ctx.Config.StrictMode then
                            (!- "{" +> genFill +> !- "}") ctx
                        else
                            genFill ctx)
            +> withoutSourceText "\""

        | IndexRangeExpr(None, None) -> !- "*"
        | IndexRangeExpr(Some(IndexRangeExpr(Some(ConstNumberExpr e1), Some(ConstNumberExpr e2))),
                         Some(ConstNumberExpr e3)) ->
            let hasOmittedTrailingZero (fromSourceText: range -> string option) r =
                match fromSourceText r with
                | None -> false
                | Some sourceText -> sourceText.EndsWith(".")

            let dots (ctx: Context) =
                if
                    hasOmittedTrailingZero ctx.FromSourceText e1.Range
                    || hasOmittedTrailingZero ctx.FromSourceText e2.Range
                then
                    !- " .. " ctx
                else
                    !- ".." ctx

            genExpr e1 +> dots +> genExpr e2 +> dots +> genExpr e3
        | IndexRangeExpr(e1, e2) ->
            let hasSpaces =
                let rec (|AtomicExpr|_|) e =
                    match e with
                    | NegativeNumber _ -> None
                    | SynExpr.Ident _
                    | SynExpr.Const(SynConst.Int32 _, _)
                    | IndexRangeExpr(Some(AtomicExpr _), Some(AtomicExpr _))
                    | IndexFromEndExpr(AtomicExpr _) -> Some e
                    | _ -> None

                match e1, e2 with
                | Some(AtomicExpr _), None
                | None, Some(AtomicExpr _)
                | Some(AtomicExpr _), Some(AtomicExpr _) -> false
                | _ -> true

            optSingle (fun e -> genExpr e +> onlyIf hasSpaces sepSpace) e1
            +> !- ".."
            +> optSingle (fun e -> onlyIf hasSpaces sepSpace +> genExpr e) e2
        | IndexFromEndExpr e -> !- "^" +> genExpr e
        | TyparExpr typar -> genTypar typar
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
            | SynExpr.Dynamic _ -> genTriviaFor SynExpr_Dynamic synExpr.Range
            | SynExpr.Const _ -> genTriviaFor SynExpr_Const synExpr.Range
            | SynExpr.Typed _ -> genTriviaFor SynExpr_Typed synExpr.Range
            | SynExpr.Typar _ -> genTriviaFor SynExpr_Typar synExpr.Range
            | SynExpr.LetOrUse _
            | SynExpr.Sequential _
            | SynExpr.ComputationExpr _ ->
                // first and last nested node has trivia attached to it
                id
            | SynExpr.LetOrUseBang _ ->
                // printed as part of CompBody
                id
            | SynExpr.DebugPoint _ ->
                // I don't believe the parser will ever return this node
                id)

    expr ctx

and genArrayOrListOpeningToken isArray isFixed openingTokenRange =
    if isArray then
        genTriviaFor SynExpr_ArrayOrList_OpeningDelimiter openingTokenRange (ifElse isFixed sepOpenAFixed sepOpenA)
    else
        genTriviaFor SynExpr_ArrayOrList_OpeningDelimiter openingTokenRange (ifElse isFixed sepOpenLFixed sepOpenL)

and genArrayOrListClosingTokenAux closingTokenRange f =
    genTriviaFor SynExpr_ArrayOrList_ClosingDelimiter closingTokenRange f

and genArrayOrListClosingToken isArray isFixed closingTokenRange =
    let f =
        if isArray then
            (ifElse isFixed sepCloseAFixed sepCloseA)
        else
            (ifElse isFixed sepCloseLFixed sepCloseL)

    genArrayOrListClosingTokenAux closingTokenRange f

and genLambdaPats pats =
    let shortPats = col sepSpace pats genPat
    let longPats = indentSepNlnUnindent (col sepNln pats genPat)
    expressionFitsOnRestOfLine shortPats longPats

and genOnelinerInfixExpr e1 operatorSli e2 =
    let genExpr e =
        match e with
        | Record _
        | AnonRecord _ -> atCurrentColumnIndent (genExpr e)
        | _ -> genExpr e

    genExpr e1
    +> sepSpace
    +> genSynLongIdent false operatorSli
    +> sepNlnWhenWriteBeforeNewlineNotEmpty
    +> sepSpace
    +> genExpr e2

and genMultilineInfixExpr e1 operatorText operatorSli e2 =
    let genE1 (ctx: Context) =
        match e1 with
        | SynExpr.IfThenElse _ when (ctx.Config.IndentSize - 1 <= operatorText.Length) ->
            autoParenthesisIfExpressionExceedsPageWidth (genExpr e1) ctx
        | SynExpr.Match _ when (ctx.Config.IndentSize <= operatorText.Length) ->
            let ctxAfterMatch = genExpr e1 ctx

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
                    | Some(UnIndentBy _) -> false
                    | _ -> true

            if lastClauseIsSingleLine then
                ctxAfterMatch
            else
                autoParenthesisIfExpressionExceedsPageWidth (genExpr e1) ctx
        | _ -> genExpr e1 ctx

    atCurrentColumn (
        genE1
        +> sepNln
        +> genSynLongIdent false operatorSli
        +> sepNlnWhenWriteBeforeNewlineNotEmpty
        +> sepSpace
        +> genExprInMultilineInfixExpr e2
    )

and genExprInMultilineInfixExpr e =
    match e with
    | LetOrUses(xs, e) ->
        atCurrentColumn (
            col sepNln xs (fun (lb, inKeyword) ->
                genSynBinding lb
                +> (match inKeyword with
                    | Some inKw -> genTriviaFor SynExpr_LetOrUse_In inKw !- " in"
                    | None -> !- " in"))
            +> sepNln
            +> expressionFitsOnRestOfLine
                (genExpr e)
                (let t, r = synExprToFsAstType e in

                 sepNlnConsideringTriviaContentBeforeFor t r +> genExpr e)
        )
    | Paren(lpr, (Match _ as mex), rpr, pr) ->
        fun ctx ->
            if ctx.Config.MultiLineLambdaClosingNewline then
                (sepOpenTFor lpr
                 +> indentSepNlnUnindent (genExpr mex)
                 +> sepNln
                 +> sepCloseTFor rpr
                 |> genTriviaFor SynExpr_Paren pr)
                    ctx
            else
                (sepOpenTFor lpr +> atCurrentColumnIndent (genExpr mex) +> sepCloseTFor rpr
                 |> genTriviaFor SynExpr_Paren pr)
                    ctx
    | Paren(_, InfixApp(_, _, DotGet _, _, _), _, _)
    | Paren(_, DotGetApp _, _, _) -> atCurrentColumnIndent (genExpr e)
    | MatchLambda(keywordRange, cs) ->
        (!- "function " |> genTriviaFor SynExpr_MatchLambda_Function keywordRange)
        +> indentSepNlnUnindent (genClauses cs)
        |> genTriviaFor SynExpr_MatchLambda e.Range
    | Record _ -> atCurrentColumnIndent (genExpr e)
    | _ -> genExpr e

and genSpaceBeforeLids
    (currentIndex: int)
    (lastEsIndex: int)
    (lids: SynLongIdent)
    (arg: SynExpr)
    (ctx: Context)
    : Context =
    let config =
        match lids with
        | SynLongIdent(id = (h :: _)) ->
            let s = h.idText

            if Char.IsUpper(s.[0]) then
                ctx.Config.SpaceBeforeUppercaseInvocation
            else
                ctx.Config.SpaceBeforeLowercaseInvocation
        | _ -> ctx.Config.SpaceBeforeUppercaseInvocation

    if (lastEsIndex = currentIndex) && (not (hasParenthesis arg) || config) then
        sepSpace ctx
    else
        ctx

and genFunctionNameWithMultilineLids f (synLongIdent: SynLongIdent) =
    match synLongIdent.IdentsWithTrivia with
    | h :: t ->
        genSynIdent false h
        +> indentSepNlnUnindent (col sepNln t (genSynIdent true) +> f)
    | _ -> sepNone
    |> genTriviaFor SynLongIdent_ synLongIdent.FullRange

and genMultilineFunctionApplicationArguments argExpr =
    let argsInsideParenthesis lpr rpr pr f =
        sepOpenTFor lpr +> indentSepNlnUnindent f +> sepNln +> sepCloseTFor rpr
        |> genTriviaFor SynExpr_Paren pr

    let genExpr e =
        match e with
        | InfixApp(equal, operatorSli, e1, e2, range) when (equal = "=") -> genNamedArgumentExpr operatorSli e1 e2 range
        | _ -> genExpr e

    match argExpr with
    | Paren(lpr, Lambda(pats, arrowRange, body, range), rpr, _pr) ->
        fun ctx ->
            if ctx.Config.MultiLineLambdaClosingNewline then
                let genPats =
                    let shortPats = col sepSpace pats genPat
                    let longPats = atCurrentColumn (sepNln +> col sepNln pats genPat)
                    expressionFitsOnRestOfLine shortPats longPats

                (sepOpenTFor lpr
                 +> (!- "fun " +> genPats +> genLambdaArrowWithTrivia genExpr body arrowRange
                     |> genTriviaFor SynExpr_Lambda range)
                 +> sepNln
                 +> sepCloseTFor rpr)
                    ctx
            else
                genExpr argExpr ctx
    | Paren(lpr, Tuple(args, tupleRange), rpr, pr) ->
        genTupleMultiline args
        |> genTriviaFor SynExpr_Tuple tupleRange
        |> argsInsideParenthesis lpr rpr pr
    | Paren(lpr, singleExpr, rpr, pr) -> genExpr singleExpr |> argsInsideParenthesis lpr rpr pr
    | _ -> genExpr argExpr

and genGenericTypeParameters lt ts gt =
    match ts with
    | [] -> sepNone
    | ts ->
        genTriviaFor SynExpr_TypeApp_Less lt !- "<"
        +> coli sepComma ts (fun idx t ->
            let leadingSpace =
                match t with
                | TVar(Typar(_, true), _) when idx = 0 -> sepSpace
                | _ -> sepNone

            leadingSpace +> genType t)
        +> indentIfNeeded sepNone
        +> genTriviaFor SynExpr_TypeApp_Greater gt !- ">"

and genMultilineRecordInstance
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
    let fieldsExpr = col sepNln xs genRecordFieldName

    let expr =
        match inheritOpt with
        | Some inheritCtor ->
            genTriviaFor SynExpr_Record_OpeningBrace openingBrace sepOpenS
            +> atCurrentColumn (
                !- "inherit "
                +> autoIndentAndNlnIfExpressionExceedsPageWidth (genInheritConstructor inheritCtor)
                +> onlyIf (List.isNotEmpty xs) sepNln
                +> fieldsExpr
                +> genTriviaFor SynExpr_Record_ClosingBrace closingBrace sepCloseS
            )
        | None ->
            match eo with
            | None ->
                fun (ctx: Context) ->
                    // position after `{ ` or `{`
                    let targetColumn = ctx.Column + (if ctx.Config.SpaceAroundDelimiter then 2 else 1)

                    atCurrentColumn
                        (genTriviaFor SynExpr_Record_OpeningBrace openingBrace sepOpenS
                         +> sepNlnWhenWriteBeforeNewlineNotEmpty // comment after curly brace
                         +> col sepNln xs (fun e ->
                             // Add spaces to ensure the record field (incl trivia) starts at the right column.
                             addFixedSpaces targetColumn
                             // Lock the start of the record field, however keep potential indentations in relation to the opening curly brace
                             +> atCurrentColumn (genRecordFieldName e))
                         +> genTriviaFor
                             SynExpr_Record_ClosingBrace
                             closingBrace
                             (sepNlnWhenWriteBeforeNewlineNotEmpty // comment after last record field
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
                +> atCurrentColumnIndent (genExpr e)
                +> !- " with"
                +> ifIndentLesserThan
                    3
                    (sepSpaceOrDoubleIndentAndNlnIfExpressionExceedsPageWidth fieldsExpr)
                    (sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth fieldsExpr)
                +> genTriviaFor SynExpr_Record_ClosingBrace closingBrace sepCloseS

    expr ctx

and genMultilineRecordInstanceAlignBrackets
    (openingBrace: Range)
    (inheritOpt: (SynType * SynExpr) option)
    (xs: SynExprRecordField list)
    (eo: SynExpr option)
    (closingBrace: Range)
    =
    let fieldsExpr = col sepNln xs genRecordFieldName
    let hasFields = List.isNotEmpty xs

    match inheritOpt, eo with
    | Some inheritCtor, None ->
        genTriviaFor SynExpr_Record_OpeningBrace openingBrace sepOpenSFixed
        +> indentSepNlnUnindent (
            !- "inherit "
            +> autoIndentAndNlnIfExpressionExceedsPageWidth (genInheritConstructor inheritCtor)
            +> onlyIf hasFields sepNln
            +> fieldsExpr
        )
        +> sepNln
        +> genTriviaFor SynExpr_Record_ClosingBrace closingBrace sepCloseSFixed
    | None, Some e ->
        genTriviaFor SynExpr_Record_OpeningBrace openingBrace sepOpenS
        +> atCurrentColumnIndent (genExpr e)
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
         +> indentSepNlnUnindent fieldsExpr
         +> ifElseCtx lastWriteEventIsNewline sepNone sepNln
         +> genTriviaFor SynExpr_Record_ClosingBrace closingBrace sepCloseSFixed)

and genInheritConstructor (inheritCtor: SynType * SynExpr) =
    match inheritCtor with
    | TypeOnlyInheritConstructor t -> genType t
    | UnitInheritConstructor t -> genType t +> sepSpaceBeforeClassConstructor +> sepOpenT +> sepCloseT
    | ParenInheritConstructor(t, px) ->
        genType t
        +> sepSpaceBeforeClassConstructor
        +> expressionFitsOnRestOfLine (genExpr px) (genMultilineFunctionApplicationArguments px)
    | OtherInheritConstructor(t, e) -> genType t +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (genExpr e)

and genMultilineAnonRecord openingBrace (isStruct: bool) fields copyInfo closingBrace =
    let recordExpr =
        match copyInfo with
        | Some e ->
            genTriviaFor SynExpr_AnonRecd_OpeningBrace openingBrace sepOpenAnonRecd
            +> sepNlnWhenWriteBeforeNewlineNotEmpty // comment after curly brace
            +> atCurrentColumn (
                genExpr e
                +> (!- " with" +> indentSepNlnUnindent (col sepNln fields genAnonRecordFieldName))
            )
            +> genTriviaFor SynExpr_AnonRecd_ClosingBrace closingBrace sepCloseAnonRecd
        | None ->
            fun ctx ->
                // position after `{| ` or `{|`
                let targetColumn = ctx.Column + (if ctx.Config.SpaceAroundDelimiter then 3 else 2)

                atCurrentColumn
                    (genTriviaFor SynExpr_AnonRecd_OpeningBrace openingBrace sepOpenAnonRecdFixed
                     +> sepNlnWhenWriteBeforeNewlineNotEmpty // comment after curly brace
                     +> col sepNln fields (fun (AnonRecordFieldName(ident, eq, e, range)) ->
                         let expr =
                             if ctx.Config.IndentSize < 3 then
                                 sepSpaceOrDoubleIndentAndNlnIfExpressionExceedsPageWidth (genExpr e)
                             else
                                 sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (genExpr e)

                         // Add enough spaces to start at the right column but indent from the opening curly brace.
                         // Use a double indent when using a small indent size to avoid offset warnings.
                         addFixedSpaces targetColumn
                         +> atCurrentColumn (enterNodeFor SynExpr_AnonRecd_Field range +> genIdent ident)
                         +> genEq SynExpr_AnonRecd_Field_Equals eq
                         +> expr
                         +> leaveNodeFor SynExpr_AnonRecd_Field range)
                     +> genTriviaFor SynExpr_AnonRecd_ClosingBrace closingBrace sepCloseAnonRecd)
                    ctx

    onlyIf isStruct !- "struct " +> recordExpr

and genMultilineAnonRecordAlignBrackets openingBrace (isStruct: bool) fields copyInfo closingBrace =
    let fieldsExpr = col sepNln fields genAnonRecordFieldName

    let copyExpr fieldsExpr e =
        atCurrentColumnIndent (genExpr e)
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
            genTriviaFor SynExpr_AnonRecd_OpeningBrace openingBrace sepOpenAnonRecd
            +> sepNlnWhenWriteBeforeNewlineNotEmpty // comment after curly brace
            +> copyExpr fieldsExpr ci
            +> sepNln
            +> genTriviaFor SynExpr_AnonRecd_ClosingBrace closingBrace sepCloseAnonRecdFixed
        | None ->
            genTriviaFor SynExpr_AnonRecd_OpeningBrace openingBrace sepOpenAnonRecd
            +> indentSepNlnUnindent fieldsExpr
            +> sepNln
            +> genTriviaFor SynExpr_AnonRecd_ClosingBrace closingBrace sepCloseAnonRecdFixed

    ifElse isStruct !- "struct " sepNone +> genAnonRecord

and genObjExpr t eio withKeyword bd members ims =
    // Check the role of the second part of eio
    let param = opt sepNone (Option.map fst eio) genExpr

    sepOpenS
    +> atCurrentColumn (
        !- "new "
        +> genType t
        +> param
        +> genTriviaForOption SynExpr_ObjExpr_With withKeyword !- " with"
        +> indentSepNlnUnindent (genSynBindings bd false +> genMemberDefnList members)
        +> colPre sepNln sepNln ims genInterfaceImpl
    )
    +> sepCloseS

and genObjExprAlignBrackets t eio withKeyword bd members ims =
    // Check the role of the second part of eio
    let param = opt sepNone (Option.map fst eio) genExpr

    let genObjExpr =
        atCurrentColumn (
            !- "new "
            +> genType t
            +> param
            +> genTriviaForOption SynExpr_ObjExpr_With withKeyword !- " with"
            +> indentSepNlnUnindent (genSynBindings bd false +> genMemberDefnList members)
            +> colPre sepNln sepNln ims genInterfaceImpl
        )

    atCurrentColumnIndent (sepOpenS +> genObjExpr +> sepNln +> sepCloseSFixed)

and genMultiLineArrayOrList (isArray: bool) xs (openingTokenRange: Range) (closingTokenRange: Range) =
    genArrayOrListOpeningToken isArray false openingTokenRange
    +> atCurrentColumnIndent (
        sepNlnWhenWriteBeforeNewlineNotEmpty
        +> col sepNln xs genExpr
        +> (genArrayOrListClosingTokenAux closingTokenRange (fun ctx ->
            let isFixed = lastWriteEventIsNewline ctx

            if isArray && isFixed then sepCloseAFixed ctx
            elif isArray then sepCloseA ctx
            elif isFixed then sepCloseLFixed ctx
            else sepCloseL ctx))
    )

and genMultiLineArrayOrListAlignBrackets (isArray: bool) xs openingTokenRange closingTokenRange =
    genArrayOrListOpeningToken isArray true openingTokenRange
    +> indent
    +> sepNlnUnlessLastEventIsNewline
    +> col sepNln xs genExpr
    +> unindent
    +> sepNlnUnlessLastEventIsNewline
    +> genArrayOrListClosingToken isArray true closingTokenRange

and genApp e es ctx =
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

        atCurrentColumn (genExpr e +> addFirstSpace +> col sepSpace es genExpr)

    let isParenLambda =
        (function
        | Paren(_, Lambda _, _, _)
        | Paren(_, MatchLambda _, _, _) -> true
        | _ -> false)

    let shouldHaveAlternativeLambdaStyle =
        let hasLambdas = List.exists isParenLambda es

        ctx.Config.MultiLineLambdaClosingNewline && hasLambdas

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
                            (sepOpenTFor lpr
                             +> !- "fun "
                             +> pats
                             +> genLambdaArrowWithTrivia genExpr bodyExpr arrowRange)
                            (fun isMultiline -> onlyIf isMultiline sepNln +> sepCloseTFor rpr)
                        |> genTriviaFor SynExpr_Paren pr

                    match e with
                    | Paren(lpr, Lambda(pats, arrowRange, expr, _range), rpr, pr) ->
                        genLambda (genLambdaPats pats) expr lpr rpr arrowRange pr
                    | _ -> genExpr e)

            genExpr e +> indentSepNlnUnindent argExpr
        else
            atCurrentColumn (genExpr e +> indentSepNlnUnindent (col sepNln es genExpr))

    if
        List.exists
            (function
            | ComputationExpr _ -> true
            | _ -> false)
            es
    then
        shortExpression ctx
    else
        expressionFitsOnRestOfLine shortExpression longExpression ctx

and genLambdaMultiLineClosingNewline

    (lpr: Range)
    (pats: SynPat list)
    (arrowRange: Range option)
    (bodyExpr: SynExpr)
    (lambdaRange: Range)
    (rpr: Range option)
    (pr: Range)
    : Context -> Context =
    leadingExpressionIsMultiline
        (sepOpenTFor lpr
         +> !- "fun "
         +> col sepSpace pats genPat
         +> genLambdaArrowWithTrivia genExpr bodyExpr arrowRange
         |> genTriviaFor SynExpr_Lambda lambdaRange)
        (fun isMultiline -> onlyIf isMultiline sepNln +> sepCloseTFor rpr)
    |> genTriviaFor SynExpr_Paren pr

and genAppWithTupledArgument (e, lpr, ts, tr, rpr, _pr) =
    genExpr e
    +> sepSpace
    +> sepOpenTFor lpr
    +> (col sepComma ts genExpr |> genTriviaFor SynExpr_Tuple tr)
    +> sepCloseTFor rpr

and genAlternativeAppWithTupledArgument (e, lpr, ts, tr, rpr, _pr) =
    genExpr e
    +> indent
    +> sepNln
    +> sepOpenTFor lpr
    +> indent
    +> sepNln
    +> (col (sepComma +> sepNln) ts genExpr |> genTriviaFor SynExpr_Tuple tr)
    +> unindent
    +> sepNln
    +> sepCloseTFor rpr
    +> unindent

and genAlternativeAppWithSingleParenthesisArgument (e, lpr, a, rpr, _pr) =
    genExpr e
    +> sepSpaceWhenOrIndentAndNlnIfExpressionExceedsPageWidth
        (fun ctx ->
            match e with
            | Paren _ -> true
            | UppercaseSynExpr _ -> ctx.Config.SpaceBeforeUppercaseInvocation
            | LowercaseSynExpr _ -> ctx.Config.SpaceBeforeLowercaseInvocation)
        (sepOpenTFor lpr
         +> expressionFitsOnRestOfLine (genExpr a) (indentSepNlnUnindent (genExpr a) +> sepNln)
         +> sepCloseTFor rpr)

and genAppWithSingleParenthesisArgument (e, lpr, a, rpr, _pr) =
    genExpr e +> sepSpace +> sepOpenTFor lpr +> (genExpr a) +> sepCloseTFor rpr

and genAppWithLambda sep (e, es, lpr, lambda, rpr, pr) =
    let short =
        genExpr e
        +> sep
        +> col sepSpace es genExpr
        +> onlyIf (List.isNotEmpty es) sepSpace
        +> (sepOpenTFor lpr
            +> (match lambda with
                | Choice1Of2(pats, arrowRange, body, lambdaRange) ->
                    !- "fun "
                    +> col sepSpace pats genPat
                    +> optSingle (fun arrowRange -> sepArrow |> genTriviaFor SynExpr_Lambda_Arrow arrowRange) arrowRange
                    +> genExpr body
                    |> genTriviaFor SynExpr_Lambda lambdaRange
                | Choice2Of2(keywordRange, cs, range) ->
                    (!- "function " |> genTriviaFor SynExpr_MatchLambda_Function keywordRange)
                    +> indentSepNlnUnindent (genClauses cs)
                    |> genTriviaFor SynExpr_MatchLambda range)
            +> sepNlnWhenWriteBeforeNewlineNotEmpty
            +> sepCloseTFor rpr
            |> genTriviaFor SynExpr_Paren pr)

    let long (ctx: Context) : Context =
        if ctx.Config.MultiLineLambdaClosingNewline then
            let genArguments =
                match es with
                | [] ->
                    match lambda with
                    | Choice1Of2(pats, arrowRange, bodyExpr, range) ->
                        sepOpenTFor lpr
                        +> (!- "fun "
                            +> col sepSpace pats genPat
                            +> genLambdaArrowWithTrivia genExpr bodyExpr arrowRange
                            |> genTriviaFor SynExpr_Lambda range)
                        +> sepNln
                        +> sepCloseTFor rpr
                        |> genTriviaFor SynExpr_Paren pr
                    | Choice2Of2(keywordRange, cs, matchLambdaRange) ->
                        sepOpenTFor lpr
                        +> indentSepNlnUnindent (
                            (!- "function " |> genTriviaFor SynExpr_MatchLambda_Function keywordRange)
                            +> sepNln
                            +> genClauses cs
                            |> genTriviaFor SynExpr_MatchLambda matchLambdaRange
                        )
                        +> sepNln
                        +> sepCloseTFor rpr
                        |> genTriviaFor SynExpr_Paren pr

                | es ->
                    col sepNln es genExpr
                    +> sepNln
                    +> (match lambda with
                        | Choice1Of2(pats, arrowRange, bodyExpr, range) ->
                            genLambdaMultiLineClosingNewline lpr pats arrowRange bodyExpr range rpr pr
                        | Choice2Of2(keywordRange, cs, matchLambdaRange) ->
                            (sepOpenTFor lpr
                             +> ((!- "function " |> genTriviaFor SynExpr_MatchLambda_Function keywordRange)
                                 +> sepNln
                                 +> genClauses cs
                                 |> genTriviaFor SynExpr_MatchLambda matchLambdaRange)
                             +> sepNln
                             +> sepCloseTFor rpr)
                            |> genTriviaFor SynExpr_Paren pr)
                    +> unindent

            (genExpr e +> ifElse (List.isEmpty es) sep (indent +> sepNln) +> genArguments) ctx
        else
            match lambda with
            | Choice1Of2(pats, arrowRange, body, lambdaRange) ->
                let singleLineTestExpr =
                    genExpr e
                    +> sep
                    +> col sepSpace es genExpr
                    +> sep
                    +> enterNodeFor SynExpr_Paren pr
                    +> sepOpenTFor lpr
                    +> enterNodeFor SynExpr_Lambda lambdaRange
                    +> !- "fun "
                    +> col sepSpace pats genPat
                    +> optSingle (fun arrowRange -> sepArrow |> genTriviaFor SynExpr_Lambda_Arrow arrowRange) arrowRange

                let singleLine =
                    genExpr e
                    +> sep
                    +> col sepSpace es genExpr
                    +> sep
                    +> (sepOpenTFor lpr
                        +> (!- "fun "
                            +> col sepSpace pats genPat
                            +> genLambdaArrowWithTrivia genExpr body arrowRange
                            |> genTriviaFor SynExpr_Lambda lambdaRange)
                        +> sepNlnWhenWriteBeforeNewlineNotEmpty
                        +> sepCloseTFor rpr
                        |> genTriviaFor SynExpr_Paren pr)

                let multiLine =
                    genExpr e
                    +> indentSepNlnUnindent (
                        col sepNln es genExpr
                        +> onlyIfNot (List.isEmpty es) sepNln
                        +> (sepOpenTFor lpr
                            +> (!- "fun "
                                +> col sepSpace pats genPat
                                +> genLambdaArrowWithTrivia genExpr body arrowRange
                                |> genTriviaFor SynExpr_Lambda lambdaRange)
                            +> sepCloseTFor rpr
                            |> genTriviaFor SynExpr_Paren pr)
                    )

                if futureNlnCheck singleLineTestExpr ctx then
                    multiLine ctx
                else
                    singleLine ctx

            | Choice2Of2(keywordRange, cs, matchLambdaRange) ->
                let singleLineTestExpr =
                    genExpr e
                    +> sep
                    +> col sepSpace es genExpr
                    +> enterNodeFor SynExpr_Paren pr
                    +> sepOpenTFor lpr
                    +> enterNodeFor SynExpr_MatchLambda matchLambdaRange
                    +> enterNodeFor SynExpr_MatchLambda_Function keywordRange
                    +> !- "function "

                let singleLine =
                    genExpr e
                    +> sep
                    +> col sepSpace es genExpr
                    +> sepSpace
                    +> (sepOpenTFor lpr
                        +> ((!- "function " |> genTriviaFor SynExpr_MatchLambda_Function keywordRange)
                            +> indentSepNlnUnindent (genClauses cs)
                            |> genTriviaFor SynExpr_MatchLambda matchLambdaRange)
                        +> sepNlnWhenWriteBeforeNewlineNotEmpty
                        +> sepCloseTFor rpr)
                    |> genTriviaFor SynExpr_Paren pr

                let multiLine =
                    genExpr e
                    +> indentSepNlnUnindent (
                        col sepNln es genExpr
                        +> sepNln
                        +> (sepOpenTFor lpr
                            +> atCurrentColumn (
                                (!- "function " |> genTriviaFor SynExpr_MatchLambda_Function keywordRange)
                                +> sepNln
                                +> genClauses cs
                                |> genTriviaFor SynExpr_MatchLambda matchLambdaRange
                            )
                            +> sepCloseTFor rpr
                            |> genTriviaFor SynExpr_Paren pr)
                    )

                if futureNlnCheck singleLineTestExpr ctx then
                    multiLine ctx
                else
                    singleLine ctx

    expressionFitsOnRestOfLine short long

and genControlExpressionStartCore
    enterStartKeyword
    genStartKeyword
    leaveStartKeyword
    innerExpr
    enterEndKeyword
    genEndKeyword
    leaveEndKeyword
    =
    let shortIfExpr =
        genStartKeyword
        +> leaveStartKeyword
        +> sepNlnWhenWriteBeforeNewlineNotEmptyOr sepSpace
        +> genExpr innerExpr
        +> sepSpace
        +> enterEndKeyword
        +> genEndKeyword

    let longIfExpr =
        genStartKeyword
        +> leaveStartKeyword
        +> indentSepNlnUnindent (genExpr innerExpr)
        +> sepNln
        +> enterEndKeyword
        +> genEndKeyword

    // A code comment before the start keyword should not make the expression long.
    enterStartKeyword
    +> expressionFitsOnRestOfLine shortIfExpr longIfExpr
    +> leaveEndKeyword

and genControlExpressionStart
    (startKeywordType: FsAstType)
    (startKeywordRange: range)
    (startKeywordText: string)
    (innerExpr: SynExpr)
    (endKeywordType: FsAstType)
    (endKeywordRange: range)
    (endKeywordText: string)
    =
    let enterStartKeyword = enterNodeFor startKeywordType startKeywordRange
    let genStartKeyword = !-startKeywordText
    let leaveStartKeyword = leaveNodeFor startKeywordType startKeywordRange
    let enterEndKeyword = enterNodeFor endKeywordType endKeywordRange
    let genEndKeyword = !-endKeywordText
    let leaveEndKeyword = leaveNodeFor endKeywordType endKeywordRange

    genControlExpressionStartCore
        enterStartKeyword
        genStartKeyword
        leaveStartKeyword
        innerExpr
        enterEndKeyword
        genEndKeyword
        leaveEndKeyword

and genIfThen (ifKeyword: range) (ifExpr: SynExpr) (thenKeyword: range) =
    genControlExpressionStart SynExpr_IfThenElse_If ifKeyword "if" ifExpr SynExpr_IfThenElse_Then thenKeyword "then"

and genIfOrElseIfOrElifThen (elseKwOpt: range option) (ifKw: range) (isElif: bool) (condExpr: SynExpr) (thenKw: range) =
    let enterStartKeyword, genStartKeyword, leaveStartKeyword =
        match elseKwOpt with
        | Some elseKw ->
            enterNodeFor SynExpr_IfThenElse_Else elseKw,
            !- "else"
            +> leaveNodeFor SynExpr_IfThenElse_Else elseKw
            +> sepNlnWhenWriteBeforeNewlineNotEmptyOr sepSpace
            +> enterNodeFor SynExpr_IfThenElse_If ifKw
            +> !- "if",
            leaveNodeFor SynExpr_IfThenElse_If ifKw
        | None ->
            if isElif then
                enterNodeFor SynExpr_IfThenElse_Elif ifKw, !- "elif", leaveNodeFor SynExpr_IfThenElse_Elif ifKw
            else
                enterNodeFor SynExpr_IfThenElse_If ifKw, !- "if", leaveNodeFor SynExpr_IfThenElse_If ifKw

    let enterEndKeyword = enterNodeFor SynExpr_IfThenElse_Then thenKw
    let genEndKeyword = !- "then"
    let leaveEndKeyword = leaveNodeFor SynExpr_IfThenElse_Then thenKw

    genControlExpressionStartCore
        enterStartKeyword
        genStartKeyword
        leaveStartKeyword
        condExpr
        enterEndKeyword
        genEndKeyword
        leaveEndKeyword

and genMatchWith (matchKeyword: range) (matchExpr: SynExpr) (withKeyword: range) =
    genControlExpressionStart SynExpr_Match_Match matchKeyword "match" matchExpr SynExpr_Match_With withKeyword "with"

and genMatchBangWith (matchKeyword: range) (matchExpr: SynExpr) (withKeyword: range) =
    genControlExpressionStart
        SynExpr_MatchBang_Match
        matchKeyword
        "match!"
        matchExpr
        SynExpr_MatchBang_With
        withKeyword
        "with"

and collectMultilineItemForSynExpr (e: SynExpr) : ColMultilineItem list =
    match e with
    | LetOrUses(bs, e) -> collectMultilineItemForLetOrUses bs (collectMultilineItemForSynExpr e)
    | Sequentials s -> s |> List.collect collectMultilineItemForSynExpr
    | _ ->
        let t, r = synExprToFsAstType e
        [ ColMultilineItem(genExpr e, sepNlnConsideringTriviaContentBeforeFor t r) ]

and collectMultilineItemForLetOrUses
    (bs: (SynBinding * range option) list)
    (itemsForExpr: ColMultilineItem list)
    : ColMultilineItem list =

    let multilineBinding (x: SynBinding) inKw =
        let expr = genSynBinding x +> genTriviaForOption SynExpr_LetOrUse_In inKw !- " in "

        let sepNln = sepNlnConsideringTriviaContentBeforeFor SynBinding_ x.FullRange
        ColMultilineItem(expr, sepNln)

    let multipleOrLongBs bs =
        bs |> List.map (fun (x, inKw) -> multilineBinding x inKw)

    match bs, itemsForExpr with
    | [], _ -> itemsForExpr
    | [ b, inKeyword ], [ ColMultilineItem(expr, sepNlnForExpr) ] ->
        // This is a trickier case
        // maybe the let binding and expression are short so they form one ColMultilineItem
        // Something like: let a = 1 in ()
        let sepNlnForBinding =
            sepNlnConsideringTriviaContentBeforeFor SynBinding_ b.FullRange

        match inKeyword with
        | Some inKw ->
            // single multiline item
            let expr =
                genSynBinding b
                +> genTriviaFor SynExpr_LetOrUse_In inKw !- " in "
                +> expressionFitsOnRestOfLine expr (sepNln +> sepNlnForExpr +> expr)

            [ ColMultilineItem(expr, sepNlnForBinding) ]
        | None -> multipleOrLongBs bs @ itemsForExpr
    | bs, _ -> multipleOrLongBs bs @ itemsForExpr

and sepNlnBetweenTypeAndMembers (withKeywordRange: range option) (ms: SynMemberDefn list) =
    match List.tryHead ms with
    | Some m -> sepNlnTypeAndMembers SynTypeDefn_With withKeywordRange m.Range (synMemberDefnToFsAstType m)
    | None -> sepNone

and genTypeDefn (TypeDef(ats, px, leadingKeyword, ao, tds, tcs, equalsRange, tdr, withKeyword, ms, lids, _) as node) =
    let genLeadingKeyword =
        match leadingKeyword with
        | SynTypeDefnLeadingKeyword.Type mType -> genAttributes ats +> genTriviaFor SynTypeDefn_Type mType !- "type "
        | SynTypeDefnLeadingKeyword.And mAnd -> genTriviaFor SynTypeDefn_And mAnd !- "and " +> genOnelinerAttributes ats
        | SynTypeDefnLeadingKeyword.StaticType _
        | SynTypeDefnLeadingKeyword.Synthetic -> sepNone

    let typeName =
        genPreXmlDoc px
        +> genLeadingKeyword
        +> genAccessOpt ao
        +> genTypeAndParam (genLongIdent lids) tds tcs

    match tdr with
    | Simple(TDSREnum ecs) ->
        typeName
        +> genEq SynTypeDefn_Equals equalsRange
        +> indent
        +> sepNln
        +> (col sepNln ecs genEnumCase
            +> onlyIf (List.isNotEmpty ms) sepNln
            +> sepNlnBetweenTypeAndMembers withKeyword ms
            +> genMemberDefnList ms
            // Add newline after un-indent to be spacing-correct
            +> unindent)

    | Simple(TDSRUnion(ao', xs)) ->
        let unionCases (ctx: Context) =
            match xs with
            | [] -> ctx
            | [ UnionCase(attrs, px, _, _, _, UnionCaseType fields, _) as x ] when List.isEmpty ms ->
                let hasVerticalBar =
                    ctx.Config.BarBeforeDiscriminatedUnionDeclaration
                    || List.isNotEmpty attrs
                    || List.isEmpty fields

                let short =
                    opt sepSpace ao' (fun vis -> genAccess vis +> onlyIfNot px.IsEmpty sepNln)
                    +> genUnionCase hasVerticalBar x

                let long =
                    opt sepSpace ao' (fun vis -> genAccess vis +> onlyIfNot px.IsEmpty sepNln)
                    +> genUnionCase true x

                expressionFitsOnRestOfLine (indent +> sepSpace +> short) (indent +> sepNln +> long) ctx
            | xs ->
                indent
                +> sepNln
                +> (opt sepNln ao' genAccess +> col sepNln xs (genUnionCase true))
                <| ctx

        typeName
        +> genEq SynTypeDefn_Equals equalsRange
        +> unionCases
        +> onlyIf (List.isNotEmpty ms) sepNln
        +> sepNlnBetweenTypeAndMembers withKeyword ms
        +> genMemberDefnList ms
        +> unindent

    | Simple(TDSRRecord(openingBrace, ao', fs, closingBrace)) ->
        let smallExpression =
            sepSpace
            +> optSingle (fun ao -> genAccess ao +> sepSpace) ao'
            +> genTriviaFor SynTypeDefnSimpleRepr_Record_OpeningBrace openingBrace sepOpenS
            +> col sepSemi fs genField
            +> genTriviaFor SynTypeDefnSimpleRepr_Record_ClosingBrace closingBrace sepCloseS

        let multilineExpression (ctx: Context) =
            if
                ctx.Config.MultilineBlockBracketsOnSameColumn
                || (List.exists (fun (SynField(xmlDoc = xmlDoc)) -> not xmlDoc.IsEmpty) fs)
            then
                genMultilineSimpleRecordTypeDefnAlignBrackets openingBrace withKeyword ms ao' fs closingBrace ctx
            else
                genMultilineSimpleRecordTypeDefn openingBrace withKeyword ms ao' fs closingBrace ctx

        let bodyExpr size ctx =
            if (List.isEmpty ms) then
                (isSmallExpression size smallExpression multilineExpression) ctx
            else
                multilineExpression ctx

        let genTypeDefinition (ctx: Context) =
            let size = getRecordSize ctx fs
            let short = bodyExpr size

            if ctx.Config.ExperimentalStroustrupStyle && ms.IsEmpty then
                (sepSpace +> short) ctx
            else
                isSmallExpression size short (indentSepNlnUnindent short) ctx

        typeName +> genEq SynTypeDefn_Equals equalsRange +> genTypeDefinition

    | Simple TDSRNone -> typeName
    | Simple(TDSRTypeAbbrev t) ->
        let genTypeAbbrev =
            let needsParenthesis =
                match t with
                | SynType.Tuple(isStruct, typeNames, _) -> (isStruct && List.length typeNames > 1)
                | _ -> false

            ifElse needsParenthesis sepOpenT sepNone
            +> genType t
            +> ifElse needsParenthesis sepCloseT sepNone

        let genMembers =
            ifElse
                (List.isEmpty ms)
                (!- "")
                (indent
                 +> sepNln
                 +> !- "with"
                 +> indent
                 +> sepNln
                 +> sepNlnBetweenTypeAndMembers withKeyword ms
                 +> genMemberDefnList ms
                 +> unindent
                 +> unindent)

        let genTypeBody =
            autoIndentAndNlnIfExpressionExceedsPageWidth genTypeAbbrev +> genMembers

        typeName +> genEq SynTypeDefn_Equals equalsRange +> sepSpace +> genTypeBody
    | Simple(TDSRException(ExceptionDefRepr(ats, px, ao, uc))) -> genExceptionBody ats px ao uc

    | ObjectModel(TCSimple(TCInterface | TCClass | TCStruct) as tdk, MemberDefnList(impCtor, others), _) ->
        typeName
        +> leadingExpressionIsMultiline
            (optSingle (fun md -> sepSpaceBeforeClassConstructor +> genMemberDefn md) impCtor)
            (fun isMulti ctx ->
                if isMulti && ctx.Config.AlternativeLongMemberDefinitions then
                    sepEqFixed ctx
                else
                    sepEq ctx)
        +> indent
        +> sepNln
        +> genTypeDefKind tdk
        +> indent
        +> onlyIf (List.isNotEmpty others) sepNln
        +> sepNlnBetweenTypeAndMembers withKeyword ms
        +> genMemberDefnList others
        +> unindent
        +> sepNln
        +> !- "end"
        +> (fun ctx ->
            match ms with
            | [] -> ctx
            | h :: _ ->
                (sepNln
                 +> sepNlnConsideringTriviaContentBeforeFor (synMemberDefnToFsAstType h) h.Range
                 +> genMemberDefnList ms)
                    ctx)
        +> unindent

    | ObjectModel(TCSimple(TCAugmentation withKeywordAug), _, _) ->
        typeName
        +> genTriviaFor SynTypeDefnKind_Augmentation_With withKeywordAug !- " with"
        +> indentSepNlnUnindent (
            // Remember that we use MemberDefn of parent node
            sepNlnBetweenTypeAndMembers withKeyword ms +> genMemberDefnList ms
        )

    | ObjectModel(TCDelegate(FunType ts), _, _) ->
        typeName
        +> genEq SynTypeDefn_Equals equalsRange
        +> sepSpace
        +> !- "delegate of "
        +> genTypeList ts

    | ObjectModel(TCSimple TCUnspecified, MemberDefnList(impCtor, others), _) when not (List.isEmpty ms) ->
        typeName
        +> opt sepNone impCtor genMemberDefn
        +> genEq SynTypeDefn_Equals equalsRange
        +> indent
        +> sepNln
        +> genMemberDefnList others
        +> sepNln
        +> genTriviaForOption SynTypeDefn_With withKeyword !- "with"
        +> indentSepNlnUnindent (genMemberDefnList ms)
        +> unindent

    | ObjectModel(_, MemberDefnList(impCtor, others), _) ->
        let hasAliasInImplicitConstructor =
            match impCtor with
            | Some(SynMemberDefn.ImplicitCtor(selfIdentifier = Some _)) -> true
            | _ -> false

        typeName
        +> leadingExpressionIsMultiline
            (opt sepNone impCtor (fun mdf -> sepSpaceBeforeClassConstructor +> genMemberDefn mdf))
            (fun isMultiline ctx ->
                if
                    ctx.Config.AlternativeLongMemberDefinitions
                    && isMultiline
                    && not hasAliasInImplicitConstructor
                then
                    genEqFixed SynTypeDefn_Equals equalsRange ctx
                else
                    genEq SynTypeDefn_Equals equalsRange ctx)
        +> indentSepNlnUnindent (genMemberDefnList others)

    | ExceptionRepr(ExceptionDefRepr(ats, px, ao, uc)) -> genExceptionBody ats px ao uc
    |> genTriviaFor SynTypeDefn_ node.Range

and genMultilineSimpleRecordTypeDefn openingBrace withKeyword ms ao' fs closingBrace =
    // the typeName is already printed
    sepNlnUnlessLastEventIsNewline
    +> opt (indent +> sepNln) ao' genAccess
    +> enterNodeFor SynTypeDefnSimpleRepr_Record_OpeningBrace openingBrace
    +> sepOpenS
    +> atCurrentColumn (
        leaveNodeFor SynTypeDefnSimpleRepr_Record_OpeningBrace openingBrace
        +> sepNlnWhenWriteBeforeNewlineNotEmpty
        +> col sepNln fs genField
    )
    +> genTriviaFor SynTypeDefnSimpleRepr_Record_ClosingBrace closingBrace sepCloseS
    +> optSingle (fun _ -> unindent) ao'
    +> onlyIf (List.isNotEmpty ms) sepNln
    +> sepNlnBetweenTypeAndMembers withKeyword ms
    +> genMemberDefnList ms

and genMultilineSimpleRecordTypeDefnAlignBrackets openingBrace withKeyword ms ao' fs closingBrace =
    let msIsEmpty = List.isEmpty ms
    // the typeName is already printed
    ifElseCtx
        (fun ctx -> ctx.Config.ExperimentalStroustrupStyle && msIsEmpty)
        (opt sepSpace ao' genAccess)
        (opt (indent +> sepNln) ao' genAccess)
    +> enterNodeFor SynTypeDefnSimpleRepr_Record_OpeningBrace openingBrace
    +> sepOpenSFixed
    +> indentSepNlnUnindent (
        atCurrentColumn (
            leaveNodeFor SynTypeDefnSimpleRepr_Record_OpeningBrace openingBrace
            +> col sepNln fs genField
        )
    )
    +> sepNln
    +> genTriviaFor SynTypeDefnSimpleRepr_Record_ClosingBrace closingBrace sepCloseSFixed
    +> onlyIfCtx
        (fun ctx -> not (ctx.Config.ExperimentalStroustrupStyle && msIsEmpty))
        (optSingle (fun _ -> unindent) ao')
    +> onlyIf (not msIsEmpty) sepNln
    +> sepNlnBetweenTypeAndMembers withKeyword ms
    +> genMemberDefnList ms

and sepNlnBetweenSigTypeAndMembers (withKeyword: range option) (ms: SynMemberSig list) : Context -> Context =
    match List.tryHead ms with
    | Some m -> sepNlnTypeAndMembers SynTypeDefnSig_With withKeyword m.Range (synMemberSigToFsAstType m)
    | None -> sepNone

and genSigTypeDefn
    (SigTypeDef(ats, px, leadingKeyword, ao, tds, tcs, equalsRange, tdr, withKeyword, ms, lid, _preferPostfix, fullRange))
    =
    let genLeadingKeyword =
        match leadingKeyword with
        | SynTypeDefnLeadingKeyword.Type mType -> genAttributes ats +> genTriviaFor SynTypeDefnSig_Type mType !- "type "
        | SynTypeDefnLeadingKeyword.And mAnd ->
            genTriviaFor SynTypeDefnSig_And mAnd !- "and " +> genOnelinerAttributes ats
        | SynTypeDefnLeadingKeyword.StaticType _
        | SynTypeDefnLeadingKeyword.Synthetic -> sepNone

    let genXmlTypeKeywordAttrsAccess =
        genPreXmlDoc px +> genLeadingKeyword +> genAccessOpt ao

    let typeName =
        genXmlTypeKeywordAttrsAccess +> genTypeAndParam (genLongIdent lid) tds tcs

    match tdr with
    | SigSimple(TDSREnum ecs) ->
        typeName
        +> genEq SynTypeDefnSig_Equals equalsRange
        +> indentSepNlnUnindent (
            col sepNln ecs genEnumCase
            +> sepNlnBetweenSigTypeAndMembers withKeyword ms
            +> colPre sepNln sepNln ms genMemberSig
        // Add newline after un-indent to be spacing-correct
        )

    | SigSimple(TDSRUnion(ao', xs)) ->
        let unionCases (ctx: Context) =
            match xs with
            | [] -> ctx
            | [ UnionCase(attrs, px, _, _, _, UnionCaseType fields, _) as x ] when List.isEmpty ms ->
                let hasVerticalBar =
                    ctx.Config.BarBeforeDiscriminatedUnionDeclaration
                    || List.isNotEmpty attrs
                    || List.isEmpty fields

                let short =
                    opt sepSpace ao' (fun vis -> genAccess vis +> onlyIfNot px.IsEmpty sepNln)
                    +> genUnionCase hasVerticalBar x

                let long =
                    opt sepSpace ao' (fun vis -> genAccess vis +> onlyIfNot px.IsEmpty sepNln)
                    +> genUnionCase true x

                expressionFitsOnRestOfLine (indent +> sepSpace +> short) (indent +> sepNln +> long) ctx
            | xs ->
                (indent
                 +> sepNln
                 +> opt sepNln ao' genAccess
                 +> col sepNln xs (genUnionCase true))
                    ctx

        typeName
        +> genEq SynTypeDefnSig_Equals equalsRange
        +> unionCases
        +> sepNlnBetweenSigTypeAndMembers withKeyword ms
        +> colPre sepNln sepNln ms genMemberSig
        +> unindent

    | SigSimple(TDSRRecord(openingBrace, ao', fs, closingBrace)) ->
        let smallExpression =
            sepSpace
            +> optSingle (fun ao -> genAccess ao +> sepSpace) ao'
            +> genTriviaFor SynTypeDefnSimpleRepr_Record_OpeningBrace openingBrace sepOpenS
            +> col sepSemi fs genField
            +> genTriviaFor SynTypeDefnSimpleRepr_Record_ClosingBrace closingBrace sepCloseS

        let multilineExpression (ctx: Context) =
            if
                ctx.Config.MultilineBlockBracketsOnSameColumn
                || (List.exists (fun (SynField(xmlDoc = xmlDoc)) -> not xmlDoc.IsEmpty) fs)
            then
                genSigSimpleRecordAlignBrackets openingBrace withKeyword ms ao' fs closingBrace ctx
            else
                genSigSimpleRecord openingBrace withKeyword ms ao' fs closingBrace ctx

        let bodyExpr size ctx =
            if (List.isEmpty ms) then
                (isSmallExpression size smallExpression multilineExpression) ctx
            else
                multilineExpression ctx

        let genTypeDefinition (ctx: Context) =
            let size = getRecordSize ctx fs
            let short = bodyExpr size

            if ctx.Config.ExperimentalStroustrupStyle && ms.IsEmpty then
                (sepSpace +> short) ctx
            else
                isSmallExpression size short (indentSepNlnUnindent short) ctx

        typeName +> sepEq +> genTypeDefinition

    | SigSimple TDSRNone ->
        let genMembers =
            match ms with
            | [] -> sepNone
            | _ ->
                !- " with"
                +> indentSepNlnUnindent (sepNlnBetweenSigTypeAndMembers withKeyword ms +> col sepNln ms genMemberSig)

        typeName +> genMembers
    | SigSimple(TDSRTypeAbbrev t) ->
        let genTypeAbbrev =
            let needsParenthesis =
                match t with
                | SynType.Tuple(isStruct, typeNames, _) -> (isStruct && List.length typeNames > 1)
                | _ -> false

            ifElse needsParenthesis sepOpenT sepNone
            +> genType t
            +> ifElse needsParenthesis sepCloseT sepNone

        let short =
            genTypeAndParam (genLongIdent lid) tds tcs
            +> genEq SynTypeDefnSig_Equals equalsRange
            +> sepSpace
            +> genTypeAbbrev

        let long =
            genTypeAndParam (genLongIdent lid) tds tcs
            +> sepSpace
            +> genEqFixed SynTypeDefnSig_Equals equalsRange
            +> indentSepNlnUnindent genTypeAbbrev

        genXmlTypeKeywordAttrsAccess +> expressionFitsOnRestOfLine short long
    | SigSimple(TDSRException(ExceptionDefRepr(ats, px, ao, uc))) -> genExceptionBody ats px ao uc

    | SigObjectModel(TCSimple(TCStruct | TCInterface | TCClass) as tdk, mds) ->
        typeName
        +> genEq SynTypeDefnSig_Equals equalsRange
        +> indent
        +> sepNln
        +> genTypeDefKind tdk
        +> indent
        +> colPre sepNln sepNln mds genMemberSig
        +> unindent
        +> sepNln
        +> !- "end"
        +> unindent

    | SigObjectModel(TCSimple(TCAugmentation withKeyword), _) ->
        typeName
        +> genTriviaFor SynTypeDefnKind_Augmentation_With withKeyword !- " with"
        // Remember that we use MemberSig of parent node
        +> indentSepNlnUnindent (col sepNln ms genMemberSig)

    | SigObjectModel(TCDelegate(FunType ts), _) ->
        typeName
        +> genEq SynTypeDefnSig_Equals equalsRange
        +> sepSpace
        +> !- "delegate of "
        +> genTypeList ts
    | SigObjectModel(_, mds) ->
        typeName
        +> genEq SynTypeDefnSig_Equals equalsRange
        +> indentSepNlnUnindent (col sepNln mds genMemberSig)

    | SigExceptionRepr(SigExceptionDefRepr(ats, px, ao, uc)) -> genExceptionBody ats px ao uc
    |> genTriviaFor SynTypeDefnSig_ fullRange

and genSigSimpleRecord openingBrace withKeyword ms ao' fs closingBrace =
    opt (indent +> sepNln) ao' genAccess
    +> sepOpenS
    +> atCurrentColumn (
        leaveNodeFor SynTypeDefnSimpleRepr_Record_OpeningBrace openingBrace
        +> sepNlnWhenWriteBeforeNewlineNotEmpty
        +> col sepNln fs genField
    )
    +> genTriviaFor SynTypeDefnSimpleRepr_Record_ClosingBrace closingBrace sepCloseS
    +> optSingle (fun _ -> unindent) ao'
    +> sepNlnBetweenSigTypeAndMembers withKeyword ms
    +> colPre sepNln sepNln ms genMemberSig

and genSigSimpleRecordAlignBrackets openingBrace withKeyword ms ao' fs closingBrace =
    opt (indent +> sepNln) ao' genAccess
    +> sepOpenSFixed
    +> indent
    +> sepNln
    +> atCurrentColumn (
        leaveNodeFor SynTypeDefnSimpleRepr_Record_OpeningBrace openingBrace
        +> col sepNln fs genField
    )
    +> unindent
    +> sepNln
    +> genTriviaFor SynTypeDefnSimpleRepr_Record_ClosingBrace closingBrace sepCloseSFixed
    +> optSingle (fun _ -> unindent) ao'
    +> sepNlnBetweenSigTypeAndMembers withKeyword ms
    +> colPre sepNln sepNln ms genMemberSig

and genMemberSig node =
    let range, mainNodeName =
        match node with
        | SynMemberSig.Member(_, _, r) -> r, SynMemberSig_Member
        | SynMemberSig.Interface(_, r) -> r, SynMemberSig_Interface
        | SynMemberSig.Inherit(_, r) -> r, SynMemberSig_Inherit
        | SynMemberSig.ValField(_, r) -> r, SynMemberSig_ValField
        | SynMemberSig.NestedType(_, r) -> r, SynMemberSig_NestedType

    match node with
    | MSMember(v, optGetSet) -> genVal v optGetSet
    | MSInterface t -> !- "interface " +> genType t
    | MSInherit t -> !- "inherit " +> genType t
    | MSValField f -> genField f
    | MSNestedType _ -> invalidArg "md" "This is not implemented in F# compiler"
    |> genTriviaFor mainNodeName range

and genConstraints ti tcs =
    let genType =
        let (FunType namedArgs) = ti
        genTypeList namedArgs

    let genConstraints =
        let short =
            ifElse (List.isNotEmpty tcs) (!- "when ") sepSpace
            +> col wordAnd tcs genTypeConstraint

        let long =
            ifElse (List.isNotEmpty tcs) (!- "when ") sepSpace
            +> col (sepNln +> wordAndFixed +> sepSpace) tcs genTypeConstraint

        expressionFitsOnRestOfLine short long

    autoIndentAndNlnIfExpressionExceedsPageWidth (
        leadingExpressionIsMultiline genType (fun isMultiline ->
            if isMultiline then
                indentSepNlnUnindent genConstraints
            else
                sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth genConstraints)
    )
// | _ -> sepNone

and genTyparDecl (isFirstTypeParam: bool) (TyparDecl(ats, (Typar(_, isHead) as tp), fullRange)) =
    genOnelinerAttributes ats
    +> onlyIf (isFirstTypeParam && isHead) sepSpace
    +> genTypar tp
    |> genTriviaFor SynTyparDecl_ fullRange

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
    | TCSimple(TCAugmentation _) -> sepNone
    | TCSimple TCIL -> sepNone
    | TCDelegate _ -> sepNone

and genExceptionBody ats px ao uc =
    genPreXmlDoc px
    +> genAttributes ats
    +> !- "exception "
    +> genAccessOpt ao
    +> genUnionCase false uc

and genException (ExceptionDef(ats, px, ao, uc, withKeyword, ms) as node) =
    genExceptionBody ats px ao uc
    +> ifElse
        ms.IsEmpty
        sepNone
        (genTriviaForOption SynExceptionDefn_With withKeyword (!- " with")
         +> indentSepNlnUnindent (genMemberDefnList ms))
    |> genTriviaFor SynExceptionDefn_ node.Range

and genSigException (SigExceptionDef(ats, px, ao, uc, withKeyword, ms)) =
    genExceptionBody ats px ao uc
    +> onlyIfNot
        ms.IsEmpty
        (genTriviaForOption SynExceptionSig_With withKeyword (!- " with")
         +> indentSepNlnUnindent (col sepNln ms genMemberSig))

and genUnionCase (hasVerticalBar: bool) (UnionCase(ats, px, barRange, _, si, UnionCaseType fs, range)) =
    let shortExpr = col sepStar fs genField

    let longExpr =
        indentSepNlnUnindent (atCurrentColumn (col (sepStar +> sepNln) fs genField))

    genPreXmlDoc px
    +> genTriviaForOptionOr SynUnionCase_Bar barRange (ifElse hasVerticalBar sepBar sepNone)
    +> atCurrentColumn (
        // If the bar has a comment after, add a newline and print the identifier on the same column on the next line.
        sepNlnWhenWriteBeforeNewlineNotEmpty
        +> genOnelinerAttributes ats
        +> genSynIdent false si
        +> onlyIf (List.isNotEmpty fs) wordOf
    )
    +> onlyIf (List.isNotEmpty fs) (expressionFitsOnRestOfLine shortExpr longExpr)
    |> genTriviaFor SynUnionCase_ range

and genEnumCase (EnumCase(ats, barRange, px, si, equalsRange, c, cr, r)) =
    let genCase =
        (genSynIdent false si
         +> genEq SynEnumCase_Equals (Some equalsRange)
         +> autoIndentAndNlnWhenWriteBeforeNewlineNotEmpty (sepSpace +> genConst c cr))

    genPreXmlDoc px
    +> (match barRange with
        | None -> sepBar
        | Some barRange -> genTriviaFor SynEnumCase_Bar barRange sepBar)
    +> genOnelinerAttributes ats
    +> genCase
    |> genTriviaFor SynEnumCase_ r

and genField (Field(ats, px, leadingKeyword, ao, isMutable, t, so, innerRange, range)) =
    // Being protective on union case declaration
    let t = genType t

    genPreXmlDoc px
    +> genAttributes ats
    +> optSingle genSynLeadingKeyword leadingKeyword
    +> ifElse isMutable (!- "mutable ") sepNone
    +> genAccessOpt ao
    +> (opt sepColon so genIdent +> autoIndentAndNlnIfExpressionExceedsPageWidth t
        |> optSingle (genTriviaFor SynField_IdentifierAndType) innerRange)
    |> genTriviaFor SynField_ range

and genType t =
    match t with
    | TFuns(ts, ret) ->
        let short =
            col sepNone ts (fun (t, arrow) ->
                genType t
                +> sepSpace
                +> genTriviaFor SynType_Fun_Arrow arrow sepArrowFixed
                +> sepSpace
                +> sepNlnWhenWriteBeforeNewlineNotEmpty)
            +> genType ret

        let long =
            match ts with
            | [] -> genType ret
            | (ht, ha) :: rest ->
                genType ht
                +> indentSepNlnUnindent (
                    genTriviaFor SynType_Fun_Arrow ha sepArrowFixed
                    +> sepSpace
                    +> col sepNone rest (fun (t, arrow) ->
                        genType t
                        +> sepNln
                        +> genTriviaFor SynType_Fun_Arrow arrow sepArrowFixed
                        +> sepSpace)
                    +> genType ret
                )

        expressionFitsOnRestOfLine short long
    | TTuple ts -> genSynTupleTypeSegments ts
    | THashConstraint t ->
        let wrapInParentheses f =
            match t with
            | TApp(_, _, ts, _, isPostfix, range) when (isPostfix && List.isNotEmpty ts) ->
                sepOpenT +> f +> sepCloseT |> genTriviaFor SynType_App range
            | _ -> f

        !- "#" +> wrapInParentheses (genType t)
    | TMeasurePower(t, n) -> genType t +> !- "^" +> str n
    | TMeasureDivide(t1, t2) -> genType t1 +> !- " / " +> genType t2
    | TStaticConstant(c, r) -> genConst c r
    | TStaticConstantExpr e -> !- "const" +> genExpr e
    | TStaticConstantNamed(t1, t2) ->
        genType t1
        +> !- "="
        +> addSpaceIfSynTypeStaticConstantHasAtSignBeforeString t2
        +> genType t2
    | TArray(t, n, r) ->
        genType t +> !- "[" +> rep (n - 1) (!- ",") +> !- "]"
        |> genTriviaFor SynType_Array r
    | TAnon -> sepWild
    | TVar(tp, r) -> genTypar tp |> genTriviaFor SynType_Var r
    | TApp(t, lessRange, ts, greaterRange, isPostfix, range) ->
        let postForm =
            match ts with
            | [] -> genType t
            | [ t' ] -> genType t' +> sepSpace +> genType t
            | ts -> sepOpenT +> col sepComma ts genType +> sepCloseT +> genType t

        ifElse
            isPostfix
            postForm
            (genType t
             +> genPrefixTypes SynType_App_Less lessRange ts SynType_App_Greater greaterRange)
        |> genTriviaFor SynType_App range

    | TLongIdentApp(t, sli, lessRange, ts, greaterRange) ->
        genType t
        +> sepDot
        +> genSynLongIdent false sli
        +> genPrefixTypes SynType_LongIdentApp_Less lessRange ts SynType_LongIdentApp_Greater greaterRange
    | TStructTuple ts -> !- "struct " +> sepOpenT +> genSynTupleTypeSegments ts +> sepCloseT
    | TWithGlobalConstraints(TVar _, [ TyparSubtypeOfType _ as tc ]) -> genTypeConstraint tc
    | TWithGlobalConstraints(t, tcs) -> genType t +> colPre (!- " when ") wordAnd tcs genTypeConstraint
    | TLongIdent sli -> genSynLongIdent false sli
    | TAnonRecord(isStruct, fields) ->
        let smallExpression = genSingleLineAnonRecordType isStruct fields

        let longExpression =
            ifAlignBrackets
                (genMultilineAnonRecordTypeAlignBrackets isStruct fields)
                (genMultilineAnonRecordType isStruct fields)

        fun (ctx: Context) ->
            let size = getRecordSize ctx fields
            isSmallExpression size smallExpression longExpression ctx
    | TParen(lpr, innerT, rpr, pr) ->
        genTriviaFor SynType_Paren_OpeningParenthesis lpr sepOpenT
        +> genType innerT
        +> genTriviaFor SynType_Paren_ClosingParenthesis rpr sepCloseT
        |> genTriviaFor SynType_Paren pr
    | TSignatureParameter(attrs, isOptional, identOpt, innerT) ->
        genOnelinerAttributes attrs
        +> onlyIf isOptional !- "?"
        +> optSingle (fun id -> genIdent id +> sepColon) identOpt
        +> autoIndentAndNlnIfExpressionExceedsPageWidth (genType innerT)
        |> genTriviaFor SynType_SignatureParameter t.Range
    | TOr(lhs, orRange, rhs) -> genType lhs +> genTriviaFor SynType_Or_Or orRange !- " or " +> genType rhs
    | t -> failwithf "Unexpected type: %O" t

and genSynTupleTypeSegments ts =
    let genTs addNewline =
        col sepSpace ts (fun t ->
            match t with
            | SynTupleTypeSegment.Type t -> genType t
            | SynTupleTypeSegment.Star _ -> !- "*" +> onlyIf addNewline sepNln
            | SynTupleTypeSegment.Slash _ -> !- "/")

    expressionFitsOnRestOfLine (genTs false) (genTs true)

// for example: FSharpx.Regex< @"(?<value>\d+)" >
and addSpaceIfSynTypeStaticConstantHasAtSignBeforeString (t: SynType) (ctx: Context) =
    let hasAtSign =
        match t with
        | TStaticConstant(SynConst.String(_, SynStringKind.Verbatim, _), _) -> true
        | _ -> false

    onlyIf hasAtSign sepSpace ctx

and genAnonRecordFieldType (AnonRecordFieldType(ident, t)) =
    genIdent ident
    +> sepColon
    +> autoIndentAndNlnIfExpressionExceedsPageWidth (genType t)

and genSingleLineAnonRecordType isStruct fields =
    ifElse isStruct !- "struct " sepNone
    +> sepOpenAnonRecd
    +> col sepSemi fields genAnonRecordFieldType
    +> sepCloseAnonRecd

and genMultilineAnonRecordType isStruct fields =
    let fieldsExpr = col sepNln fields genAnonRecordFieldType
    let genRecord = sepOpenAnonRecd +> atCurrentColumn fieldsExpr +> sepCloseAnonRecd
    ifElse isStruct !- "struct " sepNone +> genRecord

and genMultilineAnonRecordTypeAlignBrackets (isStruct: bool) fields =
    let fieldsExpr = col sepNln fields genAnonRecordFieldType

    let genRecord =
        sepOpenAnonRecdFixed
        +> indentSepNlnUnindent (atCurrentColumnIndent fieldsExpr)
        +> sepNln
        +> sepCloseAnonRecdFixed

    ifElse isStruct !- "struct " sepNone +> genRecord

and genPrefixTypes

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
    | TVar(Typar(_, true), _r) as t :: ts ->
        (genTriviaForOption lessNodeType lessRange !- "< "
         +> col sepComma (t :: ts) genType
         +> genTriviaForOption greaterNodeType greaterRange !- " >")
            ctx
    | t :: _ ->
        (genTriviaForOption lessNodeType lessRange !- "<"
         +> addSpaceIfSynTypeStaticConstantHasAtSignBeforeString t
         +> col sepComma ts genType
         +> addSpaceIfSynTypeStaticConstantHasAtSignBeforeString t
         +> genTriviaForOption greaterNodeType greaterRange !- ">")
            ctx

and genTypeList node =
    let gt addTrailingSpace (t, optArrow) =
        let genType =
            match t with
            | TTuple ts' ->
                let gt sepBefore =
                    let ts' =
                        List.choose
                            (fun t ->
                                match t with
                                | SynTupleTypeSegment.Type t -> Some t
                                | _ -> None)
                            ts'

                    col sepBefore ts' genType

                let shortExpr = gt sepStar
                let longExpr = gt (sepSpace +> sepStarFixed +> sepNln)

                expressionFitsOnRestOfLine shortExpr longExpr
                |> genTriviaFor SynType_Tuple t.Range

            | _ -> genType t

        genType
        +> optSingle
            (fun arrow ->
                sepSpace
                +> genTriviaFor SynType_Fun_Arrow arrow sepArrowFixed
                +> onlyIf addTrailingSpace sepSpace)
            optArrow

    let shortExpr = col sepNone node (gt true)

    let longExpr =
        let lastIndex = node.Length - 1

        let isTupleOrLastIndex index =
            index = lastIndex
            || match List.tryItem (index - 1) node with
               | Some(TTuple _, _) -> true
               | _ -> false

        let resetIndent =
            if lastIndex < 1 then
                id
            else
                [ 0..lastIndex ]
                |> List.choose (fun idx -> if isTupleOrLastIndex idx then Some unindent else None)
                |> List.reduce (+>)

        colii (fun idx -> onlyIf (isTupleOrLastIndex idx) indent +> sepNln) node (fun _ -> gt false)
        +> resetIndent

    expressionFitsOnRestOfLine shortExpr longExpr

and genTypar (Typar(ident, isHead)) =
    ifElse isHead (!- "^") (!- "'") +> genIdent ident

and genTypeConstraint node =
    match node with
    | TyparSingle(kind, tp) -> genTypar tp +> sepColon +> !-(sprintf "%O" kind)
    | TyparDefaultsToType(tp, t) -> !- "default " +> genTypar tp +> sepColon +> genType t
    | TyparSubtypeOfType(tp, t) -> genTypar tp +> !- " :> " +> genType t
    | TyparSupportsMember(tps, msg) -> genType tps +> sepColon +> sepOpenT +> genMemberSig msg +> sepCloseT
    | TyparIsEnum(tp, ts) -> genTypar tp +> sepColon +> !- "enum<" +> col sepComma ts genType +> !- ">"
    | TyparIsDelegate(tp, ts) -> genTypar tp +> sepColon +> !- "delegate<" +> col sepComma ts genType +> !- ">"
    | TyparWhereSelfConstrained t -> genType t

and genInterfaceImpl (InterfaceImpl(t, withKeywordRange, bs, members)) =
    if bs.IsEmpty && members.IsEmpty then
        !- "interface " +> genType t
    else
        !- "interface "
        +> genType t
        +> genTriviaForOption SynInterfaceImpl_With withKeywordRange !- " with"
        +> indentSepNlnUnindent (genSynBindings bs false +> genMemberDefnList members)

and genClause
    (isLastItem: bool)
    (hasMultipleClausesWhereOneHasStroustrup: bool)
    (Clause(barRange, p, eo, arrowRange, e, clauseRange))
    =
    let patAndBody =
        genPatInClause p
        +> leadingExpressionIsMultiline
            (optPre (!- " when") sepNone eo (fun e ->
                let short = sepSpace +> genExpr e
                let long = indentSepNlnUnindent (genExpr e)

                expressionFitsOnRestOfLine short long))
            (fun isMultiline ctx ->
                if isMultiline then
                    (indent
                     +> sepNln
                     +> optSingle
                         (fun arrowRange -> sepArrowFixed |> genTriviaFor SynMatchClause_Arrow arrowRange)
                         arrowRange
                     +> sepNln
                     +> genExpr e
                     +> unindent)
                        ctx
                else
                    (optSingle (fun arrowRange -> sepArrow |> genTriviaFor SynMatchClause_Arrow arrowRange) arrowRange
                     +> (if ctx.Config.ExperimentalKeepIndentInBranch && isLastItem then
                             let short = genExpr e

                             let long =
                                 match barRange with
                                 | None -> autoIndentAndNlnIfExpressionExceedsPageWidthUnlessStroustrup genExpr e
                                 | Some barRange -> genKeepIdent barRange e +> sepNln +> genExpr e +> unindent

                             expressionFitsOnRestOfLine short long
                         else
                             autoIndentAndNlnIfExpressionExceedsPageWidthUnlessStroustrup genExpr e))
                        ctx)

    let genBar =
        match barRange with
        | Some barRange -> genTriviaFor SynMatchClause_Bar barRange sepBar
        | None -> sepBar

    (genBar
     +> (fun ctx ->
         if hasMultipleClausesWhereOneHasStroustrup then
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
     |> genTriviaFor SynMatchClause_ clauseRange)

and genPatInClause (pat: SynPat) =
    let genPatOrs (p, ps) =
        genPat p
        +> sepNln
        +> col sepNln ps (fun (barRange, p, patOrRange) ->
            genTriviaFor SynPat_Or_Bar barRange sepBar
            +> genPat p
            +> leaveNodeFor SynPat_Or patOrRange)

    match pat with
    | PatOrs pats -> genPatOrs pats
    | PatAs(PatOrs pats, patAs, r) -> genPatOrs pats +> !- " as " +> genPat patAs |> genTriviaFor SynPat_As r
    | _ -> genPat pat

and genClauses cs (ctx: Context) =
    let lastIndex = cs.Length - 1

    coli
        sepNln
        cs
        (fun idx clause ->
            let isLastItem = lastIndex = idx

            genClause

                isLastItem
                (hasMultipleClausesWhereOneHasStroustrup ctx.Config.ExperimentalStroustrupStyle cs)
                clause)
        ctx

/// Each multiline member definition has a pre and post new line.
and genMemberDefnList ms =
    ms
    |> List.map (fun md ->
        ColMultilineItem(
            genMemberDefn md,
            sepNlnConsideringTriviaContentBeforeFor (synMemberDefnToFsAstType md) md.Range
        ))
    |> colWithNlnWhenItemIsMultilineUsingConfig

and genMemberDefn node =
    match node with
    | MDNestedType _ -> invalidArg "md" "This is not implemented in F# compiler"
    | MDOpen lid -> !- "open " +> genSynLongIdent false lid
    // What is the role of so
    | MDImplicitInherit(t, e, _) ->
        !- "inherit "
        +> autoIndentAndNlnIfExpressionExceedsPageWidth (genInheritConstructor (t, e))

    | MDInherit(t, _) -> !- "inherit " +> genType t
    | MDValField f -> genField f
    | MDImplicitCtor(PreXmlDoc(xmlDoc, _) as preXmlDoc, ats, ao, ps, so) ->
        let rec simplePats ps =
            match ps with
            | SynSimplePats.SimplePats(pats, _) -> pats
            | SynSimplePats.Typed(spts, _, _) -> simplePats spts

        let genCtor =
            let shortExpr =
                optPre sepSpace sepSpace ao genAccess
                +> ((sepOpenT +> col sepComma (simplePats ps) genSimplePat +> sepCloseT)
                    |> genTriviaFor SynSimplePats_SimplePats ps.Range)

            let emptyPats =
                let rec isEmpty ps =
                    match ps with
                    | SynSimplePats.SimplePats([], _) -> true
                    | SynSimplePats.SimplePats _ -> false
                    | SynSimplePats.Typed(spts, _, _) -> isEmpty spts

                isEmpty ps

            let hasXmlDocComment = xmlDoc.Length > 0

            let longExpr ctx =
                (indent
                 +> sepNln
                 +> genPreXmlDoc preXmlDoc
                 +> optSingle (fun ao -> genAccess ao +> sepNln) ao
                 +> ifElse emptyPats (sepOpenT +> sepCloseT) (fun ctx ->
                     let shortPats = sepOpenT +> col sepComma (simplePats ps) genSimplePat +> sepCloseT

                     let longPats =
                         sepOpenT
                         +> indentSepNlnUnindent (col (sepComma +> sepNln) (simplePats ps) genSimplePat)
                         +> sepNln
                         +> sepCloseT

                     let hasTriviaBeforePats = ctx.HasContentBefore(SynSimplePats_SimplePats, ps.Range)

                     if hasTriviaBeforePats then
                         genTriviaFor
                             SynSimplePats_SimplePats
                             ps.Range
                             (expressionFitsOnRestOfLine shortPats longPats)
                             ctx
                     elif hasXmlDocComment then
                         expressionFitsOnRestOfLine shortPats longPats ctx
                     else
                         longPats ctx)
                 +> onlyIf ctx.Config.AlternativeLongMemberDefinitions sepNln
                 +> unindent)
                    ctx

            if hasXmlDocComment then
                longExpr
            else
                expressionFitsOnRestOfLine shortExpr longExpr

        // In implicit constructor, attributes should come even before access qualifiers
        ifElse ats.IsEmpty sepNone (sepSpace +> genOnelinerAttributes ats)
        +> genCtor
        +> optPre (sepSpace +> !- "as ") sepNone so genIdent

    | MDMember b
    | LongGetMember b -> genMemberBinding b
    | MDLetBindings [ ExternBinding eb ] -> genExternBinding eb
    | MDLetBindings [ DoBinding(ats, px, leadingKeyword, e) ] ->
        genPreXmlDoc px
        +> genAttributes ats
        +> genSynLeadingKeyword leadingKeyword
        +> autoIndentAndNlnIfExpressionExceedsPageWidth (genExpr e)
    | MDLetBindings bs -> genSynBindings bs true
    | MDInterface(t, withKeyword, mdo) ->
        !- "interface "
        +> genType t
        +> opt sepNone mdo (fun mds ->
            genTriviaForOption SynMemberDefn_Interface_With withKeyword !- " with"
            +> indentSepNlnUnindent (genMemberDefnList mds))

    | MDAutoProperty(ats, px, ao, leadingKeyword, equalsRange, e, optWithGetSet, ident, _isStatic, typeOpt) ->
        genPreXmlDoc px
        +> genAttributes ats
        +> genSynLeadingKeyword leadingKeyword
        +> genAccessOpt ao
        +> genIdent ident
        +> optPre sepColon sepNone typeOpt genType
        +> genEq SynMemberDefn_AutoProperty_Equals equalsRange
        +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (genExpr e +> optSingle (!-) optWithGetSet)

    | MDAbstractSlot(ats, px, ao, leadingKeyword, si, t, ValTyparDecls(tds, _), optWithGetSet) ->
        let hasGenerics = Option.isSome tds

        genPreXmlDoc px
        +> genAttributes ats
        +> genAccessOpt ao
        +> genSynLeadingKeyword leadingKeyword
        +> genSynIdent false si
        +> genTypeParamPostfix tds
        +> ifElse hasGenerics sepColonWithSpacesFixed sepColon
        +> genTypeInSignature t
        +> optSingle (!-) optWithGetSet

    | MDPropertyGetSet(px,
                       ats,
                       leadingKeyword,
                       isInline,
                       ao,
                       memberName,
                       withKeyword,
                       firstBinding,
                       andKeyword,
                       secondBinding) ->
        genPreXmlDoc px
        +> genAttributes ats
        +> genSynLeadingKeyword leadingKeyword
        +> ifElse isInline (!- "inline ") sepNone
        +> genAccessOpt ao
        +> genSynLongIdent false memberName
        +> indent
        +> sepNln
        +> genTriviaFor SynMemberDefn_GetSetMember_With withKeyword !- "with"
        +> sepSpace
        +> genProperty firstBinding
        +> optSingle
            (fun andKeyword ->
                genTriviaFor
                    SynMemberDefn_GetSetMember_And
                    andKeyword
                    (sepNlnConsideringTriviaContentBeforeFor SynMemberDefn_GetSetMember_And andKeyword
                     +> !- "and"
                     +> sepSpace))
            andKeyword
        +> optSingle genProperty secondBinding
        +> unindent

    | md -> failwithf "Unexpected member definition: %O" md
    |> genTriviaFor (synMemberDefnToFsAstType node) node.Range

and genPropertyKind useSyntacticSugar memberKind =
    match memberKind with
    | PropertyGet ->
        // Try to use syntactic sugar on real properties (not methods in disguise)
        if useSyntacticSugar then "" else " with get"
    | PropertySet -> " with set"
    | PropertyGetSet -> " with get, set"
    | _ -> ""

and genSimplePat node =
    match node with
    | SPId(ident, isOptArg, _) -> onlyIf isOptArg (!- "?") +> genIdent ident
    | SPTyped(sp, t) ->
        genSimplePat sp
        +> sepColon
        +> autoIndentAndNlnIfExpressionExceedsPageWidth (genType t)
    | SPAttrib(ats, sp) -> genOnelinerAttributes ats +> genSimplePat sp

and genSimplePats node =
    match node with
    // Remove parentheses on an extremely simple pattern
    | SimplePats [ SPId _ as sp ] -> genSimplePat sp
    | SimplePats ps -> sepOpenT +> col sepComma ps genSimplePat +> sepCloseT
    | SPSTyped(ps, t) -> genSimplePats ps +> sepColon +> genType t

and genPatRecordFieldName ((lid: LongIdent, ident: Ident), _: range, p: SynPat) =
    ifElse
        lid.IsEmpty
        (genIdent ident +> sepEq +> sepSpace)
        (genLongIdent lid +> sepDot +> genIdent ident +> sepEq +> sepSpace)
    +> genPat p

and genPat pat =
    match pat with
    | PatOptionalVal ident -> !- "?" +> genIdent ident
    | PatAttrib(p, ats) -> genOnelinerAttributes ats +> genPat p
    | PatOr(p1, barRange, p2) -> genPat p1 +> sepSpace +> genTriviaFor SynPat_Or_Bar barRange sepBar +> genPat p2
    | PatAnds ps -> col (!- " & ") ps genPat
    | PatNullary PatNull -> !- "null"
    | PatNullary PatWild -> sepWild
    | PatTyped(p, t) ->
        genPat p
        +> sepColon
        +> autoIndentAndNlnIfExpressionExceedsPageWidth (atCurrentColumnIndent (genType t))

    | PatNamed(ao, SynIdent(_, Some(ParenStarSynIdent(lpr, op, rpr)))) ->
        genAccessOpt ao
        +> sepOpenTFor lpr
        +> sepSpace
        +> !-op
        +> sepSpace
        +> sepCloseTFor (Some rpr)
    | PatNamed(ao, si) -> genAccessOpt ao +> genSynIdent false si
    | PatAs(p1, p2, r) -> genPat p1 +> !- " as " +> genPat p2 |> genTriviaFor SynPat_As r

    | PatListCons(p1, colonColonRange, p2) ->
        genPat p1
        +> genTriviaFor SynPat_ListCons_ColonColon colonColonRange !- " :: "
        +> genPat p2

    | PatNamePatPairs(sli, vtdo, lpr, nps, rpr, r) ->
        let genPatWithIdent (ident, eq, p) =
            genIdent ident
            +> genEq SynArgPats_NamePatPairs_Equals (Some eq)
            +> sepSpace
            +> genPat p

        let pats =
            expressionFitsOnRestOfLine
                (atCurrentColumn (col sepSemi nps genPatWithIdent))
                (atCurrentColumn (col sepNln nps genPatWithIdent))

        genSynLongIdent false sli
        +> genValTyparDeclsOpt vtdo
        +> addSpaceBeforeParenInPattern sli
        +> genTriviaFor SynArgPats_NamePatPairs_OpeningParenthesis lpr sepOpenT
        +> autoIndentAndNlnIfExpressionExceedsPageWidth (sepNlnWhenWriteBeforeNewlineNotEmpty +> pats)
        +> genTriviaFor SynArgPats_NamePatPairs_ClosingParenthesis rpr sepCloseT
        |> genTriviaFor SynArgPats_NamePatPairs r

    | PatLongIdent(ao, sli, [ PatParen _ as p ], vtdo) ->
        genAccessOpt ao
        +> genSynLongIdent false sli
        +> genValTyparDeclsOpt vtdo
        +> addSpaceBeforeParenInPattern sli
        +> genPat p

    | PatLongIdent(ao, sli, ps, vtdo) ->
        let genName =
            genAccessOpt ao +> genSynLongIdent false sli +> genValTyparDeclsOpt vtdo

        let genParameters =
            match ps with
            | [] -> sepNone
            | ps -> sepSpace +> atCurrentColumn (col sepSpace ps genPat)

        genName +> genParameters

    | PatParen(_, PatUnitConst, _) -> !- "()"
    | PatParen(lpr, p, rpr) ->
        genTriviaFor SynPat_Paren_OpeningParenthesis lpr sepOpenT
        +> genPat p
        +> genTriviaFor SynPat_Paren_ClosingParenthesis rpr sepCloseT
    | PatTuple ps ->
        expressionFitsOnRestOfLine (col sepComma ps genPat) (atCurrentColumn (col (sepComma +> sepNln) ps genPat))
    | PatStructTuple ps ->
        !- "struct "
        +> sepOpenT
        +> atCurrentColumn (colAutoNlnSkip0 sepComma ps genPat)
        +> sepCloseT
    | PatSeq(patListType, [ PatOrs(patOr, patOrs) ]) ->
        let sepOpen, sepClose =
            match patListType with
            | PatArray -> sepOpenA, sepCloseA
            | PatList -> sepOpenL, sepCloseL

        let short =
            sepOpen
            +> genPat patOr
            +> sepSpace
            +> col sepSpace patOrs (fun (barRange, p, _) -> genTriviaFor SynPat_Or_Bar barRange sepBar +> genPat p)
            +> sepClose

        let long =
            sepOpen
            +> atCurrentColumnIndent (
                match patOrs with
                | [] -> sepNone
                | pats ->
                    genPat patOr
                    +> sepNln
                    +> !- " "
                    +> atCurrentColumn (
                        col sepNln pats (fun (barRange, p, _) -> genTriviaFor SynPat_Or_Bar barRange sepBar +> genPat p)
                    )
            )
            +> sepClose

        expressionFitsOnRestOfLine short long
    | PatSeq(PatList, ps) ->
        let genPats =
            let short = colAutoNlnSkip0 sepSemi ps genPat
            let long = col sepNln ps genPat
            expressionFitsOnRestOfLine short long

        ifElse ps.IsEmpty (sepOpenLFixed +> sepCloseLFixed) (sepOpenL +> atCurrentColumn genPats +> sepCloseL)

    | PatSeq(PatArray, ps) ->
        let genPats =
            let short = colAutoNlnSkip0 sepSemi ps genPat
            let long = col sepNln ps genPat
            expressionFitsOnRestOfLine short long

        ifElse ps.IsEmpty (sepOpenAFixed +> sepCloseAFixed) (sepOpenA +> atCurrentColumn genPats +> sepCloseA)

    | PatRecord xs ->
        let smallRecordExpr = sepOpenS +> col sepSemi xs genPatRecordFieldName +> sepCloseS

        // Note that MultilineBlockBracketsOnSameColumn is not taken into account here.
        let multilineRecordExpr =
            sepOpenS +> atCurrentColumn (col sepNln xs genPatRecordFieldName) +> sepCloseS

        let multilineRecordExprAlignBrackets =
            sepOpenSFixed
            +> indent
            +> sepNln
            +> atCurrentColumn (col sepNln xs genPatRecordFieldName)
            +> unindent
            +> sepNln
            +> sepCloseSFixed
            |> atCurrentColumnIndent

        let multilineExpressionIfAlignBrackets =
            ifAlignBrackets multilineRecordExprAlignBrackets multilineRecordExpr

        fun ctx ->
            let size = getRecordSize ctx xs
            isSmallExpression size smallRecordExpr multilineExpressionIfAlignBrackets ctx
    | PatConst(c, r) -> genConst c r
    | PatIsInst t -> !- ":? " +> genType t
    // Quotes will be printed by inner expression
    | PatQuoteExpr e -> genExpr e
    | p -> failwithf "Unexpected pattern: %O" p
    |> (match pat with
        | SynPat.Named _ -> genTriviaFor SynPat_Named pat.Range
        | SynPat.Wild _ -> genTriviaFor SynPat_Wild pat.Range
        | SynPat.LongIdent _ -> genTriviaFor SynPat_LongIdent pat.Range
        | SynPat.ListCons _ -> genTriviaFor SynPat_ListCons pat.Range
        | SynPat.Paren _ -> genTriviaFor SynPat_Paren pat.Range
        | SynPat.Or _ -> genTriviaFor SynPat_Or pat.Range
        | _ -> id)

and genValTyparDeclsOpt (vtdOpt: SynValTyparDecls option) : Context -> Context =
    optSingle (fun (ValTyparDecls(tds, _)) -> genTypeParamPostfix tds) vtdOpt

and clean_up_space_settings leadingKeyword ctx =
    match leadingKeyword with
    | SynLeadingKeyword.Member _
    | SynLeadingKeyword.Override _
    | SynLeadingKeyword.StaticMember _
    | SynLeadingKeyword.AbstractMember _
    | SynLeadingKeyword.Default _ -> ctx.Config.SpaceBeforeMember, ctx.Config.AlternativeLongMemberDefinitions
    | _ -> ctx.Config.SpaceBeforeParameter, ctx.Config.AlignFunctionSignatureToIndentation

and clean_up_is_rec_function leadingKeyword =
    match leadingKeyword with
    | SynLeadingKeyword.And _ -> true
    | _ -> false

// TODO: at some point absorb the existing helper functions and streamline the whole thing

and genSynBinding (Binding(ats, px, leadingKeyword, ao, isInline, isMutable, p, returnInfo, equalsRange, e, range)) =
    match returnInfo, p with
    | Some(colon, t), PatLongIdent(ao, sli, ps, tpso) when (List.isNotEmpty ps) ->
        genSynBindingFunctionWithReturnType
            px
            ats
            leadingKeyword
            ao
            isInline
            isMutable
            sli
            ps
            tpso
            colon
            t
            equalsRange
            e
    | None, PatLongIdent(ao, sli, ps, tpso) when (List.isNotEmpty ps) ->
        genSynBindingFunction px ats leadingKeyword ao isInline isMutable sli ps tpso equalsRange e
    | Some(colon, t), pat ->
        genSynBindingValue px ats leadingKeyword ao isInline isMutable pat (Some colon) (Some t) equalsRange e
    | _, PatTuple _ -> genLetBindingDestructedTuple px ats leadingKeyword ao isInline isMutable p equalsRange e
    | _, pat -> genSynBindingValue px ats leadingKeyword ao isInline isMutable pat None None equalsRange e
    |> genTriviaFor SynBinding_ range

and genSynBindings bs withUseConfig =
    bs
    |> List.map (fun (b: SynBinding) ->
        let expr = genSynBinding b

        let sepNln = sepNlnConsideringTriviaContentBeforeFor SynBinding_ b.FullRange

        ColMultilineItem(expr, sepNln))
    |> (if withUseConfig then
            colWithNlnWhenItemIsMultilineUsingConfig
        else
            colWithNlnWhenItemIsMultiline)

and genSynBindingFunction
    (px: PreXmlDoc)
    (ats: SynAttributes)
    (leadingKeyword: SynLeadingKeyword)
    (ao: SynAccess option)
    (isInline: bool)
    (isMutable: bool)
    (functionName: SynLongIdent)
    (parameters: SynPat list)
    (genericTypeParameters: SynValTyparDecls option)
    (equalsRange: range option)
    (e: SynExpr)
    (ctx: Context)
    : Context =
    let spaceBefore, alternativeSyntax = clean_up_space_settings leadingKeyword ctx
    let isRecursiveLetOrUseFunction = clean_up_is_rec_function leadingKeyword

    let genAttrIsFirstChild =
        onlyIf (not isRecursiveLetOrUseFunction) (genAttributes ats)

    let genPref =
        if not isRecursiveLetOrUseFunction then
            genSynLeadingKeyword leadingKeyword
        else
            genSynLeadingKeyword leadingKeyword +> genOnelinerAttributes ats

    let afterLetKeyword =
        ifElse isMutable (!- "mutable ") sepNone
        +> ifElse isInline (!- "inline ") sepNone
        +> genAccessOpt ao

    let genFunctionName =
        genSynBindingFunctionName functionName
        +> genValTyparDeclsOpt genericTypeParameters

    let genSignature =
        let spaceBeforeParameters =
            match parameters with
            | [] -> sepNone
            | [ p ] -> ifElse (addSpaceBeforeParensInFunDef spaceBefore functionName p) sepSpace sepNone
            | _ -> sepSpace

        let short =
            afterLetKeyword
            +> genFunctionName
            +> spaceBeforeParameters
            +> col sepSpace parameters genPat
            +> genEq SynBinding_Equals equalsRange

        let long (ctx: Context) =
            let genParameters, hasSingleTupledArg =
                match parameters with
                | [ PatParen(lpr, PatTuple ps, rpr) as pp ] ->
                    genParenTupleWithIndentAndNewlines lpr ps rpr pp.Range, true
                | _ -> col sepNln parameters genPat, false

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

    let body (ctx: Context) = genExpr e ctx

    let genExpr isMultiline =
        if isMultiline then
            indentSepNlnUnindent body
        else
            let short = sepSpace +> body

            let long =
                autoIndentAndNlnExpressUnlessStroustrup (fun e -> sepSpace +> genExpr e) e

            isShortExpression ctx.Config.MaxFunctionBindingWidth short long

    (genPreXmlDoc px
     +> genAttrIsFirstChild
     +> genPref
     +> leadingExpressionIsMultiline genSignature genExpr)
        ctx

and genSynBindingFunctionWithReturnType
    (px: PreXmlDoc)
    (ats: SynAttributes)
    (leadingKeyword: SynLeadingKeyword)
    (ao: SynAccess option)
    (isInline: bool)
    (isMutable: bool)
    (functionName: SynLongIdent)
    (parameters: SynPat list)
    (genericTypeParameters: SynValTyparDecls option)
    (colonRange: range)
    (returnType: SynType)
    (equalsRange: range option)
    (e: SynExpr)
    (ctx: Context)
    : Context =
    let spaceBefore, alternativeSyntax = clean_up_space_settings leadingKeyword ctx
    let isRecursiveLetOrUseFunction = clean_up_is_rec_function leadingKeyword

    let genAttrIsFirstChild =
        onlyIf (not isRecursiveLetOrUseFunction) (genAttributes ats)

    let genPref =
        if not isRecursiveLetOrUseFunction then
            genSynLeadingKeyword leadingKeyword
        else
            genSynLeadingKeyword leadingKeyword +> genOnelinerAttributes ats

    let afterLetKeyword =
        ifElse isMutable (!- "mutable ") sepNone
        +> ifElse isInline (!- "inline ") sepNone
        +> genAccessOpt ao

    let genFunctionName =
        genSynBindingFunctionName functionName
        +> genValTyparDeclsOpt genericTypeParameters

    let genReturnType isFixed =
        genTriviaFor
            SynBinding_ReturnType_Colon
            colonRange
            (ifElse isFixed (sepColonFixed +> sepSpace) sepColonWithSpacesFixed)
        +> atCurrentColumnIndent (genType returnType)

    let genSignature =
        let spaceBeforeParameters =
            match parameters with
            | [] -> sepNone
            | [ p ] -> ifElse (addSpaceBeforeParensInFunDef spaceBefore functionName p) sepSpace sepNone
            | _ -> sepSpace

        let short =
            afterLetKeyword
            +> sepSpace
            +> genFunctionName
            +> spaceBeforeParameters
            +> col sepSpace parameters genPat
            +> genReturnType false
            +> genEq SynBinding_Equals equalsRange

        let long (ctx: Context) =
            let genParameters, hasSingleTupledArg =
                match parameters with
                | [ PatParen(lpr, PatTuple ps, rpr) as pp ] ->
                    genParenTupleWithIndentAndNewlines lpr ps rpr pp.Range, true
                | _ -> col sepNln parameters genPat, false

            (afterLetKeyword
             +> sepSpace
             +> genFunctionName
             +> indent
             +> sepNln
             +> genParameters
             +> onlyIf (not hasSingleTupledArg || alternativeSyntax) sepNln
             +> leadingExpressionIsMultiline
                 (genReturnType (not hasSingleTupledArg || alternativeSyntax))
                 (fun isMultiline ->
                     ifElse
                         (alternativeSyntax || isMultiline)
                         (sepNln +> genEqFixed SynBinding_Equals equalsRange)
                         sepEq)
             +> unindent)
                ctx

        expressionFitsOnRestOfLine short long

    let body = genExpr e

    let genExpr isMultiline =
        if isMultiline then
            indentSepNlnUnindent body
        else
            let short = sepSpace +> body

            let long =
                autoIndentAndNlnExpressUnlessStroustrup (fun e -> sepSpace +> genExpr e) e

            isShortExpression ctx.Config.MaxFunctionBindingWidth short long

    (genPreXmlDoc px
     +> genAttrIsFirstChild
     +> genPref
     +> leadingExpressionIsMultiline genSignature genExpr)
        ctx

and genLetBindingDestructedTuple
    (px: PreXmlDoc)
    (ats: SynAttributes)
    (leadingKeyword: SynLeadingKeyword)
    (ao: SynAccess option)
    (isInline: bool)
    (isMutable: bool)
    (pat: SynPat)
    (equalsRange: range option)
    (e: SynExpr)
    =
    let isRecursiveLetOrUseFunction =
        match leadingKeyword with
        | SynLeadingKeyword.LetRec _
        | SynLeadingKeyword.Use _ -> true
        | _ -> false

    let genAttrAndPref =
        if not isRecursiveLetOrUseFunction then
            (genAttributes ats +> genSynLeadingKeyword leadingKeyword)
        else
            (genSynLeadingKeyword leadingKeyword +> genOnelinerAttributes ats)

    let afterLetKeyword =
        genAccessOpt ao
        +> ifElse isMutable (!- "mutable ") sepNone
        +> ifElse isInline (!- "inline ") sepNone

    let genDestructedTuples =
        expressionFitsOnRestOfLine (genPat pat) (sepOpenT +> genPat pat +> sepCloseT)

    genPreXmlDoc px
    +> genAttrAndPref
    +> (fun ctx ->
        let prefix =
            afterLetKeyword
            +> sepSpace
            +> genDestructedTuples
            +> genEq SynBinding_Equals equalsRange

        let long = prefix +> indentSepNlnUnindent (genExpr e)
        let short = prefix +> sepSpace +> genExpr e
        isShortExpression ctx.Config.MaxValueBindingWidth short long ctx)

and genSynBindingValue
    (px: PreXmlDoc)
    (ats: SynAttributes)
    (leadingKeyword: SynLeadingKeyword)
    (ao: SynAccess option)
    (isInline: bool)
    (isMutable: bool)
    (valueName: SynPat)
    (colonRange: range option)
    (returnType: SynType option)
    (equalsRange: range option)
    (e: SynExpr)
    : Context -> Context =
    let isRecursiveLetOrUseFunction =
        match leadingKeyword with
        | SynLeadingKeyword.LetRec _
        | SynLeadingKeyword.Use _ -> true
        | _ -> false

    let genAttrIsFirstChild =
        onlyIf (not isRecursiveLetOrUseFunction) (genAttributes ats)

    let genPref =
        if not isRecursiveLetOrUseFunction then
            genSynLeadingKeyword leadingKeyword
        else
            (genSynLeadingKeyword leadingKeyword +> genOnelinerAttributes ats)

    let afterLetKeyword =
        genAccessOpt ao
        +> ifElse isMutable (!- "mutable ") sepNone
        +> ifElse isInline (!- "inline ") sepNone

    let genValueName = genPat valueName

    let genEqualsInBinding (ctx: Context) =
        (genEqFixed SynBinding_Equals equalsRange
         +> sepSpaceUnlessWriteBeforeNewlineNotEmpty)
            ctx

    let genReturnType =
        match returnType, colonRange with
        | Some rt, Some colon ->
            let hasGenerics =
                match valueName with
                | SynPat.LongIdent(_, _, Some _, _, _, _) -> true
                | _ -> false

            genTriviaFor SynBinding_ReturnType_Colon colon (ifElse hasGenerics sepColonWithSpacesFixed sepColon)
            +> genType rt
            +> sepSpaceUnlessWriteBeforeNewlineNotEmpty
            +> autoIndentAndNlnWhenWriteBeforeNewlineNotEmpty genEqualsInBinding
        | _ -> sepSpace +> genEqualsInBinding

    genPreXmlDoc px
    +> genAttrIsFirstChild
    +> genPref
    +> (fun ctx ->
        let prefix = afterLetKeyword +> sepSpace +> genValueName +> genReturnType
        let short = prefix +> genExpr e
        let long = prefix +> autoIndentAndNlnExpressUnlessStroustrup genExpr e
        isShortExpression ctx.Config.MaxValueBindingWidth short long ctx)

// Example case: let ( *= ) a b = ()
// There must be spaces due to the *
// The idea is to solve this only where this can occur and not at the SynIdent level.
and genSynBindingFunctionName (functionName: SynLongIdent) =
    match functionName with
    | OperatorNameWithStar(lpr, text, rpr, synIdentRange, synLongIdentRange) ->
        sepOpenTFor lpr +> sepSpace +> !-text +> sepSpace +> sepCloseTFor (Some rpr)
        |> genTriviaFor SynIdent_ synIdentRange
        |> genTriviaFor SynLongIdent_ synLongIdentRange
    | PrefixedOperatorNameWithStar(prefix, lpr, text, rpr, synIdentRange, synLongIdentRange) ->
        genSynIdent false prefix
        +> sepDot
        +> sepOpenTFor lpr
        +> sepSpace
        +> !-text
        +> sepSpace
        +> sepCloseTFor (Some rpr)
        |> genTriviaFor SynIdent_ synIdentRange
        |> genTriviaFor SynLongIdent_ synLongIdentRange
    | _ -> genSynLongIdent false functionName

and genParenTupleWithIndentAndNewlines (lpr: range) (ps: SynPat list) (rpr: range) (pr: range) : Context -> Context =
    genTriviaFor SynPat_Paren_OpeningParenthesis lpr sepOpenT
    +> indent
    +> sepNln
    +> col (sepComma +> sepNln) ps genPat
    +> unindent
    +> sepNln
    +> genTriviaFor SynPat_Paren_ClosingParenthesis rpr sepCloseT
    |> genTriviaFor SynPat_Paren pr

and genConst (c: SynConst) (r: Range) =
    match c with
    | SynConst.Unit ->
        let lpr, rpr = RangeHelpers.mkStartEndRange 1 r

        genTriviaFor SynConst_Unit_OpeningParenthesis lpr sepOpenT
        +> genTriviaFor SynConst_Unit_ClosingParenthesis rpr sepCloseT
        |> genTriviaFor SynConst_Unit r
    | SynConst.Bool b -> !-(if b then "true" else "false") |> genTriviaFor SynConst_Bool r
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
    | SynConst.String(s, kind, r) ->
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
    | SynConst.Bytes(bytes, _, r) ->
        // TODO: take kind into account
        genConstBytes bytes r |> genTriviaFor SynConst_Bytes r
    | SynConst.Measure(c, numberRange, m) ->
        let genNumber (ctx: Context) = genConstNumber c numberRange ctx

        genNumber +> genMeasure m
    | SynConst.SourceIdentifier(c, _, r) -> !-c |> genTriviaFor SynConst_SourceIdentifier r

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
            | SynConst.UserNum(v, s) -> findNumberFromSourceText (!- $"%s{v}%s{s}") SynConst_UserNum
            | _ -> failwithf $"Cannot generating Const number for %A{c}"

        expr ctx

and genConstBytes (bytes: byte[]) (r: Range) =
    fun (ctx: Context) ->
        let expr =
            match ctx.FromSourceText r with
            | Some s -> !-s
            | None ->
                let content = String(Array.map (fun (byte: byte) -> Convert.ToChar(byte)) bytes)

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

and genSynStaticOptimizationConstraint (constraints: SynStaticOptimizationConstraint list) : Context -> Context =
    let genConstraint con =
        match con with
        | SynStaticOptimizationConstraint.WhenTyparTyconEqualsTycon(t1, t2, _) ->
            genTypar t1 +> sepColon +> sepSpace +> genType t2
        | SynStaticOptimizationConstraint.WhenTyparIsStruct(t, _) -> genTypar t

    !- " when " +> col sepSpace constraints genConstraint

and genTriviaFor (mainNodeName: FsAstType) (range: Range) f ctx =
    (enterNodeFor mainNodeName range +> f +> leaveNodeFor mainNodeName range) ctx

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
    optSingle (fun arrowRange -> sepArrow |> genTriviaFor SynExpr_Lambda_Arrow arrowRange) arrowRange
    +> (fun ctx ->
        if hasWriteBeforeNewlineContent ctx then
            indentSepNlnUnindent (bodyExpr body) ctx
        else
            autoIndentAndNlnIfExpressionExceedsPageWidthUnlessStroustrup bodyExpr body ctx)

and addSpaceBeforeClassConstructor expr =
    match expr with
    | Paren _
    | ConstExpr(SynConst.Unit, _) -> sepSpaceBeforeClassConstructor
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
        | SynMeasure.Var(Typar(s, _), _) -> !-s.idText
        | SynMeasure.Anon _ -> !- "_"
        | SynMeasure.One -> !- "1"
        | SynMeasure.Product(m1, m2, _) -> loop m1 +> !- " * " +> loop m2
        | SynMeasure.Divide(m1, m2, _) -> loop m1 +> !- " / " +> loop m2
        | SynMeasure.Power(m, RationalConst n, _) -> loop m +> !- $"^{n}"
        | SynMeasure.Seq(ms, _) -> col sepSpace ms loop
        | SynMeasure.Named(lid, _) -> genLongIdent lid
        | SynMeasure.Paren(m, _) -> sepOpenT +> loop m +> sepCloseT

    !- "<" +> loop measure +> !- ">"

and genKeepIdent startRange (e: SynExpr) ctx =
    if
        ctx.Config.ExperimentalKeepIndentInBranch
        && (startRange.StartColumn = e.Range.StartColumn)
    then
        let t, r = synExprToFsAstType e
        sepNlnConsideringTriviaContentBeforeFor t r ctx
    else
        indent ctx
