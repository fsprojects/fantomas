module Fantomas.FCS.Parse

open System
open System.Text
open System.Diagnostics
open Fantomas.FCS.Diagnostics
open Fantomas.FCS.DiagnosticMessage
open Internal.Utilities
open Internal.Utilities.Library
open Fantomas.FCS
open Fantomas.FCS.AbstractIL.IL
open Fantomas.FCS.DiagnosticsLogger
open Fantomas.FCS.Features
open Fantomas.FCS.LexerStore
open Fantomas.FCS.Lexhelp
open Fantomas.FCS.Text
open Fantomas.FCS.Text.Position
open Fantomas.FCS.Text.Range
open Fantomas.FCS.Xml
open Fantomas.FCS.Syntax
open Fantomas.FCS.SyntaxTrivia
open Fantomas.FCS.Syntax.PrettyNaming
open Fantomas.FCS.SyntaxTreeOps
open Fantomas.FCS.IO
open Fantomas.FCS.ParseHelpers

let FSharpSigFileSuffixes = [ ".mli"; ".fsi" ]

let mlCompatSuffixes = [ ".mli"; ".ml" ]

let FSharpImplFileSuffixes = [ ".ml"; ".fs"; ".fsscript"; ".fsx" ]

let FSharpScriptFileSuffixes = [ ".fsscript"; ".fsx" ]

let CanonicalizeFilename filename =
    let basic = FileSystemUtils.fileNameOfPath filename

    String.capitalize (
        try
            FileSystemUtils.chopExtension basic
        with _ ->
            basic
    )

let ComputeAnonModuleName check defaultNamespace filename (m: range) =
    let modname = CanonicalizeFilename filename

    if
        check
        && not (modname |> String.forall (fun c -> Char.IsLetterOrDigit c || c = '_'))
    then
        if
            not (
                filename.EndsWith("fsx", StringComparison.OrdinalIgnoreCase)
                || filename.EndsWith("fsscript", StringComparison.OrdinalIgnoreCase)
            )
        then
            warning (
                Error(
                    FSComp.SR.buildImplicitModuleIsNotLegalIdentifier (
                        modname,
                        (FileSystemUtils.fileNameOfPath filename)
                    ),
                    m
                )
            )

    let combined =
        match defaultNamespace with
        | None -> modname
        | Some ns -> textOfPath [ ns; modname ]

    let anonymousModuleNameRange =
        let filename = m.FileName
        mkRange filename pos0 pos0

    pathToSynLid anonymousModuleNameRange (splitNamespace combined)

let IsScript filename =
    let lower = String.lowercase filename
    FSharpScriptFileSuffixes |> List.exists (FileSystemUtils.checkSuffix lower)

let PostParseModuleImpl (_i, defaultNamespace, _isLastCompiland, filename, impl) =
    match impl with
    | ParsedImplFileFragment.NamedModule(SynModuleOrNamespace(lid,
                                                              isRec,
                                                              kind,
                                                              decls,
                                                              xmlDoc,
                                                              attribs,
                                                              access,
                                                              m,
                                                              trivia)) ->
        let lid =
            match lid with
            | [ id ] when kind.IsModule && id.idText = MangledGlobalName ->
                error (Error(FSComp.SR.buildInvalidModuleOrNamespaceName (), id.idRange))
            | id :: rest when id.idText = MangledGlobalName -> rest
            | _ -> lid

        SynModuleOrNamespace(lid, isRec, kind, decls, xmlDoc, attribs, access, m, trivia)

    | ParsedImplFileFragment.AnonModule(defs, m) ->
        let modname =
            ComputeAnonModuleName (not (isNil defs)) defaultNamespace filename (trimRangeToLine m)

        let trivia: SynModuleOrNamespaceTrivia =
            { LeadingKeyword = SynModuleOrNamespaceLeadingKeyword.None }

        SynModuleOrNamespace(
            modname,
            false,
            SynModuleOrNamespaceKind.AnonModule,
            defs,
            PreXmlDoc.Empty,
            [],
            None,
            m,
            trivia
        )

    | ParsedImplFileFragment.NamespaceFragment(lid, isRecursive, kind, decls, xmlDoc, attributes, range, trivia) ->
        let lid, kind =
            match lid with
            | id :: rest when id.idText = MangledGlobalName ->
                rest,
                if List.isEmpty rest then
                    SynModuleOrNamespaceKind.GlobalNamespace
                else
                    kind
            | _ -> lid, kind

        SynModuleOrNamespace(lid, isRecursive, kind, decls, xmlDoc, attributes, None, range, trivia)

// Give a unique name to the different kinds of inputs. Used to correlate signature and implementation files
//   QualFileNameOfModuleName - files with a single module declaration or an anonymous module
let QualFileNameOfModuleName m filename modname =
    QualifiedNameOfFile(mkSynId m (textOfLid modname + (if IsScript filename then "$fsx" else "")))

let QualFileNameOfFilename m filename =
    QualifiedNameOfFile(mkSynId m (CanonicalizeFilename filename + (if IsScript filename then "$fsx" else "")))

let QualFileNameOfSpecs filename specs =
    match specs with
    | [ SynModuleOrNamespaceSig(longId = modname; kind = kind; range = m) ] when kind.IsModule ->
        QualFileNameOfModuleName m filename modname
    | [ SynModuleOrNamespaceSig(kind = kind; range = m) ] when not kind.IsModule -> QualFileNameOfFilename m filename
    | _ -> QualFileNameOfFilename (mkRange filename pos0 pos0) filename

let QualFileNameOfImpls filename specs =
    match specs with
    | [ SynModuleOrNamespace(longId = modname; kind = kind; range = m) ] when kind.IsModule ->
        QualFileNameOfModuleName m filename modname
    | [ SynModuleOrNamespace(kind = kind; range = m) ] when not kind.IsModule -> QualFileNameOfFilename m filename
    | _ -> QualFileNameOfFilename (mkRange filename pos0 pos0) filename

let collectCodeComments (lexbuf: UnicodeLexing.Lexbuf) =
    let tripleSlashComments = XmlDocStore.ReportInvalidXmlDocPositions lexbuf

    [ yield! CommentStore.GetComments(lexbuf)
      yield! List.map CommentTrivia.LineComment tripleSlashComments ]
    |> List.sortBy (function
        | CommentTrivia.LineComment r
        | CommentTrivia.BlockComment r -> r.StartLine, r.StartColumn)

let PostParseModuleImpls (defaultNamespace, filename, isLastCompiland, ParsedImplFile(hashDirectives, impls), lexbuf) =
    match
        impls
        |> List.rev
        |> List.tryPick (function
            | ParsedImplFileFragment.NamedModule(SynModuleOrNamespace(longId = lid)) -> Some lid
            | _ -> None)
    with
    | Some lid when impls.Length > 1 -> errorR (Error(FSComp.SR.buildMultipleToplevelModules (), rangeOfLid lid))
    | _ -> ()

    let impls =
        impls
        |> List.mapi (fun i x -> PostParseModuleImpl(i, defaultNamespace, isLastCompiland, filename, x))

    let qualName = QualFileNameOfImpls filename impls
    let isScript = IsScript filename

    let trivia =
        { ConditionalDirectives = IfdefStore.GetTrivia(lexbuf)
          WarnDirectives = WarnScopes.getDirectiveTrivia (lexbuf)
          CodeComments = collectCodeComments lexbuf }

    ParsedInput.ImplFile(
        ParsedImplFileInput(filename, isScript, qualName, hashDirectives, impls, isLastCompiland, trivia, Set.empty)
    )

let PostParseModuleSpec (_i, defaultNamespace, _isLastCompiland, filename, intf) =
    match intf with
    | ParsedSigFileFragment.NamedModule(SynModuleOrNamespaceSig(lid,
                                                                isRec,
                                                                kind,
                                                                decls,
                                                                xmlDoc,
                                                                attribs,
                                                                access,
                                                                m,
                                                                trivia)) ->
        let lid =
            match lid with
            | [ id ] when kind.IsModule && id.idText = MangledGlobalName ->
                error (Error(FSComp.SR.buildInvalidModuleOrNamespaceName (), id.idRange))
            | id :: rest when id.idText = MangledGlobalName -> rest
            | _ -> lid

        SynModuleOrNamespaceSig(
            lid,
            isRec,
            SynModuleOrNamespaceKind.NamedModule,
            decls,
            xmlDoc,
            attribs,
            access,
            m,
            trivia
        )

    | ParsedSigFileFragment.AnonModule(defs, m) ->
        let modname =
            ComputeAnonModuleName (not (isNil defs)) defaultNamespace filename (trimRangeToLine m)

        let trivia: SynModuleOrNamespaceSigTrivia =
            { LeadingKeyword = SynModuleOrNamespaceLeadingKeyword.None }

        SynModuleOrNamespaceSig(
            modname,
            false,
            SynModuleOrNamespaceKind.AnonModule,
            defs,
            PreXmlDoc.Empty,
            [],
            None,
            m,
            trivia
        )

    | ParsedSigFileFragment.NamespaceFragment(lid, isRecursive, kind, decls, xmlDoc, attributes, range, trivia) ->
        let lid, kind =
            match lid with
            | id :: rest when id.idText = MangledGlobalName ->
                rest,
                if List.isEmpty rest then
                    SynModuleOrNamespaceKind.GlobalNamespace
                else
                    kind
            | _ -> lid, kind

        SynModuleOrNamespaceSig(lid, isRecursive, kind, decls, xmlDoc, attributes, None, range, trivia)

let PostParseModuleSpecs
    (defaultNamespace, filename, isLastCompiland, ParsedSigFile(hashDirectives, specs), lexbuf: UnicodeLexing.Lexbuf)
    =
    match
        specs
        |> List.rev
        |> List.tryPick (function
            | ParsedSigFileFragment.NamedModule(SynModuleOrNamespaceSig(longId = lid)) -> Some lid
            | _ -> None)
    with
    | Some lid when specs.Length > 1 -> errorR (Error(FSComp.SR.buildMultipleToplevelModules (), rangeOfLid lid))
    | _ -> ()

    let specs =
        specs
        |> List.mapi (fun i x -> PostParseModuleSpec(i, defaultNamespace, isLastCompiland, filename, x))

    let qualName = QualFileNameOfSpecs filename specs

    let trivia =
        { ConditionalDirectives = IfdefStore.GetTrivia(lexbuf)
          WarnDirectives = WarnScopes.getDirectiveTrivia (lexbuf)
          CodeComments = collectCodeComments lexbuf }

    ParsedInput.SigFile(ParsedSigFileInput(filename, qualName, hashDirectives, specs, trivia, Set.empty))

let ParseInput
    (
        lexer,
        errorLogger: CapturingDiagnosticsLogger,
        lexbuf: UnicodeLexing.Lexbuf,
        defaultNamespace,
        filename,
        isLastCompiland
    ) =
    // The assert below is almost ok, but it fires in two cases:
    //  - fsi.exe sometimes passes "stdin" as a dummy filename
    //  - if you have a #line directive, e.g.
    //        # 1000 "Line01.fs"
    //    then it also asserts. But these are edge cases that can be fixed later, e.g. in bug 4651.
    //System.Diagnostics.Debug.Assert(System.IO.Path.IsPathRooted filename, sprintf "should be absolute: '%s'" filename)
    let lower = String.lowercase filename

    // Delay sending errors and warnings until after the file is parsed. This gives us a chance to scrape the
    // #nowarn declarations for the file
    let delayLogger = CapturingDiagnosticsLogger("Parsing")
    use _ = UseDiagnosticsLogger delayLogger
    use _ = UseBuildPhase BuildPhase.Parse

    try
        if mlCompatSuffixes |> List.exists (FileSystemUtils.checkSuffix lower) then
            if lexbuf.SupportsFeature LanguageFeature.MLCompatRevisions then
                errorR (Error(FSComp.SR.buildInvalidSourceFileExtensionML filename, rangeStartup))
            else
                mlCompatWarning (FSComp.SR.buildCompilingExtensionIsForML ()) rangeStartup

        // Call the appropriate parser - for signature files or implementation files
        if FSharpImplFileSuffixes |> List.exists (FileSystemUtils.checkSuffix lower) then
            let impl = Parser.implementationFile lexer lexbuf
            PostParseModuleImpls(defaultNamespace, filename, isLastCompiland, impl, lexbuf)
        elif FSharpSigFileSuffixes |> List.exists (FileSystemUtils.checkSuffix lower) then
            let intfs = Parser.signatureFile lexer lexbuf
            PostParseModuleSpecs(defaultNamespace, filename, isLastCompiland, intfs, lexbuf)
        else if lexbuf.SupportsFeature LanguageFeature.MLCompatRevisions then
            error (Error(FSComp.SR.buildInvalidSourceFileExtensionUpdated filename, rangeStartup))
        else
            error (Error(FSComp.SR.buildInvalidSourceFileExtension filename, rangeStartup))
    finally
        // OK, now commit the errors, since the ScopedPragmas will (hopefully) have been scraped
        let filteringErrorLogger = errorLogger // TODO: does this matter? //GetErrorLoggerFilteringByScopedPragmas(false, scopedPragmas, diagnosticOptions, errorLogger)
        delayLogger.CommitDelayedDiagnostics filteringErrorLogger

let EmptyParsedInput (filename, isLastCompiland) =
    let lower = String.lowercase filename

    if FSharpSigFileSuffixes |> List.exists (FileSystemUtils.checkSuffix lower) then
        ParsedInput.SigFile(
            ParsedSigFileInput(filename, QualFileNameOfImpls filename [], [], [], ParsedInputTrivia.Empty, Set.empty)
        )
    else
        ParsedInput.ImplFile(
            ParsedImplFileInput(
                filename,
                false,
                QualFileNameOfImpls filename [],
                [],
                [],
                isLastCompiland,
                ParsedInputTrivia.Empty,
                Set.empty
            )
        )

let createLexbuf langVersion sourceText =
    UnicodeLexing.SourceTextAsLexbuf(true, LanguageVersion(langVersion), Some true, sourceText)

let createLexerFunction (defines: string list) lexbuf (errorLogger: CapturingDiagnosticsLogger) =
    let lightStatus = IndentationAwareSyntaxStatus(true, true)

    // Note: we don't really attempt to intern strings across a large scope.
    let lexResourceManager = LexResourceManager()

    let lexargs =
        mkLexargs (defines, lightStatus, lexResourceManager, [], errorLogger, PathMap.empty, false)

    let lexargs =
        { lexargs with
            applyLineDirectives = false }

    let compilingFsLib = false

    let tokenizer =
        LexFilter.LexFilter(lightStatus, compilingFsLib, Lexer.token lexargs true, lexbuf, false)

    (fun _ -> tokenizer.GetToken())

type FSharpParserDiagnostic =
    { Severity: FSharpDiagnosticSeverity
      SubCategory: string
      Range: range option
      // GetDiagnosticNumber from dotnet/fsharp/src/fsharp/CompilerDiagnostics.fs
      ErrorNumber: int option
      Message: string }

let getErrorString key = SR.GetString key

let UnexpectedEndOfInputE () =
    DeclareResourceString("UnexpectedEndOfInput", "")

let OBlockEndSentenceE () =
    DeclareResourceString("BlockEndSentence", "")

let UnexpectedE () =
    DeclareResourceString("Unexpected", "%s")

let NONTERM_interactionE () =
    DeclareResourceString("NONTERM.interaction", "")

let NONTERM_hashDirectiveE () =
    DeclareResourceString("NONTERM.hashDirective", "")

let NONTERM_fieldDeclE () =
    DeclareResourceString("NONTERM.fieldDecl", "")

let NONTERM_unionCaseReprE () =
    DeclareResourceString("NONTERM.unionCaseRepr", "")

let NONTERM_localBindingE () =
    DeclareResourceString("NONTERM.localBinding", "")

let NONTERM_hardwhiteLetBindingsE () =
    DeclareResourceString("NONTERM.hardwhiteLetBindings", "")

let NONTERM_classDefnMemberE () =
    DeclareResourceString("NONTERM.classDefnMember", "")

let NONTERM_defnBindingsE () =
    DeclareResourceString("NONTERM.defnBindings", "")

let NONTERM_classMemberSpfnE () =
    DeclareResourceString("NONTERM.classMemberSpfn", "")

let NONTERM_valSpfnE () =
    DeclareResourceString("NONTERM.valSpfn", "")

let NONTERM_tyconSpfnE () =
    DeclareResourceString("NONTERM.tyconSpfn", "")

let NONTERM_anonLambdaExprE () =
    DeclareResourceString("NONTERM.anonLambdaExpr", "")

let NONTERM_attrUnionCaseDeclE () =
    DeclareResourceString("NONTERM.attrUnionCaseDecl", "")

let NONTERM_cPrototypeE () =
    DeclareResourceString("NONTERM.cPrototype", "")

let NONTERM_objectImplementationMembersE () =
    DeclareResourceString("NONTERM.objectImplementationMembers", "")

let NONTERM_ifExprCasesE () =
    DeclareResourceString("NONTERM.ifExprCases", "")

let NONTERM_openDeclE () =
    DeclareResourceString("NONTERM.openDecl", "")

let NONTERM_fileModuleSpecE () =
    DeclareResourceString("NONTERM.fileModuleSpec", "")

let NONTERM_patternClausesE () =
    DeclareResourceString("NONTERM.patternClauses", "")

let NONTERM_beginEndExprE () =
    DeclareResourceString("NONTERM.beginEndExpr", "")

let NONTERM_recdExprE () =
    DeclareResourceString("NONTERM.recdExpr", "")

let NONTERM_tyconDefnE () =
    DeclareResourceString("NONTERM.tyconDefn", "")

let NONTERM_exconCoreE () =
    DeclareResourceString("NONTERM.exconCore", "")

let NONTERM_typeNameInfoE () =
    DeclareResourceString("NONTERM.typeNameInfo", "")

let NONTERM_attributeListE () =
    DeclareResourceString("NONTERM.attributeList", "")

let NONTERM_quoteExprE () =
    DeclareResourceString("NONTERM.quoteExpr", "")

let NONTERM_typeConstraintE () =
    DeclareResourceString("NONTERM.typeConstraint", "")

let NONTERM_Category_ImplementationFileE () =
    DeclareResourceString("NONTERM.Category.ImplementationFile", "")

let NONTERM_Category_DefinitionE () =
    DeclareResourceString("NONTERM.Category.Definition", "")

let NONTERM_Category_SignatureFileE () =
    DeclareResourceString("NONTERM.Category.SignatureFile", "")

let NONTERM_Category_PatternE () =
    DeclareResourceString("NONTERM.Category.Pattern", "")

let NONTERM_Category_ExprE () =
    DeclareResourceString("NONTERM.Category.Expr", "")

let NONTERM_Category_TypeE () =
    DeclareResourceString("NONTERM.Category.Type", "")

let NONTERM_typeArgsActualE () =
    DeclareResourceString("NONTERM.typeArgsActual", "")

let TokenName1E () =
    DeclareResourceString("TokenName1", "%s")

let TokenName1TokenName2E () =
    DeclareResourceString("TokenName1TokenName2", "%s%s")

let TokenName1TokenName2TokenName3E () =
    DeclareResourceString("TokenName1TokenName2TokenName3", "%s%s%s")

let LibraryUseOnlyE () =
    DeclareResourceString("LibraryUseOnly", "")

let getSyntaxErrorMessage ctxt =
    let ctxt =
        unbox<Internal.Utilities.Text.Parsing.ParseErrorContext<Parser.token>> ctxt

    let os = StringBuilder()

    let (|EndOfStructuredConstructToken|_|) token =
        match token with
        | Parser.TOKEN_ODECLEND
        | Parser.TOKEN_OBLOCKSEP
        | Parser.TOKEN_OEND
        | Parser.TOKEN_ORIGHT_BLOCK_END
        | Parser.TOKEN_OBLOCKEND
        | Parser.TOKEN_OBLOCKEND_COMING_SOON
        | Parser.TOKEN_OBLOCKEND_IS_HERE -> Some()
        | _ -> None

    let tokenIdToText tid =
        match tid with
        | Parser.TOKEN_IDENT -> getErrorString "Parser.TOKEN.IDENT"
        | Parser.TOKEN_BIGNUM
        | Parser.TOKEN_INT8
        | Parser.TOKEN_UINT8
        | Parser.TOKEN_INT16
        | Parser.TOKEN_UINT16
        | Parser.TOKEN_INT32
        | Parser.TOKEN_UINT32
        | Parser.TOKEN_INT64
        | Parser.TOKEN_UINT64
        | Parser.TOKEN_UNATIVEINT
        | Parser.TOKEN_NATIVEINT -> getErrorString "Parser.TOKEN.INT"
        | Parser.TOKEN_IEEE32
        | Parser.TOKEN_IEEE64 -> getErrorString "Parser.TOKEN.FLOAT"
        | Parser.TOKEN_DECIMAL -> getErrorString "Parser.TOKEN.DECIMAL"
        | Parser.TOKEN_CHAR -> getErrorString "Parser.TOKEN.CHAR"

        | Parser.TOKEN_BASE -> getErrorString "Parser.TOKEN.BASE"
        | Parser.TOKEN_LPAREN_STAR_RPAREN -> getErrorString "Parser.TOKEN.LPAREN.STAR.RPAREN"
        | Parser.TOKEN_DOLLAR -> getErrorString "Parser.TOKEN.DOLLAR"
        | Parser.TOKEN_INFIX_STAR_STAR_OP -> getErrorString "Parser.TOKEN.INFIX.STAR.STAR.OP"
        | Parser.TOKEN_INFIX_COMPARE_OP -> getErrorString "Parser.TOKEN.INFIX.COMPARE.OP"
        | Parser.TOKEN_COLON_GREATER -> getErrorString "Parser.TOKEN.COLON.GREATER"
        | Parser.TOKEN_COLON_COLON -> getErrorString "Parser.TOKEN.COLON.COLON"
        | Parser.TOKEN_PERCENT_OP -> getErrorString "Parser.TOKEN.PERCENT.OP"
        | Parser.TOKEN_INFIX_AT_HAT_OP -> getErrorString "Parser.TOKEN.INFIX.AT.HAT.OP"
        | Parser.TOKEN_INFIX_BAR_OP -> getErrorString "Parser.TOKEN.INFIX.BAR.OP"
        | Parser.TOKEN_PLUS_MINUS_OP -> getErrorString "Parser.TOKEN.PLUS.MINUS.OP"
        | Parser.TOKEN_PREFIX_OP -> getErrorString "Parser.TOKEN.PREFIX.OP"
        | Parser.TOKEN_COLON_QMARK_GREATER -> getErrorString "Parser.TOKEN.COLON.QMARK.GREATER"
        | Parser.TOKEN_INFIX_STAR_DIV_MOD_OP -> getErrorString "Parser.TOKEN.INFIX.STAR.DIV.MOD.OP"
        | Parser.TOKEN_INFIX_AMP_OP -> getErrorString "Parser.TOKEN.INFIX.AMP.OP"
        | Parser.TOKEN_AMP -> getErrorString "Parser.TOKEN.AMP"
        | Parser.TOKEN_AMP_AMP -> getErrorString "Parser.TOKEN.AMP.AMP"
        | Parser.TOKEN_BAR_BAR -> getErrorString "Parser.TOKEN.BAR.BAR"
        | Parser.TOKEN_LESS -> getErrorString "Parser.TOKEN.LESS"
        | Parser.TOKEN_GREATER -> getErrorString "Parser.TOKEN.GREATER"
        | Parser.TOKEN_QMARK -> getErrorString "Parser.TOKEN.QMARK"
        | Parser.TOKEN_QMARK_QMARK -> getErrorString "Parser.TOKEN.QMARK.QMARK"
        | Parser.TOKEN_COLON_QMARK -> getErrorString "Parser.TOKEN.COLON.QMARK"
        | Parser.TOKEN_INT32_DOT_DOT -> getErrorString "Parser.TOKEN.INT32.DOT.DOT"
        | Parser.TOKEN_DOT_DOT -> getErrorString "Parser.TOKEN.DOT.DOT"
        | Parser.TOKEN_DOT_DOT_HAT -> getErrorString "Parser.TOKEN.DOT.DOT"
        | Parser.TOKEN_QUOTE -> getErrorString "Parser.TOKEN.QUOTE"
        | Parser.TOKEN_STAR -> getErrorString "Parser.TOKEN.STAR"
        | Parser.TOKEN_HIGH_PRECEDENCE_TYAPP -> getErrorString "Parser.TOKEN.HIGH.PRECEDENCE.TYAPP"
        | Parser.TOKEN_COLON -> getErrorString "Parser.TOKEN.COLON"
        | Parser.TOKEN_COLON_EQUALS -> getErrorString "Parser.TOKEN.COLON.EQUALS"
        | Parser.TOKEN_LARROW -> getErrorString "Parser.TOKEN.LARROW"
        | Parser.TOKEN_EQUALS -> getErrorString "Parser.TOKEN.EQUALS"
        | Parser.TOKEN_GREATER_BAR_RBRACK -> getErrorString "Parser.TOKEN.GREATER.BAR.RBRACK"
        | Parser.TOKEN_MINUS -> getErrorString "Parser.TOKEN.MINUS"
        | Parser.TOKEN_ADJACENT_PREFIX_OP -> getErrorString "Parser.TOKEN.ADJACENT.PREFIX.OP"
        | Parser.TOKEN_FUNKY_OPERATOR_NAME -> getErrorString "Parser.TOKEN.FUNKY.OPERATOR.NAME"
        | Parser.TOKEN_COMMA -> getErrorString "Parser.TOKEN.COMMA"
        | Parser.TOKEN_DOT -> getErrorString "Parser.TOKEN.DOT"
        | Parser.TOKEN_BAR -> getErrorString "Parser.TOKEN.BAR"
        | Parser.TOKEN_HASH -> getErrorString "Parser.TOKEN.HASH"
        | Parser.TOKEN_UNDERSCORE -> getErrorString "Parser.TOKEN.UNDERSCORE"
        | Parser.TOKEN_SEMICOLON -> getErrorString "Parser.TOKEN.SEMICOLON"
        | Parser.TOKEN_SEMICOLON_SEMICOLON -> getErrorString "Parser.TOKEN.SEMICOLON.SEMICOLON"
        | Parser.TOKEN_LPAREN -> getErrorString "Parser.TOKEN.LPAREN"
        | Parser.TOKEN_RPAREN
        | Parser.TOKEN_RPAREN_COMING_SOON
        | Parser.TOKEN_RPAREN_IS_HERE -> getErrorString "Parser.TOKEN.RPAREN"
        | Parser.TOKEN_LQUOTE -> getErrorString "Parser.TOKEN.LQUOTE"
        | Parser.TOKEN_LBRACK -> getErrorString "Parser.TOKEN.LBRACK"
        | Parser.TOKEN_LBRACE_BAR -> getErrorString "Parser.TOKEN.LBRACE.BAR"
        | Parser.TOKEN_LBRACK_BAR -> getErrorString "Parser.TOKEN.LBRACK.BAR"
        | Parser.TOKEN_LBRACK_LESS -> getErrorString "Parser.TOKEN.LBRACK.LESS"
        | Parser.TOKEN_LBRACE -> getErrorString "Parser.TOKEN.LBRACE"
        | Parser.TOKEN_BAR_RBRACK -> getErrorString "Parser.TOKEN.BAR.RBRACK"
        | Parser.TOKEN_BAR_RBRACE -> getErrorString "Parser.TOKEN.BAR.RBRACE"
        | Parser.TOKEN_GREATER_RBRACK -> getErrorString "Parser.TOKEN.GREATER.RBRACK"
        | Parser.TOKEN_RQUOTE_DOT
        | Parser.TOKEN_RQUOTE -> getErrorString "Parser.TOKEN.RQUOTE"
        | Parser.TOKEN_RBRACK -> getErrorString "Parser.TOKEN.RBRACK"
        | Parser.TOKEN_RBRACE
        | Parser.TOKEN_RBRACE_COMING_SOON
        | Parser.TOKEN_RBRACE_IS_HERE -> getErrorString "Parser.TOKEN.RBRACE"
        | Parser.TOKEN_PUBLIC -> getErrorString "Parser.TOKEN.PUBLIC"
        | Parser.TOKEN_PRIVATE -> getErrorString "Parser.TOKEN.PRIVATE"
        | Parser.TOKEN_INTERNAL -> getErrorString "Parser.TOKEN.INTERNAL"
        | Parser.TOKEN_CONSTRAINT -> getErrorString "Parser.TOKEN.CONSTRAINT"
        | Parser.TOKEN_INSTANCE -> getErrorString "Parser.TOKEN.INSTANCE"
        | Parser.TOKEN_DELEGATE -> getErrorString "Parser.TOKEN.DELEGATE"
        | Parser.TOKEN_INHERIT -> getErrorString "Parser.TOKEN.INHERIT"
        | Parser.TOKEN_CONSTRUCTOR -> getErrorString "Parser.TOKEN.CONSTRUCTOR"
        | Parser.TOKEN_DEFAULT -> getErrorString "Parser.TOKEN.DEFAULT"
        | Parser.TOKEN_OVERRIDE -> getErrorString "Parser.TOKEN.OVERRIDE"
        | Parser.TOKEN_ABSTRACT -> getErrorString "Parser.TOKEN.ABSTRACT"
        | Parser.TOKEN_CLASS -> getErrorString "Parser.TOKEN.CLASS"
        | Parser.TOKEN_MEMBER -> getErrorString "Parser.TOKEN.MEMBER"
        | Parser.TOKEN_STATIC -> getErrorString "Parser.TOKEN.STATIC"
        | Parser.TOKEN_NAMESPACE -> getErrorString "Parser.TOKEN.NAMESPACE"
        | Parser.TOKEN_OBLOCKBEGIN -> getErrorString "Parser.TOKEN.OBLOCKBEGIN"
        | EndOfStructuredConstructToken -> getErrorString "Parser.TOKEN.OBLOCKEND"
        | Parser.TOKEN_THEN
        | Parser.TOKEN_OTHEN -> getErrorString "Parser.TOKEN.OTHEN"
        | Parser.TOKEN_ELSE
        | Parser.TOKEN_OELSE -> getErrorString "Parser.TOKEN.OELSE"
        | Parser.TOKEN_LET
        | Parser.TOKEN_OLET -> getErrorString "Parser.TOKEN.OLET"
        | Parser.TOKEN_OBINDER
        | Parser.TOKEN_BINDER -> getErrorString "Parser.TOKEN.BINDER"
        | Parser.TOKEN_OAND_BANG
        | Parser.TOKEN_AND_BANG -> getErrorString "Parser.TOKEN.AND.BANG"
        | Parser.TOKEN_ODO -> getErrorString "Parser.TOKEN.ODO"
        | Parser.TOKEN_OWITH -> getErrorString "Parser.TOKEN.OWITH"
        | Parser.TOKEN_OFUNCTION -> getErrorString "Parser.TOKEN.OFUNCTION"
        | Parser.TOKEN_OFUN -> getErrorString "Parser.TOKEN.OFUN"
        | Parser.TOKEN_ORESET -> getErrorString "Parser.TOKEN.ORESET"
        | Parser.TOKEN_ODUMMY -> getErrorString "Parser.TOKEN.ODUMMY"
        | Parser.TOKEN_DO_BANG
        | Parser.TOKEN_ODO_BANG -> getErrorString "Parser.TOKEN.ODO.BANG"
        | Parser.TOKEN_YIELD -> getErrorString "Parser.TOKEN.YIELD"
        | Parser.TOKEN_YIELD_BANG -> getErrorString "Parser.TOKEN.YIELD.BANG"
        | Parser.TOKEN_OINTERFACE_MEMBER -> getErrorString "Parser.TOKEN.OINTERFACE.MEMBER"
        | Parser.TOKEN_ELIF -> getErrorString "Parser.TOKEN.ELIF"
        | Parser.TOKEN_RARROW -> getErrorString "Parser.TOKEN.RARROW"
        | Parser.TOKEN_SIG -> getErrorString "Parser.TOKEN.SIG"
        | Parser.TOKEN_STRUCT -> getErrorString "Parser.TOKEN.STRUCT"
        | Parser.TOKEN_UPCAST -> getErrorString "Parser.TOKEN.UPCAST"
        | Parser.TOKEN_DOWNCAST -> getErrorString "Parser.TOKEN.DOWNCAST"
        | Parser.TOKEN_NULL -> getErrorString "Parser.TOKEN.NULL"
        | Parser.TOKEN_RESERVED -> getErrorString "Parser.TOKEN.RESERVED"
        | Parser.TOKEN_MODULE
        | Parser.TOKEN_MODULE_COMING_SOON
        | Parser.TOKEN_MODULE_IS_HERE -> getErrorString "Parser.TOKEN.MODULE"
        | Parser.TOKEN_AND -> getErrorString "Parser.TOKEN.AND"
        | Parser.TOKEN_AS -> getErrorString "Parser.TOKEN.AS"
        | Parser.TOKEN_ASSERT -> getErrorString "Parser.TOKEN.ASSERT"
        | Parser.TOKEN_OASSERT -> getErrorString "Parser.TOKEN.ASSERT"
        | Parser.TOKEN_ASR -> getErrorString "Parser.TOKEN.ASR"
        | Parser.TOKEN_DOWNTO -> getErrorString "Parser.TOKEN.DOWNTO"
        | Parser.TOKEN_EXCEPTION -> getErrorString "Parser.TOKEN.EXCEPTION"
        | Parser.TOKEN_FALSE -> getErrorString "Parser.TOKEN.FALSE"
        | Parser.TOKEN_FOR -> getErrorString "Parser.TOKEN.FOR"
        | Parser.TOKEN_FUN -> getErrorString "Parser.TOKEN.FUN"
        | Parser.TOKEN_FUNCTION -> getErrorString "Parser.TOKEN.FUNCTION"
        | Parser.TOKEN_FINALLY -> getErrorString "Parser.TOKEN.FINALLY"
        | Parser.TOKEN_LAZY -> getErrorString "Parser.TOKEN.LAZY"
        | Parser.TOKEN_OLAZY -> getErrorString "Parser.TOKEN.LAZY"
        | Parser.TOKEN_MATCH -> getErrorString "Parser.TOKEN.MATCH"
        | Parser.TOKEN_MATCH_BANG -> getErrorString "Parser.TOKEN.MATCH.BANG"
        | Parser.TOKEN_MUTABLE -> getErrorString "Parser.TOKEN.MUTABLE"
        | Parser.TOKEN_NEW -> getErrorString "Parser.TOKEN.NEW"
        | Parser.TOKEN_OF -> getErrorString "Parser.TOKEN.OF"
        | Parser.TOKEN_OPEN -> getErrorString "Parser.TOKEN.OPEN"
        | Parser.TOKEN_OR -> getErrorString "Parser.TOKEN.OR"
        | Parser.TOKEN_VOID -> getErrorString "Parser.TOKEN.VOID"
        | Parser.TOKEN_EXTERN -> getErrorString "Parser.TOKEN.EXTERN"
        | Parser.TOKEN_INTERFACE -> getErrorString "Parser.TOKEN.INTERFACE"
        | Parser.TOKEN_REC -> getErrorString "Parser.TOKEN.REC"
        | Parser.TOKEN_TO -> getErrorString "Parser.TOKEN.TO"
        | Parser.TOKEN_TRUE -> getErrorString "Parser.TOKEN.TRUE"
        | Parser.TOKEN_TRY -> getErrorString "Parser.TOKEN.TRY"
        | Parser.TOKEN_TYPE
        | Parser.TOKEN_TYPE_COMING_SOON
        | Parser.TOKEN_TYPE_IS_HERE -> getErrorString "Parser.TOKEN.TYPE"
        | Parser.TOKEN_VAL -> getErrorString "Parser.TOKEN.VAL"
        | Parser.TOKEN_INLINE -> getErrorString "Parser.TOKEN.INLINE"
        | Parser.TOKEN_WHEN -> getErrorString "Parser.TOKEN.WHEN"
        | Parser.TOKEN_WHILE -> getErrorString "Parser.TOKEN.WHILE"
        | Parser.TOKEN_WHILE_BANG -> getErrorString "Parser.TOKEN.WHILE.BANG"
        | Parser.TOKEN_WITH -> getErrorString "Parser.TOKEN.WITH"
        | Parser.TOKEN_IF -> getErrorString "Parser.TOKEN.IF"
        | Parser.TOKEN_DO -> getErrorString "Parser.TOKEN.DO"
        | Parser.TOKEN_GLOBAL -> getErrorString "Parser.TOKEN.GLOBAL"
        | Parser.TOKEN_DONE -> getErrorString "Parser.TOKEN.DONE"
        | Parser.TOKEN_IN
        | Parser.TOKEN_JOIN_IN -> getErrorString "Parser.TOKEN.IN"
        | Parser.TOKEN_HIGH_PRECEDENCE_PAREN_APP -> getErrorString "Parser.TOKEN.HIGH.PRECEDENCE.PAREN.APP"
        | Parser.TOKEN_HIGH_PRECEDENCE_BRACK_APP -> getErrorString "Parser.TOKEN.HIGH.PRECEDENCE.BRACK.APP"
        | Parser.TOKEN_BEGIN -> getErrorString "Parser.TOKEN.BEGIN"
        | Parser.TOKEN_END -> getErrorString "Parser.TOKEN.END"
        | Parser.TOKEN_HASH_LIGHT
        | Parser.TOKEN_HASH_LINE
        | Parser.TOKEN_HASH_IF
        | Parser.TOKEN_HASH_ELSE
        | Parser.TOKEN_HASH_ENDIF -> getErrorString "Parser.TOKEN.HASH.ENDIF"
        | Parser.TOKEN_INACTIVECODE -> getErrorString "Parser.TOKEN.INACTIVECODE"
        | Parser.TOKEN_LEX_FAILURE -> getErrorString "Parser.TOKEN.LEX.FAILURE"
        | Parser.TOKEN_WHITESPACE -> getErrorString "Parser.TOKEN.WHITESPACE"
        | Parser.TOKEN_COMMENT -> getErrorString "Parser.TOKEN.COMMENT"
        | Parser.TOKEN_LINE_COMMENT -> getErrorString "Parser.TOKEN.LINE.COMMENT"
        | Parser.TOKEN_STRING_TEXT -> getErrorString "Parser.TOKEN.STRING.TEXT"
        | Parser.TOKEN_BYTEARRAY -> getErrorString "Parser.TOKEN.BYTEARRAY"
        | Parser.TOKEN_STRING -> getErrorString "Parser.TOKEN.STRING"
        | Parser.TOKEN_KEYWORD_STRING -> getErrorString "Parser.TOKEN.KEYWORD_STRING"
        | Parser.TOKEN_EOF -> getErrorString "Parser.TOKEN.EOF"
        | Parser.TOKEN_CONST -> getErrorString "Parser.TOKEN.CONST"
        | Parser.TOKEN_FIXED -> getErrorString "Parser.TOKEN.FIXED"
        | Parser.TOKEN_INTERP_STRING_BEGIN_END -> getErrorString "Parser.TOKEN.INTERP.STRING.BEGIN.END"
        | Parser.TOKEN_INTERP_STRING_BEGIN_PART -> getErrorString "Parser.TOKEN.INTERP.STRING.BEGIN.PART"
        | Parser.TOKEN_INTERP_STRING_PART -> getErrorString "Parser.TOKEN.INTERP.STRING.PART"
        | Parser.TOKEN_INTERP_STRING_END -> getErrorString "Parser.TOKEN.INTERP.STRING.END"
        | unknown ->
            Debug.Assert(false, "unknown token tag")
            let result = $"%+A{unknown}"
            Debug.Assert(false, result)
            result

    match ctxt.CurrentToken with
    | None -> os.Append(UnexpectedEndOfInputE().Format) |> ignore
    | Some token ->
        match (token |> Parser.tagOfToken |> Parser.tokenTagToTokenId), token with
        | EndOfStructuredConstructToken, _ -> os.Append(OBlockEndSentenceE().Format) |> ignore
        | Parser.TOKEN_LEX_FAILURE, Parser.LEX_FAILURE str -> Printf.bprintf os $"%s{str}" (* Fix bug://2431 *)
        | token, _ -> os.Append(UnexpectedE().Format(token |> tokenIdToText)) |> ignore

        (* Search for a state producing a single recognized non-terminal in the states on the stack *)
        let foundInContext =

            (* Merge a bunch of expression non terminals *)
            let (|NONTERM_Category_Expr|_|) =
                function
                | Parser.NONTERM_argExpr
                | Parser.NONTERM_minusExpr
                | Parser.NONTERM_parenExpr
                | Parser.NONTERM_atomicExpr
                | Parser.NONTERM_appExpr
                | Parser.NONTERM_tupleExpr
                | Parser.NONTERM_declExpr
                | Parser.NONTERM_braceExpr
                | Parser.NONTERM_braceBarExpr
                | Parser.NONTERM_typedSequentialExprBlock
                | Parser.NONTERM_interactiveExpr -> Some()
                | _ -> None

            (* Merge a bunch of pattern non terminals *)
            let (|NONTERM_Category_Pattern|_|) =
                function
                | Parser.NONTERM_constrPattern
                | Parser.NONTERM_parenPattern
                | Parser.NONTERM_atomicPattern -> Some()
                | _ -> None

            (* Merge a bunch of if/then/else non terminals *)
            let (|NONTERM_Category_IfThenElse|_|) =
                function
                | Parser.NONTERM_ifExprThen
                | Parser.NONTERM_ifExprElifs
                | Parser.NONTERM_ifExprCases -> Some()
                | _ -> None

            (* Merge a bunch of non terminals *)
            let (|NONTERM_Category_SignatureFile|_|) =
                function
                | Parser.NONTERM_signatureFile
                | Parser.NONTERM_moduleSpfn
                | Parser.NONTERM_moduleSpfns -> Some()
                | _ -> None

            let (|NONTERM_Category_ImplementationFile|_|) =
                function
                | Parser.NONTERM_implementationFile
                | Parser.NONTERM_fileNamespaceImpl
                | Parser.NONTERM_fileNamespaceImpls -> Some()
                | _ -> None

            let (|NONTERM_Category_Definition|_|) =
                function
                | Parser.NONTERM_fileModuleImpl
                | Parser.NONTERM_moduleDefn
                | Parser.NONTERM_interactiveDefns
                | Parser.NONTERM_moduleDefns
                | Parser.NONTERM_moduleDefnsOrExpr -> Some()
                | _ -> None

            let (|NONTERM_Category_Type|_|) =
                function
                | Parser.NONTERM_typ
                | Parser.NONTERM_tupleType -> Some()
                | _ -> None

            let (|NONTERM_Category_Interaction|_|) =
                function
                | Parser.NONTERM_interactiveItemsTerminator
                | Parser.NONTERM_interaction
                | Parser.NONTERM__startinteraction -> Some()
                | _ -> None

            // Canonicalize the categories and check for a unique category
            ctxt.ReducibleProductions
            |> List.exists (fun prods ->
                match
                    prods
                    |> List.map Parser.prodIdxToNonTerminal
                    |> List.map (function
                        | NONTERM_Category_Type -> Parser.NONTERM_typ
                        | NONTERM_Category_Expr -> Parser.NONTERM_declExpr
                        | NONTERM_Category_Pattern -> Parser.NONTERM_atomicPattern
                        | NONTERM_Category_IfThenElse -> Parser.NONTERM_ifExprThen
                        | NONTERM_Category_SignatureFile -> Parser.NONTERM_signatureFile
                        | NONTERM_Category_ImplementationFile -> Parser.NONTERM_implementationFile
                        | NONTERM_Category_Definition -> Parser.NONTERM_moduleDefn
                        | NONTERM_Category_Interaction -> Parser.NONTERM_interaction
                        | nt -> nt)
                    |> Set.ofList
                    |> Set.toList
                with
                | [ Parser.NONTERM_interaction ] ->
                    os.Append(NONTERM_interactionE().Format) |> ignore
                    true
                | [ Parser.NONTERM_hashDirective ] ->
                    os.Append(NONTERM_hashDirectiveE().Format) |> ignore
                    true
                | [ Parser.NONTERM_fieldDecl ] ->
                    os.Append(NONTERM_fieldDeclE().Format) |> ignore
                    true
                | [ Parser.NONTERM_unionCaseRepr ] ->
                    os.Append(NONTERM_unionCaseReprE().Format) |> ignore
                    true
                | [ Parser.NONTERM_localBinding ] ->
                    os.Append(NONTERM_localBindingE().Format) |> ignore
                    true
                | [ Parser.NONTERM_hardwhiteLetBindings ] ->
                    os.Append(NONTERM_hardwhiteLetBindingsE().Format) |> ignore
                    true
                | [ Parser.NONTERM_classDefnMember ] ->
                    os.Append(NONTERM_classDefnMemberE().Format) |> ignore
                    true
                | [ Parser.NONTERM_defnBindings ] ->
                    os.Append(NONTERM_defnBindingsE().Format) |> ignore
                    true
                | [ Parser.NONTERM_classMemberSpfn ] ->
                    os.Append(NONTERM_classMemberSpfnE().Format) |> ignore
                    true
                | [ Parser.NONTERM_valSpfn ] ->
                    os.Append(NONTERM_valSpfnE().Format) |> ignore
                    true
                | [ Parser.NONTERM_tyconSpfn ] ->
                    os.Append(NONTERM_tyconSpfnE().Format) |> ignore
                    true
                | [ Parser.NONTERM_anonLambdaExpr ] ->
                    os.Append(NONTERM_anonLambdaExprE().Format) |> ignore
                    true
                | [ Parser.NONTERM_attrUnionCaseDecl ] ->
                    os.Append(NONTERM_attrUnionCaseDeclE().Format) |> ignore
                    true
                | [ Parser.NONTERM_cPrototype ] ->
                    os.Append(NONTERM_cPrototypeE().Format) |> ignore
                    true
                | [ Parser.NONTERM_objExpr | Parser.NONTERM_objectImplementationMembers ] ->
                    os.Append(NONTERM_objectImplementationMembersE().Format) |> ignore
                    true
                | [ Parser.NONTERM_ifExprThen | Parser.NONTERM_ifExprElifs | Parser.NONTERM_ifExprCases ] ->
                    os.Append(NONTERM_ifExprCasesE().Format) |> ignore
                    true
                | [ Parser.NONTERM_openDecl ] ->
                    os.Append(NONTERM_openDeclE().Format) |> ignore
                    true
                | [ Parser.NONTERM_fileModuleSpec ] ->
                    os.Append(NONTERM_fileModuleSpecE().Format) |> ignore
                    true
                | [ Parser.NONTERM_patternClauses ] ->
                    os.Append(NONTERM_patternClausesE().Format) |> ignore
                    true
                | [ Parser.NONTERM_beginEndExpr ] ->
                    os.Append(NONTERM_beginEndExprE().Format) |> ignore
                    true
                | [ Parser.NONTERM_recdExpr ] ->
                    os.Append(NONTERM_recdExprE().Format) |> ignore
                    true
                | [ Parser.NONTERM_tyconDefn ] ->
                    os.Append(NONTERM_tyconDefnE().Format) |> ignore
                    true
                | [ Parser.NONTERM_exconCore ] ->
                    os.Append(NONTERM_exconCoreE().Format) |> ignore
                    true
                | [ Parser.NONTERM_typeNameInfo ] ->
                    os.Append(NONTERM_typeNameInfoE().Format) |> ignore
                    true
                | [ Parser.NONTERM_attributeList ] ->
                    os.Append(NONTERM_attributeListE().Format) |> ignore
                    true
                | [ Parser.NONTERM_quoteExpr ] ->
                    os.Append(NONTERM_quoteExprE().Format) |> ignore
                    true
                | [ Parser.NONTERM_typeConstraint ] ->
                    os.Append(NONTERM_typeConstraintE().Format) |> ignore
                    true
                | [ NONTERM_Category_ImplementationFile ] ->
                    os.Append(NONTERM_Category_ImplementationFileE().Format) |> ignore
                    true
                | [ NONTERM_Category_Definition ] ->
                    os.Append(NONTERM_Category_DefinitionE().Format) |> ignore
                    true
                | [ NONTERM_Category_SignatureFile ] ->
                    os.Append(NONTERM_Category_SignatureFileE().Format) |> ignore
                    true
                | [ NONTERM_Category_Pattern ] ->
                    os.Append(NONTERM_Category_PatternE().Format) |> ignore
                    true
                | [ NONTERM_Category_Expr ] ->
                    os.Append(NONTERM_Category_ExprE().Format) |> ignore
                    true
                | [ NONTERM_Category_Type ] ->
                    os.Append(NONTERM_Category_TypeE().Format) |> ignore
                    true
                | [ Parser.NONTERM_typeArgsActual ] ->
                    os.Append(NONTERM_typeArgsActualE().Format) |> ignore
                    true
                | _ -> false)

#if DEBUG
        if not foundInContext then
            Printf.bprintf
                os
                $". (no 'in' context found: %+A{List.map (List.map Parser.prodIdxToNonTerminal) ctxt.ReducibleProductions})"
#else
        foundInContext |> ignore // suppress unused variable warning in RELEASE
#endif
        let fix (s: string) =
            s
                .Replace(SR.GetString("FixKeyword"), "")
                .Replace(SR.GetString("FixSymbol"), "")
                .Replace(SR.GetString("FixReplace"), "")

        match
            (ctxt.ShiftTokens
             |> List.map Parser.tokenTagToTokenId
             |> List.filter (function
                 | Parser.TOKEN_error
                 | Parser.TOKEN_EOF -> false
                 | _ -> true)
             |> List.map tokenIdToText
             |> Set.ofList
             |> Set.toList)
        with
        | [ tokenName1 ] -> os.Append(TokenName1E().Format(fix tokenName1)) |> ignore
        | [ tokenName1; tokenName2 ] ->
            os.Append(TokenName1TokenName2E().Format (fix tokenName1) (fix tokenName2))
            |> ignore
        | [ tokenName1; tokenName2; tokenName3 ] ->
            os.Append(TokenName1TokenName2TokenName3E().Format (fix tokenName1) (fix tokenName2) (fix tokenName3))
            |> ignore
        | _ -> ()

    os.ToString()

let parseFile
    (isSignature: bool)
    (sourceText: ISourceText)
    (defines: string list)
    : ParsedInput * FSharpParserDiagnostic list =
    let errorLogger = CapturingDiagnosticsLogger("ErrorHandler")

    let parseResult =
        let fileName = if isSignature then "tmp.fsi" else "tmp.fsx"

        usingLexbufForParsing (createLexbuf "preview" sourceText, fileName) (fun lexbuf ->

            let lexfun = createLexerFunction defines lexbuf errorLogger
            // both don't matter for Fantomas
            let isLastCompiland = false
            let isExe = false

            try
                ParseInput(lexfun, errorLogger, lexbuf, None, fileName, (isLastCompiland, isExe))
            with e ->
                errorLogger.StopProcessingRecovery e range0 // don't re-raise any exceptions, we must return None.
                EmptyParsedInput(fileName, (isLastCompiland, isExe)))

    let diagnostics =
        List.map
            (fun (p, severity) ->
                // See https://github.com/dotnet/fsharp/blob/2a25184293e39a635217670652b00680de04472a/src/Compiler/Driver/CompilerDiagnostics.fs#L214
                // for the error codes
                let range, message, errorNumber =
                    match p.Exception with
                    | :? IndentationProblem as ip -> Some ip.Data1, ip.Data0, Some 58
                    | :? SyntaxError as se -> Some se.range, (getSyntaxErrorMessage se.Data0), Some 10
                    | :? LibraryUseOnly as luo -> Some luo.range, LibraryUseOnlyE().Format, Some 42
                    | :? DiagnosticWithText as dwt -> Some dwt.range, dwt.message, Some dwt.number
                    | :? ReservedKeyword as rkw -> Some rkw.Data1, rkw.Data0, Some 46
                    | _ -> None, p.Exception.Message, None

                { Severity = severity
                  SubCategory = "parse"
                  Range = range
                  ErrorNumber = errorNumber
                  Message = message })
            errorLogger.Diagnostics

    parseResult, diagnostics
