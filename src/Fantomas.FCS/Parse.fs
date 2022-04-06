module internal Fantomas.FCS.Parse

open System
open Internal.Utilities
open Internal.Utilities.Library
open FSharp.Compiler
open FSharp.Compiler.AbstractIL.IL
open FSharp.Compiler.ErrorLogger
open FSharp.Compiler.Features
open FSharp.Compiler.Lexhelp
open FSharp.Compiler.Text
open FSharp.Compiler.Text.Position
open FSharp.Compiler.Text.Range
open FSharp.Compiler.Xml
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Syntax.PrettyNaming
open FSharp.Compiler.SyntaxTreeOps
open FSharp.Compiler.IO
open FSharp.Compiler.ParseHelpers
open FSharp.Compiler.ParseAndCheckInputs

let ComputeAnonModuleName check defaultNamespace filename (m: range) =
    let modname = CanonicalizeFilename filename

    if
        check
        && not
            (
                modname
                |> String.forall (fun c -> Char.IsLetterOrDigit c || c = '_')
            )
    then
        if
            not
                (
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

    FSharpScriptFileSuffixes
    |> List.exists (FileSystemUtils.checkSuffix lower)

let PostParseModuleImpl (_i, defaultNamespace, _isLastCompiland, filename, impl) =
    match impl with
    | ParsedImplFileFragment.NamedModule (SynModuleOrNamespace (lid, isRec, kind, decls, xmlDoc, attribs, access, m)) ->
        let lid =
            match lid with
            | [ id ] when kind.IsModule && id.idText = MangledGlobalName ->
                error (Error(FSComp.SR.buildInvalidModuleOrNamespaceName (), id.idRange))
            | id :: rest when id.idText = MangledGlobalName -> rest
            | _ -> lid

        SynModuleOrNamespace(lid, isRec, kind, decls, xmlDoc, attribs, access, m)

    | ParsedImplFileFragment.AnonModule (defs, m) ->
        //        let isLast, isExe = isLastCompiland
//        let lower = String.lowercase filename
//        if not (isLast && isExe) && not (doNotRequireNamespaceOrModuleSuffixes |> List.exists (FileSystemUtils.checkSuffix lower)) then
//            match defs with
//            | SynModuleDecl.NestedModule _ :: _ -> errorR(Error(FSComp.SR.noEqualSignAfterModule(), trimRangeToLine m))
//            | _ -> errorR(Error(FSComp.SR.buildMultiFileRequiresNamespaceOrModule(), trimRangeToLine m))

        let modname =
            ComputeAnonModuleName (not (isNil defs)) defaultNamespace filename (trimRangeToLine m)

        SynModuleOrNamespace(modname, false, SynModuleOrNamespaceKind.AnonModule, defs, PreXmlDoc.Empty, [], None, m)

    | ParsedImplFileFragment.NamespaceFragment (lid, a, kind, c, d, e, m) ->
        let lid, kind =
            match lid with
            | id :: rest when id.idText = MangledGlobalName ->
                rest,
                if List.isEmpty rest then
                    SynModuleOrNamespaceKind.GlobalNamespace
                else
                    kind
            | _ -> lid, kind

        SynModuleOrNamespace(lid, a, kind, c, d, e, None, m)

// Give a unique name to the different kinds of inputs. Used to correlate signature and implementation files
//   QualFileNameOfModuleName - files with a single module declaration or an anonymous module
let QualFileNameOfModuleName m filename modname =
    QualifiedNameOfFile(
        mkSynId
            m
            (textOfLid modname
             + (if IsScript filename then "$fsx" else ""))
    )

let QualFileNameOfFilename m filename =
    QualifiedNameOfFile(
        mkSynId
            m
            (CanonicalizeFilename filename
             + (if IsScript filename then "$fsx" else ""))
    )

let QualFileNameOfImpls filename specs =
    match specs with
    | [ SynModuleOrNamespace (modname, _, kind, _, _, _, _, m) ] when kind.IsModule ->
        QualFileNameOfModuleName m filename modname
    | [ SynModuleOrNamespace (_, _, kind, _, _, _, _, m) ] when not kind.IsModule -> QualFileNameOfFilename m filename
    | _ -> QualFileNameOfFilename (mkRange filename pos0 pos0) filename

let QualFileNameOfSpecs filename specs =
    match specs with
    | [ SynModuleOrNamespaceSig (modname, _, kind, _, _, _, _, m) ] when kind.IsModule ->
        QualFileNameOfModuleName m filename modname
    | [ SynModuleOrNamespaceSig (_, _, kind, _, _, _, _, m) ] when not kind.IsModule ->
        QualFileNameOfFilename m filename
    | _ -> QualFileNameOfFilename (mkRange filename pos0 pos0) filename

//let GetScopedPragmasForHashDirective hd =
//    [ match hd with
//      | ParsedHashDirective("nowarn", numbers, m) ->
//          for s in numbers do
//              match s with
//              | ParsedHashDirectiveArgument.SourceIdentifier _ -> ()
//              | ParsedHashDirectiveArgument.String (s, _, _) ->
//                  match GetWarningNumber(m, s) with
//                  | None -> ()
//                  | Some n -> yield ScopedPragma.WarningOff(m, n)
//      | _ -> () ]

let GetScopedPragmasForInput input =
    match input with
    | ParsedInput.SigFile (ParsedSigFileInput (scopedPragmas = pragmas)) -> pragmas
    | ParsedInput.ImplFile (ParsedImplFileInput (scopedPragmas = pragmas)) -> pragmas

let private collectCodeComments (lexbuf: UnicodeLexing.Lexbuf) (tripleSlashComments: range list) =
    [ yield! LexbufCommentStore.GetComments(lexbuf)
      yield! (List.map CommentTrivia.LineComment tripleSlashComments) ]
    |> List.sortBy (function
        | CommentTrivia.LineComment r
        | CommentTrivia.BlockComment r -> r.StartLine, r.StartColumn)

let PostParseModuleImpls
    (
        defaultNamespace,
        filename,
        isLastCompiland,
        ParsedImplFile (hashDirectives, impls),
        lexbuf: UnicodeLexing.Lexbuf,
        tripleSlashComments: range list
    ) =
    match impls
          |> List.rev
          |> List.tryPick (function
              | ParsedImplFileFragment.NamedModule (SynModuleOrNamespace (lid, _, _, _, _, _, _, _)) -> Some lid
              | _ -> None)
        with
    | Some lid when impls.Length > 1 -> errorR (Error(FSComp.SR.buildMultipleToplevelModules (), rangeOfLid lid))
    | _ -> ()

    let impls =
        impls
        |> List.mapi (fun i x -> PostParseModuleImpl(i, defaultNamespace, isLastCompiland, filename, x))

    let qualName = QualFileNameOfImpls filename impls
    let isScript = IsScript filename

    let scopedPragmas = []
    let conditionalDirectives = LexbufIfdefStore.GetTrivia(lexbuf)
    let codeComments = collectCodeComments lexbuf tripleSlashComments

    ParsedInput.ImplFile(
        ParsedImplFileInput(
            filename,
            isScript,
            qualName,
            scopedPragmas,
            hashDirectives,
            impls,
            isLastCompiland,
            { ConditionalDirectives = conditionalDirectives
              CodeComments = codeComments }
        )
    )

let PostParseModuleSpec (_i, defaultNamespace, _isLastCompiland, filename, intf) =
    match intf with
    | ParsedSigFileFragment.NamedModule (SynModuleOrNamespaceSig (lid, isRec, kind, decls, xmlDoc, attribs, access, m)) ->
        let lid =
            match lid with
            | [ id ] when kind.IsModule && id.idText = MangledGlobalName ->
                error (Error(FSComp.SR.buildInvalidModuleOrNamespaceName (), id.idRange))
            | id :: rest when id.idText = MangledGlobalName -> rest
            | _ -> lid

        SynModuleOrNamespaceSig(lid, isRec, SynModuleOrNamespaceKind.NamedModule, decls, xmlDoc, attribs, access, m)

    | ParsedSigFileFragment.AnonModule (defs, m) ->
        //        let isLast, isExe = isLastCompiland
//        let lower = String.lowercase filename
//        if not (isLast && isExe) && not (doNotRequireNamespaceOrModuleSuffixes |> List.exists (FileSystemUtils.checkSuffix lower)) then
//            match defs with
//            | SynModuleSigDecl.NestedModule _ :: _ -> errorR(Error(FSComp.SR.noEqualSignAfterModule(), m))
//            | _ -> errorR(Error(FSComp.SR.buildMultiFileRequiresNamespaceOrModule(), m))

        let modname =
            ComputeAnonModuleName (not (isNil defs)) defaultNamespace filename (trimRangeToLine m)

        SynModuleOrNamespaceSig(modname, false, SynModuleOrNamespaceKind.AnonModule, defs, PreXmlDoc.Empty, [], None, m)

    | ParsedSigFileFragment.NamespaceFragment (lid, a, kind, c, d, e, m) ->
        let lid, kind =
            match lid with
            | id :: rest when id.idText = MangledGlobalName ->
                rest,
                if List.isEmpty rest then
                    SynModuleOrNamespaceKind.GlobalNamespace
                else
                    kind
            | _ -> lid, kind

        SynModuleOrNamespaceSig(lid, a, kind, c, d, e, None, m)

let PostParseModuleSpecs
    (
        defaultNamespace,
        filename,
        isLastCompiland,
        ParsedSigFile (hashDirectives, specs),
        lexbuf: UnicodeLexing.Lexbuf,
        tripleSlashComments: range list
    ) =
    match specs
          |> List.rev
          |> List.tryPick (function
              | ParsedSigFileFragment.NamedModule (SynModuleOrNamespaceSig (lid, _, _, _, _, _, _, _)) -> Some lid
              | _ -> None)
        with
    | Some lid when specs.Length > 1 -> errorR (Error(FSComp.SR.buildMultipleToplevelModules (), rangeOfLid lid))
    | _ -> ()

    let specs =
        specs
        |> List.mapi (fun i x -> PostParseModuleSpec(i, defaultNamespace, isLastCompiland, filename, x))

    let qualName = QualFileNameOfSpecs filename specs
    let scopedPragmas = []
    //        [ for SynModuleOrNamespaceSig(_, _, _, decls, _, _, _, _) in specs do
//            for d in decls do
//                match d with
//                | SynModuleSigDecl.HashDirective(hd, _) -> yield! GetScopedPragmasForHashDirective hd
//                | _ -> ()
//          for hd in hashDirectives do
//              yield! GetScopedPragmasForHashDirective hd ]

    let conditionalDirectives = LexbufIfdefStore.GetTrivia(lexbuf)
    let codeComments = collectCodeComments lexbuf tripleSlashComments

    ParsedInput.SigFile(
        ParsedSigFileInput(
            filename,
            qualName,
            scopedPragmas,
            hashDirectives,
            specs,
            { ConditionalDirectives = conditionalDirectives
              CodeComments = codeComments }
        )
    )

let ParseInput
    (
        lexer,
        errorLogger: ErrorLogger,
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
    let delayLogger = CapturingErrorLogger("Parsing")
    use unwindEL = PushErrorLoggerPhaseUntilUnwind(fun _ -> delayLogger)
    use unwindBP = PushThreadBuildPhaseUntilUnwind BuildPhase.Parse

    let mutable scopedPragmas = []

    try
        let input =
            if
                mlCompatSuffixes
                |> List.exists (FileSystemUtils.checkSuffix lower)
            then
                if lexbuf.SupportsFeature LanguageFeature.MLCompatRevisions then
                    errorR (Error(FSComp.SR.buildInvalidSourceFileExtensionML filename, rangeStartup))
                else
                    mlCompatWarning (FSComp.SR.buildCompilingExtensionIsForML ()) rangeStartup

            // Call the appropriate parser - for signature files or implementation files
            if
                FSharpImplFileSuffixes
                |> List.exists (FileSystemUtils.checkSuffix lower)
            then
                let impl = Parser.implementationFile lexer lexbuf

                let tripleSlashComments =
                    LexbufLocalXmlDocStore.ReportInvalidXmlDocPositions(lexbuf)

                PostParseModuleImpls(defaultNamespace, filename, isLastCompiland, impl, lexbuf, tripleSlashComments)
            elif
                FSharpSigFileSuffixes
                |> List.exists (FileSystemUtils.checkSuffix lower)
            then
                let intfs = Parser.signatureFile lexer lexbuf

                let tripleSlashComments =
                    LexbufLocalXmlDocStore.ReportInvalidXmlDocPositions(lexbuf)

                PostParseModuleSpecs(defaultNamespace, filename, isLastCompiland, intfs, lexbuf, tripleSlashComments)
            else if lexbuf.SupportsFeature LanguageFeature.MLCompatRevisions then
                error (Error(FSComp.SR.buildInvalidSourceFileExtensionUpdated filename, rangeStartup))
            else
                error (Error(FSComp.SR.buildInvalidSourceFileExtension filename, rangeStartup))


        scopedPragmas <- GetScopedPragmasForInput input
        input
    finally
        // OK, now commit the errors, since the ScopedPragmas will (hopefully) have been scraped
        let filteringErrorLogger = errorLogger // TODO: does this matter? //GetErrorLoggerFilteringByScopedPragmas(false, scopedPragmas, diagnosticOptions, errorLogger)
        delayLogger.CommitDelayedDiagnostics filteringErrorLogger

let EmptyParsedInput (filename, isLastCompiland) =
    let lower = String.lowercase filename

    if
        FSharpSigFileSuffixes
        |> List.exists (FileSystemUtils.checkSuffix lower)
    then
        ParsedInput.SigFile(
            ParsedSigFileInput(
                filename,
                QualFileNameOfImpls filename [],
                [],
                [],
                [],
                { ConditionalDirectives = []
                  CodeComments = [] }
            )
        )
    else
        ParsedInput.ImplFile(
            ParsedImplFileInput(
                filename,
                false,
                QualFileNameOfImpls filename [],
                [],
                [],
                [],
                isLastCompiland,
                { ConditionalDirectives = []
                  CodeComments = [] }
            )
        )

let createLexbuf langVersion sourceText =
    UnicodeLexing.SourceTextAsLexbuf(true, LanguageVersion(langVersion), sourceText)

let createLexerFunction (defines: string list) lexbuf (errorLogger: ErrorLogger) =
    let lightStatus = LightSyntaxStatus(true, true) // getLightSyntaxStatus fileName options

    // Note: we don't really attempt to intern strings across a large scope.
    let lexResourceManager = LexResourceManager()

    // When analyzing files using ParseOneFile, i.e. for the use of editing clients, we do not apply line directives.
    // TODO(pathmap): expose PathMap on the service API, and thread it through here
    let lexargs =
        mkLexargs (defines, lightStatus, lexResourceManager, [], errorLogger, PathMap.empty)

    let lexargs = { lexargs with applyLineDirectives = false }

    let compilingFsLib = false

    let tokenizer =
        LexFilter.LexFilter(lightStatus, compilingFsLib, Lexer.token lexargs true, lexbuf)

    (fun _ -> tokenizer.GetToken())

let parseFile (isSignature: bool) (sourceText: ISourceText) (defines: string list) =
    // let errHandler = ErrorHandler(true, fileName, options.ErrorSeverityOptions, sourceText, suggestNamesForErrors)
    // TODO
    // CapturingErrorLogger
    let errorLogger = CapturingErrorLogger("ErrorHandler")
    //        { new ErrorLogger("ErrorHandler") with
//            member x.DiagnosticSink (exn, severity) =
//                match exn.Exception with
//                | :? SyntaxError as syntaxError ->
//                    match syntaxError.Data0 with
//                    | :? ParseErrorContext<_> as pec ->
//                        printfn "%A" pec
//                    | _ -> ()
//                | _ -> ()
//            member x.ErrorCount = 0 }

    // use unwindEL = PushErrorLoggerPhaseUntilUnwind (fun _oldLogger -> errHandler.ErrorLogger)
//   use unwindBP = PushThreadBuildPhaseUntilUnwind BuildPhase.Parse

    let parseResult =
        let fileName =
            if isSignature then
                "tmp.fsi"
            else
                "tmp.fsx"

        usingLexbufForParsing (createLexbuf "preview" sourceText, fileName) (fun lexbuf ->

            let lexfun = createLexerFunction defines lexbuf errorLogger
            // both don't matter for Fantomas
            let isLastCompiland = false
            let isExe = false

            try
                ParseInput(lexfun, errorLogger, lexbuf, None, fileName, (isLastCompiland, isExe))
            with
            | e ->
                errorLogger.StopProcessingRecovery e range0 // don't re-raise any exceptions, we must return None.
                EmptyParsedInput(fileName, (isLastCompiland, isExe)))

    // TODO: think about what to return
    // errHandler.CollectedDiagnostics, parseResult, errHandler.AnyErrors
    parseResult, errorLogger.Diagnostics
