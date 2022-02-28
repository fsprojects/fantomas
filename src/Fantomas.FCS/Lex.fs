module internal Fantomas.FCS.Lex

open System
open FSharp.Compiler
open FSharp.Compiler.ErrorLogger
open FSharp.Compiler.Lexhelp
open FSharp.Compiler.ParseHelpers
open FSharp.Compiler.Text

[<Flags>]
type FSharpLexerFlags =
    | Default = 0x11011
    | LightSyntaxOn = 0x00001
    | Compiling = 0x00010
    | CompilingFSharpCore = 0x00110
    | SkipTrivia = 0x01000
    | UseLexFilter = 0x10000

let lex
    (onToken: Parser.token -> range -> unit)
    (conditionalCompilationDefines: string list)
    (text: ISourceText)
    : unit =
    let errorLogger: ErrorLogger =
        { new ErrorLogger("DiscardErrorsLogger") with
            member x.DiagnosticSink(phasedError, severity) = ()
            member x.ErrorCount = 0 }

    let langVersion = Features.LanguageVersion.Default
    let reportLibraryOnlyFeatures = false
    let canSkipTrivia = true //(flags &&& FSharpLexerFlags.SkipTrivia) = FSharpLexerFlags.SkipTrivia
    let isLightSyntaxOn = true // (flags &&& FSharpLexerFlags.LightSyntaxOn) = FSharpLexerFlags.LightSyntaxOn
    let isCompiling = false // (flags &&& FSharpLexerFlags.Compiling) = FSharpLexerFlags.Compiling
    let isCompilingFSharpCore = false // (flags &&& FSharpLexerFlags.CompilingFSharpCore) = FSharpLexerFlags.CompilingFSharpCore
    let canUseLexFilter = true // No idea...  (flags &&& FSharpLexerFlags.UseLexFilter) = FSharpLexerFlags.UseLexFilter

    let lexbuf =
        UnicodeLexing.SourceTextAsLexbuf(reportLibraryOnlyFeatures, langVersion, text)

    let lightStatus = LightSyntaxStatus(isLightSyntaxOn, true)

    let lexargs =
        mkLexargs (
            conditionalCompilationDefines,
            lightStatus,
            LexResourceManager(0),
            [],
            errorLogger,
            Internal.Utilities.PathMap.empty
        )

    let lexargs = { lexargs with applyLineDirectives = isCompiling }

    let getNextToken =
        let lexer = Lexer.token lexargs canSkipTrivia

        if canUseLexFilter then
            let lexFilter =
                LexFilter.LexFilter(lexargs.lightStatus, isCompilingFSharpCore, lexer, lexbuf)

            (fun _ -> lexFilter.GetToken())
        else
            lexer

    resetLexbufPos "" lexbuf

    while not lexbuf.IsPastEndOfStream do
        // ct.ThrowIfCancellationRequested ()
        onToken (getNextToken lexbuf) lexbuf.LexemeRange

//        let source = "let a = 0 // comment" |> SourceText.ofString
//        let tokens = ResizeArray()
//        lex  source [] (fun t _r -> tokens.Add(t)) PathMap.empty
//        Assert.IsFalse(Seq.isEmpty tokens)
