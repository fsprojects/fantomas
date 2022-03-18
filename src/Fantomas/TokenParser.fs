module internal Fantomas.TokenParser

open System.Text
open Microsoft.FSharp.Core.CompilerServices
open FSharp.Compiler
open FSharp.Compiler.Text
open FSharp.Compiler.Parser
open FSharp.Compiler.ParseHelpers
open FSharp.Compiler.Syntax.PrettyNaming
open FSharp.Compiler.SyntaxTrivia
open Fantomas.FCS.Lex
open Fantomas
open Fantomas.ISourceTextExtensions
open Fantomas.TokenParserBoolExpr
open Fantomas.TriviaTypes

let private (|Newline|_|) (range: range) (token: token) =
    match token with
    | WHITESPACE _ ->
        if range.StartLine < range.EndLine
           && range.StartColumn = 0
           && range.EndColumn = 0 then
            Some token
        else
            None
    | _ -> None

let private (|NonTripleSlashOpeningCommentToken|_|) token =
    match token with
    | LINE_COMMENT (LexerContinuation.SingleLineComment (range = range)) ->
        if range.EndColumn - range.StartColumn <> 3 then
            Some()
        else
            None
    | _ -> None

let private (|CommentContentToken|_|) (range: range) (token: token) =
    match token with
    | EOF _ -> None
    | _ ->
        if (range.StartLine = range.EndLine) then
            Some()
        else
            None

let private (|BlockCommentOpeningCommentToken|_|) (token: token) =
    match token with
    | COMMENT _ -> Some "(*"
    | _ -> None

let private (|QuoteStringToken|_|) token =
    match token with
    | STRING_TEXT (LexerContinuation.String (style = style)) -> Some style
    | _ -> None

[<NoEquality; NoComparison>]
type TriviaCollectorState =
    | NotCollecting of lastTokenAndRange: SourceToken
    | CaptureLineComment of
        lastTokenAndRange: SourceToken *
        afterSourceCode: bool *
        startRange: range *
        endRange: range *
        collector: StringBuilder
    | CaptureBlockComment of
        lastTokenAndRange: SourceToken *
        startRange: range *
        /// Edge case, we need to keep track of nested block comments.
        /// We don't necessarily consider a comment closed from the first *)
        level: int *
        previousTokenWasStar: bool *
        collector: StringBuilder
    | CaptureSingleQuoteString of lastTokenWasBackSlash: bool
    | CaptureTripleQuoteString of lastTokenWasBackSlash: bool
    | CaptureVerbatimString of lastTokenWasBackSlash: bool

[<RequireQualifiedAccess>]
type ConditionalCodeTree =
    | TopLevel
    | Nested of currentNode: ConditionalCodeNode
    | NestedAndDead of deadNode: ConditionalCodeNode * endIfLevel: int

and ConditionalCodeNode =
    { IsActive: bool
      Parent: ConditionalCodeTree
      IfCondition: IfDirectiveExpression
      StartRange: range
      Else: range option }

type TriviaBuilderState =
    { CodePath: ConditionalCodeTree
      CollectorState: TriviaCollectorState }

let private createLineComment afterSourceCode startRange range sb =
    let r = Range.unionRanges startRange range
    let comment = sb.ToString().TrimEnd()

    let item =
        if afterSourceCode then
            Comment.LineCommentAfterSourceCode
        else
            Comment.LineCommentOnSingleLine

    { Item = Comment(item comment)
      Range = r }

let private (|DecompiledOperatorToken|_|) (token: token) =
    match token with
    | IDENT ident ->
        if (ident.StartsWith("op_"))
           && (let d = DecompileOpName ident in d <> ident) then
            Some ident
        else
            None
    | _ -> None

let private ifDefKey = "Ifdef"

let private getLastItemFromStore (lexbuf: UnicodeLexing.Lexbuf) : int * ConditionalDirectiveTrivia option =
    match lexbuf.BufferLocalStore.TryGetValue ifDefKey with
    | true, store -> store
    | _ ->
        let store = box (ResizeArray<ConditionalDirectiveTrivia>())
        lexbuf.BufferLocalStore.[ifDefKey] <- store
        store
    |> unbox<ResizeArray<ConditionalDirectiveTrivia>>
    |> fun trivia -> trivia.Count, Seq.tryLast trivia

let getTokensFromSource (source: ISourceText) : SourceToken list =
    let mutable tokenCollector = ListCollector<SourceToken>()
    let mutable currentLine = 0
    let mutable currentHashDirectiveTriviaCount = 0

    let onToken (token: token) (lexbuf: UnicodeLexing.Lexbuf) =
        let range = lexbuf.LexemeRange

        match token with
        | OBLOCKBEGIN
        | OBLOCKEND_COMING_SOON
        | OBLOCKEND_IS_HERE
        | ORIGHT_BLOCK_END
        | ODECLEND
        | OBLOCKSEP
        | OEND
        | RPAREN_COMING_SOON
        | TYPE_COMING_SOON
        | MODULE_COMING_SOON
        | RBRACE_COMING_SOON
        | RPAREN_COMING_SOON
        | HIGH_PRECEDENCE_TYAPP
        | HIGH_PRECEDENCE_PAREN_APP
        | HIGH_PRECEDENCE_BRACK_APP -> ()
        // Edge where there is a `;` at the start of a line
        | SEMICOLON when (range.StartLine > currentLine) -> ()
        | _ ->
            match token with
            | WHITESPACE _ -> tokenCollector.Add(SourceToken.Token(token, range))
            | HASH_IF _
            | HASH_ELSE _
            | HASH_ENDIF _ ->
                let triviaCount, lastItem = getLastItemFromStore lexbuf

                if triviaCount > currentHashDirectiveTriviaCount then
                    Option.iter (fun item -> tokenCollector.Add(SourceToken.Hash(item))) lastItem
                else
                    tokenCollector.Add(SourceToken.Token(token, range))
            | _ ->
                currentLine <- range.EndLine
                tokenCollector.Add(SourceToken.Token(token, range))

    lex onToken source
    tokenCollector.Close()

let private getIndividualDefine (hashDirectives: ConditionalDirectiveTrivia list) : string list list =
    let rec visit (expr: IfDirectiveExpression) : string list =
        match expr with
        | IfDirectiveExpression.Not expr -> visit expr
        | IfDirectiveExpression.And (e1, e2)
        | IfDirectiveExpression.Or (e1, e2) -> visit e1 @ visit e2
        | IfDirectiveExpression.Ident s -> List.singleton s

    hashDirectives
    |> List.collect (function
        | ConditionalDirectiveTrivia.If (expr, _r) -> visit expr
        | _ -> [])
    |> List.distinct
    |> List.map List.singleton

let private getDefineExprs (hashDirectives: ConditionalDirectiveTrivia list) =
    let result =
        (([], []), hashDirectives)
        ||> List.fold (fun (contextExprs, exprAcc) hashLine ->
            let contextExpr e =
                e :: contextExprs
                |> List.reduce (fun x y -> IfDirectiveExpression.And(x, y))

            match hashLine with
            | ConditionalDirectiveTrivia.If (expr, _) -> expr :: contextExprs, contextExpr expr :: exprAcc
            | ConditionalDirectiveTrivia.Else _ ->
                contextExprs,
                IfDirectiveExpression.Not(
                    contextExprs
                    |> List.reduce (fun x y -> IfDirectiveExpression.And(x, y))
                )
                :: exprAcc
            | ConditionalDirectiveTrivia.EndIf _ -> List.tail contextExprs, exprAcc)
        |> snd
        |> List.rev

    result

let private getOptimizedDefinesSets (hashDirectives: ConditionalDirectiveTrivia list) =
    let maxSteps = FormatConfig.satSolveMaxStepsMaxSteps
    let defineExprs = getDefineExprs hashDirectives

    match mergeBoolExprs maxSteps defineExprs
          |> List.map snd
        with
    | [] -> [ [] ]
    | xs -> xs

let getDefineCombination (hashDirectives: ConditionalDirectiveTrivia list) : DefineCombination list =

    [ yield [] // always include the empty defines set
      yield! getOptimizedDefinesSets hashDirectives
      yield! getIndividualDefine hashDirectives ]
    |> List.distinct

let getTriviaFromTokens
    (source: ISourceText)
    (tokens: SourceToken list)
    (defineCombination: DefineCombination)
    : Trivia list =
    let mutable triviaCollection = ListCollector<Trivia>()

    // tokens |> List.iter (printfn "%A")

    let initialState: TriviaBuilderState =
        { CodePath = ConditionalCodeTree.TopLevel
          CollectorState = NotCollecting(SourceToken.Token(OBLOCKBEGIN, Range.Zero)) }

    tokens
    |> List.skipWhile (function
        | SourceToken.Token (WHITESPACE _, _) -> true
        | _ -> false)
    |> List.fold
        (fun (acc: TriviaBuilderState) sourceToken ->

            match acc.CollectorState, acc.CodePath, sourceToken with
            // New level of conditional code
            | TriviaCollectorState.NotCollecting _,
              currentCodePath,
              SourceToken.Hash (ConditionalDirectiveTrivia.If (expr, _)) ->

                let isActiveCode =
                    match currentCodePath with
                    // If we are currently inside dead code, the new code can never be active
                    | ConditionalCodeTree.Nested { IsActive = false } -> false
                    | _ -> solveExprForDefines expr defineCombination

                let nextCodePath =
                    match currentCodePath with
                    | ConditionalCodeTree.TopLevel ->
                        ConditionalCodeTree.Nested
                            { IsActive = isActiveCode
                              Parent = ConditionalCodeTree.TopLevel
                              IfCondition = expr
                              StartRange = sourceToken.Range
                              Else = None }

                    | ConditionalCodeTree.Nested { IsActive = true } ->
                        ConditionalCodeTree.Nested
                            { IsActive = isActiveCode
                              IfCondition = expr
                              StartRange = sourceToken.Range
                              Else = None
                              Parent = currentCodePath }
                    | ConditionalCodeTree.Nested ({ IsActive = false } as current) ->
                        ConditionalCodeTree.NestedAndDead(current, 1)
                    | ConditionalCodeTree.NestedAndDead (current, endIfLevel) ->
                        ConditionalCodeTree.NestedAndDead(current, endIfLevel + 1)

                { acc with
                    CollectorState = NotCollecting sourceToken
                    CodePath = nextCodePath }

            // Visiting other branch of conditional code
            | TriviaCollectorState.NotCollecting _,
              ConditionalCodeTree.Nested node,
              SourceToken.Hash (ConditionalDirectiveTrivia.Else _) ->
                let codePath =
                    // either we just walked over some dead code of we didn't
                    // swap the code path
                    ConditionalCodeTree.Nested
                        { node with
                            Else = Some sourceToken.Range
                            IsActive = not node.IsActive }

                { acc with
                    CollectorState = NotCollecting sourceToken
                    CodePath = codePath }

            // capture #else in dead code
            | TriviaCollectorState.NotCollecting _,
              ConditionalCodeTree.NestedAndDead (node, endIfLevel),
              SourceToken.Hash (ConditionalDirectiveTrivia.Else _) ->
                if endIfLevel = 0 then
                    { acc with
                        CollectorState = NotCollecting sourceToken
                        CodePath = ConditionalCodeTree.Nested({ node with IsActive = not node.IsActive }) }
                else
                    // #else as part of nested and dead code
                    { acc with
                        CollectorState = NotCollecting sourceToken
                        CodePath = ConditionalCodeTree.NestedAndDead(node, endIfLevel) }

            // Ending some branch in dead code
            | TriviaCollectorState.NotCollecting _,
              ConditionalCodeTree.NestedAndDead (node, endIfLevel),
              SourceToken.Hash (ConditionalDirectiveTrivia.EndIf _) ->
                if endIfLevel = 0 then
                    { acc with
                        CollectorState = NotCollecting sourceToken
                        CodePath = node.Parent }
                else
                    { acc with
                        CollectorState = NotCollecting sourceToken
                        CodePath = ConditionalCodeTree.NestedAndDead(node, endIfLevel - 1) }

            // Ignoring dead code
            | TriviaCollectorState.NotCollecting _, ConditionalCodeTree.Nested { IsActive = false }, _
            | TriviaCollectorState.NotCollecting _, ConditionalCodeTree.NestedAndDead _, _ -> //
                acc
            | _, _, SourceToken.Token (token, range) ->
                // Normal trivia collection code flow
                match acc.CollectorState, token with
                | NotCollecting lastTokenAndRange, Newline range _ ->
                    // TODO: capture as one newline
                    let trivia =
                        { Item = Newline
                          Range = Range.mkRange range.FileName range.Start range.Start }

                    triviaCollection.Add trivia
                    { acc with CollectorState = NotCollecting lastTokenAndRange }

                | NotCollecting lastTokenAndRange, WHITESPACE _ ->
                    { acc with CollectorState = NotCollecting lastTokenAndRange }

                | NotCollecting lastTokenAndRange, NonTripleSlashOpeningCommentToken ->
                    let afterSourceCode = lastTokenAndRange.Range.EndLine = range.EndLine

                    let sb = StringBuilder(source.GetContentAt range)
                    { acc with CollectorState = CaptureLineComment(lastTokenAndRange, afterSourceCode, range, range, sb) }

                | NotCollecting lastTokenAndRange, BlockCommentOpeningCommentToken opener ->
                    let sb = StringBuilder(opener)
                    { acc with CollectorState = CaptureBlockComment(lastTokenAndRange, range, 0, false, sb) }

                | NotCollecting _, QuoteStringToken style ->
                    match style with
                    | LexerStringStyle.SingleQuote -> { acc with CollectorState = CaptureSingleQuoteString false }
                    | LexerStringStyle.TripleQuote -> { acc with CollectorState = CaptureTripleQuoteString false }
                    | LexerStringStyle.Verbatim -> { acc with CollectorState = CaptureVerbatimString false }

                | NotCollecting _, DecompiledOperatorToken ident ->
                    let trivia =
                        { Item = IdentOperatorAsWord ident
                          Range = range }

                    triviaCollection.Add trivia
                    { acc with CollectorState = NotCollecting sourceToken }

                | NotCollecting _, IDENT _ ->
                    let content = source.GetContentAt range

                    if content.StartsWith("``") && content.EndsWith("``") then
                        let trivia =
                            { Item = IdentBetweenTicks content
                              Range = range }

                        triviaCollection.Add trivia

                    { acc with CollectorState = NotCollecting sourceToken }

                // There is content in this token
                | NotCollecting _, _ -> { acc with CollectorState = NotCollecting sourceToken }

                | CaptureLineComment (lastTokenAndRange, afterSourceCode, startRange, _, sb), CommentContentToken range ->
                    if startRange.StartLine = range.EndLine then
                        let content = source.GetContentAt range

                        { acc with
                            CollectorState =
                                CaptureLineComment(
                                    lastTokenAndRange,
                                    afterSourceCode,
                                    startRange,
                                    range,
                                    sb.Append(content)
                                ) }
                    else
                        let trivia = createLineComment afterSourceCode startRange range sb

                        triviaCollection.Add trivia
                        { acc with CollectorState = NotCollecting lastTokenAndRange }

                | CaptureLineComment (lastTokenAndRange, afterSourceCode, startRange, endRange, sb), _ ->
                    let trivia = createLineComment afterSourceCode startRange endRange sb

                    triviaCollection.Add trivia
                    { acc with CollectorState = NotCollecting lastTokenAndRange }

                | CaptureBlockComment (lastTokenAndRange, startRange, level, _, sb), BlockCommentOpeningCommentToken bc ->
                    { acc with
                        CollectorState =
                            CaptureBlockComment(lastTokenAndRange, startRange, level + 1, false, sb.Append(bc)) }

                | CaptureBlockComment (lastTokenAndRange, startRange, level, _, sb), STAR ->
                    { acc with
                        CollectorState = CaptureBlockComment(lastTokenAndRange, startRange, level, true, sb.Append("*")) }

                | CaptureBlockComment (lastTokenAndRange, startRange, level, previousTokenWasStar, sb), RPAREN_IS_HERE ->
                    if previousTokenWasStar && level = 0 then
                        // End of block comment *)
                        let content = sb.Append(")").ToString()

                        let trivia =
                            { Item = Comment(BlockComment(content, false, false))
                              Range = Range.unionRanges startRange range }

                        triviaCollection.Add trivia
                        { acc with CollectorState = NotCollecting lastTokenAndRange }
                    elif previousTokenWasStar then
                        { acc with
                            CollectorState =
                                CaptureBlockComment(lastTokenAndRange, startRange, level - 1, true, sb.Append(")")) }
                    else
                        { acc with
                            CollectorState =
                                CaptureBlockComment(lastTokenAndRange, startRange, level, true, sb.Append(")")) }

                | CaptureBlockComment (lastTokenAndRange, startRange, level, previousTokenWasStar, sb), _ ->
                    let content = source.GetContentAt range

                    { acc with
                        CollectorState =
                            CaptureBlockComment(
                                lastTokenAndRange,
                                startRange,
                                level,
                                previousTokenWasStar,
                                sb.Append(content)
                            ) }

                // backslash before non back slash
                | CaptureSingleQuoteString false, LEX_FAILURE "Unexpected character '\\'" ->
                    { acc with CollectorState = CaptureSingleQuoteString true }

                // backslash before non back slash in triple quote
                | CaptureTripleQuoteString false, LEX_FAILURE "Unexpected character '\\'" ->
                    { acc with CollectorState = CaptureTripleQuoteString true }

                // backslash before quote
                | CaptureSingleQuoteString true, QuoteStringToken _ ->
                    { acc with CollectorState = CaptureSingleQuoteString false }

                // backslash before triple quote
                | CaptureTripleQuoteString true, QuoteStringToken LexerStringStyle.TripleQuote ->
                    { acc with CollectorState = CaptureTripleQuoteString false }

                // End of single quote string
                | CaptureSingleQuoteString _, QuoteStringToken LexerStringStyle.SingleQuote ->
                    { acc with CollectorState = NotCollecting sourceToken }

                // End of triple quoted string
                | CaptureTripleQuoteString _, QuoteStringToken LexerStringStyle.TripleQuote ->
                    { acc with CollectorState = NotCollecting sourceToken }

                // End of verbatim string
                | CaptureVerbatimString _, QuoteStringToken LexerStringStyle.SingleQuote ->
                    { acc with CollectorState = NotCollecting sourceToken }

                // capture normal token as part of single quote string
                | CaptureSingleQuoteString _, _token -> { acc with CollectorState = CaptureSingleQuoteString false }

                // capture normal token as part of triple quote string
                | CaptureTripleQuoteString _, _token -> { acc with CollectorState = CaptureTripleQuoteString false }

                | CaptureVerbatimString _, _token -> { acc with CollectorState = CaptureVerbatimString false }

                | _ -> acc
            | _ -> acc)
        initialState
    |> ignore<TriviaBuilderState>

    triviaCollection.Close()
