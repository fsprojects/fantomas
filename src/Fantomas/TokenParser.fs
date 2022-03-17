module internal Fantomas.TokenParser

open System
open System.Text
open Microsoft.FSharp.Core.CompilerServices
open FSharp.Compiler.Text
open FSharp.Compiler.Parser
open FSharp.Compiler.ParseHelpers
open FSharp.Compiler.Syntax.PrettyNaming
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
    | NotCollecting of lastTokenAndRange: TokenAndRange
    | CaptureLineComment of
        lastTokenAndRange: TokenAndRange *
        afterSourceCode: bool *
        startRange: range *
        endRange: range *
        collector: StringBuilder
    | CaptureBlockComment of
        lastTokenAndRange: TokenAndRange *
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
    | NestedAndDead of deadNode: ConditionalCodeNode * hashLines: string list * endIfLevel: int

and ConditionalCodeNode =
    { IsActive: bool
      Parent: ConditionalCodeTree
      IfCondition: string
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

let private getTokensFromSource (source: ISourceText) : TokenWithRangeList =
    let mutable tokenCollector = ListCollector<token * range>()
    let mutable currentLine = 0

    let onToken (token: token) (range: FSharp.Compiler.Text.Range) =
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
            | WHITESPACE _ -> ()
            | _ -> currentLine <- range.EndLine

            tokenCollector.Add(token, range)

    lex onToken source
    tokenCollector.Close()

let private parseHashLine (content: string) : string list * string list =
    let content =
        content
            .Trim()
            .Substring(3) // strip #if
            .Split([| "//" |], StringSplitOptions.RemoveEmptyEntries).[0] // strip any comments

    let source = SourceText.ofString content
    let tokens = getTokensFromSource source

    let content =
        tokens
        |> List.choose (fun (t, _r) ->
            match t with
            | IDENT s -> Some s
            | BAR_BAR -> Some "||"
            | AMP_AMP
            | ADJACENT_PREFIX_OP "&&" -> Some "&&"
            | LPAREN -> Some "("
            | RPAREN
            | RPAREN_IS_HERE -> Some ")"
            | PREFIX_OP _ -> Some "!"
            | TRUE -> Some "true"
            | FALSE -> Some "false"
            | _ -> None)

    let words =
        tokens
        |> List.choose (fun (t, _r) ->
            match t with
            | IDENT s -> Some s
            | _ -> None)
        |> List.distinct

    content, words

let private getDefinesFromTokens (tokens: TokenWithRangeList) : HashLine list =
    let mutable hashTokenContent = ListCollector<HashLine>()
    let initialState: TriviaCollectorState = NotCollecting(OBLOCKBEGIN, Range.Zero)

    (initialState, tokens)
    ||> List.fold (fun acc tokenAndRange ->
        let token, range = tokenAndRange

        match acc, token with
        | NotCollecting lastTokenAndRange, HASH_IF (_, content, _) ->
            let hashIf = content |> parseHashLine |> HashLine.If

            hashTokenContent.Add(hashIf)
            NotCollecting lastTokenAndRange

        | NotCollecting lastTokenAndRange, HASH_ELSE _ ->
            hashTokenContent.Add HashLine.Else
            NotCollecting lastTokenAndRange

        | NotCollecting lastTokenAndRange, HASH_ENDIF _ ->
            hashTokenContent.Add HashLine.EndIf
            NotCollecting lastTokenAndRange

        | NotCollecting lastTokenAndRange, NonTripleSlashOpeningCommentToken ->
            CaptureLineComment(lastTokenAndRange, false, range, range, StringBuilder())

        | NotCollecting lastTokenAndRange, BlockCommentOpeningCommentToken _ ->
            CaptureBlockComment(lastTokenAndRange, range, 0, false, StringBuilder())

        | NotCollecting _, QuoteStringToken style ->
            match style with
            | LexerStringStyle.SingleQuote -> CaptureSingleQuoteString false
            | LexerStringStyle.TripleQuote -> CaptureTripleQuoteString false
            | LexerStringStyle.Verbatim -> CaptureVerbatimString false

        // There is content in this token
        | NotCollecting _, _ -> NotCollecting tokenAndRange

        | CaptureLineComment (lastTokenAndRange, afterSourceCode, startRange, _, sb), CommentContentToken range ->
            if startRange.StartLine = range.EndLine then
                CaptureLineComment(lastTokenAndRange, afterSourceCode, startRange, range, sb)
            else
                NotCollecting lastTokenAndRange

        | CaptureLineComment (lastTokenAndRange, _, _, _, _), _ -> NotCollecting lastTokenAndRange

        | CaptureBlockComment (lastTokenAndRange, startRange, level, _, sb), BlockCommentOpeningCommentToken _ ->
            CaptureBlockComment(lastTokenAndRange, startRange, level + 1, false, sb)

        | CaptureBlockComment (lastTokenAndRange, startRange, level, _, sb), STAR ->
            CaptureBlockComment(lastTokenAndRange, startRange, level, true, sb)

        | CaptureBlockComment (lastTokenAndRange, startRange, level, previousTokenWasStar, sb), RPAREN_IS_HERE ->
            if previousTokenWasStar && level = 0 then
                NotCollecting lastTokenAndRange
            elif previousTokenWasStar then
                CaptureBlockComment(lastTokenAndRange, startRange, level - 1, false, sb)
            else
                CaptureBlockComment(lastTokenAndRange, startRange, level, false, sb)

        | CaptureBlockComment (lastTokenAndRange, startRange, level, previousTokenWasStar, sb), _ ->
            CaptureBlockComment(lastTokenAndRange, startRange, level, previousTokenWasStar, sb)

        // backslash before non back slash
        | CaptureSingleQuoteString false, LEX_FAILURE "Unexpected character '\\'" -> CaptureSingleQuoteString true

        // backslash before non back slash in triple quote
        | CaptureTripleQuoteString false, LEX_FAILURE "Unexpected character '\\'" -> CaptureTripleQuoteString true

        // backslash before quote
        | CaptureSingleQuoteString true, QuoteStringToken _ -> CaptureSingleQuoteString false

        // backslash before triple quote
        | CaptureTripleQuoteString true, QuoteStringToken LexerStringStyle.TripleQuote -> CaptureTripleQuoteString false

        // End of single quote string
        | CaptureSingleQuoteString _, QuoteStringToken LexerStringStyle.SingleQuote -> NotCollecting tokenAndRange

        // End of triple quoted string
        | CaptureTripleQuoteString _, QuoteStringToken LexerStringStyle.TripleQuote -> NotCollecting tokenAndRange

        // End of verbatim string
        | CaptureVerbatimString _, QuoteStringToken LexerStringStyle.SingleQuote -> NotCollecting tokenAndRange

        // capture normal token as part of single quote string
        | CaptureSingleQuoteString _, _token -> CaptureSingleQuoteString false

        // capture normal token as part of triple quote string
        | CaptureTripleQuoteString _, _token -> CaptureTripleQuoteString false

        | CaptureVerbatimString _, _token -> CaptureVerbatimString false

        | _ -> acc)
    |> ignore<TriviaCollectorState>

    hashTokenContent.Close()

let private getIndividualDefine (hashLines: HashLine list) : string list list =
    hashLines
    |> List.collect (function
        | HashLine.If (words = words) -> words
        | _ -> [])
    |> List.distinct
    |> List.map List.singleton

let private getDefineExprs (hashLines: HashLine list) =
    let result =
        (([], []), hashLines)
        ||> List.fold (fun (contextExprs, exprAcc) hashLine ->
            let contextExpr e =
                e :: contextExprs
                |> List.reduce (fun x y -> BoolExpr.And(x, y))

            match hashLine with
            | HashLine.If (content = content) ->
                BoolExprParser.parse content
                |> Option.map (fun e -> e :: contextExprs, contextExpr e :: exprAcc)
                |> Option.defaultValue (contextExprs, exprAcc)
            | HashLine.Else ->
                contextExprs,
                BoolExpr.Not(
                    contextExprs
                    |> List.reduce (fun x y -> BoolExpr.And(x, y))
                )
                :: exprAcc
            | HashLine.EndIf -> List.tail contextExprs, exprAcc)
        |> snd
        |> List.rev

    result

let private getOptimizedDefinesSets (hashTokens: HashLine list) =
    let maxSteps = FormatConfig.satSolveMaxStepsMaxSteps

    match getDefineExprs hashTokens
          |> BoolExpr.mergeBoolExprs maxSteps
          |> List.map snd
        with
    | [] -> [ [] ]
    | xs -> xs

let getDefineCombination (source: ISourceText) : TokenWithRangeList * DefineCombination list =
    let tokens = getTokensFromSource source

    let hashTokens: HashLine list = getDefinesFromTokens tokens

    let defineCombinations =
        [ yield [] // always include the empty defines set
          yield! getOptimizedDefinesSets hashTokens
          yield! getIndividualDefine hashTokens ]
        |> List.distinct

    tokens, defineCombinations

let getTriviaFromTokens
    (source: ISourceText)
    (tokens: TokenWithRangeList)
    (defineCombination: DefineCombination)
    : Trivia list =
    let mutable triviaCollection = ListCollector<Trivia>()

    // tokens |> List.iter (printfn "%A")

    let createDeadCodeDirective
        (startRange: range)
        (startContent: string)
        (endRange: range)
        (endContent: string)
        : Trivia =
        let range = Range.mkRange startRange.FileName startRange.Start endRange.End

        let directiveContent = String.Concat(startContent, "\n", endContent)

        { Item = Directive directiveContent
          Range = range }

    let initialState: TriviaBuilderState =
        { CodePath = ConditionalCodeTree.TopLevel
          CollectorState = NotCollecting(OBLOCKBEGIN, Range.Zero) }

    tokens
    |> List.skipWhile (function
        | WHITESPACE _, _ -> true
        | _ -> false)
    |> List.fold
        (fun (acc: TriviaBuilderState) tokenAndRange ->
            let token, range = tokenAndRange

            match acc.CollectorState, acc.CodePath, token with
            // New level of conditional code
            | TriviaCollectorState.NotCollecting _, currentCodePath, HASH_IF (_, content, _) ->
                let content = content.TrimStart()

                let isActiveCode =
                    match currentCodePath with
                    // If we are currently inside dead code, the new code can never be active
                    | ConditionalCodeTree.Nested { IsActive = false } -> false
                    | _ ->
                        let expr =
                            parseHashLine content
                            |> fst
                            |> BoolExprParser.parse

                        match expr with
                        | None -> failwithf $"Could not solve expression %s{content} for defines %A{defineCombination}"
                        | Some expr -> BoolExpr.solveExprForDefines expr defineCombination

                let nextCodePath =
                    match currentCodePath with
                    | ConditionalCodeTree.TopLevel ->
                        ConditionalCodeTree.Nested
                            { IsActive = isActiveCode
                              Parent = ConditionalCodeTree.TopLevel
                              IfCondition = content
                              StartRange = range
                              Else = None }

                    | ConditionalCodeTree.Nested { IsActive = true } ->
                        ConditionalCodeTree.Nested
                            { IsActive = isActiveCode
                              IfCondition = content
                              StartRange = range
                              Else = None
                              Parent = currentCodePath }
                    | ConditionalCodeTree.Nested ({ IsActive = false } as current) ->
                        ConditionalCodeTree.NestedAndDead(current, [ content ], 1)
                    | ConditionalCodeTree.NestedAndDead (current, hashLines, endIfLevel) ->
                        ConditionalCodeTree.NestedAndDead(current, content :: hashLines, endIfLevel + 1)

                // Only add a directive trivia when the following code is active
                // dead code is capture after the `#else` or `#endif`
                if isActiveCode then
                    let hashIf =
                        { Item = Directive content
                          Range = range }

                    triviaCollection.Add hashIf

                { acc with
                    CollectorState = NotCollecting tokenAndRange
                    CodePath = nextCodePath }

            // Visiting other branch of conditional code
            | TriviaCollectorState.NotCollecting _, ConditionalCodeTree.Nested node, HASH_ELSE _ ->
                if not node.IsActive then
                    // Add dead code, next branch will be active
                    let trivia = createDeadCodeDirective node.StartRange node.IfCondition range "#else"
                    triviaCollection.Add trivia

                let codePath =
                    // either we just walked over some dead code of we didn't
                    // swap the code path
                    ConditionalCodeTree.Nested
                        { node with
                            Else = Some range
                            IsActive = not node.IsActive }

                { acc with
                    CollectorState = NotCollecting tokenAndRange
                    CodePath = codePath }

            // capture #else in dead code
            | TriviaCollectorState.NotCollecting _,
              ConditionalCodeTree.NestedAndDead (node, hashLines, endIfLevel),
              HASH_ELSE _ ->
                if endIfLevel = 0 then
                    // #else closed the `node`
                    let content =
                        [ yield node.IfCondition
                          yield! (List.rev hashLines)
                          yield "#else" ]
                        |> String.concat "\n"

                    let range = Range.mkRange node.StartRange.FileName node.StartRange.Start range.End

                    let trivia =
                        { Item = Directive content
                          Range = range }

                    triviaCollection.Add trivia

                    { acc with
                        CollectorState = NotCollecting tokenAndRange
                        CodePath = ConditionalCodeTree.Nested({ node with IsActive = not node.IsActive }) }
                else
                    // #else as part of nested and dead code
                    { acc with
                        CollectorState = NotCollecting tokenAndRange
                        CodePath = ConditionalCodeTree.NestedAndDead(node, "#else" :: hashLines, endIfLevel) }

            // Ending current branch of conditional code
            | TriviaCollectorState.NotCollecting _, ConditionalCodeTree.Nested node, HASH_ENDIF _ ->
                if not node.IsActive then
                    // Add dead code
                    let startRange, startContent =
                        match node.Else with
                        | None -> node.StartRange, node.IfCondition
                        | Some elseRange -> elseRange, "#else"

                    let trivia = createDeadCodeDirective startRange startContent range "#endif"
                    triviaCollection.Add trivia
                else
                    let hashEndIf =
                        { Item = Directive "#endif"
                          Range = range }

                    triviaCollection.Add hashEndIf

                { acc with
                    CollectorState = NotCollecting tokenAndRange
                    // pop the current code path
                    CodePath = node.Parent }

            // Ending some branch in dead code
            | TriviaCollectorState.NotCollecting _,
              ConditionalCodeTree.NestedAndDead (node, hashLines, endIfLevel),
              HASH_ENDIF _ ->
                if endIfLevel = 0 then
                    // Close of the original dead code
                    let startRange, startContent =
                        match node.Else with
                        | Some elseRange when not node.IsActive -> elseRange, "#else"
                        | _ -> node.StartRange, node.IfCondition

                    let content =
                        [ yield startContent
                          yield! (List.rev hashLines)
                          yield "#endif" ]
                        |> String.concat "\n"

                    let range = Range.mkRange node.StartRange.FileName startRange.Start range.End

                    let trivia =
                        { Item = Directive content
                          Range = range }

                    triviaCollection.Add trivia

                    { acc with
                        CollectorState = NotCollecting tokenAndRange
                        CodePath = node.Parent }
                else
                    { acc with
                        CollectorState = NotCollecting tokenAndRange
                        CodePath = ConditionalCodeTree.NestedAndDead(node, "#endif" :: hashLines, endIfLevel - 1) }

            // Respect string content in dead code, this should not yield any hash lines
            | TriviaCollectorState.NotCollecting _,
              ConditionalCodeTree.Nested { IsActive = false },
              QuoteStringToken LexerStringStyle.TripleQuote
            | TriviaCollectorState.NotCollecting _,
              ConditionalCodeTree.NestedAndDead _,
              QuoteStringToken LexerStringStyle.TripleQuote ->
                { acc with CollectorState = CaptureTripleQuoteString false }

            // Close string content in dead code
            | TriviaCollectorState.CaptureTripleQuoteString _,
              ConditionalCodeTree.Nested { IsActive = false },
              QuoteStringToken LexerStringStyle.TripleQuote
            | TriviaCollectorState.CaptureTripleQuoteString _,
              ConditionalCodeTree.NestedAndDead _,
              QuoteStringToken LexerStringStyle.TripleQuote ->
                { acc with CollectorState = TriviaCollectorState.NotCollecting tokenAndRange }

            // Ignoring dead code
            | TriviaCollectorState.NotCollecting _, ConditionalCodeTree.Nested { IsActive = false }, _
            | TriviaCollectorState.NotCollecting _, ConditionalCodeTree.NestedAndDead _, _ -> //
                acc
            | _ ->
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
                    let afterSourceCode = (snd lastTokenAndRange).EndLine = range.EndLine

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
                    { acc with CollectorState = NotCollecting tokenAndRange }

                | NotCollecting _, IDENT _ ->
                    let content = source.GetContentAt range

                    if content.StartsWith("``") && content.EndsWith("``") then
                        let trivia =
                            { Item = IdentBetweenTicks content
                              Range = range }

                        triviaCollection.Add trivia

                    { acc with CollectorState = NotCollecting tokenAndRange }

                // There is content in this token
                | NotCollecting _, _ -> { acc with CollectorState = NotCollecting tokenAndRange }

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
                    { acc with CollectorState = NotCollecting tokenAndRange }

                // End of triple quoted string
                | CaptureTripleQuoteString _, QuoteStringToken LexerStringStyle.TripleQuote ->
                    { acc with CollectorState = NotCollecting tokenAndRange }

                // End of verbatim string
                | CaptureVerbatimString _, QuoteStringToken LexerStringStyle.SingleQuote ->
                    { acc with CollectorState = NotCollecting tokenAndRange }

                // capture normal token as part of single quote string
                | CaptureSingleQuoteString _, _token -> { acc with CollectorState = CaptureSingleQuoteString false }

                // capture normal token as part of triple quote string
                | CaptureTripleQuoteString _, _token -> { acc with CollectorState = CaptureTripleQuoteString false }

                | CaptureVerbatimString _, _token -> { acc with CollectorState = CaptureVerbatimString false }

                | _ -> acc)
        initialState
    |> ignore<TriviaBuilderState>

    triviaCollection.Close()
