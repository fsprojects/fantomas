module internal Fantomas.TokenParser

open System
open System.Text
open Fantomas
open Fantomas.TokenParserBoolExpr
open Fantomas.TriviaTypes
open FSharp.Compiler.SourceCodeServices

let private whiteSpaceTag = 4
let private lineCommentTag = 8
let private greaterTag = 160

// workaround for cases where tokenizer dont output "delayed" part of operator after ">."
// See https://github.com/fsharp/FSharp.Compiler.Service/issues/874
let private isTokenAfterGreater token (greaterToken: Token) =
    let greaterToken = greaterToken.TokenInfo

    greaterToken.Tag = greaterTag
    && token.Tag <> greaterTag
    && greaterToken.RightColumn <> (token.LeftColumn + 1)

let private getTokenText (sourceCodeLines: string list) line (token: FSharpTokenInfo) =
    sourceCodeLines.[line - 1]
        .Substring(token.LeftColumn, token.RightColumn - token.LeftColumn + 1)
    |> String.normalizeNewLine

/// Tokenize a single line of F# code
let rec private tokenizeLine (tokenizer: FSharpLineTokenizer) sourceCodeLines state lineNumber tokens =
    match tokenizer.ScanToken(state), List.tryHead tokens with
    | (Some tok, state), Some greaterToken when (isTokenAfterGreater tok greaterToken) ->
        let extraTokenInfo =
            { tok with
                  TokenName = "DELAYED"
                  LeftColumn = greaterToken.TokenInfo.RightColumn + 1
                  Tag = -1
                  CharClass = FSharpTokenCharKind.Operator
                  RightColumn = tok.LeftColumn - 1 }

        let extraToken =
            { TokenInfo = extraTokenInfo
              LineNumber = lineNumber
              Content = getTokenText sourceCodeLines lineNumber extraTokenInfo }

        let token =
            { TokenInfo = tok
              LineNumber = lineNumber
              Content = getTokenText sourceCodeLines lineNumber tok }

        tokenizeLine tokenizer sourceCodeLines state lineNumber (token :: extraToken :: tokens)

    | (Some tok, state), _ ->
        let token : Token =
            { TokenInfo = tok
              LineNumber = lineNumber
              Content = getTokenText sourceCodeLines lineNumber tok }
        // Tokenize the rest, in the new state
        tokenizeLine tokenizer sourceCodeLines state lineNumber (token :: tokens)

    | (None, state), _ -> state, tokens

let private tokenizeLines (sourceTokenizer: FSharpSourceTokenizer) allLines state =
    allLines
    |> List.mapi (fun index line -> line, (index + 1)) // line number is needed in tokenizeLine
    |> List.fold
        (fun (state, tokens) (line, lineNumber) ->
            let tokenizer =
                sourceTokenizer.CreateLineTokenizer(line)

            let nextState, tokensOfLine =
                tokenizeLine tokenizer allLines state lineNumber []

            let allTokens =
                List.append tokens (List.rev tokensOfLine) // tokens of line are add in reversed order

            (nextState, allTokens))
        (state, []) // empty tokens to start with
    |> snd // ignore the state

let private createHashToken lineNumber content offset =
    let (left, right) = offset, String.length content + offset

    { LineNumber = lineNumber
      Content = content
      TokenInfo =
          { TokenName = "HASH_IF"
            LeftColumn = left
            RightColumn = right
            ColorClass = FSharpTokenColorKind.PreprocessorKeyword
            CharClass = FSharpTokenCharKind.WhiteSpace
            FSharpTokenTriggerClass = FSharpTokenTriggerClass.None
            Tag = 0
            FullMatchedLength = String.length content } }

type SourceCodeState =
    | Normal
    | InsideString
    | InsideTripleQuoteString of startIndex: int
    | InsideMultilineComment

type SourceCodeParserState =
    { State: SourceCodeState
      NewlineIndexes: int list
      Defines: (Token list) list }

let rec private getTokenizedHashes (sourceCode: string) : Token list =
    let hasNoHashDirectiveStart (source: string) = not (source.Contains("#if"))

    if hasNoHashDirectiveStart sourceCode then
        []
    else
        let equalsChar c v = if c = v then Some() else None
        let differsFromChar c v = if c <> v then Some() else None

        let (|DoubleQuoteChar|_|) = equalsChar '"'

        let (|TripleQuoteChars|_|) v =
            match v with
            | DoubleQuoteChar, DoubleQuoteChar, DoubleQuoteChar -> Some()
            | _ -> None

        let (|OpenParenChar|_|) = equalsChar '('
        let (|AsteriskChar|_|) = equalsChar '*'
        let (|NoCloseParenChar|_|) = differsFromChar ')'
        let (|NewlineChar|_|) = equalsChar '\n'
        let (|HashChar|_|) = equalsChar '#'
        let (|BackSlashChar|_|) = equalsChar '\\'
        let (|NoBackSlashChar|_|) = differsFromChar '\\'
        let (|CloseParenChar|_|) = equalsChar ')'
        let isSpace = (=) ' '

        let processLine (hashContent: string) (lineContent: string) (lineNumber: int) (offset: int) : Token list =
            let hashContentLength = String.length hashContent

            let tokens =
                let defineExpressionWithHash = lineContent.Substring(hashContentLength)

                if String.isNotNullOrEmpty defineExpressionWithHash then
                    tokenize [] [] defineExpressionWithHash
                else
                    []

            tokens
            |> List.map
                (fun t ->
                    let info =
                        { t.TokenInfo with
                              LeftColumn = t.TokenInfo.LeftColumn + hashContentLength
                              RightColumn = t.TokenInfo.RightColumn + hashContentLength }

                    { t with
                          LineNumber = lineNumber
                          TokenInfo = info })
            |> fun rest ->
                (createHashToken lineNumber hashContent offset)
                :: rest

        let sourceLength = String.length sourceCode
        // stop scanning the source code three characters before the end
        // three because of how triple quote string are opened and closed
        // the scan looks three characters ahead (zero, plusOne, plusTwo)
        // and sometimes also two characters behind (minusTwo, minusOne)
        // In theory there is will also never be any new hash detect inside the last three characters
        let lastIndex = sourceLength - 3
        // check if the current # char is part of an define expression
        // if so add to defines
        let captureHashDefine (state: SourceCodeParserState) idx =
            let lastNewlineIdx =
                Seq.tryHead state.NewlineIndexes
                |> Option.defaultValue -1

            let leadingCharactersBeforeLastNewlineAreSpaces =
                let take = Math.Max(idx - lastNewlineIdx - 1, 0)

                sourceCode
                |> Seq.skip (lastNewlineIdx + 1)
                |> Seq.take take
                |> Seq.forall isSpace

            if leadingCharactersBeforeLastNewlineAreSpaces then
                let skip =
                    if lastNewlineIdx = -1 then
                        0
                    else
                        lastNewlineIdx + 1 // zero when the source starts with an #

                let currentLine =
                    sourceCode
                    |> Seq.skip skip
                    |> Seq.takeWhile
                        (function
                        | NewlineChar -> false
                        | _ -> true)
                    |> Seq.toArray
                    |> fun chars -> new string (chars)

                let trimmed = currentLine.TrimStart()

                let offset =
                    (String.length currentLine - String.length trimmed)

                let lineNumber = List.length state.NewlineIndexes + 1 // line numbers are 1 based.

                if trimmed.StartsWith("#if") then
                    { state with
                          Defines =
                              (processLine "#if" trimmed lineNumber offset)
                              :: state.Defines }
                elif trimmed.StartsWith("#elseif") then
                    { state with
                          Defines =
                              (processLine "#elseif" trimmed lineNumber offset)
                              :: state.Defines }
                elif trimmed.StartsWith("#else") then
                    { state with
                          Defines =
                              (processLine "#else" trimmed lineNumber offset)
                              :: state.Defines }
                elif trimmed.StartsWith("#endif") then
                    { state with
                          Defines =
                              (processLine "#endif" trimmed lineNumber offset)
                              :: state.Defines }
                else
                    state
            else
                state

        let initialState =
            { State = Normal
              NewlineIndexes = []
              Defines = [] }

        [ 0 .. lastIndex ]
        |> List.fold
            (fun acc idx ->
                let zero = sourceCode.[idx]
                let plusOne = sourceCode.[idx + 1]
                let plusTwo = sourceCode.[idx + 2]

                if idx < 2 then
                    match acc.State, (zero, plusOne, plusTwo) with
                    | Normal, TripleQuoteChars ->
                        { acc with
                              State = InsideTripleQuoteString(idx) }
                    | Normal, (DoubleQuoteChar, _, _) -> { acc with State = InsideString }
                    | Normal, (OpenParenChar, AsteriskChar, NoCloseParenChar) when (sourceLength > 3) ->
                        { acc with
                              State = InsideMultilineComment }
                    | Normal, (NewlineChar, _, _) ->
                        { acc with
                              NewlineIndexes = idx :: acc.NewlineIndexes }
                    | Normal, (HashChar, _, _) -> captureHashDefine acc idx
                    | _ -> acc

                elif idx < lastIndex then
                    let minusTwo = sourceCode.[idx - 2]
                    let minusOne = sourceCode.[idx - 1]

                    match acc.State, (zero, plusOne, plusTwo) with
                    | Normal, TripleQuoteChars ->
                        { acc with
                              State = InsideTripleQuoteString idx }
                    | Normal, (DoubleQuoteChar, _, _) -> { acc with State = InsideString }
                    | Normal, (OpenParenChar, AsteriskChar, NoCloseParenChar) ->
                        { acc with
                              State = InsideMultilineComment }
                    | Normal, (NewlineChar, _, _) ->
                        { acc with
                              NewlineIndexes = idx :: acc.NewlineIndexes }
                    | Normal, (HashChar, _, _) -> captureHashDefine acc idx
                    | InsideString, (NewlineChar, _, _) ->
                        { acc with
                              NewlineIndexes = idx :: acc.NewlineIndexes }
                    | InsideString, (DoubleQuoteChar, _, _) ->
                        let minusThree = sourceCode.[idx - 3]

                        match minusOne, minusTwo, minusThree with
                        | BackSlashChar, NoBackSlashChar, _
                        | BackSlashChar, BackSlashChar, BackSlashChar -> acc
                        | _ -> { acc with State = Normal }
                    | InsideString, (DoubleQuoteChar, _, _) -> { acc with State = Normal }
                    | InsideTripleQuoteString _, (NewlineChar, _, _) ->
                        { acc with
                              NewlineIndexes = idx :: acc.NewlineIndexes }
                    | InsideTripleQuoteString startIndex, _ when (startIndex + 2 < idx) ->
                        match (minusTwo, minusOne, zero) with
                        | TripleQuoteChars when ((startIndex - 1) > 0) ->
                            let minusThree = sourceCode.[idx - 3]
                            // check if there is no backslash before the first `"` of `"""`
                            // so no `\"""` characters
                            match minusThree with
                            | NoBackSlashChar -> { acc with State = Normal }
                            | _ -> acc
                        | _ -> acc
                    | InsideMultilineComment, (NewlineChar, _, _) ->
                        { acc with
                              NewlineIndexes = idx :: acc.NewlineIndexes }
                    | InsideMultilineComment, (CloseParenChar, _, _) ->
                        match minusOne with
                        | AsteriskChar -> { acc with State = Normal }
                        | _ -> acc
                    | _ -> acc

                else
                    acc)
            initialState
        |> fun state -> state.Defines |> List.rev |> List.collect id

and tokenize defines (hashTokens: Token list) (content: string) : Token list =
    let sourceTokenizer =
        FSharpSourceTokenizer(defines, Some "/tmp.fsx")

    let lines =
        String.normalizeThenSplitNewLine content
        |> Array.toList

    let tokens =
        tokenizeLines sourceTokenizer lines FSharpTokenizerLexState.Initial
        |> List.filter (fun t -> t.TokenInfo.TokenName <> "INACTIVECODE")

    let existingLines =
        tokens
        |> List.map (fun t -> t.LineNumber)
        |> List.distinct

    let combined =
        if List.isNotEmpty hashTokens then
            let filteredHashes =
                hashTokens
                |> List.filter (fun t -> not (List.contains t.LineNumber existingLines))
            // filter hashes that are present in source code parsed by the Tokenizer.
            tokens @ filteredHashes
            |> List.sortBy (fun t -> t.LineNumber, t.TokenInfo.LeftColumn)
        else
            tokens

    combined

let getDefinesWords (tokens: Token list) =
    tokens
    |> List.filter (fun { TokenInfo = { TokenName = tn } } -> tn = "IDENT")
    |> List.map (fun t -> t.Content)
    |> List.distinct

let getDefineExprs (hashTokens: Token list) =
    let parseHashContent tokens =
        let allowedContent = set [ "||"; "&&"; "!"; "("; ")" ]

        tokens
        |> Seq.filter
            (fun t ->
                t.TokenInfo.TokenName = "IDENT"
                || Set.contains t.Content allowedContent)
        |> Seq.map (fun t -> t.Content)
        |> Seq.toList
        |> BoolExprParser.parse

    let tokensByLine =
        hashTokens
        |> List.groupBy (fun t -> t.LineNumber)
        |> List.sortBy fst

    let result =
        (([], []), tokensByLine)
        ||> List.fold
                (fun (contextExprs, exprAcc) (_, lineTokens) ->
                    let contextExpr e =
                        e :: contextExprs
                        |> List.reduce (fun x y -> BoolExpr.And(x, y))

                    let t =
                        lineTokens
                        |> Seq.tryFind (fun x -> x.TokenInfo.TokenName = "HASH_IF")

                    match t |> Option.map (fun x -> x.Content) with
                    | Some "#if" ->
                        parseHashContent lineTokens
                        |> Option.map (fun e -> e :: contextExprs, contextExpr e :: exprAcc)
                        |> Option.defaultValue (contextExprs, exprAcc)
                    | Some "#else" ->
                        contextExprs,
                        BoolExpr.Not(
                            contextExprs
                            |> List.reduce (fun x y -> BoolExpr.And(x, y))
                        )
                        :: exprAcc
                    | Some "#endif" -> List.tail contextExprs, exprAcc
                    | _ -> contextExprs, exprAcc)
        |> snd
        |> List.rev

    result

let internal getOptimizedDefinesSets (hashTokens: Token list) =
    let maxSteps = FormatConfig.satSolveMaxStepsMaxSteps

    match getDefineExprs hashTokens
          |> BoolExpr.mergeBoolExprs maxSteps
          |> List.map snd with
    | [] -> [ [] ]
    | xs -> xs

let getDefines sourceCode =
    let hashTokens = getTokenizedHashes sourceCode

    let defineCombinations =
        getOptimizedDefinesSets hashTokens
        @ (getDefinesWords hashTokens
           |> List.map List.singleton)
          @ [ [] ]
        |> List.distinct

    defineCombinations, hashTokens

let private getRangeBetween (mkRange: MkRange) startToken endToken =
    let l = startToken.TokenInfo.LeftColumn
    let r = endToken.TokenInfo.RightColumn
    mkRange (startToken.LineNumber, l) (endToken.LineNumber, (if l = r then r + 1 else r))

let private getRangeForSingleToken (mkRange: MkRange) token =
    let l = token.TokenInfo.LeftColumn
    let r = l + token.TokenInfo.FullMatchedLength
    mkRange (token.LineNumber, l) (token.LineNumber, r)

let private hasOnlySpacesAndLineCommentsOnLine lineNumber tokens =
    if List.isEmpty tokens then
        false
    else
        tokens
        |> List.filter (fun t -> t.LineNumber = lineNumber)
        |> List.forall
            (fun t ->
                t.TokenInfo.Tag = whiteSpaceTag
                || t.TokenInfo.Tag = lineCommentTag)

let private getContentFromTokens tokens =
    tokens
    |> List.map (fun t -> t.Content)
    |> String.concat String.Empty

let private keywordTrivia =
    [ "IF"
      "ELIF"
      "ELSE"
      "THEN"
      "OVERRIDE"
      "MEMBER"
      "DEFAULT"
      "ABSTRACT"
      "KEYWORD_STRING"
      "QMARK"
      "IN" ]

let private numberTrivia =
    [ "UINT8"
      "INT8"
      "UINT16"
      "INT16"
      "UINT32"
      "INT32"
      "UINT64"
      "INT64"
      "IEEE32"
      "DECIMAL"
      "IEEE64"
      "BIGNUM"
      "NATIVEINT"
      "UNATIVEINT" ]

let private isOperatorOrKeyword ({ TokenInfo = { CharClass = cc } }) =
    cc = FSharpTokenCharKind.Keyword
    || cc = FSharpTokenCharKind.Operator

let private onlyNumberRegex =
    System.Text.RegularExpressions.Regex(@"^\d+$")

let private isNumber ({ TokenInfo = tn; Content = content }) =
    tn.ColorClass = FSharpTokenColorKind.Number
    && List.contains tn.TokenName numberTrivia
    && not (onlyNumberRegex.IsMatch(content))

let private digitOrLetterCharRegex =
    System.Text.RegularExpressions.Regex(@"^'(\d|[a-zA-Z])'$")

let private (|CharToken|_|) token =
    if
        token.TokenInfo.TokenName = "CHAR"
        && not (digitOrLetterCharRegex.IsMatch(token.Content))
    then
        Some token
    else
        None

let private (|StringTextToken|_|) token =
    if token.TokenInfo.TokenName = "STRING_TEXT" then
        Some token
    else
        None

let private (|InterpStringEndOrPartToken|_|) token =
    if token.TokenInfo.TokenName = "INTERP_STRING_END"
       || token.TokenInfo.TokenName = "INTERP_STRING_PART" then
        Some token
    else
        None

let escapedCharacterRegex =
    System.Text.RegularExpressions.Regex("(\\\\(n|r|u|'|\\\"|\\\\))+")

let rec private (|EndOfInterpolatedString|_|) tokens =
    match tokens with
    | StringTextToken (stToken) :: InterpStringEndOrPartToken (endToken) :: rest -> Some([ stToken ], endToken, rest)
    | StringTextToken (stToken) :: EndOfInterpolatedString (stringTokens, endToken, rest) ->
        Some(stToken :: stringTokens, endToken, rest)
    | _ -> None

let private (|StringText|_|) tokens =
    match tokens with
    | StringTextToken head :: rest ->
        let stringTokens =
            rest
            |> List.takeWhile (fun { TokenInfo = { TokenName = tn } } -> tn = "STRING_TEXT")
            |> fun others ->
                let length = List.length others
                let closingQuote = rest.[length]

                [ yield head
                  yield! others
                  yield closingQuote ]

        let stringContent =
            let builder = StringBuilder()

            stringTokens
            |> List.fold
                (fun (b: StringBuilder, currentLine) st ->
                    if currentLine <> st.LineNumber then
                        let delta = st.LineNumber - currentLine

                        [ 1 .. delta ]
                        |> List.iter (fun _ -> b.Append("\n") |> ignore)

                        b.Append(st.Content), st.LineNumber
                    else
                        b.Append(st.Content), st.LineNumber)
                (builder, head.LineNumber)
            |> fst
            |> fun b -> b.ToString()

        let stringStartIsSpecial () =
            if stringContent.Length > 2 then
                match stringContent.[0], stringContent.[1], stringContent.[2] with
                | '@', '"', _
                | '$', '"', _
                | '"', '"', '"' -> true
                | _ -> false
            else
                false

        let hasEscapedCharacter () =
            escapedCharacterRegex.IsMatch(stringContent)

        let hasNewlines () = stringContent.Contains("\n")
        let endsWithBinaryCharacter () = stringContent.EndsWith("\"B")

        if stringStartIsSpecial ()
           || hasEscapedCharacter ()
           || hasNewlines ()
           || endsWithBinaryCharacter () then
            Some(head, stringTokens, rest, stringContent)
        else
            None
    | _ -> None

let private identIsDecompiledOperator (token: Token) =
    let decompiledName =
        PrettyNaming.DecompileOpName token.Content

    token.TokenInfo.TokenName = "IDENT"
    && decompiledName <> token.Content

let private extractContentPreservingNewLines (tokens: Token list) =
    let rec loop result =
        function
        | [] -> result
        | [ final ] -> final.Content :: result
        | current :: ((next :: _) as rest) when (current.LineNumber <> next.LineNumber) ->
            loop ("\n" :: current.Content :: result) rest
        | current :: rest -> loop (current.Content :: result) rest

    loop [] tokens |> List.rev

let ``only whitespaces were found in the remainder of the line`` lineNumber tokens =
    tokens
    |> List.exists
        (fun t ->
            t.LineNumber = lineNumber
            && t.TokenInfo.Tag <> whiteSpaceTag)
    |> not

let rec private getTriviaFromTokensThemSelves
    (mkRange: MkRange)
    (allTokens: Token list)
    (tokens: Token list)
    foundTrivia
    =
    match tokens with
    | headToken :: rest when (headToken.TokenInfo.Tag = lineCommentTag) ->
        let lineCommentTokens =
            Seq.zip
                rest
                (headToken :: rest
                 |> List.map (fun x -> x.LineNumber))
            |> Seq.takeWhile
                (fun (t, currentLineNumber) ->
                    t.TokenInfo.Tag = lineCommentTag
                    && t.LineNumber <= (currentLineNumber + 1))
            |> Seq.map fst
            |> Seq.toList

        let comment =
            headToken
            |> List.prependItem lineCommentTokens
            |> List.groupBy (fun t -> t.LineNumber)
            |> List.map (snd >> getContentFromTokens)
            |> String.concat "\n"

        let nextTokens =
            List.length lineCommentTokens
            |> fun length -> List.skip length rest

        let range =
            let lastToken = List.tryLast lineCommentTokens
            getRangeBetween mkRange headToken (Option.defaultValue headToken lastToken)

        let info =
            let toLineComment =
                allTokens
                |> List.exists
                    (fun t ->
                        t.LineNumber = headToken.LineNumber
                        && t.TokenInfo.Tag <> whiteSpaceTag
                        && t.TokenInfo.RightColumn < headToken.TokenInfo.LeftColumn)
                |> fun e ->
                    if e then
                        LineCommentAfterSourceCode
                    else
                        LineCommentOnSingleLine

            let comment = toLineComment comment |> Comment

            Trivia.Create comment range
            |> List.appendItem foundTrivia

        getTriviaFromTokensThemSelves mkRange allTokens nextTokens info

    | headToken :: rest when (headToken.TokenInfo.TokenName = "COMMENT") ->
        let blockCommentTokens =
            rest
            |> List.takeWhileState
                (fun depth t ->
                    let newDepth =
                        match t.Content with
                        | "(*" -> depth + 1
                        | "*)" -> depth - 1
                        | _ -> depth

                    newDepth, t.TokenInfo.TokenName = "COMMENT" && depth > 0)
                1

        let comment =
            let groupedByLineNumber =
                headToken
                |> List.prependItem blockCommentTokens
                |> List.groupBy (fun t -> t.LineNumber)

            let newLines =
                let (min, _) = List.minBy fst groupedByLineNumber
                let (max, _) = List.maxBy fst groupedByLineNumber

                [ min .. max ]
                |> List.filter (fun l -> not (List.exists (fst >> ((=) l)) groupedByLineNumber))
                |> List.map (fun l -> l, String.Empty)

            groupedByLineNumber
            |> List.map (fun (l, g) -> l, getContentFromTokens g)
            |> (@) newLines
            |> List.sortBy fst
            |> List.map snd
            |> String.concat Environment.NewLine
            |> String.normalizeNewLine

        let nextTokens =
            List.length blockCommentTokens
            |> fun length -> List.skip length rest

        let range =
            let lastToken = List.last blockCommentTokens
            getRangeBetween mkRange headToken lastToken

        let info =
            Trivia.Create(Comment(BlockComment(comment, false, false))) range
            |> List.prependItem foundTrivia

        getTriviaFromTokensThemSelves mkRange allTokens nextTokens info

    | headToken :: rest when
        (isOperatorOrKeyword headToken
         && List.exists (fun k -> headToken.TokenInfo.TokenName = k) keywordTrivia) ->
        let range =
            getRangeBetween mkRange headToken headToken

        let info =
            Trivia.Create(Keyword(headToken)) range
            |> List.prependItem foundTrivia

        getTriviaFromTokensThemSelves mkRange allTokens rest info

    | headToken :: rest when (headToken.TokenInfo.TokenName = "HASH_IF") ->
        let directiveTokens =
            rest
            |> List.filter (fun r -> r.LineNumber = headToken.LineNumber)
            |> fun others -> List.prependItem others headToken

        let directiveContent =
            directiveTokens
            |> List.map (fun t -> t.Content)
            |> String.concat String.empty

        let range =
            getRangeBetween mkRange headToken (List.last directiveTokens)

        let info =
            Trivia.Create(Directive(directiveContent)) range
            |> List.prependItem foundTrivia

        let nextRest =
            match rest with
            | [] -> []
            | _ -> List.skip (List.length directiveTokens - 1) rest

        getTriviaFromTokensThemSelves mkRange allTokens nextRest info

    | EndOfInterpolatedString (stringTokens, interpStringEnd, rest) ->
        let stringContent =
            [ yield! extractContentPreservingNewLines stringTokens
              yield interpStringEnd.Content ]
            |> String.concat String.Empty

        let range =
            getRangeBetween mkRange stringTokens.Head interpStringEnd

        let info =
            Trivia.Create(StringContent(stringContent)) range
            |> List.prependItem foundTrivia

        getTriviaFromTokensThemSelves mkRange allTokens rest info

    | StringText (head, stringTokens, rest, stringContent) ->
        let lastToken =
            List.tryLast stringTokens
            |> Option.defaultValue head

        let range = getRangeBetween mkRange head lastToken

        let info =
            Trivia.Create(StringContent(stringContent)) range
            |> List.prependItem foundTrivia

        let nextRest =
            match rest with
            | [] -> []
            | _ -> List.skip (List.length stringTokens - 1) rest

        getTriviaFromTokensThemSelves mkRange allTokens nextRest info

    | minus :: head :: rest when
        (minus.TokenInfo.TokenName = "MINUS"
         && isNumber head) ->
        let range = getRangeBetween mkRange minus head

        let info =
            Trivia.Create(Number(minus.Content + head.Content)) range
            |> List.prependItem foundTrivia

        getTriviaFromTokensThemSelves mkRange allTokens rest info

    | head :: rest when (isNumber head) ->
        let range = getRangeForSingleToken mkRange head

        let info =
            Trivia.Create(Number(head.Content)) range
            |> List.prependItem foundTrivia

        getTriviaFromTokensThemSelves mkRange allTokens rest info

    | head :: rest when (identIsDecompiledOperator head) ->
        let range = getRangeBetween mkRange head head

        let info =
            Trivia.Create(IdentOperatorAsWord head.Content) range
            |> List.prependItem foundTrivia

        getTriviaFromTokensThemSelves mkRange allTokens rest info

    | head :: rest when
        (head.TokenInfo.TokenName = "IDENT"
         && head.Content.StartsWith("``")
         && head.Content.EndsWith("``")) ->
        let range = getRangeBetween mkRange head head

        let info =
            Trivia.Create(IdentBetweenTicks(head.Content)) range
            |> List.prependItem foundTrivia

        getTriviaFromTokensThemSelves mkRange allTokens rest info

    | CharToken (head) :: rest ->
        let range = getRangeBetween mkRange head head

        let info =
            Trivia.Create(CharContent(head.Content)) range
            |> List.prependItem foundTrivia

        getTriviaFromTokensThemSelves mkRange allTokens rest info

    | _ :: rest -> getTriviaFromTokensThemSelves mkRange allTokens rest foundTrivia

    | [] -> foundTrivia

let private createNewLine (mkRange: MkRange) lineNumber =
    let range = mkRange (lineNumber, 0) (lineNumber, 0)
    { Item = Newline; Range = range }

let private findEmptyNewlinesInTokens
    (mkRange: MkRange)
    (tokens: Token list)
    (ignoreRanges: FSharp.Compiler.Text.Range list)
    =
    let nonWhitespaceLines =
        tokens
        |> List.choose
            (fun t ->
                if t.TokenInfo.Tag <> whiteSpaceTag then
                    Some t.LineNumber
                else
                    None)
        |> List.distinct

    let ignoreRanges =
        ignoreRanges
        |> List.collect (fun r -> [ r.StartLine .. r.EndLine ])

    let lastLineWithContent =
        List.tryLast nonWhitespaceLines
        |> Option.defaultValue 1

    [ 1 .. lastLineWithContent ]
    |> List.except (nonWhitespaceLines @ ignoreRanges)
    |> List.map (createNewLine mkRange)

let getTriviaFromTokens (mkRange: MkRange) (tokens: Token list) =
    let fromTokens =
        getTriviaFromTokensThemSelves mkRange tokens tokens []

    let blockComments =
        fromTokens
        |> List.choose
            (fun tc ->
                match tc.Item with
                | Comment (BlockComment _) -> Some tc.Range
                | _ -> None)

    let isMultilineString (s: string) =
        s.Split([| "\n" |], StringSplitOptions.None)
        |> (Seq.isEmpty >> not)

    let multilineStrings =
        fromTokens
        |> List.choose
            (fun tc ->
                match tc.Item with
                | StringContent (sc) when (isMultilineString sc) -> Some tc.Range
                | _ -> None)

    let newLines =
        findEmptyNewlinesInTokens mkRange tokens (blockComments @ multilineStrings)

    fromTokens @ newLines
    |> List.sortBy (fun t -> t.Range.StartLine, t.Range.StartColumn)

let private tokenNames =
    [ "LBRACE"
      "RBRACE"
      "LPAREN"
      "RPAREN"
      "LBRACK"
      "RBRACK"
      "LBRACK_BAR"
      "BAR_RBRACK"
      "EQUALS"
      "IF"
      "THEN"
      "ELSE"
      "ELIF"
      "BAR"
      "RARROW"
      "TRY"
      "FINALLY"
      "WITH"
      "MEMBER"
      "AND_BANG"
      "FUNCTION"
      "IN" ]

let private tokenKinds = [ FSharpTokenCharKind.Operator ]

let internal getFsToken tokenName =
    match tokenName with
    | "LBRACE" -> LBRACE
    | "RBRACE" -> RBRACE
    | "LPAREN" -> LPAREN
    | "RPAREN" -> RPAREN
    | "LBRACK" -> LBRACK
    | "RBRACK" -> RBRACK
    | "LBRACK_BAR" -> LBRACK_BAR
    | "BAR_RBRACK" -> BAR_RBRACK
    | "EQUALS" -> EQUALS
    | "IF" -> IF
    | "THEN" -> THEN
    | "ELSE" -> ELSE
    | "ELIF" -> ELIF
    | "BAR" -> BAR
    | "RARROW" -> RARROW
    | "TRY" -> TRY
    | "FINALLY" -> FINALLY
    | "WITH" -> WITH
    | "MEMBER" -> MEMBER
    | "AND_BANG" -> AND_BANG
    | "PERCENT_OP" -> PERCENT_OP
    | "AMP" -> AMP
    | "INFIX_BAR_OP" -> INFIX_BAR_OP
    | "INFIX_COMPARE_OP" -> INFIX_COMPARE_OP
    | "LESS" -> LESS
    | "AMP_AMP" -> AMP_AMP
    | "GREATER" -> GREATER
    | "INFIX_STAR_DIV_MOD_OP" -> INFIX_STAR_DIV_MOD_OP
    | "DELAYED" -> DELAYED
    | "PLUS_MINUS_OP" -> PLUS_MINUS_OP
    | "QMARK" -> QMARK
    | "MINUS" -> MINUS
    | "COLON_QMARK" -> COLON_QMARK
    | "DOT_DOT" -> DOT_DOT
    | "INT32_DOT_DOT" -> INT32_DOT_DOT
    | "COLON_EQUALS" -> COLON_EQUALS
    | "PREFIX_OP" -> PREFIX_OP
    | "INFIX_AMP_OP" -> INFIX_AMP_OP
    | "COLON_QMARK_GREATER" -> COLON_QMARK_GREATER
    | "COLON_COLON" -> COLON_COLON
    | "COLON_GREATER" -> COLON_GREATER
    | "DOT_DOT_HAT" -> DOT_DOT_HAT
    | "BAR_BAR" -> BAR_BAR
    | "INFIX_STAR_STAR_OP" -> INFIX_STAR_STAR_OP
    | "FUNCTION" -> FUNCTION
    | "LPAREN_STAR_RPAREN" -> LPAREN_STAR_RPAREN
    | "IN" -> IN
    | "DO" -> DO
    | _ -> failwithf "was not expecting token %s" tokenName

let getTriviaNodesFromTokens (mkRange: MkRange) (tokens: Token list) =
    tokens
    |> List.filter
        (fun t ->
            List.exists (fun tn -> tn = t.TokenInfo.TokenName) tokenNames
            || List.exists (fun tk -> tk = t.TokenInfo.CharClass) tokenKinds)
    |> List.map
        (fun t ->
            let range = getRangeBetween mkRange t t
            TriviaNodeAssigner(TriviaNodeType.Token(getFsToken t.TokenInfo.TokenName, t), range))
