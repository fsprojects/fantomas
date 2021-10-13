module internal Fantomas.TokenParser

open System
open System.Text
open Fantomas
open Fantomas.TokenParserBoolExpr
open Fantomas.TriviaTypes
open FSharp.Compiler.SourceCodeServices

let private whiteSpaceTag = 4
let private lineCommentTag = 8
let private commentTag = 3
let private greaterTag = 160
let private identTag = 191

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
        let token: Token =
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
    let left, right = offset, String.length content + offset

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
    | InsideVerbatimString of startIndex: int
    | InsideMultilineComment
    | InsideLineComment

type SourceCodeParserState =
    { State: SourceCodeState
      NewlineIndexes: int list
      Defines: Token list list }

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
        let (|ForwardSlashChar|_|) = equalsChar '/'
        let (|AtChar|_|) = equalsChar '@'

        let (|LineCommentStart|_|) v =
            match v with
            | ForwardSlashChar, ForwardSlashChar, _ -> Some()
            | _ -> None

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
                              LeftColumn =
                                  t.TokenInfo.LeftColumn
                                  + hashContentLength
                                  + offset
                              RightColumn =
                                  t.TokenInfo.RightColumn
                                  + hashContentLength
                                  + offset }

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
                    | Normal, (AtChar, DoubleQuoteChar, _) ->
                        { acc with
                              State = InsideVerbatimString idx }
                    | Normal, (DoubleQuoteChar, _, _) -> { acc with State = InsideString }
                    | Normal, (OpenParenChar, AsteriskChar, NoCloseParenChar) when (sourceLength > 3) ->
                        { acc with
                              State = InsideMultilineComment }
                    | Normal, (NewlineChar, _, _) ->
                        { acc with
                              NewlineIndexes = idx :: acc.NewlineIndexes }
                    | Normal, (HashChar, _, _) -> captureHashDefine acc idx
                    | Normal, LineCommentStart -> { acc with State = InsideLineComment }
                    | _ -> acc

                elif idx < lastIndex then
                    let minusTwo = sourceCode.[idx - 2]
                    let minusOne = sourceCode.[idx - 1]

                    match acc.State, (zero, plusOne, plusTwo) with
                    | Normal, TripleQuoteChars ->
                        { acc with
                              State = InsideTripleQuoteString idx }
                    | Normal, (AtChar, DoubleQuoteChar, _) ->
                        { acc with
                              State = InsideVerbatimString idx }
                    | Normal, (DoubleQuoteChar, _, _) -> { acc with State = InsideString }
                    | Normal, (OpenParenChar, AsteriskChar, NoCloseParenChar) ->
                        { acc with
                              State = InsideMultilineComment }
                    | Normal, (NewlineChar, _, _) ->
                        { acc with
                              NewlineIndexes = idx :: acc.NewlineIndexes }
                    | Normal, (HashChar, _, _) -> captureHashDefine acc idx
                    | Normal, LineCommentStart -> { acc with State = InsideLineComment }
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
                    | InsideVerbatimString _, (DoubleQuoteChar, DoubleQuoteChar, _) -> acc
                    | InsideVerbatimString startIndex, (DoubleQuoteChar, _, _) ->
                        if idx = startIndex + 1 then
                            // Still at the start of the verbatim string @"
                            acc
                        else
                            { acc with State = Normal }
                    | InsideMultilineComment, (NewlineChar, _, _) ->
                        { acc with
                              NewlineIndexes = idx :: acc.NewlineIndexes }
                    | InsideMultilineComment, (CloseParenChar, _, _) ->
                        match minusOne with
                        | AsteriskChar -> { acc with State = Normal }
                        | _ -> acc
                    | InsideLineComment, (NewlineChar, _, _) ->
                        { acc with
                              State = Normal
                              NewlineIndexes = idx :: acc.NewlineIndexes }
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

    if List.isNotEmpty hashTokens then
        let filteredHashes =
            hashTokens
            |> List.filter (fun t -> not (List.contains t.LineNumber existingLines))
        // filter hashes that are present in source code parsed by the Tokenizer.
        tokens @ filteredHashes
        |> List.sortBy (fun t -> t.LineNumber, t.TokenInfo.LeftColumn)
    else
        tokens

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

let private isOperatorOrKeyword { TokenInfo = { CharClass = cc } } =
    cc = FSharpTokenCharKind.Keyword
    || cc = FSharpTokenCharKind.Operator

let private (|KeywordOrOperatorToken|_|) (token: Token) =
    let isOperatorOrKeyword = isOperatorOrKeyword token

    let isKnownKeywordTrivia () =
        List.exists (fun k -> token.TokenInfo.TokenName = k) keywordTrivia

    if isOperatorOrKeyword && isKnownKeywordTrivia () then
        Some token
    else
        None

let private onlyNumberRegex =
    System.Text.RegularExpressions.Regex(@"^\d+$")

let private isNumber { TokenInfo = tn; Content = content } =
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
    System.Text.RegularExpressions.Regex("(\\\\(a|b|f|n|r|t|u|v|x|'|\\\"|\\\\))+")

let private (|MultipleStringTextTokens|_|) tokens =
    let f _ =
        function
        | StringTextToken _ -> true
        | _ -> false

    tokens
    |> List.partitionWhile f
    |> fun (before, after) ->
        if List.isEmpty before then
            None
        else
            Some(before, after)

let private (|EndOfInterpolatedString|_|) tokens =
    match tokens with
    | MultipleStringTextTokens (stringTokens, rest) ->
        match rest with
        | InterpStringEndOrPartToken endToken :: rest2 -> Some(stringTokens, endToken, rest2)
        | _ -> None
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
                | '$', '@', '"'
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
    let decompiledName () =
        PrettyNaming.DecompileOpName token.Content

    token.TokenInfo.Tag = identTag
    && (decompiledName () <> token.Content)

let private (|DecompiledOperatorToken|_|) (token: Token) =
    if identIsDecompiledOperator token then
        Some token
    else
        None

let private (|IdentBetweenTicksToken|_|) (token: Token) =
    if
        token.TokenInfo.Tag = identTag
        && token.Content.StartsWith("``")
        && token.Content.EndsWith("``")
    then
        Some token
    else
        None

let private extractContentPreservingNewLines (tokens: Token list) =
    let rec loop result =
        function
        | [] -> result
        | [ final ] -> final.Content :: result
        | current :: (next :: _ as rest) when (current.LineNumber <> next.LineNumber) ->
            let delta = next.LineNumber - current.LineNumber

            let newlines =
                [ 1 .. delta ] |> List.map (fun _ -> "\n")

            loop
                [ yield! newlines
                  yield current.Content
                  yield! result ]
                rest
        | current :: rest -> loop (current.Content :: result) rest

    loop [] tokens |> List.rev

let ``only whitespaces were found in the remainder of the line`` lineNumber tokens =
    tokens
    |> List.exists
        (fun t ->
            t.LineNumber = lineNumber
            && t.TokenInfo.Tag <> whiteSpaceTag)
    |> not

let private (|LineCommentToken|_|) (token: Token) =
    if token.TokenInfo.Tag = lineCommentTag then
        Some token
    else
        None

let private (|NoCommentToken|_|) (token: Token) =
    if token.TokenInfo.Tag <> lineCommentTag
       && token.TokenInfo.Tag <> commentTag then
        Some token
    else
        None

let private (|CommentToken|_|) (token: Token) =
    if token.TokenInfo.Tag = commentTag then
        Some token
    else
        None

let private (|WhiteSpaceToken|_|) (token: Token) =
    if token.TokenInfo.Tag = whiteSpaceTag then
        Some token
    else
        None

let private (|NonWhiteSpaceToken|_|) (token: Token) =
    if token.TokenInfo.Tag <> whiteSpaceTag then
        Some token
    else
        None

let private (|SemicolonToken|_|) (token: Token) =
    if token.TokenInfo.Tag = 83 then
        Some token
    else
        None

let private (|LineComments|_|) (tokens: Token list) =
    let rec collect
        (tokens: Token list)
        (lastLineNumber: int)
        (finalContinuation: Token list -> Token list)
        : Token list * Token list =
        match tokens with
        | LineCommentToken lc :: rest when (lc.LineNumber <= lastLineNumber + 1) ->
            collect rest lc.LineNumber (fun commentTokens -> lc :: commentTokens |> finalContinuation)
        | _ -> finalContinuation [], tokens

    match tokens with
    | LineCommentToken h :: _ ->
        let commentTokens, rest = collect tokens h.LineNumber id
        Some(commentTokens, rest)
    | _ -> None

let private collectComment (commentTokens: Token list) =
    commentTokens
    |> List.groupBy (fun t -> t.LineNumber)
    |> List.map (snd >> getContentFromTokens)
    |> String.concat "\n"

let private (|EmbeddedILTokens|_|) (tokens: Token list) =
    match tokens with
    | { TokenInfo = { TokenName = "LPAREN"
                      CharClass = FSharpTokenCharKind.Delimiter } } :: { TokenInfo = { TokenName = "HASH"
                                                                                       CharClass = FSharpTokenCharKind.Delimiter } } :: { TokenInfo = { TokenName = "WHITESPACE"
                                                                                                                                                        CharClass = FSharpTokenCharKind.WhiteSpace } } :: rest ->
        let embeddedTokens =
            tokens
            |> List.takeWhile
                (fun t ->
                    not (
                        t.TokenInfo.CharClass = FSharpTokenCharKind.Delimiter
                        && t.TokenInfo.TokenName = "RPAREN"
                    ))

        let lastTokens =
            embeddedTokens.[(embeddedTokens.Length - 2)..]

        match lastTokens with
        | [ { TokenInfo = { TokenName = "WHITESPACE"
                            CharClass = FSharpTokenCharKind.WhiteSpace } }
            { TokenInfo = { TokenName = "HASH"
                            CharClass = FSharpTokenCharKind.Delimiter } } ] ->
            Some(List.take (embeddedTokens.Length + 1) tokens, rest)
        | _ -> None
    | _ -> None

let rec private (|HashTokens|_|) (tokens: Token list) =
    match tokens with
    | { TokenInfo = { TokenName = "HASH_IF" } } as head :: rest ->
        let tokensFromSameLine =
            List.takeWhile (fun t -> t.LineNumber = head.LineNumber) rest

        let nextTokens =
            List.skip tokensFromSameLine.Length rest
            |> List.skipWhile (fun t -> t.TokenInfo.Tag = whiteSpaceTag)

        match nextTokens with
        | HashTokens (nextHashTokens, rest) ->
            let totalHashTokens =
                [ yield head
                  yield! tokensFromSameLine
                  yield! nextHashTokens ]

            Some(totalHashTokens, rest)
        | _ -> Some(head :: tokensFromSameLine, rest)
    | _ -> None

let private (|KeywordString|_|) (token: Token) =
    if token.TokenInfo.Tag = 192 then
        Some token
    else
        None

let private (|BlockCommentTokens|_|) (tokens: Token list) =
    let rec collectTokens (rest: Token list) (finalContinuation: Token list -> Token list) : Token list * Token list =
        match rest with
        | CommentToken ct :: rest -> collectTokens rest (fun commentTokens -> ct :: commentTokens |> finalContinuation)
        | _ -> finalContinuation [], rest

    match tokens with
    | CommentToken { Content = "(*" } :: _ ->
        let comments, rest = collectTokens tokens id
        Some(comments, rest)
    | _ -> None

let private (|MinusToken|_|) (token: Token) =
    if token.TokenInfo.Tag = 62 then
        Some token
    else
        None

let private (|NumberToken|_|) (token: Token) =
    if isNumber token then
        Some token
    else
        None

let rec private lastTwoItems
    (project: 't -> 'ret)
    (fallbackLastButOne: 'ret)
    (fallbackLast: 'ret)
    (items: 't list)
    : 'ret * 'ret =
    match items with
    | [ f; s ] -> project f, project s
    | [ s ] -> fallbackLast, project s
    | [] -> fallbackLastButOne, fallbackLast
    | _ :: tail -> lastTwoItems project fallbackLastButOne fallbackLast tail

let rec private getTriviaFromTokensThemSelves
    (mkRange: MkRange)
    (lastButOneNonWhiteSpaceToken: Token option)
    (lastNonWhiteSpaceToken: Token option)
    (tokens: Token list)
    foundTrivia
    =
    match tokens with
    | LineComments ({ LineNumber = headLineNumber } :: _ as commentTokens, rest) ->
        let isAfterSourceCode =
            match lastButOneNonWhiteSpaceToken, lastNonWhiteSpaceToken with
            | Some otherLineToken, Some (SemicolonToken sc) when otherLineToken.LineNumber <> sc.LineNumber ->
                // IDENT SEMICOLON LINE_COMMENT
                // See https://github.com/fsprojects/fantomas/issues/1643
                false
            | _, Some t -> headLineNumber = t.LineNumber
            | _ -> false

        let info =
            if isAfterSourceCode then
                // Only capture the first line of the comment as LineCommentAfterSourceCode
                // The next line(s) will be a LineCommentOnSingleLine
                let commentsByLine =
                    commentTokens
                    |> List.groupBy (fun t -> t.LineNumber)

                let firstComment =
                    List.tryHead commentsByLine |> Option.map snd

                match firstComment with
                | Some (headToken :: _ as afterSourceTokens) ->
                    let afterSourceCodeTrivia =
                        let tc =
                            collectComment afterSourceTokens
                            |> LineCommentAfterSourceCode
                            |> Comment

                        let lastToken = List.tryLast afterSourceTokens

                        let r =
                            getRangeBetween mkRange headToken (Option.defaultValue headToken lastToken)

                        Trivia.Create tc r

                    let lineCommentOnSingleLine =
                        if commentTokens.Length > afterSourceTokens.Length then
                            let commentTokens =
                                commentTokens
                                |> List.skip afterSourceTokens.Length

                            let range =
                                let headToken = List.head commentTokens
                                let lastToken = List.tryLast commentTokens
                                getRangeBetween mkRange headToken (Option.defaultValue headToken lastToken)

                            let triviaContent =
                                (collectComment commentTokens, range)
                                |> LineCommentOnSingleLine
                                |> Comment

                            Trivia.Create triviaContent range |> Some
                        else
                            None

                    match lineCommentOnSingleLine with
                    | Some lcsl -> afterSourceCodeTrivia :: lcsl :: foundTrivia
                    | None -> afterSourceCodeTrivia :: foundTrivia
                | _ ->
                    // We should not hit this branch
                    foundTrivia
            else
                let range =
                    let headToken = List.head commentTokens
                    let lastToken = List.tryLast commentTokens
                    getRangeBetween mkRange headToken (Option.defaultValue headToken lastToken)

                let triviaContent =
                    (collectComment commentTokens, range)
                    |> LineCommentOnSingleLine
                    |> Comment

                (Trivia.Create triviaContent range :: foundTrivia)

        getTriviaFromTokensThemSelves mkRange lastButOneNonWhiteSpaceToken lastNonWhiteSpaceToken rest info

    | BlockCommentTokens (headToken :: _ as blockCommentTokens, rest) ->
        let comment =
            let groupedByLineNumber =
                blockCommentTokens
                |> List.groupBy (fun t -> t.LineNumber)

            let newLines =
                let min, _ = List.minBy fst groupedByLineNumber
                let max, _ = List.maxBy fst groupedByLineNumber

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

        let lastButOne, lastToken =
            lastTwoItems Some lastNonWhiteSpaceToken (Some headToken) blockCommentTokens

        let range =
            getRangeBetween mkRange headToken (Option.defaultValue headToken lastToken)

        let info =
            Trivia.Create(Comment(BlockComment(comment, false, false))) range
            |> List.prependItem foundTrivia

        getTriviaFromTokensThemSelves mkRange lastButOne lastToken rest info

    | KeywordString ks :: rest ->
        let range = getRangeBetween mkRange ks ks

        let info =
            Trivia.Create(KeywordString(ks.Content)) range
            |> List.prependItem foundTrivia

        getTriviaFromTokensThemSelves mkRange lastNonWhiteSpaceToken (Some ks) rest info

    | KeywordOrOperatorToken koo :: rest ->
        let range = getRangeBetween mkRange koo koo

        let info =
            Trivia.Create(Keyword(koo)) range
            |> List.prependItem foundTrivia

        getTriviaFromTokensThemSelves mkRange lastNonWhiteSpaceToken (Some koo) rest info

    | HashTokens (hashTokens, rest) ->
        let directiveContent =
            let sb = StringBuilder()

            hashTokens
            |> List.fold
                (fun (acc: StringBuilder, lastLine) token ->
                    let sb =
                        let delta = token.LineNumber - lastLine

                        if delta > 0 then
                            [ 1 .. delta ]
                            |> List.fold (fun (sb: StringBuilder) _ -> sb.Append("\n")) acc
                        else
                            acc

                    sb.Append(token.Content), token.LineNumber)
                (sb, hashTokens.[0].LineNumber)
            |> fun (sb, _) -> sb.ToString()

        let range =
            getRangeBetween mkRange (List.head hashTokens) (List.last hashTokens)

        let info =
            Trivia.Create(Directive(directiveContent)) range
            |> List.prependItem foundTrivia

        getTriviaFromTokensThemSelves mkRange lastButOneNonWhiteSpaceToken lastNonWhiteSpaceToken rest info

    | EndOfInterpolatedString (stringTokens, interpStringEnd, rest) ->
        let stringContent =
            let addExtraNewline =
                match List.tryLast stringTokens with
                | Some lst ->
                    let delta =
                        interpStringEnd.LineNumber - lst.LineNumber

                    if delta > 0 then
                        [ 1 .. delta ] |> List.map (fun _ -> "\n")
                    else
                        []
                | _ -> []

            [ yield! extractContentPreservingNewLines stringTokens
              yield! addExtraNewline
              yield interpStringEnd.Content ]
            |> String.concat String.Empty

        let range =
            getRangeBetween mkRange stringTokens.Head interpStringEnd

        let info =
            Trivia.Create(StringContent(stringContent)) range
            |> List.prependItem foundTrivia

        let prevButOne, prev =
            List.tryLast stringTokens, Some interpStringEnd

        getTriviaFromTokensThemSelves mkRange prevButOne prev rest info

    | StringText (head, stringTokens, rest, stringContent) ->
        let lastButOne, lastToken =
            lastTwoItems Some None (Some head) stringTokens

        let range =
            getRangeBetween mkRange head (Option.defaultValue head lastToken)

        let info =
            Trivia.Create(StringContent(stringContent)) range
            |> List.prependItem foundTrivia

        let nextRest =
            match rest with
            | [] -> []
            | _ -> List.skip (List.length stringTokens - 1) rest

        getTriviaFromTokensThemSelves mkRange lastButOne lastToken nextRest info

    | MinusToken minus :: NumberToken number :: rest ->
        let range = getRangeBetween mkRange minus number

        let info =
            Trivia.Create(Number(minus.Content + number.Content)) range
            |> List.prependItem foundTrivia

        getTriviaFromTokensThemSelves mkRange (Some minus) (Some number) rest info

    | NumberToken number :: rest ->
        let range = getRangeForSingleToken mkRange number

        let info =
            Trivia.Create(Number(number.Content)) range
            |> List.prependItem foundTrivia

        getTriviaFromTokensThemSelves mkRange lastNonWhiteSpaceToken (Some number) rest info

    | DecompiledOperatorToken ident :: rest ->
        let range = getRangeBetween mkRange ident ident

        let info =
            Trivia.Create(IdentOperatorAsWord ident.Content) range
            |> List.prependItem foundTrivia

        getTriviaFromTokensThemSelves mkRange lastNonWhiteSpaceToken (Some ident) rest info

    | IdentBetweenTicksToken ident :: rest ->
        let range = getRangeBetween mkRange ident ident

        let info =
            Trivia.Create(IdentBetweenTicks(ident.Content)) range
            |> List.prependItem foundTrivia

        getTriviaFromTokensThemSelves mkRange lastNonWhiteSpaceToken (Some ident) rest info

    | CharToken head :: rest ->
        let range = getRangeBetween mkRange head head

        let info =
            Trivia.Create(CharContent(head.Content)) range
            |> List.prependItem foundTrivia

        getTriviaFromTokensThemSelves mkRange lastNonWhiteSpaceToken (Some head) rest info

    | EmbeddedILTokens (embeddedTokens, rest) ->
        let content =
            embeddedTokens
            |> List.map (fun t -> t.Content)
            |> String.concat String.Empty

        let range =
            let startT = embeddedTokens.Head
            let endT = List.last embeddedTokens
            // There is a one off problem in the range of SynExpr_LibraryOnlyILAssembly
            mkRange (startT.LineNumber, startT.TokenInfo.LeftColumn) (endT.LineNumber, endT.TokenInfo.RightColumn + 1)

        let info =
            Trivia.Create(EmbeddedIL(content)) range
            |> List.prependItem foundTrivia

        let prevButOne, prev =
            lastTwoItems Some lastButOneNonWhiteSpaceToken lastNonWhiteSpaceToken embeddedTokens

        getTriviaFromTokensThemSelves mkRange prevButOne prev rest info

    | NonWhiteSpaceToken h :: rest ->
        let prevButOne = lastNonWhiteSpaceToken
        let prev = Some h
        getTriviaFromTokensThemSelves mkRange prevButOne prev rest foundTrivia

    | _ :: rest ->
        getTriviaFromTokensThemSelves mkRange lastButOneNonWhiteSpaceToken lastNonWhiteSpaceToken rest foundTrivia

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
        getTriviaFromTokensThemSelves mkRange None None tokens []

    let isMultilineString (s: string) = s.Contains("\n")

    let ignoreRanges =
        fromTokens
        |> List.choose
            (fun tc ->
                match tc.Item with
                | Directive dc when (isMultilineString dc) -> Some tc.Range
                | Comment (BlockComment _) -> Some tc.Range
                | StringContent sc when (isMultilineString sc) -> Some tc.Range
                | _ -> None)

    let newLines =
        findEmptyNewlinesInTokens mkRange tokens ignoreRanges

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
      "IN" ]

let private tokenKinds = [ FSharpTokenCharKind.Operator ]

let internal getFsToken tokenName =
    match tokenName with
    | "AMP" -> AMP
    | "AMP_AMP" -> AMP_AMP
    | "AND_BANG" -> AND_BANG
    | "BAR" -> BAR
    | "BAR_BAR" -> BAR_BAR
    | "BAR_RBRACK" -> BAR_RBRACK
    | "COLON_COLON" -> COLON_COLON
    | "COLON_EQUALS" -> COLON_EQUALS
    | "COLON_GREATER" -> COLON_GREATER
    | "COLON_QMARK" -> COLON_QMARK
    | "COLON_QMARK_GREATER" -> COLON_QMARK_GREATER
    | "DELAYED" -> DELAYED
    | "DO" -> DO
    | "DOLLAR" -> DOLLAR
    | "DOT_DOT" -> DOT_DOT
    | "DOT_DOT_HAT" -> DOT_DOT_HAT
    | "ELIF" -> ELIF
    | "ELSE" -> ELSE
    | "EQUALS" -> EQUALS
    | "FINALLY" -> FINALLY
    | "GREATER" -> GREATER
    | "IF" -> IF
    | "IN" -> IN
    | "INFIX_AMP_OP" -> INFIX_AMP_OP
    | "INFIX_BAR_OP" -> INFIX_BAR_OP
    | "INFIX_COMPARE_OP" -> INFIX_COMPARE_OP
    | "INFIX_STAR_DIV_MOD_OP" -> INFIX_STAR_DIV_MOD_OP
    | "INFIX_STAR_STAR_OP" -> INFIX_STAR_STAR_OP
    | "INT32_DOT_DOT" -> INT32_DOT_DOT
    | "LBRACE" -> LBRACE
    | "LBRACK" -> LBRACK
    | "LBRACK_BAR" -> LBRACK_BAR
    | "LESS" -> LESS
    | "LPAREN" -> LPAREN
    | "LPAREN_STAR_RPAREN" -> LPAREN_STAR_RPAREN
    | "MEMBER" -> MEMBER
    | "MINUS" -> MINUS
    | "PERCENT_OP" -> PERCENT_OP
    | "PLUS_MINUS_OP" -> PLUS_MINUS_OP
    | "PREFIX_OP" -> PREFIX_OP
    | "QMARK" -> QMARK
    | "QMARK_QMARK" -> QMARK_QMARK
    | "RARROW" -> RARROW
    | "RBRACE" -> RBRACE
    | "RBRACK" -> RBRACK
    | "RPAREN" -> RPAREN
    | "THEN" -> THEN
    | "TRY" -> TRY
    | "WITH" -> WITH
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
