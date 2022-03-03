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
open Fantomas.TokenParserBoolExpr
open Fantomas.TriviaTypes

type ISourceText with
    member this.GetContentAt(range: range) : string =
        let startLine = range.StartLine - 1
        let line = this.GetLineString startLine

        if range.StartLine = range.EndLine then
            let length = range.EndColumn - range.StartColumn
            line.Substring(range.StartColumn, length)
        else
            let firstLineContent = line.Substring(range.StartColumn)
            let sb = StringBuilder().AppendLine(firstLineContent)

            (sb, [ range.StartLine .. range.EndLine - 2 ])
            ||> List.fold (fun sb lineNumber -> sb.AppendLine(this.GetLineString lineNumber))
            |> fun sb ->
                let lastLine = this.GetLineString(range.EndLine - 1)

                sb
                    .Append(lastLine.Substring(0, range.EndColumn))
                    .ToString()


//
//open System
//open System.Text
//open FSharp.Compiler.Syntax
//open FSharp.Compiler.Tokenization
//open Fantomas
//open Fantomas.TokenParserBoolExpr
//open Fantomas.TriviaTypes
//
//let private whiteSpaceTag = 4
//let private lineCommentTag = 8
//let private commentTag = 3
//let private greaterTag = 160
//let private identTag = 192
//let private equalsTag = 69
//
//// workaround for cases where tokenizer dont output "delayed" part of operator after ">."
//// See https://github.com/fsharp/FSharp.Compiler.Service/issues/874
//let private isTokenAfterGreater token (greaterToken: Token) =
//    let greaterToken = greaterToken.TokenInfo
//
//    greaterToken.Tag = greaterTag
//    && token.Tag <> greaterTag
//    && greaterToken.RightColumn <> (token.LeftColumn + 1)
//
//let private getTokenText (sourceCodeLines: string list) line (token: FSharpTokenInfo) =
//    sourceCodeLines.[line - 1]
//        .Substring(token.LeftColumn, token.RightColumn - token.LeftColumn + 1)
//    |> String.normalizeNewLine
//
///// Tokenize a single line of F# code
//let rec private tokenizeLine (tokenizer: FSharpLineTokenizer) sourceCodeLines state lineNumber tokens =
//    match tokenizer.ScanToken(state), List.tryHead tokens with
//    | (Some tok, state), Some greaterToken when (isTokenAfterGreater tok greaterToken) ->
//        let extraTokenInfo =
//            { tok with
//                TokenName = "DELAYED"
//                LeftColumn = greaterToken.TokenInfo.RightColumn + 1
//                Tag = -1
//                CharClass = FSharpTokenCharKind.Operator
//                RightColumn = tok.LeftColumn - 1 }
//
//        let extraToken =
//            { TokenInfo = extraTokenInfo
//              LineNumber = lineNumber
//              Content = getTokenText sourceCodeLines lineNumber extraTokenInfo }
//
//        let token =
//            { TokenInfo = tok
//              LineNumber = lineNumber
//              Content = getTokenText sourceCodeLines lineNumber tok }
//
//        tokenizeLine tokenizer sourceCodeLines state lineNumber (token :: extraToken :: tokens)
//
//    | (Some tok, state), _ ->
//        let token: Token =
//            { TokenInfo = tok
//              LineNumber = lineNumber
//              Content = getTokenText sourceCodeLines lineNumber tok }
//        // Tokenize the rest, in the new state
//        tokenizeLine tokenizer sourceCodeLines state lineNumber (token :: tokens)
//
//    | (None, state), _ -> state, tokens
//
//let private tokenizeLines (sourceTokenizer: FSharpSourceTokenizer) allLines state =
//    allLines
//    |> List.mapi (fun index line -> line, (index + 1)) // line number is needed in tokenizeLine
//    |> List.fold
//        (fun (state, tokens) (line, lineNumber) ->
//            let tokenizer = sourceTokenizer.CreateLineTokenizer(line)
//
//            let nextState, tokensOfLine = tokenizeLine tokenizer allLines state lineNumber []
//
//            let allTokens = List.append tokens (List.rev tokensOfLine) // tokens of line are add in reversed order
//
//            (nextState, allTokens))
//        (state, []) // empty tokens to start with
//    |> snd // ignore the state
//
//let private createHashToken lineNumber content offset =
//    let left, right = offset, String.length content + offset
//
//    { LineNumber = lineNumber
//      Content = content
//      TokenInfo =
//        { TokenName = "HASH_IF"
//          LeftColumn = left
//          RightColumn = right
//          ColorClass = FSharpTokenColorKind.PreprocessorKeyword
//          CharClass = FSharpTokenCharKind.WhiteSpace
//          FSharpTokenTriggerClass = FSharpTokenTriggerClass.None
//          Tag = 0
//          FullMatchedLength = String.length content } }
//
//type SourceCodeState =
//    | Normal
//    | InsideString
//    | InsideTripleQuoteString of startIndex: int
//    | InsideVerbatimString of startIndex: int
//    | InsideMultilineComment
//    | InsideLineComment
//
//type SourceCodeParserState =
//    { State: SourceCodeState
//      NewlineIndexes: int list
//      Defines: Token list list }
//
//let rec private getTokenizedHashes (sourceCode: string) : Token list =
//    let hasNoHashDirectiveStart (source: string) = not (source.Contains("#if"))
//
//    if hasNoHashDirectiveStart sourceCode then
//        []
//    else
//        let equalsChar c v = if c = v then Some() else None
//        let differsFromChar c v = if c <> v then Some() else None
//
//        let (|DoubleQuoteChar|_|) = equalsChar '"'
//
//        let (|TripleQuoteChars|_|) v =
//            match v with
//            | DoubleQuoteChar, DoubleQuoteChar, DoubleQuoteChar -> Some()
//            | _ -> None
//
//        let (|OpenParenChar|_|) = equalsChar '('
//        let (|AsteriskChar|_|) = equalsChar '*'
//        let (|NoCloseParenChar|_|) = differsFromChar ')'
//        let (|NewlineChar|_|) = equalsChar '\n'
//        let (|HashChar|_|) = equalsChar '#'
//        let (|BackSlashChar|_|) = equalsChar '\\'
//        let (|NoBackSlashChar|_|) = differsFromChar '\\'
//        let (|CloseParenChar|_|) = equalsChar ')'
//        let (|ForwardSlashChar|_|) = equalsChar '/'
//        let (|AtChar|_|) = equalsChar '@'
//
//        let (|LineCommentStart|_|) v =
//            match v with
//            | ForwardSlashChar, ForwardSlashChar, _ -> Some()
//            | _ -> None
//
//        let isSpace = (=) ' '
//
//        let processLine (hashContent: string) (lineContent: string) (lineNumber: int) (offset: int) : Token list =
//            let hashContentLength = String.length hashContent
//
//            let tokens =
//                let defineExpressionWithHash = lineContent.Substring(hashContentLength)
//
//                if String.isNotNullOrEmpty defineExpressionWithHash then
//                    tokenize [] [] defineExpressionWithHash
//                else
//                    []
//
//            tokens
//            |> List.map (fun t ->
//                let info =
//                    { t.TokenInfo with
//                        LeftColumn =
//                            t.TokenInfo.LeftColumn
//                            + hashContentLength
//                            + offset
//                        RightColumn =
//                            t.TokenInfo.RightColumn
//                            + hashContentLength
//                            + offset }
//
//                { t with
//                    LineNumber = lineNumber
//                    TokenInfo = info })
//            |> fun rest ->
//                (createHashToken lineNumber hashContent offset)
//                :: rest
//
//        let sourceLength = String.length sourceCode
//        // stop scanning the source code three characters before the end
//        // three because of how triple quote string are opened and closed
//        // the scan looks three characters ahead (zero, plusOne, plusTwo)
//        // and sometimes also two characters behind (minusTwo, minusOne)
//        // In theory there is will also never be any new hash detect inside the last three characters
//        let lastIndex = sourceLength - 3
//        // check if the current # char is part of an define expression
//        // if so add to defines
//        let captureHashDefine (state: SourceCodeParserState) idx =
//            let lastNewlineIdx =
//                Seq.tryHead state.NewlineIndexes
//                |> Option.defaultValue -1
//
//            let leadingCharactersBeforeLastNewlineAreSpaces =
//                let take = Math.Max(idx - lastNewlineIdx - 1, 0)
//
//                sourceCode
//                |> Seq.skip (lastNewlineIdx + 1)
//                |> Seq.take take
//                |> Seq.forall isSpace
//
//            if leadingCharactersBeforeLastNewlineAreSpaces then
//                let skip =
//                    if lastNewlineIdx = -1 then
//                        0
//                    else
//                        lastNewlineIdx + 1 // zero when the source starts with an #
//
//                let currentLine =
//                    sourceCode
//                    |> Seq.skip skip
//                    |> Seq.takeWhile (function
//                        | NewlineChar -> false
//                        | _ -> true)
//                    |> Seq.toArray
//                    |> fun chars -> new string (chars)
//
//                let trimmed = currentLine.TrimStart()
//
//                let offset = (String.length currentLine - String.length trimmed)
//
//                let lineNumber = List.length state.NewlineIndexes + 1 // line numbers are 1 based.
//
//                if trimmed.StartsWith("#if") then
//                    { state with
//                        Defines =
//                            (processLine "#if" trimmed lineNumber offset)
//                            :: state.Defines }
//                elif trimmed.StartsWith("#elseif") then
//                    { state with
//                        Defines =
//                            (processLine "#elseif" trimmed lineNumber offset)
//                            :: state.Defines }
//                elif trimmed.StartsWith("#else") then
//                    { state with
//                        Defines =
//                            (processLine "#else" trimmed lineNumber offset)
//                            :: state.Defines }
//                elif trimmed.StartsWith("#endif") then
//                    { state with
//                        Defines =
//                            (processLine "#endif" trimmed lineNumber offset)
//                            :: state.Defines }
//                else
//                    state
//            else
//                state
//
//        let initialState =
//            { State = Normal
//              NewlineIndexes = []
//              Defines = [] }
//
//        [ 0..lastIndex ]
//        |> List.fold
//            (fun acc idx ->
//                let zero = sourceCode.[idx]
//                let plusOne = sourceCode.[idx + 1]
//                let plusTwo = sourceCode.[idx + 2]
//
//                if idx < 2 then
//                    match acc.State, (zero, plusOne, plusTwo) with
//                    | Normal, TripleQuoteChars -> { acc with State = InsideTripleQuoteString(idx) }
//                    | Normal, (AtChar, DoubleQuoteChar, _) -> { acc with State = InsideVerbatimString idx }
//                    | Normal, (DoubleQuoteChar, _, _) -> { acc with State = InsideString }
//                    | Normal, (OpenParenChar, AsteriskChar, NoCloseParenChar) when (sourceLength > 3) ->
//                        { acc with State = InsideMultilineComment }
//                    | Normal, (NewlineChar, _, _) -> { acc with NewlineIndexes = idx :: acc.NewlineIndexes }
//                    | Normal, (HashChar, _, _) -> captureHashDefine acc idx
//                    | Normal, LineCommentStart -> { acc with State = InsideLineComment }
//                    | _ -> acc
//
//                elif idx < lastIndex then
//                    let minusTwo = sourceCode.[idx - 2]
//                    let minusOne = sourceCode.[idx - 1]
//
//                    match acc.State, (zero, plusOne, plusTwo) with
//                    | Normal, TripleQuoteChars -> { acc with State = InsideTripleQuoteString idx }
//                    | Normal, (AtChar, DoubleQuoteChar, _) -> { acc with State = InsideVerbatimString idx }
//                    | Normal, (DoubleQuoteChar, _, _) -> { acc with State = InsideString }
//                    | Normal, (OpenParenChar, AsteriskChar, NoCloseParenChar) ->
//                        { acc with State = InsideMultilineComment }
//                    | Normal, (NewlineChar, _, _) -> { acc with NewlineIndexes = idx :: acc.NewlineIndexes }
//                    | Normal, (HashChar, _, _) -> captureHashDefine acc idx
//                    | Normal, LineCommentStart -> { acc with State = InsideLineComment }
//                    | InsideString, (NewlineChar, _, _) -> { acc with NewlineIndexes = idx :: acc.NewlineIndexes }
//                    | InsideString, (DoubleQuoteChar, _, _) ->
//                        let minusThree = sourceCode.[idx - 3]
//
//                        match minusOne, minusTwo, minusThree with
//                        | BackSlashChar, NoBackSlashChar, _
//                        | BackSlashChar, BackSlashChar, BackSlashChar -> acc
//                        | _ -> { acc with State = Normal }
//                    | InsideString, (DoubleQuoteChar, _, _) -> { acc with State = Normal }
//                    | InsideTripleQuoteString _, (NewlineChar, _, _) ->
//                        { acc with NewlineIndexes = idx :: acc.NewlineIndexes }
//                    | InsideTripleQuoteString startIndex, _ when (startIndex + 2 < idx) ->
//                        match (minusTwo, minusOne, zero) with
//                        | TripleQuoteChars when ((startIndex - 1) > 0) ->
//                            let minusThree = sourceCode.[idx - 3]
//                            // check if there is no backslash before the first `"` of `"""`
//                            // so no `\"""` characters
//                            match minusThree with
//                            | NoBackSlashChar -> { acc with State = Normal }
//                            | _ -> acc
//                        | _ -> acc
//                    | InsideVerbatimString _, (DoubleQuoteChar, DoubleQuoteChar, _) -> acc
//                    | InsideVerbatimString startIndex, (DoubleQuoteChar, _, _) ->
//                        if idx = startIndex + 1 then
//                            // Still at the start of the verbatim string @"
//                            acc
//                        else
//                            { acc with State = Normal }
//                    | InsideMultilineComment, (NewlineChar, _, _) ->
//                        { acc with NewlineIndexes = idx :: acc.NewlineIndexes }
//                    | InsideMultilineComment, (CloseParenChar, _, _) ->
//                        match minusOne with
//                        | AsteriskChar -> { acc with State = Normal }
//                        | _ -> acc
//                    | InsideLineComment, (NewlineChar, _, _) ->
//                        { acc with
//                            State = Normal
//                            NewlineIndexes = idx :: acc.NewlineIndexes }
//                    | _ -> acc
//
//                else
//                    acc)
//            initialState
//        |> fun state -> state.Defines |> List.rev |> List.collect id
//
//and tokenize defines (hashTokens: Token list) (content: string) : Token list =
//    let sourceTokenizer = FSharpSourceTokenizer(defines, Some "/tmp.fsx")
//
//    let lines =
//        String.normalizeThenSplitNewLine content
//        |> Array.toList
//
//    let tokens =
//        tokenizeLines sourceTokenizer lines FSharpTokenizerLexState.Initial
//        |> List.filter (fun t -> t.TokenInfo.TokenName <> "INACTIVECODE")
//
//    let existingLines =
//        tokens
//        |> List.map (fun t -> t.LineNumber)
//        |> List.distinct
//
//    if List.isNotEmpty hashTokens then
//        let filteredHashes =
//            hashTokens
//            |> List.filter (fun t -> not (List.contains t.LineNumber existingLines))
//        // filter hashes that are present in source code parsed by the Tokenizer.
//        tokens @ filteredHashes
//        |> List.sortBy (fun t -> t.LineNumber, t.TokenInfo.LeftColumn)
//    else
//        tokens
//
//let getDefinesWords (tokens: Token list) =
//    tokens
//    |> List.filter (fun { TokenInfo = { TokenName = tn } } -> tn = "IDENT" || tn = "FALSE" || tn = "TRUE")
//    |> List.map (fun t -> t.Content)
//    |> List.distinct
//
//let getDefineExprs (hashTokens: Token list) =
//    let parseHashContent tokens =
//        let allowedContent = set [ "||"; "&&"; "!"; "("; ")" ]
//
//        tokens
//        |> Seq.filter (fun t ->
//            t.TokenInfo.TokenName = "IDENT"
//            || t.TokenInfo.TokenName = "TRUE"
//            || t.TokenInfo.TokenName = "FALSE"
//            || Set.contains t.Content allowedContent)
//        |> Seq.map (fun t -> t.Content)
//        |> Seq.toList
//        |> BoolExprParser.parse
//
//    let tokensByLine =
//        hashTokens
//        |> List.groupBy (fun t -> t.LineNumber)
//        |> List.sortBy fst
//
//    let result =
//        (([], []), tokensByLine)
//        ||> List.fold (fun (contextExprs, exprAcc) (_, lineTokens) ->
//            let contextExpr e =
//                e :: contextExprs
//                |> List.reduce (fun x y -> BoolExpr.And(x, y))
//
//            let t =
//                lineTokens
//                |> Seq.tryFind (fun x -> x.TokenInfo.TokenName = "HASH_IF")
//
//            match t |> Option.map (fun x -> x.Content) with
//            | Some "#if" ->
//                parseHashContent lineTokens
//                |> Option.map (fun e -> e :: contextExprs, contextExpr e :: exprAcc)
//                |> Option.defaultValue (contextExprs, exprAcc)
//            | Some "#else" ->
//                contextExprs,
//                BoolExpr.Not(
//                    contextExprs
//                    |> List.reduce (fun x y -> BoolExpr.And(x, y))
//                )
//                :: exprAcc
//            | Some "#endif" -> List.tail contextExprs, exprAcc
//            | _ -> contextExprs, exprAcc)
//        |> snd
//        |> List.rev
//
//    result
//
//let internal getOptimizedDefinesSets (hashTokens: Token list) =
//    let maxSteps = FormatConfig.satSolveMaxStepsMaxSteps
//
//    match getDefineExprs hashTokens
//          |> BoolExpr.mergeBoolExprs maxSteps
//          |> List.map snd
//        with
//    | [] -> [ [] ]
//    | xs -> xs
//
//let getDefines sourceCode =
//    let hashTokens = getTokenizedHashes sourceCode
//
//    let defineCombinations =
//        getOptimizedDefinesSets hashTokens
//        @ (getDefinesWords hashTokens
//           |> List.map List.singleton)
//          @ [ [] ]
//        |> List.distinct
//
//    defineCombinations, hashTokens
//
//let private getRangeBetween (mkRange: MkRange) startToken endToken =
//    let l = startToken.TokenInfo.LeftColumn
//    let r = endToken.TokenInfo.RightColumn
//    mkRange (startToken.LineNumber, l) (endToken.LineNumber, (if l = r then r + 1 else r))
//
//let private getRangeForSingleToken (mkRange: MkRange) token =
//    let l = token.TokenInfo.LeftColumn
//    let r = l + token.TokenInfo.FullMatchedLength
//    mkRange (token.LineNumber, l) (token.LineNumber, r)
//
//let private hasOnlySpacesAndLineCommentsOnLine lineNumber tokens =
//    if List.isEmpty tokens then
//        false
//    else
//        tokens
//        |> List.filter (fun t -> t.LineNumber = lineNumber)
//        |> List.forall (fun t ->
//            t.TokenInfo.Tag = whiteSpaceTag
//            || t.TokenInfo.Tag = lineCommentTag)
//
//let private getContentFromTokens tokens =
//    tokens
//    |> List.map (fun t -> t.Content)
//    |> String.concat String.Empty
//
//let private keywordTrivia = [ "KEYWORD_STRING"; "QMARK" ]
//
//let private numberTrivia =
//    [ "UINT8"
//      "INT8"
//      "UINT16"
//      "INT16"
//      "UINT32"
//      "INT32"
//      "UINT64"
//      "INT64"
//      "IEEE32"
//      "DECIMAL"
//      "IEEE64"
//      "BIGNUM"
//      "NATIVEINT"
//      "UNATIVEINT" ]
//
//let private isOperatorOrKeyword { TokenInfo = { CharClass = cc } } =
//    cc = FSharpTokenCharKind.Keyword
//    || cc = FSharpTokenCharKind.Operator
//
//let private (|KeywordOrOperatorToken|_|) (token: Token) =
//    let isOperatorOrKeyword = isOperatorOrKeyword token
//
//    let isKnownKeywordTrivia () =
//        List.exists (fun k -> token.TokenInfo.TokenName = k) keywordTrivia
//
//    if isOperatorOrKeyword && isKnownKeywordTrivia () then
//        Some token
//    else
//        None
//
//let private onlyNumberRegex = System.Text.RegularExpressions.Regex(@"^\d+$")
//
//let private isNumber { TokenInfo = tn; Content = content } =
//    tn.ColorClass = FSharpTokenColorKind.Number
//    && List.contains tn.TokenName numberTrivia
//    && not (onlyNumberRegex.IsMatch(content))
//
//let private digitOrLetterCharRegex =
//    System.Text.RegularExpressions.Regex(@"^'(\d|[a-zA-Z])'$")
//
//let private (|CharToken|_|) token =
//    if
//        token.TokenInfo.TokenName = "CHAR"
//        && not (digitOrLetterCharRegex.IsMatch(token.Content))
//    then
//        Some token
//    else
//        None
//
//let private (|StringTextToken|_|) token =
//    if token.TokenInfo.TokenName = "STRING_TEXT" then
//        Some token
//    else
//        None
//
//let private (|InterpStringEndOrPartToken|_|) token =
//    if token.TokenInfo.TokenName = "INTERP_STRING_END"
//       || token.TokenInfo.TokenName = "INTERP_STRING_PART" then
//        Some token
//    else
//        None
//
let escapedCharacterRegex =
    System.Text.RegularExpressions.Regex("(\\\\(a|b|f|n|r|t|u|v|x|0|'|\\\"|\\\\))+")

//
//let private (|MultipleStringTextTokens|_|) tokens =
//    let f _ =
//        function
//        | StringTextToken _ -> true
//        | _ -> false
//
//    tokens
//    |> List.partitionWhile f
//    |> fun (before, after) ->
//        if List.isEmpty before then
//            None
//        else
//            Some(before, after)
//
//let private (|EndOfInterpolatedString|_|) tokens =
//    match tokens with
//    | MultipleStringTextTokens (stringTokens, rest) ->
//        match rest with
//        | InterpStringEndOrPartToken endToken :: rest2 -> Some(stringTokens, endToken, rest2)
//        | _ -> None
//    | _ -> None
//
//let private (|StringText|_|) tokens =
//    match tokens with
//    | StringTextToken head :: rest ->
//        let stringTokens =
//            rest
//            |> List.takeWhile (fun { TokenInfo = { TokenName = tn } } -> tn = "STRING_TEXT")
//            |> fun others ->
//                let length = List.length others
//                let closingQuote = rest.[length]
//
//                [ yield head
//                  yield! others
//                  yield closingQuote ]
//
//        let stringContent =
//            let builder = StringBuilder()
//
//            stringTokens
//            |> List.fold
//                (fun (b: StringBuilder, currentLine) st ->
//                    if currentLine <> st.LineNumber then
//                        let delta = st.LineNumber - currentLine
//
//                        [ 1..delta ]
//                        |> List.iter (fun _ -> b.Append("\n") |> ignore)
//
//                        b.Append(st.Content), st.LineNumber
//                    else
//                        b.Append(st.Content), st.LineNumber)
//                (builder, head.LineNumber)
//            |> fst
//            |> fun b -> b.ToString()
//
//        let stringStartIsSpecial () =
//            if stringContent.Length > 2 then
//                match stringContent.[0], stringContent.[1], stringContent.[2] with
//                | '@', '"', _
//                | '$', '"', _
//                | '$', '@', '"'
//                | '"', '"', '"' -> true
//                | _ -> false
//            else
//                false
//
//        let hasEscapedCharacter () =
//            escapedCharacterRegex.IsMatch(stringContent)
//
//        let hasNewlines () = stringContent.Contains("\n")
//        let endsWithBinaryCharacter () = stringContent.EndsWith("\"B")
//
//        if stringStartIsSpecial ()
//           || hasEscapedCharacter ()
//           || hasNewlines ()
//           || endsWithBinaryCharacter () then
//            Some(head, stringTokens, rest, stringContent)
//        else
//            None
//    | _ -> None
//
//let private identIsDecompiledOperator (token: Token) =
//    let decompiledName () =
//        PrettyNaming.DecompileOpName token.Content
//
//    token.TokenInfo.Tag = identTag
//    && (decompiledName () <> token.Content)
//
//let private (|DecompiledOperatorToken|_|) (token: Token) =
//    if identIsDecompiledOperator token then
//        Some token
//    else
//        None
//
//let private (|IdentBetweenTicksToken|_|) (token: Token) =
//    if
//        token.TokenInfo.Tag = identTag
//        && token.Content.StartsWith("``")
//        && token.Content.EndsWith("``")
//    then
//        Some token
//    else
//        None
//
//let private extractContentPreservingNewLines (tokens: Token list) =
//    let rec loop result =
//        function
//        | [] -> result
//        | [ final ] -> final.Content :: result
//        | current :: (next :: _ as rest) when (current.LineNumber <> next.LineNumber) ->
//            let delta = next.LineNumber - current.LineNumber
//
//            let newlines = [ 1..delta ] |> List.map (fun _ -> "\n")
//
//            loop
//                [ yield! newlines
//                  yield current.Content
//                  yield! result ]
//                rest
//        | current :: rest -> loop (current.Content :: result) rest
//
//    loop [] tokens |> List.rev
//
//let ``only whitespaces were found in the remainder of the line`` lineNumber tokens =
//    tokens
//    |> List.exists (fun t ->
//        t.LineNumber = lineNumber
//        && t.TokenInfo.Tag <> whiteSpaceTag)
//    |> not
//
//let private (|LineCommentToken|_|) (token: Token) =
//    if token.TokenInfo.Tag = lineCommentTag then
//        Some token
//    else
//        None
//
//let private (|NoCommentToken|_|) (token: Token) =
//    if token.TokenInfo.Tag <> lineCommentTag
//       && token.TokenInfo.Tag <> commentTag then
//        Some token
//    else
//        None
//
//let private (|CommentToken|_|) (token: Token) =
//    if token.TokenInfo.Tag = commentTag then
//        Some token
//    else
//        None
//
//let private (|WhiteSpaceToken|_|) (token: Token) =
//    if token.TokenInfo.Tag = whiteSpaceTag then
//        Some token
//    else
//        None
//
//let private (|NonWhiteSpaceToken|_|) (token: Token) =
//    if token.TokenInfo.Tag <> whiteSpaceTag then
//        Some token
//    else
//        None
//
//let private (|SemicolonToken|_|) (token: Token) =
//    if token.TokenInfo.Tag = 83 then
//        Some token
//    else
//        None
//
//let private (|LineComments|_|) (tokens: Token list) =
//    let rec collect
//        (tokens: Token list)
//        (lastLineNumber: int)
//        (finalContinuation: Token list -> Token list)
//        : Token list * Token list =
//        match tokens with
//        | LineCommentToken lc :: rest when (lc.LineNumber <= lastLineNumber + 1) ->
//            collect rest lc.LineNumber (fun commentTokens -> lc :: commentTokens |> finalContinuation)
//        | _ -> finalContinuation [], tokens
//
//    match tokens with
//    | LineCommentToken h :: _ ->
//        let commentTokens, rest = collect tokens h.LineNumber id
//        Some(commentTokens, rest)
//    | _ -> None
//
//let private (|TripleSlashLineComment|_|) (tokens: Token list) =
//    let rec collect (tokens: Token list) (lineNumber: int) : Token list =
//        match tokens with
//        | LineCommentToken lc :: rest when (lc.LineNumber = lineNumber) -> collect rest lc.LineNumber
//        | _ -> tokens
//
//    match tokens with
//    | LineCommentToken { Content = ("///" | "///<")
//                         LineNumber = ln } :: _ ->
//        let rest = collect tokens ln
//        Some(rest)
//    | _ -> None
//
//let private collectComment (commentTokens: Token list) =
//    commentTokens
//    |> List.groupBy (fun t -> t.LineNumber)
//    |> List.map (snd >> getContentFromTokens)
//    |> String.concat "\n"
//
//let private (|EmbeddedILTokens|_|) (tokens: Token list) =
//    match tokens with
//    | { TokenInfo = { TokenName = "LPAREN"
//                      CharClass = FSharpTokenCharKind.Delimiter } } :: { TokenInfo = { TokenName = "HASH"
//                                                                                       CharClass = FSharpTokenCharKind.Delimiter } } :: { TokenInfo = { TokenName = "WHITESPACE"
//                                                                                                                                                        CharClass = FSharpTokenCharKind.WhiteSpace } } :: rest ->
//        let embeddedTokens =
//            tokens
//            |> List.takeWhile (fun t ->
//                not (
//                    t.TokenInfo.CharClass = FSharpTokenCharKind.Delimiter
//                    && t.TokenInfo.TokenName = "RPAREN"
//                ))
//
//        let lastTokens = embeddedTokens.[(embeddedTokens.Length - 2) ..]
//
//        match lastTokens with
//        | [ { TokenInfo = { TokenName = "WHITESPACE"
//                            CharClass = FSharpTokenCharKind.WhiteSpace } }
//            { TokenInfo = { TokenName = "HASH"
//                            CharClass = FSharpTokenCharKind.Delimiter } } ] ->
//            Some(List.take (embeddedTokens.Length + 1) tokens, rest)
//        | _ -> None
//    | _ -> None
//
//let rec private (|HashTokens|_|) (tokens: Token list) =
//    match tokens with
//    | { TokenInfo = { TokenName = "HASH_IF" } } as head :: rest ->
//        let tokensFromSameLine =
//            List.takeWhile (fun t -> t.LineNumber = head.LineNumber) rest
//
//        let nextTokens =
//            List.skip tokensFromSameLine.Length rest
//            |> List.skipWhile (fun t -> t.TokenInfo.Tag = whiteSpaceTag)
//
//        match nextTokens with
//        | HashTokens (nextHashTokens, rest) ->
//            let totalHashTokens =
//                [ yield head
//                  yield! tokensFromSameLine
//                  yield! nextHashTokens ]
//
//            Some(totalHashTokens, rest)
//        | _ -> Some(head :: tokensFromSameLine, rest)
//    | _ -> None
//
//let private (|BlockCommentTokens|_|) (tokens: Token list) =
//    let rec collectTokens (rest: Token list) (finalContinuation: Token list -> Token list) : Token list * Token list =
//        match rest with
//        | CommentToken ct :: rest -> collectTokens rest (fun commentTokens -> ct :: commentTokens |> finalContinuation)
//        | _ -> finalContinuation [], rest
//
//    match tokens with
//    | CommentToken { Content = "(*" } :: _ ->
//        let comments, rest = collectTokens tokens id
//        Some(comments, rest)
//    | _ -> None
//
//let private (|MinusToken|_|) (token: Token) =
//    if token.TokenInfo.Tag = 62 then
//        Some token
//    else
//        None
//
//let private (|NumberToken|_|) (token: Token) =
//    if isNumber token then
//        Some token
//    else
//        None
//
//let rec private lastTwoItems
//    (project: 't -> 'ret)
//    (fallbackLastButOne: 'ret)
//    (fallbackLast: 'ret)
//    (items: 't list)
//    : 'ret * 'ret =
//    match items with
//    | [ f; s ] -> project f, project s
//    | [ s ] -> fallbackLast, project s
//    | [] -> fallbackLastButOne, fallbackLast
//    | _ :: tail -> lastTwoItems project fallbackLastButOne fallbackLast tail
//
//let rec private getTriviaFromTokensThemSelves
//    (mkRange: MkRange)
//    (lastButOneNonWhiteSpaceToken: Token option)
//    (lastNonWhiteSpaceToken: Token option)
//    (tokens: Token list)
//    foundTrivia
//    =
//    match tokens with
//    // Skip triple slash comments
//    | TripleSlashLineComment (rest) ->
//        getTriviaFromTokensThemSelves mkRange lastButOneNonWhiteSpaceToken lastNonWhiteSpaceToken rest foundTrivia
//
//    // Skip triple slash comments
//    | LineComments ({ Content = "///" } :: _, rest) ->
//        getTriviaFromTokensThemSelves mkRange lastButOneNonWhiteSpaceToken lastNonWhiteSpaceToken rest foundTrivia
//
//    | LineComments ({ LineNumber = headLineNumber } :: _ as commentTokens, rest) ->
//        let isAfterSourceCode =
//            match lastButOneNonWhiteSpaceToken, lastNonWhiteSpaceToken with
//            | Some otherLineToken, Some (SemicolonToken sc) when otherLineToken.LineNumber <> sc.LineNumber ->
//                // IDENT SEMICOLON LINE_COMMENT
//                // See https://github.com/fsprojects/fantomas/issues/1643
//                false
//            | _, Some t -> headLineNumber = t.LineNumber
//            | _ -> false
//
//        let info =
//            if isAfterSourceCode then
//                // Only capture the first line of the comment as LineCommentAfterSourceCode
//                // The next line(s) will be a LineCommentOnSingleLine
//                let commentsByLine =
//                    commentTokens
//                    |> List.groupBy (fun t -> t.LineNumber)
//
//                let firstComment = List.tryHead commentsByLine |> Option.map snd
//
//                match firstComment with
//                | Some (headToken :: _ as afterSourceTokens) ->
//                    let afterSourceCodeTrivia =
//                        let tc =
//                            collectComment afterSourceTokens
//                            |> LineCommentAfterSourceCode
//                            |> Comment
//
//                        let lastToken = List.tryLast afterSourceTokens
//
//                        let r = getRangeBetween mkRange headToken (Option.defaultValue headToken lastToken)
//
//                        Trivia.Create tc r
//
//                    let lineCommentOnSingleLine =
//                        if commentTokens.Length > afterSourceTokens.Length then
//                            let commentTokens =
//                                commentTokens
//                                |> List.skip afterSourceTokens.Length
//
//                            let triviaContent =
//                                collectComment commentTokens
//                                |> LineCommentOnSingleLine
//                                |> Comment
//
//                            let range =
//                                let headToken = List.head commentTokens
//                                let lastToken = List.tryLast commentTokens
//                                getRangeBetween mkRange headToken (Option.defaultValue headToken lastToken)
//
//                            Trivia.Create triviaContent range |> Some
//                        else
//                            None
//
//                    match lineCommentOnSingleLine with
//                    | Some lcsl -> afterSourceCodeTrivia :: lcsl :: foundTrivia
//                    | None -> afterSourceCodeTrivia :: foundTrivia
//                | _ ->
//                    // We should not hit this branch
//                    foundTrivia
//            else
//                let triviaContent =
//                    collectComment commentTokens
//                    |> LineCommentOnSingleLine
//                    |> Comment
//
//                let range =
//                    let headToken = List.head commentTokens
//                    let lastToken = List.tryLast commentTokens
//                    getRangeBetween mkRange headToken (Option.defaultValue headToken lastToken)
//
//                (Trivia.Create triviaContent range :: foundTrivia)
//
//        getTriviaFromTokensThemSelves mkRange lastButOneNonWhiteSpaceToken lastNonWhiteSpaceToken rest info
//
//    | BlockCommentTokens (headToken :: _ as blockCommentTokens, rest) ->
//        let comment =
//            let groupedByLineNumber =
//                blockCommentTokens
//                |> List.groupBy (fun t -> t.LineNumber)
//
//            let newLines =
//                let min, _ = List.minBy fst groupedByLineNumber
//                let max, _ = List.maxBy fst groupedByLineNumber
//
//                [ min..max ]
//                |> List.filter (fun l -> not (List.exists (fst >> ((=) l)) groupedByLineNumber))
//                |> List.map (fun l -> l, String.Empty)
//
//            groupedByLineNumber
//            |> List.map (fun (l, g) -> l, getContentFromTokens g)
//            |> (@) newLines
//            |> List.sortBy fst
//            |> List.map snd
//            |> String.concat Environment.NewLine
//            |> String.normalizeNewLine
//
//        let lastButOne, lastToken =
//            lastTwoItems Some lastNonWhiteSpaceToken (Some headToken) blockCommentTokens
//
//        let range =
//            getRangeBetween mkRange headToken (Option.defaultValue headToken lastToken)
//
//        let info =
//            Trivia.Create(Comment(BlockComment(comment, false, false))) range
//            |> List.prependItem foundTrivia
//
//        getTriviaFromTokensThemSelves mkRange lastButOne lastToken rest info
//
//    | KeywordOrOperatorToken koo :: rest ->
//        let range = getRangeBetween mkRange koo koo
//
//        let info =
//            Trivia.Create(Keyword(koo)) range
//            |> List.prependItem foundTrivia
//
//        getTriviaFromTokensThemSelves mkRange lastNonWhiteSpaceToken (Some koo) rest info
//
//    | HashTokens (hashTokens, rest) ->
//        let directiveContent =
//            let sb = StringBuilder()
//
//            hashTokens
//            |> List.fold
//                (fun (acc: StringBuilder, lastLine) token ->
//                    let sb =
//                        let delta = token.LineNumber - lastLine
//
//                        if delta > 0 then
//                            [ 1..delta ]
//                            |> List.fold (fun (sb: StringBuilder) _ -> sb.Append("\n")) acc
//                        else
//                            acc
//
//                    sb.Append(token.Content), token.LineNumber)
//                (sb, hashTokens.[0].LineNumber)
//            |> fun (sb, _) -> sb.ToString()
//
//        let range = getRangeBetween mkRange (List.head hashTokens) (List.last hashTokens)
//
//        let info =
//            Trivia.Create(Directive(directiveContent)) range
//            |> List.prependItem foundTrivia
//
//        getTriviaFromTokensThemSelves mkRange lastButOneNonWhiteSpaceToken lastNonWhiteSpaceToken rest info
//
//    | EndOfInterpolatedString (stringTokens, interpStringEnd, rest) ->
//        let stringContent =
//            let addExtraNewline =
//                match List.tryLast stringTokens with
//                | Some lst ->
//                    let delta = interpStringEnd.LineNumber - lst.LineNumber
//
//                    if delta > 0 then
//                        [ 1..delta ] |> List.map (fun _ -> "\n")
//                    else
//                        []
//                | _ -> []
//
//            [ yield! extractContentPreservingNewLines stringTokens
//              yield! addExtraNewline
//              yield interpStringEnd.Content ]
//            |> String.concat String.Empty
//
//        let range = getRangeBetween mkRange stringTokens.Head interpStringEnd
//
//        let info =
//            Trivia.Create(StringContent(stringContent)) range
//            |> List.prependItem foundTrivia
//
//        let prevButOne, prev = List.tryLast stringTokens, Some interpStringEnd
//
//        getTriviaFromTokensThemSelves mkRange prevButOne prev rest info
//
//    | StringText (head, stringTokens, rest, stringContent) ->
//        let lastButOne, lastToken = lastTwoItems Some None (Some head) stringTokens
//
//        let range = getRangeBetween mkRange head (Option.defaultValue head lastToken)
//
//        let info =
//            Trivia.Create(StringContent(stringContent)) range
//            |> List.prependItem foundTrivia
//
//        let nextRest =
//            match rest with
//            | [] -> []
//            | _ -> List.skip (List.length stringTokens - 1) rest
//
//        getTriviaFromTokensThemSelves mkRange lastButOne lastToken nextRest info
//
//    | MinusToken minus :: NumberToken number :: rest ->
//        let range = getRangeBetween mkRange minus number
//
//        let info =
//            Trivia.Create(Number(minus.Content + number.Content)) range
//            |> List.prependItem foundTrivia
//
//        getTriviaFromTokensThemSelves mkRange (Some minus) (Some number) rest info
//
//    | NumberToken number :: rest ->
//        let range = getRangeForSingleToken mkRange number
//
//        let info =
//            Trivia.Create(Number(number.Content)) range
//            |> List.prependItem foundTrivia
//
//        getTriviaFromTokensThemSelves mkRange lastNonWhiteSpaceToken (Some number) rest info
//
//    | DecompiledOperatorToken ident :: rest ->
//        let range = getRangeBetween mkRange ident ident
//
//        let info =
//            Trivia.Create(IdentOperatorAsWord ident.Content) range
//            |> List.prependItem foundTrivia
//
//        getTriviaFromTokensThemSelves mkRange lastNonWhiteSpaceToken (Some ident) rest info
//
//    | IdentBetweenTicksToken ident :: rest ->
//        let range = getRangeBetween mkRange ident ident
//
//        let info =
//            Trivia.Create(IdentBetweenTicks(ident.Content)) range
//            |> List.prependItem foundTrivia
//
//        getTriviaFromTokensThemSelves mkRange lastNonWhiteSpaceToken (Some ident) rest info
//
//    | CharToken head :: rest ->
//        let range = getRangeBetween mkRange head head
//
//        let info =
//            Trivia.Create(CharContent(head.Content)) range
//            |> List.prependItem foundTrivia
//
//        getTriviaFromTokensThemSelves mkRange lastNonWhiteSpaceToken (Some head) rest info
//
//    | EmbeddedILTokens (embeddedTokens, rest) ->
//        let content =
//            embeddedTokens
//            |> List.map (fun t -> t.Content)
//            |> String.concat String.Empty
//
//        let range =
//            let startT = embeddedTokens.Head
//            let endT = List.last embeddedTokens
//            // There is a one off problem in the range of SynExpr_LibraryOnlyILAssembly
//            mkRange (startT.LineNumber, startT.TokenInfo.LeftColumn) (endT.LineNumber, endT.TokenInfo.RightColumn + 1)
//
//        let info =
//            Trivia.Create(EmbeddedIL(content)) range
//            |> List.prependItem foundTrivia
//
//        let prevButOne, prev =
//            lastTwoItems Some lastButOneNonWhiteSpaceToken lastNonWhiteSpaceToken embeddedTokens
//
//        getTriviaFromTokensThemSelves mkRange prevButOne prev rest info
//
//    | NonWhiteSpaceToken h :: rest ->
//        let prevButOne = lastNonWhiteSpaceToken
//        let prev = Some h
//        getTriviaFromTokensThemSelves mkRange prevButOne prev rest foundTrivia
//
//    | _ :: rest ->
//        getTriviaFromTokensThemSelves mkRange lastButOneNonWhiteSpaceToken lastNonWhiteSpaceToken rest foundTrivia
//
//    | [] -> foundTrivia
//
let private createNewLine (mkRange: MkRange) lineNumber =
    let range = mkRange (lineNumber, 0) (lineNumber, 0)
    { Item = Newline; Range = range }
//
//let private findEmptyNewlinesInTokens
//    (mkRange: MkRange)
//    (tokens: Token list)
//    (ignoreRanges: FSharp.Compiler.Text.Range list)
//    =
//    let nonWhitespaceLines =
//        tokens
//        |> List.choose (fun t ->
//            if t.TokenInfo.Tag <> whiteSpaceTag then
//                Some t.LineNumber
//            else
//                None)
//        |> List.distinct
//
//    let ignoreRanges =
//        ignoreRanges
//        |> List.collect (fun r -> [ r.StartLine .. r.EndLine ])
//
//    let lastLineWithContent =
//        List.tryLast nonWhitespaceLines
//        |> Option.defaultValue 1
//
//    [ 1..lastLineWithContent ]
//    |> List.except (nonWhitespaceLines @ ignoreRanges)
//    |> List.map (createNewLine mkRange)
//

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

let private digitOrLetterCharRegex =
    System.Text.RegularExpressions.Regex(@"^'(\d|[a-zA-Z])'$")

let private (|CharToken|_|) (token: token) =
    match token with
    | CHAR _ -> Some()
    | _ -> None

let private onlyNumberRegex = System.Text.RegularExpressions.Regex(@"^\d+$")

let private (|NumberToken|_|) (token: token) =
    match token with
    | DECIMAL _
    | BIGNUM _
    | INT8 _
    | UINT8 _
    | INT16 _
    | UINT16 _
    | INT32 _
    | UINT32 _
    | INT64 _
    | UINT64 _
    | UNATIVEINT _
    | NATIVEINT _
    | IEEE32 _
    | IEEE64 _ -> Some()
    | _ -> None

let private getTokensFromSource (source: ISourceText) (defines: string list) : TokenWithRangeList =
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

    lex onToken defines source
    tokenCollector.Close()

let private parseHashLine (content: string) : string list * string list =
    let content =
        content
            .Trim()
            .Substring(3) // strip #if
            .Split([| "//" |], StringSplitOptions.RemoveEmptyEntries).[0] // strip any comments

    let source = SourceText.ofString content
    let tokens = getTokensFromSource source []

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

    // tokens |> List.iter (printfn "%A")
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
//    let defineTokens =
//    hashTokenContent.Close()
//    |> Seq.collect (fun hashContent ->
//        hashContent.Split([| "#if"; " "; "("; ")"; "&"; "|" |], StringSplitOptions.RemoveEmptyEntries))
//    |> Seq.map (fun s -> s.Trim())
//    |> Seq.distinct
//    |> Seq.toList


// hier

let private getIndividualDefine (hashLines: HashLine list) : string list list =
    hashLines
    |> List.collect (function
        | HashLine.If (words = words) -> words
        | _ -> [])
    // { TokenInfo = { TokenName = tn } } -> tn = "IDENT" || tn = "FALSE" || tn = "TRUE")
    // |> List.map (fun t -> t.Content)
    |> List.distinct
    |> List.map List.singleton

let private getDefineExprs (hashLines: HashLine list) =
    //    let parseHashContent (tokens: TokenWithRangeList) =
//        let allowedContent = set [ "||"; "&&"; "!"; "("; ")" ]
//
//        tokens
//        |> List.choose (fun (t,r) ->
//            match t with
//            | IDENT s -> Some s
//            | _ -> None)
////            t.TokenInfo.TokenName = "IDENT"
////            || t.TokenInfo.TokenName = "TRUE"
////            || t.TokenInfo.TokenName = "FALSE"
////            || Set.contains t.Content allowedContent)
////      |> Seq.map (fun t -> t.Content)
////        |> Seq.toList
//        |> BoolExprParser.parse

    //    let tokensByLine =
//        hashTokens
//        |> List.groupBy (fun (_,r) -> r.StartLine)
//        |> List.sortBy fst

    let result =
        (([], []), hashLines)
        ||> List.fold (fun (contextExprs, exprAcc) hashLine ->
            let contextExpr e =
                e :: contextExprs
                |> List.reduce (fun x y -> BoolExpr.And(x, y))

            //            let t =
//                lineTokens
//                |> Seq.tryFind (fun x -> x.TokenInfo.TokenName = "HASH_IF")

            // match t |> Option.map (fun x -> x.Content) with
            match hashLine with
            | HashLine.If (content = content) ->
                BoolExprParser.parse content
                // | Some "#if" ->
                // parseHashContent lineTokens
                //  content
                |> Option.map (fun e -> e :: contextExprs, contextExpr e :: exprAcc)
                |> Option.defaultValue (contextExprs, exprAcc)
            //| Some "#else" ->
            | HashLine.Else ->
                contextExprs,
                BoolExpr.Not(
                    contextExprs
                    |> List.reduce (fun x y -> BoolExpr.And(x, y))
                )
                :: exprAcc
            //| Some "#endif" -> List.tail contextExprs, exprAcc
            | HashLine.EndIf -> List.tail contextExprs, exprAcc)
        // | _ -> contextExprs, exprAcc)
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
    let tokens = getTokensFromSource source []

    // List.iter (printfn "%A") tokens

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
    (nodes: TriviaNodeAssigner list)
    (defineCombination: DefineCombination)
    : TriviaCollectionResult =
    let mutable triviaCollection = ListCollector<Trivia>()
    let mutable assignedContentItself = false

    let triviaFromSourceCaptured =
        nodes
        |> List.map (fun n ->
            match n.Type with
            | SynConst_String
            | SynConst_Bytes
            | SynInterpolatedStringPart_String ->
                assignedContentItself <- true
                let content = source.GetContentAt n.Range
                n.ContentItself <- Some(TriviaContent.StringContent content)
                n
            | SynExpr_LibraryOnlyILAssembly ->
                assignedContentItself <- true
                let content = source.GetContentAt n.Range
                n.ContentItself <- Some(TriviaContent.EmbeddedIL content)
                n
            | _ -> n)

    // tokens |> List.iter (printfn "%A")

    let creatDeadCodeDirective
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
                    let trivia = creatDeadCodeDirective node.StartRange node.IfCondition range "#else"
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

                    let trivia = creatDeadCodeDirective startRange startContent range "#endif"
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

                | NotCollecting _, CharToken ->
                    let content = source.GetContentAt range

                    if not (digitOrLetterCharRegex.IsMatch(content)) then
                        let trivia =
                            { Item = CharContent content
                              Range = range }

                        triviaCollection.Add trivia

                    { acc with CollectorState = NotCollecting tokenAndRange }

                | NotCollecting _, NumberToken ->
                    let content = source.GetContentAt range

                    if not (onlyNumberRegex.IsMatch(content)) then
                        let trivia = { Item = Number content; Range = range }
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

    { AssignedContentItself = assignedContentItself
      Trivia = triviaCollection.Close()
      Nodes = triviaFromSourceCaptured }

//    let fromTokens = getTriviaFromTokensThemSelves mkRange None None tokens []
//
//    let isMultilineString (s: string) = s.Contains("\n")
//
//    let ignoreRanges =
//        fromTokens
//        |> List.choose (fun tc ->
//            match tc.Item with
//            | Directive dc when (isMultilineString dc) -> Some tc.Range
//            | Comment (BlockComment _) -> Some tc.Range
//            | StringContent sc when (isMultilineString sc) -> Some tc.Range
//            | _ -> None)
//
//    let newLines = findEmptyNewlinesInTokens mkRange tokens ignoreRanges
//
//    fromTokens @ newLines
//    |> List.sortBy (fun t -> t.Range.StartLine, t.Range.StartColumn)
