module Fantomas.DefineParser
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