module internal Fantomas.TokenParser

open FSharp.Compiler.AbstractIL.Internal.Library
open System
open FSharp.Compiler.SourceCodeServices
open Fantomas
open Fantomas.TriviaTypes

// workaround for cases where tokenizer dont output "delayed" part of operator after ">."
// See https://github.com/fsharp/FSharp.Compiler.Service/issues/874
let private isTokenAfterGreater token (greaterToken: Token) =
    let greaterToken = greaterToken.TokenInfo
    greaterToken.TokenName = "GREATER" && token.TokenName <> "GREATER" && greaterToken.RightColumn <> (token.LeftColumn + 1)

let private getTokenText (sourceCodeLines: string list) line (token: FSharpTokenInfo) =
    sourceCodeLines.[line - 1].Substring(token.LeftColumn, token.RightColumn - token.LeftColumn + 1)
    |> String.normalizeNewLine

/// Tokenize a single line of F# code
let rec private tokenizeLine (tokenizer:FSharpLineTokenizer) sourceCodeLines state lineNumber tokens =
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
          { TokenInfo = extraTokenInfo; LineNumber = lineNumber; Content = getTokenText sourceCodeLines lineNumber extraTokenInfo }
          
      let token =
          { TokenInfo = tok; LineNumber = lineNumber; Content = getTokenText sourceCodeLines lineNumber tok }
      
      tokenizeLine tokenizer sourceCodeLines state lineNumber ([token;extraToken] @ tokens)
      
  | (Some tok, state), _ ->
      let token: Token = { TokenInfo = tok; LineNumber = lineNumber; Content = getTokenText sourceCodeLines lineNumber tok }
      // Tokenize the rest, in the new state
      tokenizeLine tokenizer sourceCodeLines state lineNumber (token::tokens)

  | (None, state), _ ->
      state, tokens
  
let private tokenizeLines (sourceTokenizer: FSharpSourceTokenizer) allLines state =
  allLines
  |> List.mapi (fun index line -> line, (index + 1)) // line number is needed in tokenizeLine
  |> List.fold (fun (state, tokens) (line, lineNumber) ->
      let tokenizer = sourceTokenizer.CreateLineTokenizer(line)
      let nextState, tokensOfLine =
          tokenizeLine tokenizer allLines state lineNumber []
      
      let allTokens = List.append tokens (List.rev tokensOfLine) // tokens of line are add in reversed order
      (nextState, allTokens)
  ) (state, []) // empty tokens to start with
  |> snd // ignore the state

let private createHashToken lineNumber content fullMatchedLength offset =
    let (left,right) = offset, String.length content + offset

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
            FullMatchedLength = fullMatchedLength } }


let rec private getTokenizedHashes sourceCode =
    let processLine content (trimmed:string) lineNumber fullMatchedLength offset =
        let contentLength = String.length content
        tokenize [] (trimmed.Substring(contentLength))
        |> fst
        |> List.map (fun t ->
            let info =
                { t.TokenInfo with
                        LeftColumn = t.TokenInfo.LeftColumn + contentLength
                        RightColumn = t.TokenInfo.RightColumn + contentLength }
            { t with
                LineNumber = lineNumber
                TokenInfo = info }
        )
        |> fun rest -> (createHashToken lineNumber content fullMatchedLength offset)::rest

    sourceCode
    |> String.normalizeThenSplitNewLine
    |> Array.indexed
    |> Array.map (fun (idx, line) ->
        let lineNumber  = idx + 1
        let fullMatchedLength = String.length line
        let trimmed = line.TrimStart()
        let offset = String.length line - String.length trimmed

        if trimmed.StartsWith("#if") then
            processLine "#if" trimmed lineNumber fullMatchedLength offset
        elif trimmed.StartsWith("#elseif") then
            processLine "#elseif" trimmed lineNumber fullMatchedLength offset
        elif trimmed.StartsWith("#else") then
            processLine "#else" trimmed lineNumber fullMatchedLength offset
        elif trimmed.StartsWith("#endif") then
            processLine "#endif" trimmed lineNumber fullMatchedLength offset
        else
            []
    )
    |> Seq.collect id
    |> Seq.toList

and tokenize defines (content : string) : Token list * int =
    let sourceTokenizer = FSharpSourceTokenizer(defines, Some "/tmp.fsx")
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
        if content.Contains("#") then
            let hashes =
                getTokenizedHashes content

            let filteredHashes =
                hashes
                |> List.filter (fun t -> not(List.contains t.LineNumber existingLines))
                // filter hashes that are present in source code parsed by the Tokenizer.
            tokens @ filteredHashes
            |> List.sortBy (fun t-> t.LineNumber, t.TokenInfo.LeftColumn)
        else
            tokens

    combined, List.length lines
    
let getDefines sourceCode =
    getTokenizedHashes sourceCode
    |> List.filter (fun {TokenInfo = {TokenName = tn }} -> tn = "IDENT")
    |> List.map (fun t -> t.Content)
    |> List.distinct

let private getRangeBetween name startToken endToken =
    let l = startToken.TokenInfo.LeftColumn
    let r = endToken.TokenInfo.RightColumn
    let start = FSharp.Compiler.Range.mkPos startToken.LineNumber l
    let endR = FSharp.Compiler.Range.mkPos endToken.LineNumber (if l=r then r+1 else r)
    FSharp.Compiler.Range.mkRange name start endR

let private hasOnlySpacesAndLineCommentsOnLine lineNumber tokens =
    if List.isEmpty tokens then
        false
    else
        tokens
        |> List.filter (fun t -> t.LineNumber = lineNumber)
        |> List.forall (fun t -> t.TokenInfo.TokenName = "WHITESPACE" || t.TokenInfo.TokenName = "LINE_COMMENT")
    
let private getContentFromTokens tokens =
    tokens
    |> List.map (fun t -> t.Content)
    |> String.concat String.Empty
    
let private keywordTrivia = ["IF"; "ELIF"]

let rec private getTriviaFromTokensThemSelves (allTokens: Token list) (tokens: Token list) foundTrivia =
    match tokens with
    | headToken::rest when (headToken.TokenInfo.TokenName = "LINE_COMMENT") ->
        let lineCommentTokens =
            rest
            |> List.takeWhile (fun t -> t.TokenInfo.TokenName = "LINE_COMMENT" && t.LineNumber = headToken.LineNumber)
            
        let comment =
            headToken
            |> List.prependItem lineCommentTokens
            |> getContentFromTokens
            
        let nextTokens =
            List.length lineCommentTokens
            |> fun length -> List.skip length rest
            
        let range =
            let lastToken = List.last lineCommentTokens
            getRangeBetween "line comment" headToken lastToken
            
        let info =
            let toLineComment =
                allTokens
                |> List.exists (fun t -> t.LineNumber = headToken.LineNumber && t.TokenInfo.TokenName <> "WHITESPACE" && t.TokenInfo.RightColumn < headToken.TokenInfo.LeftColumn)
                |> fun e -> if e then LineCommentAfterSourceCode else LineCommentOnSingleLine
            
            let comment =
                toLineComment comment
                |> Comment
            
            Trivia.Create comment range
            |> List.appendItem foundTrivia
            
        getTriviaFromTokensThemSelves allTokens nextTokens info
        
    | headToken::rest when (headToken.TokenInfo.TokenName = "COMMENT") ->
        let blockCommentTokens =
            rest
            |> List.takeWhileState (fun depth t ->
                let newDepth = match t.Content with | "(*" -> depth+1 | "*)" -> depth-1 | _ -> depth
                newDepth, t.TokenInfo.TokenName = "COMMENT" && depth > 0) 1
            
        let comment =
            headToken
            |> List.prependItem blockCommentTokens
            |> List.groupBy (fun t -> t.LineNumber)
            |> List.map (fun (_, g) -> getContentFromTokens g)
            |> String.concat Environment.NewLine
            |> String.normalizeNewLine
            
        let nextTokens =
            List.length blockCommentTokens
            |> fun length -> List.skip length rest
            
        let range =
            let lastToken = List.last blockCommentTokens
            getRangeBetween "block comment" headToken lastToken
            
        let info =
            Trivia.Create (Comment(BlockComment(comment))) range
            |> List.prependItem foundTrivia
            
        getTriviaFromTokensThemSelves allTokens nextTokens info
        
    | headToken::rest when (headToken.TokenInfo.CharClass = FSharp.Compiler.SourceCodeServices.FSharpTokenCharKind.Keyword &&
                            List.exists (fun k -> headToken.TokenInfo.TokenName = k) keywordTrivia) ->
        let range = getRangeBetween "keyword" headToken headToken
        let info =
            Trivia.Create (Keyword(headToken.Content)) range
            |> List.prependItem foundTrivia

        getTriviaFromTokensThemSelves allTokens rest info
        
    | headToken::rest when (headToken.TokenInfo.TokenName = "HASH_IF") ->
        let directiveTokens =
            rest
            |> List.filter (fun r -> r.LineNumber = headToken.LineNumber)
            |> fun others -> List.prependItem others headToken
            
        let directiveContent =
            directiveTokens
            |> List.map (fun t -> t.Content)
            |> String.concat System.String.Empty
            
        let range = getRangeBetween "directive" headToken (List.last directiveTokens)
        let info =
            Trivia.Create (Directive(directiveContent, false (*false for now, later determined in Trivia.fs*))) range
            |> List.prependItem foundTrivia
        
        let nextRest =
            match rest with
            | [] -> []
            | _ ->
                List.skip (List.length directiveTokens - 1) rest

        getTriviaFromTokensThemSelves allTokens nextRest info

    | (_)::rest -> getTriviaFromTokensThemSelves allTokens rest foundTrivia
    
    | [] -> foundTrivia

let private createNewLine lineNumber =
    let pos = FSharp.Compiler.Range.mkPos lineNumber 0
    let range = FSharp.Compiler.Range.mkRange "newline" pos pos
    { Item = Newline; Range = range }

let private findEmptyNewlinesInTokens (tokens: Token list) (lineCount) =
    let lastLineWithContent =
        tokens
        |> List.tryFindBack (fun t -> t.TokenInfo.TokenName <> "WHITESPACE")
        |> Option.map (fun t -> t.LineNumber)
        |> Option.defaultValue lineCount

    let completeEmptyLines =
        [1 .. lastLineWithContent]
        |> List.filter (fun line ->
            not (List.exists (fun t -> t.LineNumber = line) tokens)
        )
        |> List.map (fun line -> createNewLine line)

    let linesWithOnlySpaces =
        tokens
        |> List.groupBy (fun t -> t.LineNumber)
        |> List.filter (fun (ln, g) -> ln <= lastLineWithContent && (List.length g) = 1 && (List.head g).TokenInfo.TokenName = "WHITESPACE")
        |> List.map (fst >> createNewLine)
        
    completeEmptyLines @ linesWithOnlySpaces

let getTriviaFromTokens (tokens: Token list) linesCount =
    let fromTokens = getTriviaFromTokensThemSelves tokens tokens []
    let newLines = findEmptyNewlinesInTokens tokens linesCount

    fromTokens @ newLines
    |> List.sortBy (fun t -> t.Range.StartLine, t.Range.StartColumn)
    
let private tokenNames = ["LBRACE";"RBRACE"; "LPAREN";"RPAREN"; "EQUALS"; "ELSE"; "BAR"]
let private tokenKinds = [FSharpTokenCharKind.Operator]
    
let getTriviaNodesFromTokens (tokens: Token list) : TriviaNode list =
    tokens
    |> List.filter (fun t -> List.exists (fun tn -> tn = t.TokenInfo.TokenName) tokenNames || List.exists (fun tk -> tk = t.TokenInfo.CharClass) tokenKinds)
    |> List.map (fun t ->
        { Type = TriviaNodeType.Token(t)
          ContentBefore = []
          ContentAfter = []
          Range = getRangeBetween t.TokenInfo.TokenName t t }
    )