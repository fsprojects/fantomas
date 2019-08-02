module Fantomas.TokenParser

open FSharp.Compiler.AbstractIL.Internal.Library
open System
open System.Text
open FSharp.Compiler.SourceCodeServices
open Fantomas
open Fantomas.TriviaTypes

[<RequireQualifiedAccess>]
type BoolExpr =
    | Ident of string
    | And of BoolExpr * BoolExpr
    | Or of BoolExpr * BoolExpr
    | Not of BoolExpr

module BoolExpr =
    let rec map f e =
        match f e with
        | Some x -> x
        | None ->
        match e with
        | BoolExpr.Not e -> BoolExpr.Not (map f e)
        | BoolExpr.And (e1, e2) -> BoolExpr.And (map f e1, map f e2)
        | BoolExpr.Or (e1, e2) -> BoolExpr.Or (map f e1, map f e2)
        | _ -> e
        
    let rec forall f e =
        f e &&
        match e with
        | BoolExpr.Not e -> forall f e
        | BoolExpr.And (e1, e2)
        | BoolExpr.Or (e1, e2) -> forall f e1 && forall f e2
        | _ -> true
    
    let normalizeCNF expr =
        let mapUntilNotChanged mapFunctions expr =
            let oneStep e = mapFunctions |> Seq.fold (fun e f -> map f e) e
            expr |> Seq.unfold (fun e -> let e' = oneStep e in if e = e' then None else Some (e',e')) |> Seq.tryLast |> Option.defaultValue expr
        let doubleNegative = function | BoolExpr.Not(BoolExpr.Not(e)) -> Some e | _ -> None
        let deMorgan = function
            | BoolExpr.Not(BoolExpr.And(e1, e2)) -> Some (BoolExpr.Or(BoolExpr.Not e1, BoolExpr.Not e2))
            | BoolExpr.Not(BoolExpr.Or(e1, e2)) -> Some (BoolExpr.And(BoolExpr.Not e1, BoolExpr.Not e2))
            | _ -> None
        let expandOr = function
            | BoolExpr.Or(e1, BoolExpr.And(e2, e3))
            | BoolExpr.Or(BoolExpr.And(e2, e3),e1) -> Some (BoolExpr.And(BoolExpr.Or(e1, e2), BoolExpr.Or(e1, e3)))
            | _ -> None
        expr |> mapUntilNotChanged [doubleNegative; deMorgan; expandOr]
        
    type Literal = Positive of string | Negative of string
    type SATSolveResult = Satisfiable of string list | Unsatisfiable | Unconclusive
    // result: list of AND-connected terms; term - OR-connected Literals
    let toFlatCNF expr =
        let e = expr |> normalizeCNF
        let rec toAndList = function
            | BoolExpr.And (e1, e2) -> toAndList e1 @ toAndList e2
            | e -> [e]
        let rec toOrList = function
            | BoolExpr.Or (e1, e2) -> toOrList e1 @ toOrList e2
            | e -> [e]
        let splitByNeg xs =
            xs |> List.map (function | BoolExpr.Not (BoolExpr.Ident x) -> Negative  x | BoolExpr.Ident x -> Positive x | _ -> failwithf "Expr not in CNF: %A" e)
            |> set
        e |> toAndList |> List.map toOrList |> List.map splitByNeg
    
    let eval cnf vals =
        let vals = set vals
        let evalTerm s = Set.intersect vals s |> Set.isEmpty |> not
        cnf |> List.forall evalTerm
        |> Dbg.tee (printfn "eval %A %A %A" cnf vals)
    
    let trySolveSAT maxSteps cnf =
        let allLiterals = cnf |> Seq.collect id |> Seq.toList
        let groupedLiterals ls =
            ls |> Seq.groupBy (function | Positive x | Negative x -> x)
            |> Seq.map (fun (key, g) -> key, g |> Seq.distinct |> Seq.toList)
            |> Seq.toList
        let enforcedLit = cnf |> List.filter (fun t -> Set.count t = 1) |> List.collect Set.toList
        if groupedLiterals enforcedLit |> Seq.exists (fun (_,g) -> List.length g > 1) then Unsatisfiable
        else
        let (singletons, toSolve) = groupedLiterals allLiterals |> List.partition (fun (_,g) -> List.length g = 1)
        let singletonsLit = singletons |> List.collect snd
        let solvedSet = set (enforcedLit @ singletonsLit)
        let toSolve = toSolve |> List.filter (fun (k,_) -> not(Set.contains (Positive k) solvedSet || Set.contains (Negative k) solvedSet))
        let rec genCombinations groups = seq {
            match groups with
            | [] -> yield []
            | g::gs -> yield! genCombinations gs |> Seq.collect (fun l -> g |> Seq.map (fun x -> x :: l))
        }
        let solve vals groups =
            let combinations = genCombinations (groups |> List.map snd)
            combinations |> Seq.mapi (fun i comb -> 
                if i > maxSteps then Some Unconclusive else
                let vals = vals @ comb
                if eval cnf vals then Some (Satisfiable (vals |> List.choose (function | Positive x -> Some x | _ -> None))) else None)
            |> Seq.tryPick id |> Option.defaultValue Unsatisfiable
        solve (singletonsLit @ enforcedLit) toSolve
        
    let mergeBoolExprs maxSolveSteps exprs =
        let solve e = e |> toFlatCNF |> trySolveSAT maxSolveSteps |> function | Satisfiable x -> Some x | _ -> None
        let pairSolve e1 e2 = BoolExpr.And(e1, e2) |> solve //|> Dbg.tee (fun r -> printfn "%A: %A" (BoolExpr.And(e1, e2)) r)
        let allPairs xs = xs |> Seq.mapi (fun i x -> xs |> Seq.mapi (fun j y -> if i < j then Some (x, y) else None)) |> Seq.collect id |> Seq.choose id
        let exprs = exprs |> List.map (fun x -> x, None)
        let rec f exprs =
            let exprsIndexed = exprs |> Seq.mapi (fun i x -> i, x)
            match exprsIndexed |> allPairs |> Seq.tryPick (fun ((i,(x,_)),(j,(y,_))) -> pairSolve x y |> Option.map (fun r -> (i, x), (j, y), r)) with
            | None -> exprs
            | Some ((i, x), (j, y), r) -> f ((exprsIndexed |> Seq.filter (fun (k,_) -> i<>k && j<>k) |> Seq.map snd |> Seq.toList) @ [ BoolExpr.And(x, y), Some r ])
        f exprs |> List.choose (fun (e, r) -> r |> Option.orElseWith (fun () -> solve e) |> Option.map (fun x -> e, x))
        
    let solveDefinesForExpr maxSolveSteps e =
        e |> toFlatCNF |> trySolveSAT maxSolveSteps |> function | Satisfiable x -> Some x | _ -> None 
    
module BoolExprParser =
    let (|Eq|_|) x y = if x=y then Some() else None

    let (|TakeUntil|_|) x xs =
        match List.takeWhile ((<>)x) xs with
        | y when y = xs -> None
        | y -> Some (y, List.skipWhile ((<>)x) xs)

    let (|ListSurround|_|) before after xs =
        let rec f d acc xs =
            match xs with
            | _ when d < 0 -> None
            | Eq before :: rest -> f (d+1) (before::acc) rest
            | Eq after :: [] when d = 1 ->
                List.rev acc |> Some
            | Eq after :: rest -> f (d-1) (after::acc) rest
            | x :: rest -> f d (x::acc) rest
            | _ -> None
        match xs with
        | Eq before :: rest -> f 1 [] rest
        | _ -> None

    let (|ListSplit|_|) split xs =
        match xs with
        | TakeUntil split (x1, (_ :: x2)) -> Some (x1, x2)
        | _ -> None
        
    let (|ListSplitPick|_|) split f xs =
        let rec loop prev xs = seq {
            match xs with
            | TakeUntil split (x1, (_ :: x2)) ->
                yield (prev @ x1, x2)
                yield! loop (prev @ x1 @ [split]) x2
            | _ -> () }
        loop [] xs |> Seq.tryPick f

    let rec (|SubExpr|_|) = function
        | ListSurround "(" ")" (ExprPat e) -> Some e
        | _ -> None
    and (|AndExpr|_|) =
        let chooser = function |(ExprPat e1, ExprPat e2) -> Some (e1, e2) |_ -> None
        function
        | ListSplitPick "&&" chooser (e1, e2) -> Some (BoolExpr.And (e1, e2))
        | _ -> None
    and (|OrExpr|_|) =
        let chooser = function |(ExprPat e1, ExprPat e2) -> Some (e1, e2) |_ -> None
        function
        | ListSplitPick "||" chooser (e1, e2) -> Some (BoolExpr.Or (e1, e2))
        | _ -> None
    and (|NotSubExpr|_|) = function
        | "!" :: SubExpr e -> Some (BoolExpr.Not e)
        | _ -> None
    and (|NotIdentExpr|_|) = function
        | "!" :: x :: [] -> Some (BoolExpr.Not (BoolExpr.Ident x))
        | _ -> None

    and (|ExprPat|_|) = function
        | NotSubExpr e
        | SubExpr e
        | AndExpr e
        | OrExpr e
        | NotIdentExpr e
            -> Some e
        | x :: [] -> BoolExpr.Ident x |> Some
        | _ -> None
        
    let parse = function | [] -> None | ExprPat e -> Some e | s -> failwithf "Fail to parse bool expr in #if: %A" s
    
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
        let tokens = tokenize [] (trimmed.Substring(contentLength)) |> fst
        tokens
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

let getDefineExprs sourceCode =
    let parseHashContent tokens =
        let allowedContent = set ["||"; "&&"; "!"; "("; ")"]
        tokens |> Seq.filter (fun t -> t.TokenInfo.TokenName = "IDENT" || Set.contains t.Content allowedContent)
        |> Seq.map (fun t -> t.Content) |> Seq.toList
        |> BoolExprParser.parse
    
    let tokens = getTokenizedHashes sourceCode
    let tokensByLine = tokens |> List.groupBy (fun t -> t.LineNumber) |> List.sortBy fst
    let result =
        (([],[]), tokensByLine) ||> List.fold (fun (contextExprs, exprAcc) (_, lineTokens) ->
            let contextExpr e = e :: contextExprs |> List.reduce (fun x y -> BoolExpr.And(x,y))
            let t = lineTokens |> Seq.tryFind (fun x -> x.TokenInfo.TokenName = "HASH_IF")
            match t |> Option.map (fun x -> x.Content) with
            | Some "#if" ->
                parseHashContent lineTokens |> Option.map (fun e -> e::contextExprs, contextExpr e :: exprAcc)
                |> Option.defaultValue (contextExprs, exprAcc)
            | Some "#else" ->
                contextExprs, BoolExpr.Not (contextExprs |> List.reduce (fun x y -> BoolExpr.And(x,y))) :: exprAcc
            | Some "#endif" ->
                List.tail contextExprs, exprAcc
            | _ -> contextExprs, exprAcc)
        |> snd |> List.rev
    result
    
let getOptimizedDefinesSets sourceCode =
    let maxSteps = 5
    getDefineExprs sourceCode |> BoolExpr.mergeBoolExprs maxSteps |> List.map snd

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
    
let private keywordTrivia = ["IF"; "ELIF"; "OVERRIDE"; "MEMBER"; "DEFAULT"; "KEYWORD_STRING"; "QMARK"]
let private numberTrivia = ["UINT8";"INT8";"UINT16";"INT16";"UINT32";"INT32";"UINT64";"IEEE32";
                            "DECIMAL";"IEEE64";"BIGNUM";"NATIVEINT";"UNATIVEINT"]

let private isOperatorOrKeyword ({TokenInfo = {CharClass = cc}}) =
    cc = FSharp.Compiler.SourceCodeServices.FSharpTokenCharKind.Keyword || cc = FSharp.Compiler.SourceCodeServices.FSharpTokenCharKind.Operator

let private isNumber ({TokenInfo = tn}) =
    tn.ColorClass = FSharp.Compiler.SourceCodeServices.FSharpTokenColorKind.Number && List.contains tn.TokenName numberTrivia
    
let private identIsDecompiledOperator (token: Token) =
    let decompiledName = FSharp.Compiler.PrettyNaming.DecompileOpName token.Content
    token.TokenInfo.TokenName = "IDENT" && decompiledName <> token.Content

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
            let lastToken = List.tryLast lineCommentTokens
            getRangeBetween "line comment" headToken (Option.defaultValue headToken lastToken)
            
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
            let groupedByLineNumber =
                headToken
                |> List.prependItem blockCommentTokens
                |> List.groupBy (fun t -> t.LineNumber)
                
            let newLines =
                let (min,_) = List.minBy fst groupedByLineNumber
                let (max,_) = List.maxBy fst groupedByLineNumber
                [min .. max]
                |> List.filter (fun l -> not (List.exists (fst >> ((=) l)) groupedByLineNumber))
                |> List.map (fun l -> l, System.String.Empty)

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
            getRangeBetween "block comment" headToken lastToken
            
        let info =
            Trivia.Create (Comment(BlockComment(comment))) range
            |> List.prependItem foundTrivia
            
        getTriviaFromTokensThemSelves allTokens nextTokens info
        
    | headToken::rest when (isOperatorOrKeyword headToken &&
                            List.exists (fun k -> headToken.TokenInfo.TokenName = k) keywordTrivia) ->
        let range = getRangeBetween "keyword" headToken headToken
        let info =
            Trivia.Create (Keyword(headToken)) range
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

    | head::rest when (head.TokenInfo.TokenName = "STRING_TEXT") ->
        let stringTokens =
            rest
            |> List.takeWhile (fun ({TokenInfo = {TokenName = tn}}) -> tn = "STRING_TEXT" || tn = "STRING")
            |> fun others -> List.prependItem others head

        let stringContent =
            let builder = StringBuilder()
            stringTokens
            |> List.fold(fun (b: StringBuilder, currentLine) st ->
                if currentLine <> st.LineNumber then
                    b.Append("\n").Append(st.Content), st.LineNumber
                else
                    b.Append(st.Content), st.LineNumber
            ) (builder, head.LineNumber)
            |> fst
            |> fun b -> b.ToString()

        let lastToken =
            List.tryLast stringTokens
            |> Option.defaultValue head

        let range = getRangeBetween "string content" head lastToken
        let info =
            Trivia.Create (StringContent(stringContent)) range
            |> List.prependItem foundTrivia

        let nextRest =
            match rest with
            | [] -> []
            | _ ->
                List.skip (List.length stringTokens - 1) rest

        getTriviaFromTokensThemSelves allTokens nextRest info

    | head::rest when (isNumber head) ->
        let range = getRangeBetween "number" head head
        let info =
            Trivia.Create (Number(head.Content)) range
            |> List.prependItem foundTrivia

        getTriviaFromTokensThemSelves allTokens rest info
        
    | head::rest when (identIsDecompiledOperator head) ->
        let range = getRangeBetween "operator as word" head head
        let info =
            Trivia.Create (IdentOperatorAsWord head.Content) range
            |> List.prependItem foundTrivia
        getTriviaFromTokensThemSelves allTokens rest info

    | (_)::rest -> getTriviaFromTokensThemSelves allTokens rest foundTrivia
    
    | [] -> foundTrivia

let private createNewLine lineNumber =
    let pos = FSharp.Compiler.Range.mkPos lineNumber 0
    let range = FSharp.Compiler.Range.mkRange "newline" pos pos
    { Item = Newline; Range = range }

let private findEmptyNewlinesInTokens (tokens: Token list) (lineCount) (blockComments: FSharp.Compiler.Range.range list) =
    let lastLineWithContent =
        tokens
        |> List.tryFindBack (fun t -> t.TokenInfo.TokenName <> "WHITESPACE")
        |> Option.map (fun t -> t.LineNumber)
        |> Option.defaultValue lineCount

    let completeEmptyLines =
        [1 .. lastLineWithContent]
        |> List.filter (fun line ->
            not (List.exists (fun t -> t.LineNumber = line) tokens) && not (List.exists (fun (br:FSharp.Compiler.Range.range) -> br.StartLine < line && br.EndLine > line) blockComments)
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
    let blockComments = fromTokens |> List.choose (fun tc -> match tc.Item with | Comment(BlockComment(_)) -> Some tc.Range | _ -> None)
    let newLines = findEmptyNewlinesInTokens tokens linesCount blockComments

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