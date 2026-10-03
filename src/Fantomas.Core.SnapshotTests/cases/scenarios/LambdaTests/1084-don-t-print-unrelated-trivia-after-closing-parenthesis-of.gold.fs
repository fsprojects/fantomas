let private tokenizeLines (sourceTokenizer: FSharpSourceTokenizer) allLines state =
    allLines
    |> List.mapi (fun index line -> line, (index + 1)) // line number is needed in tokenizeLine
    |> List.fold
        (fun (state, tokens) (line, lineNumber) ->
            let tokenizer = sourceTokenizer.CreateLineTokenizer(line)
            let nextState, tokensOfLine = tokenizeLine tokenizer allLines state lineNumber []

            let allTokens = List.append tokens (List.rev tokensOfLine) // tokens of line are add in reversed order
            (nextState, allTokens))
        (state, []) // empty tokens to start with
    |> snd // ignore the state
