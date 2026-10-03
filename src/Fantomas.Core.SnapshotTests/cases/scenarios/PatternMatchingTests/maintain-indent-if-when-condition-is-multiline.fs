(*---
fsharp_max_infix_operator_expression = 50
---*)
    match foo with
    | headToken :: rest when (isOperatorOrKeyword headToken && List.exists (fun k -> headToken.TokenInfo.TokenName = k) keywordTrivia) ->
          let range =
              getRangeBetween "keyword" headToken headToken

          let info =
              Trivia.Create(Keyword(headToken)) range
              |> List.prependItem foundTrivia

          getTriviaFromTokensThemSelves allTokens rest info
