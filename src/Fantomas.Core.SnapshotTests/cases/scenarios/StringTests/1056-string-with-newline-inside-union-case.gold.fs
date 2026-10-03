[<Test>]
let ``newline in string`` () =
    let source =
        "\"
\""

    let triviaNodes =
        tokenize [] source
        |> getTriviaFromTokens
        |> List.filter (fun { Item = item } ->
            match item with
            | StringContent("\"\n\"") -> true
            | _ -> false)

    List.length triviaNodes == 1
