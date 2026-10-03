/// Find out the end token
let rec getEndCol (r: Range) (tokenizer: FSharpLineTokenizer) lexState =
    match tokenizer.ScanToken(!lexState) with
    | Some (tok), state ->
        Debug.WriteLine("End token: {0}", sprintf "%A" tok |> box)
        if tok.RightColumn >= r.EndColumn
           && isSignificantToken tok then
            tok.RightColumn
        else
        lexState := state
        getEndCol r tokenizer lexState
    | None, _ -> r.EndColumn
