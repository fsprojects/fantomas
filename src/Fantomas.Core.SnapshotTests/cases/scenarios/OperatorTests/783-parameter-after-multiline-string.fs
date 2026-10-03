let ``match bang`` () =
    formatSourceString false """
async {
    match! myAsyncFunction() with
    | Some x -> printfn "%A" x
    | None -> printfn "Function returned None!"
}"""   config
    |> prepend newline
    |> should equal """
async {
    match! myAsyncFunction () with
    | Some x -> printfn "%A" x
    | None -> printfn "Function returned None!"
}
"""
