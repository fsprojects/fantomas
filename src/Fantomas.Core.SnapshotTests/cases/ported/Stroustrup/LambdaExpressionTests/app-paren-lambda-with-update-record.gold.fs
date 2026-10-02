List.map (fun x -> {
    astContext with
        IsInsideMatchClausePattern = true
})
