try
    foo ()
with ex -> {
    astContext with
        IsInsideMatchClausePattern = true
}
