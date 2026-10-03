match x with
| _ -> {
    astContext with
        IsInsideMatchClausePattern = true
  }
