let describe value =
    match value with
    | Some x as whole -> whole
    | None -> None
