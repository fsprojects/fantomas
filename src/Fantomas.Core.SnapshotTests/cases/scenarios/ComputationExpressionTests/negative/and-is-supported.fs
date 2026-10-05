async {
    let! x = Async.Sleep 1.
    and! y = Async.Sleep 2.
    return 10
}
