task {
    let! abc = def ()

    and! meh = task {
        // comment
        return 42
    }

    ()
}
