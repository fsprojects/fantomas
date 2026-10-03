let myCollection =
    seq {
        let! squares = getSquares ()
        yield! (squares * level)
    }
