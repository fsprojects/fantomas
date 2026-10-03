let valueOne =
    myCe {
        let a = getA ()
        let! b = getB ()
        and! bb = getBB ()
        do c
        do! d
        return 42
    }

let valueTwo =
    myCe {
        let a = getA ()
        let! b = getB ()
        and! bb = getBB ()
        do c
        do! d
        return! getE ()
    }
