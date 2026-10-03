let foo () =
    async {
        let! a = callA ()
        let b = callB ()
        let! c = callC ()
        let d = callD ()
        let! e = callE ()
        return (a + b + c - e * d)
    }
