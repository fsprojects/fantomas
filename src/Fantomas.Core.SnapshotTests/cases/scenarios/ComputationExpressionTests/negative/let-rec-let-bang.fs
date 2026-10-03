let a =
    async {
        let rec foo a = foo a
        let! bar = async { return foo a }
        return bar
    }
