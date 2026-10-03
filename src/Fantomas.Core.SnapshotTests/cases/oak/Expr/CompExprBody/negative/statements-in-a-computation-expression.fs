let work =
    async {
        let! data = fetch ()
        do! save data
        return data
    }
