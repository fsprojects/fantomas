let fetch () =
    async {
        let! data = load ()
        return data
    }
