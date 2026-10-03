let fetchAsync (name, url: string) =
    async {
        let uri = new System.Uri(url)
        let webClient = new WebClient()
        let! html = webClient.AsyncDownloadString(uri)
        let title = html.CssSelect("title")
        return title
    }
