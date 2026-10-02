promise {
    setItems [||]
    setFetchingItems true
    let! items = Api.fetchItems partNumber
    setFetchingItems false
}
|> Promise.start
