let appendEvents userId (events: Event list) =
    let cosmoEvents = List.map (createEvent userId) events
    task { do! appendToAzureTableStorage cosmoEvents }
