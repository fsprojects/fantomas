let getEvents() =
    task {
        let! cosmoEvents = eventStore.GetEvents EventStream AllEvents
        let events = List.map (fun (ce: EventRead<JsonValue, _>) -> ce.Data) cosmoEvents
        return events
    }
