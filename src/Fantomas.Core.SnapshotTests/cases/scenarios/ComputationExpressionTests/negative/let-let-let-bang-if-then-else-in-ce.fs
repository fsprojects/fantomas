(*---
fsharp_max_if_then_else_short_width = 75
---*)
let rec private appendToAzureTableStorage (cosmoEvents: EventWrite<JsonValue> seq) =
    task {
        let moreThanBatchLimit = Seq.length cosmoEvents > BatchLimit

        let batch =
            if moreThanBatchLimit then Seq.take BatchLimit cosmoEvents else cosmoEvents
            |> List.ofSeq

        let! _ = eventStore.AppendEvents EventStream Any batch

        if moreThanBatchLimit then
            let rest = Seq.skip BatchLimit cosmoEvents
            return! appendToAzureTableStorage rest
        else
            return ()
    }
