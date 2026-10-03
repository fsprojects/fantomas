(*---
fsharp_multiline_bracket_style = cramped
---*)
Decode.map3 (fun aggregateId event commitPayload ->
    match commitPayload with
    | Some payload ->
        Some
            { AggregateId = AggregateId aggregateId
              Event = event
              Payload = payload }
    | None -> None) (Decode.field "aggregate_id" Decode.string) (Decode.field "event" Decode.string) decodePayload
