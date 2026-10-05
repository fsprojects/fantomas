(*---
fsharp_max_infix_operator_expression = 50
---*)
let action =
    <@
        let msg = %httpRequestMessageWithPayload
        RuntimeHelpers.fillHeaders msg %heads
        async {
            let! response = (%this).HttpClient.SendAsync(msg) |> Async.AwaitTask
            return response.EnsureSuccessStatusCode().Content
        }
    @>
