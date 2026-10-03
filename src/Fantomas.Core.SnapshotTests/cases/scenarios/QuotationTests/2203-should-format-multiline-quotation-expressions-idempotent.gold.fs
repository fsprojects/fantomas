let action =
    <@
        let msg = %httpRequestMessageWithPayload
        RuntimeHelpers.fillHeaders msg %heads

        async {
            let! response =
                (%this).HttpClient.SendAsync(msg)
                |> Async.AwaitTask

            return response.EnsureSuccessStatusCode().Content
        }
    @>
