(*---
max_line_length = 60
---*)
services.AddHttpsRedirection(Action<HttpsRedirectionOptions>(fun options ->
    // meh
    options.HttpsPort <- Nullable(7002)
)) |> ignore
