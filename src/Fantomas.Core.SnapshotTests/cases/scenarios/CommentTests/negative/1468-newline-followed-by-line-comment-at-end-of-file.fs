(*---
max_line_length = 55
---*)
Host
    .CreateDefaultBuilder()
    .ConfigureWebHostDefaults(fun webHostBuilder ->
        webHostBuilder
            .Configure(configureApp)
            .ConfigureServices(configureServices)
        |> ignore)
    .Build()
    .Run()

//
