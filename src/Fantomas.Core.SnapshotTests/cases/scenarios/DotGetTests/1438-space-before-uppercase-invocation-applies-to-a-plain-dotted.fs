(*---
max_line_length = 100
fsharp_space_before_uppercase_invocation = true
fsharp_multi_line_lambda_closing_newline = true
---*)
namespace AspNetSerilog

[<Extension>]
type IWebHostBuilderExtensions() =

    [<Extension>]
    static member UseSerilog(webHostBuilder : IWebHostBuilder, index : Index) =
        webHostBuilder.UseSerilog(fun context configuration ->
            configuration
                .MinimumLevel.Debug()
                .WriteTo.Logger(fun loggerConfiguration ->
                    loggerConfiguration
                        .Enrich.WithProperty("host", Environment.MachineName)
                        .Enrich.WithProperty("user", Environment.UserName)
                        .Enrich.WithProperty("application", context.HostingEnvironment.ApplicationName)
                    |> ignore
                )
            |> ignore
        )
