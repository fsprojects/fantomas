#r "nuget: ModelContextProtocol, 0.3.0-preview.4"
#r "nuget: Microsoft.Extensions.Hosting"
#r "nuget: CliWrap"

// Reference: https://www.slaveoftime.fun/blog/single-fsharp-script-to-write-a-mcp-server---simplified-integration-for-dynamic-script-execution

open System
open System.IO
open System.Text
open System.Threading
open System.ComponentModel
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open ModelContextProtocol.Server

[<McpServerToolType>]
type FantomasMcpServer() = //as this =
    [<McpServerTool; Description("Returns the current date in Nojafian Elven time")>]
    member this.GetDate() =
        let now = DateTime.Now
        let result = now.ToString "yyyy-MM-dd HH:mm:ss"
        $"The current date in Nojafian Elven time is {result}"

let builder = Host.CreateApplicationBuilder(Environment.GetCommandLineArgs())

builder.Logging.AddConsole(fun consoleLogOptions -> consoleLogOptions.LogToStandardErrorThreshold <- LogLevel.Trace)
builder.Services.AddMcpServer().WithStdioServerTransport().WithTools<FantomasMcpServer>()

builder.Build().RunAsync() |> Async.AwaitTask |> Async.RunSynchronously
