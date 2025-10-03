#r "nuget: ModelContextProtocol, 0.3.0-preview.4"
#r "nuget: Microsoft.Extensions.Hosting"
#r "nuget: CliWrap"

// Reference: https://www.slaveoftime.fun/blog/single-fsharp-script-to-write-a-mcp-server---simplified-integration-for-dynamic-script-execution

open System
open System.IO
open System.Text
open System.ComponentModel
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open ModelContextProtocol.Server
open CliWrap
open CliWrap.Buffered

let repositoryRoot = Path.Combine(__SOURCE_DIRECTORY__, "..") |> Path.GetFullPath

let fantomasCoreFsproj =
    Path.Combine(repositoryRoot, "src/Fantomas.Core/Fantomas.Core.fsproj")

let formatScript =
    Path.Combine(repositoryRoot, "mcp", "tools", "format.fsx") |> Path.GetFullPath

let buildCodebase () =
    task {
        let! buildResult =
            Cli
                .Wrap("dotnet")
                .WithArguments([| "build"; fantomasCoreFsproj; "-v"; "n" |])
                .WithValidation(CommandResultValidation.None)
                .ExecuteBufferedAsync()
                .Task

        if buildResult.ExitCode = 0 then
            return Ok()
        else
            let! gitStatus = Cli.Wrap("git").WithArguments("status").ExecuteBufferedAsync().Task

            return
                Error
                    $"""Currently the Fantomas code base does not build.

Build Error:

{buildResult.StandardOutput}{buildResult.StandardError}

Git status:

{gitStatus.StandardOutput}
                """
    }

[<McpServerToolType>]
type FantomasMcpServer() = //as this =
    [<McpServerTool;
      Description("""
The ultimate Fantomas debug tool. 
This format the input code with a locally built Fantomas.Core and will report detailed events of what happened during this formatting process.
This will give key insight of how the F# source code was transformed to Untyped AST, Syntax Oak, what trivia was collected and what writer events were produced.
It will also will also let you know if the formatted code was still valid F# code!
Upon invocation this tool will try and compile the local codebase so that the latest code changes are reflected.
If the build did not succeed, an the build error will be reported.
    """)>]
    member this.FormatCode(sourceCode: string, isSignature: bool) =
        task {
            match! buildCodebase () with
            | Error(error) -> return error
            | Ok() ->
                let inputStream = new MemoryStream(Encoding.UTF8.GetBytes(sourceCode))

                let! result =
                    Cli
                        .Wrap("dotnet")
                        .WithArguments(
                            [|
                                yield "fsi"
                                yield formatScript
                                if isSignature then
                                    yield "--"
                                    yield "--signature"
                            |]
                        )
                        .WithStandardInputPipe(PipeSource.FromStream inputStream)
                        .ExecuteBufferedAsync()
                        .Task

                return result.StandardOutput
        }

let builder = Host.CreateApplicationBuilder(Environment.GetCommandLineArgs())

builder.Logging.AddConsole(fun consoleLogOptions -> consoleLogOptions.LogToStandardErrorThreshold <- LogLevel.Trace)
builder.Services.AddMcpServer().WithStdioServerTransport().WithTools<FantomasMcpServer>()

builder.Build().RunAsync() |> Async.AwaitTask |> Async.RunSynchronously
