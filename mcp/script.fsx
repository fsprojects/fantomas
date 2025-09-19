#r "nuget: CliWrap"

open System.IO
open System.Text
open CliWrap
open CliWrap.Buffered

let input = "let    a      =     9"

let inputStream = new MemoryStream(Encoding.UTF8.GetBytes(input))

let repositoryRoot = Path.Combine(__SOURCE_DIRECTORY__, "..") |> Path.GetFullPath

let fantomasCoreFsproj =
    Path.Combine(repositoryRoot, "src/Fantomas.Core/Fantomas.Core.fsproj")

let buildResult =
    Cli
        .Wrap("dotnet")
        .WithArguments([| "build"; fantomasCoreFsproj; "-v"; "n" |])
        .WithValidation(CommandResultValidation.None)
        .ExecuteBufferedAsync()
        .Task.Result

if buildResult.ExitCode <> 0 then
    let gitStatus =
        Cli.Wrap("git").WithArguments("status").ExecuteBufferedAsync().Task.Result

    printfn
        $"""Currently the Fantomas code base does not build.

Build Error:

{buildResult.StandardOutput}{buildResult.StandardError}

Git status:

{gitStatus.StandardOutput}
"""
else
    let result =
        Cli
            .Wrap("dotnet")
            .WithArguments([| "fsi"; "/Users/nojaf/Projects/fantomas/mcp/tools/format.fsx" |])
            .WithStandardInputPipe(PipeSource.FromStream(inputStream))
            .ExecuteBufferedAsync()
            .Task.Result

    printfn $"Result: {result.StandardOutput}"
