module Fantomas.CoreGlobalTool.Tests.TestHelpers

open System
open System.Diagnostics
open System.IO
open System.Text

type TemporaryFileCodeSample internal (codeSnippet: string, ?hasByteOrderMark: bool) =
    let hasByteOrderMark = defaultArg hasByteOrderMark false
    let filename = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString() + ".fs")
    do (if hasByteOrderMark
        then File.WriteAllText(filename, codeSnippet, Encoding.UTF8)
        else File.WriteAllText(filename, codeSnippet))

    member _.Filename: string = filename
    interface IDisposable with
        member this.Dispose(): unit = File.Delete(filename)

type OutputFile internal () =
    let filename = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString() + ".fs")
    member _.Filename: string = filename
    interface IDisposable with
        member this.Dispose(): unit =
            if File.Exists(filename) then
                File.Delete(filename)

let runFantomasTool arguments =
    let pwd = Path.GetDirectoryName(typeof<TemporaryFileCodeSample>.Assembly.Location)
    let configuration =
        #if DEBUG
        "Debug"
        #else
        "Release"
        #endif

    let fantomasDll =
        Path.Combine
            (pwd, "..", "..", "..", "..", "Fantomas.CoreGlobalTool", "bin", configuration, "netcoreapp3.1",
             "fantomas-tool.dll")

    use p = new Process()
    p.StartInfo.UseShellExecute <- false
    p.StartInfo.FileName <- @"dotnet"
    p.StartInfo.Arguments <- sprintf "%s %s" fantomasDll arguments
    p.Start() |> ignore
    p.WaitForExit()
    p.ExitCode

let checkCode file =
    let arguments = sprintf "--check \"%s\"" file
    runFantomasTool arguments
