module Fantomas.CoreGlobalTool.Tests.TestHelpers

open System
open System.Diagnostics
open System.IO

type TemporaryFileCodeSample internal (codeSnippet: string) =
    let filename = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString() + ".fs")
    do File.WriteAllText(filename, codeSnippet)

    member _.Filename: string = filename
    interface IDisposable with
        member this.Dispose(): unit = File.Delete(filename)

let private runFantomasTool arguments =
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