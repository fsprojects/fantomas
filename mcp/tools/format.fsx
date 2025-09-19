#r "nuget: CliWrap"
#r "nuget: Thoth.Json.System.Text.Json"
#r "../../artifacts/bin/Fantomas.FCS/debug/Fantomas.FCS.dll"
#r "../../artifacts/bin/Fantomas.Core/debug/Fantomas.Core.dll"
#r "../../artifacts/bin/Fantomas.MCP/debug/Fantomas.MCP.dll"

open Fantomas

async {
    let input = stdin.ReadToEnd()

    let isSignature =
        System.Environment.GetCommandLineArgs() |> Array.contains "--signature"

    let! output = MCP.format input isSignature

    printfn $"formatted:\n{output}"
}
|> Async.RunSynchronously




// TODO: don't just dump a response, we should create some event sourced response.
// Numerous things happen here; code builds, input was parsed, trivia detected code formatted, result validated, result trivia detected, etc.
// We need to return this as JSON or some other structured format.
// See https://t3.chat/chat/76da167e-f594-4b3a-9f11-3facb9c11b5e
