#r "nuget: FSharp.Data, 8.2.0"

open System
open System.IO
open System.Net
open System.Xml.Linq
open System.Xml.XPath
open FSharp.Data
// Loaded by `build.fsx`, after `BuildCommon.fsx`. An error here saying BuildCommon is not defined
// means this file was run on its own; it is a library, so run a pipeline from build.fsx instead.
open BuildCommon

// Keeping the vendored FCS sources up to date: which upstream commit they came from, and fetching a
// file at that commit. `Fantomas.FCS` is a copy of the compiler, so this is how the copy moves.

let deps = repositoryRoot </> ".deps"

let fsharpCompilerHash =
    let xDoc = XElement.Load(repositoryRoot </> "Directory.Build.props")
    xDoc.XPathSelectElements("//FCSCommitHash") |> Seq.head |> (fun xe -> xe.Value)

let updateFileRaw (file: FileInfo) =
    let lines = File.ReadAllLines file.FullName

    let updatedLines =
        lines
        |> Array.map (fun line ->
            if line.StartsWith("namespace FSharp.Build") then
                line.Replace("namespace FSharp.Build", "namespace Fantomas.FCS.Build")
            elif line.Contains("FSharp.Compiler") then
                line.Replace("FSharp.Compiler", "Fantomas.FCS")
            elif line.Contains("[<TailCall>]") then
                line.Replace("[<TailCall>]", "[<Microsoft.FSharp.Core.TailCall>]")
            else
                line)

    File.WriteAllLines(file.FullName, updatedLines)

/// GitHub now and then resets the connection during the TLS handshake, and a few seconds later the
/// same request works. A `ProtocolError` is an HTTP error response, such as a 404, which asking again
/// will not change.
let rec private requestWithRetry
    (attempt: int)
    (url: string)
    (headers: (string * string) array)
    : Async<HttpResponseWithStream> =
    async {
        try
            return! Http.AsyncRequestStream(url, headers = headers)
        with :? WebException as ex when ex.Status <> WebExceptionStatus.ProtocolError && attempt < 5 ->
            printfn $"Could not connect to %s{url}: %s{ex.Message} Trying again in 5 seconds."
            do! Async.Sleep(TimeSpan.FromSeconds 5.0)
            return! requestWithRetry (attempt + 1) url headers
    }

let downloadCompilerFile commitHash relativePath =
    async {
        let file = FileInfo(deps </> commitHash </> relativePath)

        if file.Exists && file.Length <> 0 then
            return ()
        else
            file.Directory.Create()
            let fs = file.Create()
            let fileName = Path.GetFileName(relativePath)

            let url =
                $"https://raw.githubusercontent.com/dotnet/fsharp/{commitHash}/{relativePath}"

            let! response =
                requestWithRetry 1 url [| "Content-Disposition", $"attachment; filename=\"{fileName}\"" |]

            if response.StatusCode <> 200 then
                printfn $"Could not download %s{relativePath}"

            do! Async.AwaitTask(response.ResponseStream.CopyToAsync(fs))
            fs.Close()

            updateFileRaw file
    }
