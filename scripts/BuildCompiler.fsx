#r "nuget: CliWrap, 3.6.4"
#r "nuget: FSharp.Data, 6.3.0"

open System
open System.IO
open System.Xml.Linq
open System.Xml.XPath
open CliWrap
open CliWrap.Buffered
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
                Http.AsyncRequestStream(
                    url,
                    headers = [| "Content-Disposition", $"attachment; filename=\"{fileName}\"" |]
                )

            if response.StatusCode <> 200 then
                printfn $"Could not download %s{relativePath}"

            do! Async.AwaitTask(response.ResponseStream.CopyToAsync(fs))
            fs.Close()

            updateFileRaw file
    }
