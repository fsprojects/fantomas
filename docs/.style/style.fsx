#r "nuget: JavaScriptEngineSwitcher.ChakraCore.Native.win-x64"
#r "nuget: JavaScriptEngineSwitcher.ChakraCore, 3.18.2"
#r "nuget: DartSassHost, 1.0.0-preview7"
#r "nuget: FSharp.Control.Reactive, 5.0.5"

open System
open System.IO
open DartSassHost
open DartSassHost.Helpers
open JavaScriptEngineSwitcher.ChakraCore
open FSharp.Control.Reactive

let sassCompiler = new SassCompiler(new ChakraCoreJsEngineFactory())
let (</>) a b = Path.Combine(a, b)

let inputFile = __SOURCE_DIRECTORY__ </> "homepage.sass"
let inputFolder = __SOURCE_DIRECTORY__

let output =
    __SOURCE_DIRECTORY__
    </> ".."
    </> "homepage.css"

let compileSass () =
    try
        let result =
            sassCompiler.CompileFile(
                inputFile,
                ?outputPath = Some output
            )

        File.WriteAllText(output, result.CompiledContent)
        printfn "Compiled %s at %A" output DateTime.Now
    with
    | :? SassCompilerLoadException as sclex ->
        printfn
            "During loading of Sass compiler an error occurred. See details:\n%s"
            (SassErrorHelpers.GenerateErrorDetails sclex)
    | :? SassCompilationException as sce ->
        printfn
            "During compilation of SCSS code an error occurred. See details:\n%s"
            (SassErrorHelpers.GenerateErrorDetails sce)
    | :? SassException as e ->
        printfn
            "During working of Sass compiler an unknown error occurred. See details:\n%s"
            (SassErrorHelpers.GenerateErrorDetails e)
    | ex -> printfn "Unexpected exception during Sass compilation: %A" ex


let fsw = new FileSystemWatcher(inputFolder)
fsw.IncludeSubdirectories <- true
fsw.Filters.Add("*.sass")
fsw.NotifyFilter <- NotifyFilters.FileName ||| NotifyFilters.Size
fsw.EnableRaisingEvents <- true

let mapEvent ev = Observable.map (fun _ -> ()) ev

let subscriber =
    [| mapEvent fsw.Renamed
       mapEvent fsw.Changed
       mapEvent fsw.Deleted
       mapEvent fsw.Created |]
    |> Observable.mergeArray
    |> Observable.startWith [| () |]
    |> Observable.throttle (TimeSpan.FromMilliseconds 200.)
    |> Observable.subscribe compileSass

let _ = Console.ReadLine()
subscriber.Dispose()

printfn "Goodbye"
