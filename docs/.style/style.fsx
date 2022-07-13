#r "nuget: JavaScriptEngineSwitcher.ChakraCore.Native.win-x64"
#r "nuget: JavaScriptEngineSwitcher.ChakraCore.Native.linux-x64"
#r "nuget: JavaScriptEngineSwitcher.ChakraCore.Native.osx-x64"
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

let inputFileHomepage = __SOURCE_DIRECTORY__ </> "homepage.sass"
let inputFileTemplate = __SOURCE_DIRECTORY__ </> "fsdocs-custom.sass"
let inputFilesidebar = __SOURCE_DIRECTORY__ </> "sidebar.sass"
let inputFolder = __SOURCE_DIRECTORY__

let outputHomepage = __SOURCE_DIRECTORY__ </> ".." </> "homepage.css"

let outputTemplate =
    __SOURCE_DIRECTORY__
    </> "../content/"
    </> "fsdocs-custom.css"

let outputsidebar = __SOURCE_DIRECTORY__ </> "../content/" </> "sidebar.css"

let compileSass () =
    try
        let homepage =
            sassCompiler.CompileFile(inputFileHomepage, ?outputPath = Some outputHomepage)

        let template =
            sassCompiler.CompileFile(inputFileTemplate, ?outputPath = Some outputTemplate)

        let sidebar =
            sassCompiler.CompileFile(inputFilesidebar, ?outputPath = Some outputsidebar)

        File.WriteAllText(outputHomepage, homepage.CompiledContent)
        printfn "Compiled %s at %A" outputHomepage DateTime.Now

        File.WriteAllText(outputTemplate, template.CompiledContent)
        printfn "Compiled %s at %A" outputTemplate DateTime.Now

        File.WriteAllText(outputsidebar, sidebar.CompiledContent)
        printfn "Compiled %s at %A" outputsidebar DateTime.Now

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

let isWatch =
    match Seq.tryLast fsi.CommandLineArgs with
    | Some "--watch" -> true
    | _ -> false

if isWatch then
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
else
    compileSass ()
