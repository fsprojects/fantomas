module Fantomas.Tests.TestHelper

open FsUnit

open System
open Fantomas.FormatConfig
open Fantomas
open Microsoft.FSharp.Compiler.SourceCodeServices

let config = FormatConfig.Default
let newline = "\n"

let argsDotNET451 =
        [|"--noframework"; "--debug-"; "--optimize-"; "--tailcalls-";
          // Some constants are used in unit tests
          "--define:DEBUG"; "--define:TRACE"; "--define:SILVERLIGHT";
          @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.3.1.0\FSharp.Core.dll";
          @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5.1\mscorlib.dll";
          @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5.1\System.dll";
          @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5.1\System.Core.dll";
          @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5.1\System.Drawing.dll";
          @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5.1\System.Numerics.dll";
          @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5.1\System.Windows.Forms.dll"|]

let projectOptions =
    fun fileName ->
        {   ProjectFileName = @"C:\Project.fsproj"
            ProjectFileNames = [| fileName |]
            OtherOptions = argsDotNET451
            ReferencedProjects = Array.empty
            IsIncompleteTypeCheckEnvironment = false
            UseScriptResolutionRules = true
            LoadTime = DateTime.UtcNow
            UnresolvedReferences = None
            OriginalLoadReferences = List.empty
            ExtraProjectInfo = None }

let sharedChecker = lazy(FSharpChecker.Create())

let formatSourceString isFsiFile (s : string) config = 
    // On Linux/Mac this will exercise different line endings
    let s = s.Replace("\r\n", Environment.NewLine)
    let fileName = if isFsiFile then "/src.fsi" else "/src.fsx"
    CodeFormatter.FormatDocumentAsync(fileName, s, config, projectOptions fileName, sharedChecker.Value)
    |> Async.RunSynchronously
    |> fun s -> s.Replace("\r\n", "\n")

let formatSelectionFromString isFsiFile r (s : string) config = 
    let s = s.Replace("\r\n", Environment.NewLine)
    let fileName = if isFsiFile then "/tmp.fsi" else "/tmp.fsx"
    CodeFormatter.FormatSelectionInDocumentAsync(fileName, r, s, config, projectOptions fileName, sharedChecker.Value)
    |> Async.RunSynchronously
    |> fun s -> s.Replace("\r\n", "\n")

let formatSelectionOnly isFsiFile r (s : string) config = 
    let s = s.Replace("\r\n", Environment.NewLine)
    let fileName = if isFsiFile then "/tmp.fsi" else "/tmp.fsx"
    CodeFormatter.FormatSelectionAsync(fileName, r, s, config, projectOptions fileName, sharedChecker.Value)
    |> Async.RunSynchronously
    |> fun s -> s.Replace("\r\n", "\n")

let formatAroundCursor isFsiFile p (s : string) config = 
    let s = s.Replace("\r\n", Environment.NewLine)
    let fileName = if isFsiFile then "/tmp.fsi" else "/tmp.fsx"
    CodeFormatter.FormatAroundCursorAsync(fileName, p, s, config, projectOptions fileName, sharedChecker.Value)
    |> Async.RunSynchronously
    |> fun s -> s.Replace("\r\n", "\n")

let isValidFSharpCode isFsiFile s =
    let fileName = if isFsiFile then "/tmp.fsi" else "/tmp.fsx"
    CodeFormatter.IsValidFSharpCodeAsync(fileName, s, projectOptions fileName, sharedChecker.Value)
    |> Async.RunSynchronously

let parse isFsiFile s =
    let fileName = if isFsiFile then "/tmp.fsi" else "/tmp.fsx"
    CodeFormatter.ParseAsync(fileName, s, projectOptions fileName, sharedChecker.Value)
    |> Async.RunSynchronously

let formatAST a s c =
    CodeFormatter.FormatAST(a, "/tmp.fsx",s, c)

let makeRange l1 c1 l2 c2 = 
    CodeFormatter.MakeRange("/tmp.fsx", l1, c1, l2, c2)

let makePos l1 c1 = 
    CodeFormatter.MakePos(l1, c1)

let equal x = 
    let x = 
        match box x with
        | :? String as s -> s.Replace("\r\n", "\n") |> box
        | x -> x
    equal x

let inline prepend s content = s + content
let inline append s content = content + s
