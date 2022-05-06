module Fantomas.Core.Tests.HashDirectiveTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelper

[<Test>]
let ``should use verbatim strings on some hash directives`` () =
    formatSourceString
        false
        """
    #r @"C:\foo\bar.dll"
    """
        config
    |> prepend newline
    |> should
        equal
        """
#r @"C:\foo\bar.dll"
"""

[<Test>]
let ``hash directives`` () =
    formatSourceString
        false
        """
    #r "Fantomas.Tests.dll"
    #load "CodeFormatterTests.fs"
    """
        config
    |> prepend newline
    |> should
        equal
        """
#r "Fantomas.Tests.dll"
#load "CodeFormatterTests.fs"
"""

[<Test>]
let ``should support load directive multiple arguments`` () =
    formatSourceString
        false
        """
    #load "A.fs" "B.fs"
    #load "C.fs"
          "D.fs"
          "E.fs"
    """
        config
    |> prepend newline
    |> should
        equal
        """
#load "A.fs" "B.fs"
#load "C.fs" "D.fs" "E.fs"
"""

[<Test>]
let ``don't add extra new line before hash directive`` () =
    formatSourceString
        false
        """
module FantomasTools.Client.ASTViewer.Decoders

open ASTViewer.Shared
open FantomasTools.Client.ASTViewer.Model
open Thoth.Json

let decodeKeyValue: Decoder<obj> = fun _key jsonValue -> Ok jsonValue

#nowarn "40"
"""
        config
    |> prepend newline
    |> should
        equal
        """
module FantomasTools.Client.ASTViewer.Decoders

open ASTViewer.Shared
open FantomasTools.Client.ASTViewer.Model
open Thoth.Json

let decodeKeyValue: Decoder<obj> = fun _key jsonValue -> Ok jsonValue

#nowarn "40"
"""

[<Test>]
let ``#r "nuget:..." syntax`` () =
    formatSourceString
        false
        """
#r "nuget: Newtonsoft.Json"
// Optionally, specify a version explicitly
// #r "nuget: Newtonsoft.Json,11.0.1"

open Newtonsoft.Json

let o = {| X = 2; Y = "Hello" |}

printfn "%s" (JsonConvert.SerializeObject o)
"""
        config
    |> prepend newline
    |> should
        equal
        """
#r "nuget: Newtonsoft.Json"
// Optionally, specify a version explicitly
// #r "nuget: Newtonsoft.Json,11.0.1"

open Newtonsoft.Json

let o = {| X = 2; Y = "Hello" |}

printfn "%s" (JsonConvert.SerializeObject o)
"""

[<Test>]
let ``don't print trivia of other hash directive, 1464`` () =
    formatSourceString
        false
        """
#r @"C:\Program Files\dotnet\shared\Microsoft.AspNetCore.App\5.0.2\Microsoft.Extensions.Hosting.dll"
#r @"C:\Program Files\dotnet\shared\Microsoft.AspNetCore.App\5.0.2\Microsoft.Extensions.ObjectPool.dll"
#r @"C:\Program Files\dotnet\shared\Microsoft.AspNetCore.App\5.0.2\Microsoft.Extensions.Options.ConfigurationExtensions.dll"

#r "nuget: Giraffe"
"""
        config
    |> prepend newline
    |> should
        equal
        """
#r @"C:\Program Files\dotnet\shared\Microsoft.AspNetCore.App\5.0.2\Microsoft.Extensions.Hosting.dll"
#r @"C:\Program Files\dotnet\shared\Microsoft.AspNetCore.App\5.0.2\Microsoft.Extensions.ObjectPool.dll"
#r @"C:\Program Files\dotnet\shared\Microsoft.AspNetCore.App\5.0.2\Microsoft.Extensions.Options.ConfigurationExtensions.dll"

#r "nuget: Giraffe"
"""
