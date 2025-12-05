module Fantomas.Core.Tests.HashDirectiveTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers

[<Test>]
let ``should use verbatim strings on some hash directives`` () =
    formatSourceString
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

[<Test>]
let ``trivia before hash directive in signature file, 2258`` () =
    formatSignatureString
        """
// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace FSharp.Compiler.Tokenization

open System
open System.Threading
open FSharp.Compiler
open FSharp.Compiler.Text

#nowarn "57"

/// Represents encoded information for the end-of-line continuation of lexing
[<Struct; CustomEquality; NoComparison>]
type FSharpTokenizerLexState =
    { PosBits: int64
      OtherBits: int64 }

    static member Initial: FSharpTokenizerLexState

    member Equals: FSharpTokenizerLexState -> bool

/// Represents stable information for the state of the lexing engine at the end of a line
type FSharpTokenizerColorState =
    | Token = 1
    | IfDefSkip = 3
    | String = 4
    | Comment = 5
    | StringInComment = 6
    | VerbatimStringInComment = 7
    | CamlOnly = 8
    | VerbatimString = 9
    | SingleLineComment = 10
    | EndLineThenSkip = 11
    | EndLineThenToken = 12
    | TripleQuoteString = 13
    | TripleQuoteStringInComment = 14
    | InitialState = 0
"""
        config
    |> prepend newline
    |> should
        equal
        """
// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace FSharp.Compiler.Tokenization

open System
open System.Threading
open FSharp.Compiler
open FSharp.Compiler.Text

#nowarn "57"

/// Represents encoded information for the end-of-line continuation of lexing
[<Struct; CustomEquality; NoComparison>]
type FSharpTokenizerLexState =
    { PosBits: int64
      OtherBits: int64 }

    static member Initial: FSharpTokenizerLexState

    member Equals: FSharpTokenizerLexState -> bool

/// Represents stable information for the state of the lexing engine at the end of a line
type FSharpTokenizerColorState =
    | Token = 1
    | IfDefSkip = 3
    | String = 4
    | Comment = 5
    | StringInComment = 6
    | VerbatimStringInComment = 7
    | CamlOnly = 8
    | VerbatimString = 9
    | SingleLineComment = 10
    | EndLineThenSkip = 11
    | EndLineThenToken = 12
    | TripleQuoteString = 13
    | TripleQuoteStringInComment = 14
    | InitialState = 0
"""

[<Test>]
let ``#help with string`` () =
    formatSourceString
        """
#help  "List.map"
"""
        config
    |> prepend newline
    |> should
        equal
        """
#help "List.map"
"""

[<Test>]
let ``#help without string`` () =
    formatSourceString
        """
#help  List.map
"""
        config
    |> prepend newline
    |> should
        equal
        """
#help List.map
"""

// As of F# 10.0, warn directives are treated as trivia like #if, so arguments are not formatted
[<Test>]
let ``#nowarn with integer`` () =
    formatSourceString
        """
#nowarn  1182
"""
        config
    |> prepend newline
    |> should
        equal
        """
#nowarn  1182
"""
