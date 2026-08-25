module Fantomas.Tests.InvocationTests

open NUnit.Framework
open FsUnitTyped
open Fantomas

[<Test>]
let ``a global tool is the apphost, and names itself`` () =
    Invocation.nameOf (Some "/usr/local/bin/fantomas") |> shouldEqual "fantomas"

[<Test>]
let ``an apphost drops its extension`` () =
    // `fantomas.exe` on Windows. The separator is left alone here so the case reads the same on
    // every platform; what is under test is the extension, not the path.
    Invocation.nameOf (Some "/usr/local/bin/fantomas.exe") |> shouldEqual "fantomas"

[<Test>]
let ``a local tool is started by the muxer, so the command carries dotnet`` () =
    Invocation.nameOf (Some "/usr/share/dotnet/dotnet")
    |> shouldEqual "dotnet fantomas"

[<Test>]
let ``a tool-path install is named back as it was found`` () =
    Invocation.nameOf (Some "/tmp/tools/fantomas-8") |> shouldEqual "fantomas-8"

[<Test>]
let ``no process path falls back to the tool's own name`` () =
    Invocation.nameOf None |> shouldEqual "fantomas"
