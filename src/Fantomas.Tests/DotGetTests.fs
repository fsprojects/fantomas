module Fantomas.Tests.DotGetTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

[<Test>]
let ``a TypeApp inside a DotGet should stay on the same line, 994`` () =
    formatSourceString false """
Microsoft.FSharp.Reflection.FSharpType.GetUnionCases(typeof<option<option<unit>>>.GetGenericTypeDefinition().MakeGenericType(t)).Assembly
"""  config
    |> prepend newline
    |> should equal """
Microsoft.FSharp.Reflection.FSharpType.GetUnionCases(typeof<option<option<unit>>>.GetGenericTypeDefinition()
        .MakeGenericType(t))
    .Assembly
"""

[<Test>]
let ``a DotGetApp inside a DotGet should stay on the same line, 1051`` () =
    formatSourceString false """
System.Diagnostics.FileVersionInfo.GetVersionInfo(
               System.Reflection.Assembly.GetExecutingAssembly().Location).FileVersion
"""  { config with MaxLineLength = 80 }
    |> prepend newline
    |> should equal """
System.Diagnostics.FileVersionInfo.GetVersionInfo(System.Reflection.Assembly.GetExecutingAssembly()
        .Location)
    .FileVersion
"""

[<Test>]
let ``split chained method call expression, 246`` () =
    formatSourceString false """
        root.SetAttribute
          ("driverVersion",
           "AltCover.Recorder "
           + System.Diagnostics.FileVersionInfo.GetVersionInfo(
               System.Reflection.Assembly.GetExecutingAssembly().Location).FileVersion)
"""  config
    |> prepend newline
    |> should equal """
root.SetAttribute
    ("driverVersion",
     "AltCover.Recorder "
     + System.Diagnostics.FileVersionInfo.GetVersionInfo(System.Reflection.Assembly.GetExecutingAssembly().Location)
         .FileVersion)
"""
