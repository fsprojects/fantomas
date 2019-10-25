module Fantomas.Tests.LamdaTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

[<Test>]
let ``keep comment after arrow`` () =
    formatSourceString false """_Target "FSharpTypesDotNet" (fun _ -> // obsolete
 ())
"""  ({ config with IndentSpaceNum = 2; PageWidth = 90 })
    |> prepend newline
    |> should equal """
_Target "FSharpTypesDotNet" (fun _ -> // obsolete
  ())
"""