module Fantomas.Tests.NoTrailingSpacesTests

open NUnit.Framework
open FsUnit

open Fantomas.Tests.TestHelper

[<Test>]
let ``should not confuse me with an extra space at end of line v2``() =
    let codeSnippet = """let ``should not extrude without positive distance`` () =
    let args = [| "-i"; "input.dxf"; "-o"; "output.pdf"; "--op"; "extrude"; |]
    (fun () -> parseCmdLine args |> ignore)
    |> should throw typeof<Argu.ArguParseException>"""

    formatSourceString false codeSnippet ({ config with MaxInfixOperatorExpression = 90 })
    |> should equal """let ``should not extrude without positive distance`` () =
    let args =
        [| "-i"
           "input.dxf"
           "-o"
           "output.pdf"
           "--op"
           "extrude" |]

    (fun () -> parseCmdLine args |> ignore) |> should throw typeof<Argu.ArguParseException>
"""