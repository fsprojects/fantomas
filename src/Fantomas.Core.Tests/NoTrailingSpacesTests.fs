module Fantomas.Core.Tests.NoTrailingSpacesTests

open NUnit.Framework
open FsUnit

open Fantomas.Core.Tests.TestHelpers

[<Test>]
let ``should not confuse me with an extra space at end of line v2`` () =
    let codeSnippet =
        """let ``should not extrude without positive distance`` () =
    let args = [| "-i"; "input.dxf"; "-o"; "output.pdf"; "--op"; "extrude"; |]
    (fun () -> parseCmdLine args |> ignore)
    |> should throw typeof<Argu.ArguParseException>"""

    formatSourceString
        codeSnippet
        { config with
            MaxInfixOperatorExpression = 90
            MaxArrayOrListWidth = 40 }
    |> should
        equal
        """let ``should not extrude without positive distance`` () =
    let args =
        [| "-i"
           "input.dxf"
           "-o"
           "output.pdf"
           "--op"
           "extrude" |]

    (fun () -> parseCmdLine args |> ignore) |> should throw typeof<Argu.ArguParseException>
"""
