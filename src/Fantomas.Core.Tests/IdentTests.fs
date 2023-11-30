module Fantomas.Core.Tests.IdentTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers

[<Test>]
let ``type with ticks, 2164`` () =
    formatSourceString
        """
let foo = {| ``type`` = "hi" |}
"""
        config
    |> prepend newline
    |> should
        equal
        """
let foo = {| ``type`` = "hi" |}
"""

[<Test>]
let ``include with ticks, 2167`` () =
    formatSourceString
        """
            match req.``include`` with
            | None -> tc.TestItems()
            | Some includedTests -> includedTests.ToArray()
"""
        config
    |> prepend newline
    |> should
        equal
        """
match req.``include`` with
| None -> tc.TestItems()
| Some includedTests -> includedTests.ToArray()
"""

[<Test>]
let ``base without ticks, 2212`` () =
    formatSourceString
        """
type X =  
  override this.f(y) : bool =
    base.f(y)
"""
        config
    |> prepend newline
    |> should
        equal
        """
type X =
    override this.f(y) : bool = base.f (y)
"""

[<Test>]
let ``constraint with ticks, 2116`` () =
    formatSourceString
        """
let ``constraint`` = 1
"""
        config
    |> prepend newline
    |> should
        equal
        """
let ``constraint`` = 1
"""

[<Test>]
let ``process with ticks, 2034`` () =
    formatSourceString
        """
let ``process`` = 1
"""
        config
    |> prepend newline
    |> should
        equal
        """
let ``process`` = 1
"""

[<Test>]
let ``allows processing when encountering reserved words`` () =
    formatSourceString
        """
let process: IProcess = importAll "process"
"""
        config
    |> prepend newline
    |> should
        equal
        """
let process: IProcess = importAll "process"
"""
