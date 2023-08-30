module Fantomas.Core.Tests.BeginEndTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers

[<Test>]
let ``detect begin end from SynExpr.Paren, 2368`` () =
    formatSourceString
        false
        """
do
    begin 1 end
"""
        config
    |> prepend newline
    |> should
        equal
        """
do begin 1 end
"""

[<Test>]
let ``multiline begin end in expression`` () =
    formatSourceString
        false
        """
do
    let a = 1
    begin
        use b = f ()
        ()
    end
    let c = 2
    ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
do
    let a = 1

    begin
        use b = f ()
        ()
    end

    let c = 2
    ()
"""
