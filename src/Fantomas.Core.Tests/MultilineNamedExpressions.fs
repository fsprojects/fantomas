module Fantomas.Core.Tests.MultilineNamedExpressions

open NUnit.Framework
open FsUnit
open Fantomas.Core
open Fantomas.Core.Tests.TestHelpers

[<Test>]
let ``single named item expression`` () =
    formatSourceString
        false
        """
let x =
    ParsedInput.ImplFile(
        ParsedImplFileInput(
            contents =
                [ SynModuleOrNamespace.SynModuleOrNamespace(
                      decls =
                          [ a
                            //
                            b ]
                  ) ]
        )
    )
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
let x =
    ParsedInput.ImplFile(
        ParsedImplFileInput(contents = [
            SynModuleOrNamespace.SynModuleOrNamespace(decls = [
                a
                //
                b
            ])
        ])
    )
"""

// TODO: there is currently no check whether `(a =` fits on the remainder of the line.

[<Test>]
let ``multiline long identifier in named argument function call`` () =
    formatSourceString
        false
        """
XXXXXXXXXXXXXXXX
    // Some comment
    .YYYYYYYYYYYYYY
    .ZZZZZZZZZZZZZZZ(
    a =
        try b () with ex -> c 
)
"""
        config
    |> prepend newline
    |> should
        equal
        """
XXXXXXXXXXXXXXXX
    // Some comment
    .YYYYYYYYYYYYYY
    .ZZZZZZZZZZZZZZZ(a =
        try
            b ()
        with ex ->
            c
    )
"""
