module Fantomas.Core.Tests.TryWithTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelper

[<Test>]
let ``try-with expression with long when guard - when breaking line, add a double indent when the indent_size is lower than the default 4 spaces, 2784``
    ()
    =
    formatSourceString
        false
        """
try
  c ()
with
| :? WebSocketException as e when e.WebSocketErrorCode = WebSocketError.ConnectionClosedPrematurely && sourceParty = Agent ->
  ()
"""
        { config with IndentSize = 2 }
    |> prepend newline
    |> should
        equal
        """
try
  c ()
with :? WebSocketException as e when
    e.WebSocketErrorCode = WebSocketError.ConnectionClosedPrematurely
    && sourceParty = Agent ->
  ()
"""
