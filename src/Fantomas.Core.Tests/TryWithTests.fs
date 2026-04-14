module Fantomas.Core.Tests.TryWithTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers

[<Test>]
let ``try-with expression with long when guard - when breaking line, add a double indent when the indent_size is lower than the default 4 spaces, 2784``
    ()
    =
    formatSourceString
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

[<Test>]
let ``try-with with multiple typed exception patterns`` () =
    formatSourceString
        """
try
    failwith "error"
with
| :? System.ArgumentException -> ()
| :? System.InvalidOperationException -> ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
try
    failwith "error"
with
| :? System.ArgumentException -> ()
| :? System.InvalidOperationException -> ()
"""

[<Test>]
let ``try-with single clause without pipe`` () =
    formatSourceString
        """
try
    failwith "error"
with ex ->
    ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
try
    failwith "error"
with ex ->
    ()
"""

[<Test>]
let ``try-with with when guard`` () =
    formatSourceString
        """
try
    failwith "error"
with
| ex when ex.Message.StartsWith("foo") -> ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
try
    failwith "error"
with ex when ex.Message.StartsWith("foo") ->
    ()
"""

[<Test>]
let ``try-finally`` () =
    formatSourceString
        """
try
    failwith "error"
finally
    ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
try
    failwith "error"
finally
    ()
"""

[<Test>]
let ``try-with named exception binding`` () =
    formatSourceString
        """
try
    failwith "error"
with
| Failure msg -> printfn "%s" msg
| :? System.ArgumentNullException as ex -> printfn "%s" ex.Message
"""
        config
    |> prepend newline
    |> should
        equal
        """
try
    failwith "error"
with
| Failure msg -> printfn "%s" msg
| :? System.ArgumentNullException as ex -> printfn "%s" ex.Message
"""

[<Test>]
let ``nested try-with`` () =
    formatSourceString
        """
try
    try
        failwith "inner"
    with
    | Failure msg -> ()
with
| ex -> ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
try
    try
        failwith "inner"
    with Failure msg ->
        ()
with ex ->
    ()
"""

[<Test>]
let ``try-with with multiline body`` () =
    formatSourceString
        """
try
    let x = 1
    let y = 2
    x + y
with
| ex -> 0
"""
        config
    |> prepend newline
    |> should
        equal
        """
try
    let x = 1
    let y = 2
    x + y
with ex ->
    0
"""
