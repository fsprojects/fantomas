module Fantomas.Core.Tests.QuotationTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelper

[<Test>]
let ``typed quotations`` () =
    formatSourceString
        false
        """
    <@
        let f x = x + 10
        f 20
    @>"""
        config
    |> prepend newline
    |> should
        equal
        """
<@
    let f x = x + 10
    f 20
@>
"""

[<Test>]
let ``untyped quotations`` () =
    formatSourceString false "<@@ 2 + 3 @@>" config
    |> should
        equal
        """<@@ 2 + 3 @@>
"""

[<Test>]
let ``should preserve unit literal`` () =
    shouldNotChangeAfterFormat
        """
let logger =
    Mock<ILogger>()
        .Setup(fun log -> <@ log.Log(error) @>)
        .Returns(())
        .Create()
"""

[<Test>]
let ``should format multiline quotation expressions idempotent, 2203`` () =
    formatSourceString
        false
        """
let action =
    <@
        let msg = %httpRequestMessageWithPayload
        RuntimeHelpers.fillHeaders msg %heads
        async {
            let! response = (%this).HttpClient.SendAsync(msg) |> Async.AwaitTask
            return response.EnsureSuccessStatusCode().Content
        }
    @>
"""
        config
    |> prepend newline
    |> should
        equal
        """
let action =
    <@
        let msg = %httpRequestMessageWithPayload
        RuntimeHelpers.fillHeaders msg %heads

        async {
            let! response =
                (%this).HttpClient.SendAsync(msg)
                |> Async.AwaitTask

            return response.EnsureSuccessStatusCode().Content
        }
    @>
"""
