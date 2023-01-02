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
    formatSourceString
        false
        """
let logger =
    Mock<ILogger>()
        .Setup(fun log -> <@ log.Log(error) @>)
        .Returns(())
        .Create()
"""
        { config with
            MaxDotGetExpressionWidth = 50 }
    |> prepend newline
    |> should
        equal
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
        { config with
            MaxInfixOperatorExpression = 50
            MaxDotGetExpressionWidth = 50 }
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

[<Test>]
let ``should preserve comments in quotation, 2535`` () =
    formatSourceString
        false
        """
test
    <@
      result
        .Replace('\r', '\u00FF')
        .Replace('\n', '\u00FF')
        .Replace("\u00FF\u00FF", "\u00FF")
        .Replace("8.12", "8.13") // CRAP score rounding
        .Replace("4.12", "4.13") // CRAP score rounding
        .Trim([| '\u00FF' |]) = expected
        .Replace('\r', '\u00FF')
        .Replace('\n', '\u00FF')
        .Replace("\u00FF\u00FF", "\u00FF")
        .Trim([| '\u00FF' |])
    @>
"""
        config
    |> prepend newline
    |> should
        equal
        """
test
    <@
        result
            .Replace('\r', '\u00FF')
            .Replace('\n', '\u00FF')
            .Replace("\u00FF\u00FF", "\u00FF")
            .Replace("8.12", "8.13") // CRAP score rounding
            .Replace("4.12", "4.13") // CRAP score rounding
            .Trim([| '\u00FF' |]) = expected
            .Replace('\r', '\u00FF')
            .Replace('\n', '\u00FF')
            .Replace("\u00FF\u00FF", "\u00FF")
            .Trim([| '\u00FF' |])
    @>
"""

[<Test>]
let ``overly aggressive de-indentation, 2110`` () =
    formatSourceString
        false
        """
      let result =
        Instrument.I.instrumentationVisitor state' visited

      test
        <@ { result with
               RecordingMethodRef =
                 { Visit = null
                   Push = null
                   Pop = null } } = { state' with
                                        ModuleId = def.MainModule.Mvid.ToString()
                                        RecordingMethod = visit
                                        RecordingMethodRef =
                                          { Visit = null
                                            Push = null
                                            Pop = null } } @>
"""
        { config with IndentSize = 2 }
    |> prepend newline
    |> should
        equal
        """
let result = Instrument.I.instrumentationVisitor state' visited

test
  <@
    { result with
        RecordingMethodRef =
          { Visit = null
            Push = null
            Pop = null } } = { state' with
                                 ModuleId = def.MainModule.Mvid.ToString()
                                 RecordingMethod = visit
                                 RecordingMethodRef =
                                   { Visit = null
                                     Push = null
                                     Pop = null } }
  @>
"""
