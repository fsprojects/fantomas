module Fantomas.Core.Tests.SynExprNewTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers

[<Test>]
let ``combination of named and non named arguments, 1158`` () =
    formatSourceString
        """
    let private sendTooLargeError () =
        new HttpResponseMessage(HttpStatusCode.RequestEntityTooLarge,
                                Content =
                                    new StringContent("File was too way too large",
                                                      System.Text.Encoding.UTF16,
                                                      "application/text"))
"""
        config
    |> prepend newline
    |> should
        equal
        """
let private sendTooLargeError () =
    new HttpResponseMessage(
        HttpStatusCode.RequestEntityTooLarge,
        Content = new StringContent("File was too way too large", System.Text.Encoding.UTF16, "application/text")
    )
"""

[<Test>]
let ``single multi line named argument instance`` () =
    formatSourceString
        """
let myInstance =
        new EvilBadRequest(Content = new StringContent("File was too way too large, as in waaaaaaaaaaaaaaaaaaaay tooooooooo long",
                                                      System.Text.Encoding.UTF16,
                                                      "application/text"))
"""
        config
    |> prepend newline
    |> should
        equal
        """
let myInstance =
    new EvilBadRequest(
        Content =
            new StringContent(
                "File was too way too large, as in waaaaaaaaaaaaaaaaaaaay tooooooooo long",
                System.Text.Encoding.UTF16,
                "application/text"
            )
    )
"""

[<Test>]
let ``single string argument, 1363`` () =
    formatSourceString
        """
open System.IO
let f = new StringReader ""
"""
        config
    |> prepend newline
    |> should
        equal
        """
open System.IO
let f = new StringReader ""
"""
