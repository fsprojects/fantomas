module Fantomas.Tests.CastTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

[<Test>]
let ``multiline downcast expression, `` () =
    formatSourceString
        false
        """
longMethodName
    longArgument
    longArgument2
:?> List<bool>
"""
        { config with
              MaxLineLength = 30
              SpaceBeforeUppercaseInvocation = true
              SpaceBeforeColon = true
              SpaceBeforeSemicolon = true
              AlignFunctionSignatureToIndentation = true
              AlternativeLongMemberDefinitions = true
              DisableElmishSyntax = true }
    |> prepend newline
    |> should
        equal
        """
longMethodName
    longArgument
    longArgument2
:?> List<bool>
"""

[<Test>]
let ``multiline upcast expression, `` () =
    formatSourceString
        false
        """
longMethodName
    longArgument
    longArgument2
:> List<bool>
"""
        { config with
              MaxLineLength = 30
              SpaceBeforeUppercaseInvocation = true
              SpaceBeforeColon = true
              SpaceBeforeSemicolon = true
              AlignFunctionSignatureToIndentation = true
              AlternativeLongMemberDefinitions = true
              DisableElmishSyntax = true }
    |> prepend newline
    |> should
        equal
        """
longMethodName
    longArgument
    longArgument2
:> List<bool>
"""

[<Test>]
let ``trivia newline before upcast, 1227`` () =
    formatSourceString
        false
        """
module S3v2

open System.Threading.Tasks
open Amazon.Runtime

let waitAndUpcast (x: Task<'t>) =
    let t =
        x |> Async.AwaitTask |> Async.RunSynchronously
    x.Result :> AmazonWebServiceResponse

let waitAndUpcast (x: Task<'t>) =
    let t =
        x |> Async.AwaitTask |> Async.RunSynchronously

    x.Result :> AmazonWebServiceResponse
"""
        config
    |> prepend newline
    |> should
        equal
        """
module S3v2

open System.Threading.Tasks
open Amazon.Runtime

let waitAndUpcast (x: Task<'t>) =
    let t =
        x |> Async.AwaitTask |> Async.RunSynchronously

    x.Result :> AmazonWebServiceResponse

let waitAndUpcast (x: Task<'t>) =
    let t =
        x |> Async.AwaitTask |> Async.RunSynchronously

    x.Result :> AmazonWebServiceResponse
"""

[<Test>]
let ``trivia newline before downcast`` () =
    formatSourceString
        false
        """
module S3v2

open System.Threading.Tasks
open Amazon.Runtime

let waitAndUpcast (x: Task<'t>) =
    let t =
        x |> Async.AwaitTask |> Async.RunSynchronously

    x.Result :?> AmazonWebServiceResponse
"""
        config
    |> prepend newline
    |> should
        equal
        """
module S3v2

open System.Threading.Tasks
open Amazon.Runtime

let waitAndUpcast (x: Task<'t>) =
    let t =
        x |> Async.AwaitTask |> Async.RunSynchronously

    x.Result :?> AmazonWebServiceResponse
"""
