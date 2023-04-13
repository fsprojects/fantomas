module Fantomas.Core.Tests.CastTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers

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
            AlternativeLongMemberDefinitions = true }
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
            AlternativeLongMemberDefinitions = true }
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
    let t = x |> Async.AwaitTask |> Async.RunSynchronously
    x.Result :> AmazonWebServiceResponse

let waitAndUpcast (x: Task<'t>) =
    let t = x |> Async.AwaitTask |> Async.RunSynchronously

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
    let t = x |> Async.AwaitTask |> Async.RunSynchronously

    x.Result :?> AmazonWebServiceResponse
"""

[<Test>]
let ``trivia newline before inferred upcast, 1685`` () =
    formatSourceString
        false
        """
namespace Blah

module Foo =

    let foo =
        { new IDisposable with
            member __.Dispose () =
            do ()

            upcast ()
        }
"""
        { config with
            SpaceBeforeColon = true
            SpaceBeforeSemicolon = true }
    |> prepend newline
    |> should
        equal
        """
namespace Blah

module Foo =

    let foo =
        { new IDisposable with
            member __.Dispose() =
                do ()

                upcast () }
"""

[<Test>]
let ``trivia newline before inferred downcast`` () =
    formatSourceString
        false
        """
namespace Blah

module Foo =

    let foo =
        { new IDisposable with
            member __.Dispose () =
            do ()

            downcast ()
        }
"""
        { config with
            SpaceBeforeColon = true
            SpaceBeforeSemicolon = true }
    |> prepend newline
    |> should
        equal
        """
namespace Blah

module Foo =

    let foo =
        { new IDisposable with
            member __.Dispose() =
                do ()

                downcast () }
"""
