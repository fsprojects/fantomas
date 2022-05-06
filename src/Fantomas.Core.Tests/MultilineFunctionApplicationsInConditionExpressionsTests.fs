module Fantomas.Core.Tests.MultilineFunctionApplicationsInConditionExpressionsTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelper

[<Test>]
let ``inside match expression, 1403`` () =
    formatSourceString
        false
        """
let foo () =
    match b.TryGetValue (longlonglonglonglong, b) with
    | true, i -> Some i
    | false, _ -> failwith ""
"""
        { config with MaxLineLength = 40 }
    |> prepend newline
    |> should
        equal
        """
let foo () =
    match
        b.TryGetValue
            (
                longlonglonglonglong,
                b
            )
        with
    | true, i -> Some i
    | false, _ -> failwith ""
"""


[<Test>]
let ``inside match expression, single argument in parenthesis`` () =
    formatSourceString
        false
        """
let foo () =
    match b.TryGetValue (longlonglonglonglong) with
    | true, i -> Some i
    | false, _ -> failwith ""
"""
        { config with MaxLineLength = 40 }
    |> prepend newline
    |> should
        equal
        """
let foo () =
    match
        b.TryGetValue
            (longlonglonglonglong)
        with
    | true, i -> Some i
    | false, _ -> failwith ""
"""

[<Test>]
let ``inside when clause of try/with, 1406`` () =
    formatSourceString
        false
        """
module ElectrumClient =

    let private Init (fqdn: string) (port: uint32): Async<StratumClient> =
        let PROTOCOL_VERSION_SUPPORTED = Version "1.4"

        async {
            let! versionSupportedByServer =
                try
                    stratumClient.ServerVersion
                        CLIENT_NAME_SENT_TO_STRATUM_SERVER_WHEN_HELLO
                        PROTOCOL_VERSION_SUPPORTED
                with :? ElectrumServerReturningErrorException as except when
                    except.Message.EndsWith (PROTOCOL_VERSION_SUPPORTED.ToString ()) ->

                    failwith "xxx"

            return stratumClient
        }
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
module ElectrumClient =

    let private Init (fqdn: string) (port: uint32) : Async<StratumClient> =
        let PROTOCOL_VERSION_SUPPORTED = Version "1.4"

        async {
            let! versionSupportedByServer =
                try
                    stratumClient.ServerVersion
                        CLIENT_NAME_SENT_TO_STRATUM_SERVER_WHEN_HELLO
                        PROTOCOL_VERSION_SUPPORTED
                with
                | :? ElectrumServerReturningErrorException as except when
                    except.Message.EndsWith
                        (PROTOCOL_VERSION_SUPPORTED.ToString())
                    ->

                    failwith "xxx"

            return stratumClient
        }
"""

[<Test>]
let ``inside infix expression of if expression, 1402`` () =
    formatSourceString
        false
        """
let c =
    if bar |> Seq.exists ((|KeyValue|) >> snd >> (=) (Some i)) then false else true
"""
        { config with
            MaxLineLength = 40
            SpaceBeforeUppercaseInvocation = true }
    |> prepend newline
    |> should
        equal
        """
let c =
    if
        bar
        |> Seq.exists
            (
                (|KeyValue|)
                >> snd
                >> (=) (Some i)
            )
    then
        false
    else
        true
"""

[<Test>]
let ``single parenthesis arg inside if expression`` () =
    formatSourceString
        false
        """
if MyGrandFunctionThatTakesASingleArgument ( myEvenGranderArgumentNameThatGoesOnForEverAndEver ) then
    ()
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
if
    MyGrandFunctionThatTakesASingleArgument
        (myEvenGranderArgumentNameThatGoesOnForEverAndEver)
then
    ()
"""

[<Test>]
let ``inside match bang expression, single argument in parenthesis`` () =
    formatSourceString
        false
        """
let foo () =
    async {
        match! b.TryGetValue (longlonglonglonglong) with
        | true, i -> Some i
        | false, _ -> failwith ""
    }
"""
        { config with MaxLineLength = 40 }
    |> prepend newline
    |> should
        equal
        """
let foo () =
    async {
        match!
            b.TryGetValue
                (longlonglonglonglong)
            with
        | true, i -> Some i
        | false, _ -> failwith ""
    }
"""

[<Test>]
let ``inside match bang expression, tupled argument in parenthesis`` () =
    formatSourceString
        false
        """
let foo () =
    async {
        match! b.TryGetValue (longlonglonglonglong, b) with
        | true, i -> Some i
        | false, _ -> failwith ""
    }
"""
        { config with MaxLineLength = 40 }
    |> prepend newline
    |> should
        equal
        """
let foo () =
    async {
        match!
            b.TryGetValue
                (
                    longlonglonglonglong,
                    b
                )
            with
        | true, i -> Some i
        | false, _ -> failwith ""
    }
"""

[<Test>]
let ``multiple infix operator application inside if expression, 1390`` () =
    formatSourceString
        false
        """
module Web3ServerSeedList =
    let MaybeRethrow (ex: Exception): unit =
        let rpcResponseExOpt =
            FSharpUtil.FindException<RpcResponseException>
                ex

        match rpcResponseExOpt with
        | Some rpcResponseEx ->
            if rpcResponseEx.RpcError <> null then
                if (not (rpcResponseEx.RpcError.Message.Contains "pruning=archive"))
                       && (not (rpcResponseEx.RpcError.Message.Contains "header not found"))
                       && (not (rpcResponseEx.RpcError.Message.Contains "missing trie node")) then
                        raise UnexpectedRpcResponseError
        | _ -> ()
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
module Web3ServerSeedList =
    let MaybeRethrow (ex: Exception) : unit =
        let rpcResponseExOpt = FSharpUtil.FindException<RpcResponseException> ex

        match rpcResponseExOpt with
        | Some rpcResponseEx ->
            if rpcResponseEx.RpcError <> null then
                if
                    (not
                        (
                            rpcResponseEx.RpcError.Message.Contains
                                "pruning=archive"
                        ))
                    && (not
                        (
                            rpcResponseEx.RpcError.Message.Contains
                                "header not found"
                        ))
                    && (not
                        (
                            rpcResponseEx.RpcError.Message.Contains
                                "missing trie node"
                        ))
                then
                    raise UnexpectedRpcResponseError
        | _ -> ()
"""

[<Test>]
let ``inside infix expression of elif expression`` () =
    formatSourceString
        false
        """
let c =
    if blah then
        true
    elif bar |> Seq.exists ((|KeyValue|) >> snd >> (=) (Some i)) then false else true
"""
        { config with
            MaxLineLength = 40
            SpaceBeforeUppercaseInvocation = true }
    |> prepend newline
    |> should
        equal
        """
let c =
    if blah then
        true
    elif
        bar
        |> Seq.exists
            (
                (|KeyValue|)
                >> snd
                >> (=) (Some i)
            )
    then
        false
    else
        true
"""
