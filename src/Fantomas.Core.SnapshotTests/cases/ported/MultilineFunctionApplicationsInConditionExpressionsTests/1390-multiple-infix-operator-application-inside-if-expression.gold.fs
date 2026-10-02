module Web3ServerSeedList =
    let MaybeRethrow (ex: Exception) : unit =
        let rpcResponseExOpt = FSharpUtil.FindException<RpcResponseException> ex

        match rpcResponseExOpt with
        | Some rpcResponseEx ->
            if rpcResponseEx.RpcError <> null then
                if
                    (not (
                        rpcResponseEx.RpcError.Message.Contains
                            "pruning=archive"
                    ))
                    && (not (
                        rpcResponseEx.RpcError.Message.Contains
                            "header not found"
                    ))
                    && (not (
                        rpcResponseEx.RpcError.Message.Contains
                            "missing trie node"
                    ))
                then
                    raise UnexpectedRpcResponseError
        | _ -> ()
