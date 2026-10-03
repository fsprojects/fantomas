let getDefaultProxyFor =
    memoize (fun (url: string) ->
        let uri = Uri url

        let getDefault () =
            #if CUSTOM_WEBPROXY
            let result =
                { new IWebProxy with
                    member __.Credentials
                        with get () = null
                        and set _value = ()

                    member __.GetProxy _ = null
                    member __.IsBypassed(_host: Uri) = true }
            #else
            #endif
            #if CUSTOM_WEBPROXY
            let proxy = result
            #else
            #endif
            proxy.Credentials <- CredentialCache.DefaultCredentials
            proxy

        match calcEnvProxies.Force().TryFind uri.Scheme with
        | Some p -> if p.GetProxy uri <> uri then p else getDefault ()
        | None -> getDefault ())
