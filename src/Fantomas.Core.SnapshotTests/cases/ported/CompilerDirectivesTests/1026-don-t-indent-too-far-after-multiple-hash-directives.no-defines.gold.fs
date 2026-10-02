let getDefaultProxyFor =
    memoize (fun (url: string) ->
        let uri = Uri url

        let getDefault () =
            #if CUSTOM_WEBPROXY
            #else
            let result = WebRequest.GetSystemWebProxy()
            #endif
            #if CUSTOM_WEBPROXY
            #else
            let address = result.GetProxy uri

            if address = uri then
                null
            else
                let proxy = WebProxy address
                proxy.BypassProxyOnLocal <- true
                #endif
                proxy.Credentials <- CredentialCache.DefaultCredentials
                proxy

        match calcEnvProxies.Force().TryFind uri.Scheme with
        | Some p -> if p.GetProxy uri <> uri then p else getDefault ()
        | None -> getDefault ())
