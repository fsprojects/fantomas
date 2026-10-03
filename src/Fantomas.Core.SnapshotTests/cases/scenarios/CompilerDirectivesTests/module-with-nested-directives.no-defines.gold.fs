module ReactDomBindings =
    #if FABLE_REPL_LIB
    #else
    [<Import("*", "react-dom")>]
    #endif
    let ReactDom: IReactDom = jsNative

    #if !FABLE_REPL_LIB
    [<Import("default", "react-dom/server")>]
    let ReactDomServer: IReactDomServer = jsNative
#endif
