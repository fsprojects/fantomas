module ReactDomBindings =
    #if FABLE_REPL_LIB
    [<Global("ReactDOM")>]
    #else
    #endif
    let ReactDom: IReactDom = jsNative

#if !FABLE_REPL_LIB
#endif
