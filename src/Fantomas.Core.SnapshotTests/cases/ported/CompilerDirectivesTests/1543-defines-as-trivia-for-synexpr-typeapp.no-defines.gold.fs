let inputFileFlagsFsiBase (_tcConfigB: TcConfigBuilder) =
    #if NETSTANDARD
    #else
    List.empty<CompilerOption>
#endif
