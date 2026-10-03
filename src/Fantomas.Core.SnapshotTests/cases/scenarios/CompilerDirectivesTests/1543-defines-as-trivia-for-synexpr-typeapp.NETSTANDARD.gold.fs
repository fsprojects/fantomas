let inputFileFlagsFsiBase (_tcConfigB: TcConfigBuilder) =
    #if NETSTANDARD
    [ CompilerOption(
          "usesdkrefs",
          tagNone,
          OptionSwitch(SetUseSdkSwitch _tcConfigB),
          None,
          Some(FSComp.SR.useSdkRefs ())
      ) ]
#else
#endif
