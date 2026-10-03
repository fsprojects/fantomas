[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidRepetitiveCallsToPropertiesRule",
                            Scope = "member",  // MethodDefinition
                            Target = "AltCover.Recorder.Instance/I/CallTrack::instance()",
                            Justification = "Bytecode delta only")>]
()
