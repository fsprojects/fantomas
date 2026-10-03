fun config ->
#if LOGGING_DEBUG || LOGGING_LOCAL
            let template =
                "[{Timestamp:HH:mm:ss.fff} {Level:u3} {SourceContext:l}] {Message:lj} | {Properties}{NewLine}{Exception}"
#endif

            config
#if LOGGING_DEBUG
                .WriteTo
                .Debug(outputTemplate = template)
#endif
#if LOGGING_LOCAL
                .WriteTo
                .AnsiConsoleLog(
                    outputTemplate = template
                )
#endif
