(*---
fsharp_max_array_or_list_width = 40
fsharp_multiline_bracket_style = stroustrup
---*)
let private knownProviders = [
#if !FABLE_COMPILER
    (SerilogProvider.isAvailable, SerilogProvider.create)
    (MicrosoftExtensionsLoggingProvider.isAvailable, MicrosoftExtensionsLoggingProvider.create)
#endif
                                        ]
