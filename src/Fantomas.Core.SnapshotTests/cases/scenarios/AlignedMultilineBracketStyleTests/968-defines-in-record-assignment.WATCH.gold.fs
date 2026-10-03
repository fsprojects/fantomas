let config =
    {
        title = "Fantomas"
        description = "Fantomas is a code formatter for F#"
        theme_variant = Some "red"
        root_url =
            #if WATCH
            "http://localhost:8080/"
    #else
    #endif
    }
