(*---
fsharp_space_before_colon = true
fsharp_space_before_semicolon = true
---*)
let config = {
    title = "Fantomas"
    description = "Fantomas is a code formatter for F#"
    theme_variant = Some "red"
    root_url =
      #if WATCH
        "http://localhost:8080/"
      #else
        "https://fsprojects.github.io/fantomas/"
      #endif
}
