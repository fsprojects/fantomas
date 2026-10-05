(*---
fsharp_record_multiline_formatter = number_of_items
fsharp_multiline_bracket_style = cramped
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
