(*---
fsharp_multiline_bracket_style = stroustrup
---*)
type MangaDexAtHomeResponse = {
    baseUrl: string
    chapter: {|
        hash: string
        data: string[]
        otherThing: int
    |}
}
