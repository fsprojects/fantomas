(*---
fsharp_multiline_bracket_style = cramped
---*)
type Commenter =
    { [<JsonProperty("display_name")>]
      // foo
      [<Bar>]

      DisplayName: string }
