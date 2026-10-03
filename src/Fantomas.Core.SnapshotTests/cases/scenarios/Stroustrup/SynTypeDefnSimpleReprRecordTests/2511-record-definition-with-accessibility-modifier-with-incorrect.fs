(*---
fsharp_multiline_bracket_style = stroustrup
---*)
type NonEmptyList<'T> =
    private
        { List: 'T list }

    member this.Head = this.List.Head
    member this.Tail = this.List.Tail
    member this.Length = this.List.Length
