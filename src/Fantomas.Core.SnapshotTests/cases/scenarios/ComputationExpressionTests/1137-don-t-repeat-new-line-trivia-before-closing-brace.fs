(*---
fsharp_max_if_then_short_width = 80
fsharp_max_if_then_else_short_width = 80
---*)
let create: Highlighter =
    fun searchTerm ->
        let regex = searchTerm |> SearchTerm.toRegex

        fun s ->
            match s |> String.length with
            | 0 -> [] |> FormattedText
            | _ ->
                seq {
                    let ms = regex.Matches(s)

                    if ms.Count = 0 then yield (TextSpan.normal s)
                    elif ms.[0].Index > 0 then yield TextSpan.normal (s.Substring(0, ms.[0].Index))

                    for i in 0 .. ms.Count - 1 do
                        yield TextSpan.highlight ms.[i].Value
                        let regStart = ms.[i].Index + ms.[i].Length

                        if i < ms.Count - 1
                        then yield TextSpan.normal (s.Substring(regStart, ms.[i + 1].Index - regStart))
                        elif regStart < s.Length
                        then yield TextSpan.normal (s.Substring(regStart))

                }
                |> List.ofSeq
                |> FormattedText.fromList
