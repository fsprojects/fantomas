(*---
fsharp_max_array_or_list_width = 40
fsharp_experimental_keep_indent_in_branch = true
fsharp_multiline_bracket_style = cramped
---*)
let x =
            if not (
                result.HasResultsFor(
                    [ "label"
                      "ipv4"
                      "macAddress"
                      "medium"
                      "manufacturer" ]
                )
            ) then
                None
            else

            let label = string result.["label"]
            let ipv4 = string result.["ipv4"]
            None
