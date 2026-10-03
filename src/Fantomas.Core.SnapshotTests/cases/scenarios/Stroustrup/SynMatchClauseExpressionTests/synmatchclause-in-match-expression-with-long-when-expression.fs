(*---
fsharp_max_array_or_list_width = 40
fsharp_multiline_bracket_style = stroustrup
---*)
match x with
| _ when (try
            somethingDangerous ()
            true
          with | ex -> false)
                                    ->
                                        { A = longTypeName
                                          B = someOtherVariable
                                          C = ziggyBarX }
