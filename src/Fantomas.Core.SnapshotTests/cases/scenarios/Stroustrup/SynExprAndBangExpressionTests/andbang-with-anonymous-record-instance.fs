(*---
fsharp_max_array_or_list_width = 40
fsharp_multiline_bracket_style = stroustrup
---*)
opt {
    let! abc = def ()
    and! foo =
       {| A = longTypeName
          B = someOtherVariable
          C = ziggyBarX |}

    ()
}
