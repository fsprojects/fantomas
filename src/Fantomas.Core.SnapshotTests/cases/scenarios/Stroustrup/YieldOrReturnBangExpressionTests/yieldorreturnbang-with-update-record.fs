(*---
fsharp_max_array_or_list_width = 40
fsharp_multiline_bracket_style = stroustrup
---*)
myComp {
    yield!
        { bar with X = xFieldValueOne
                   Y = yFieldValueTwo
                   Z = zFieldValueThree }
    return!
        { bar with X = xFieldValueOne
                   Y = yFieldValueTwo
                   Z = zFieldValueThree }
}
