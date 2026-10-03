(*---
fsharp_newline_between_type_definition_and_members = false
fsharp_multiline_bracket_style = cramped
---*)
type Test =
    { WorkHoursPerWeek: uint<hr*(staff weeks)> }
    static member create =
     { WorkHoursPerWeek = 40u<hr*(staff weeks)> }
