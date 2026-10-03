(*---
fsharp_multiline_bracket_style = cramped
---*)
type CreateBuildingViewModel =
    new (items) as vm
        =
        let p = ""
        {
            inherit ResizeArray(seq {
                yield p
                yield! items
            })
        }
        then
            vm.program <- p
