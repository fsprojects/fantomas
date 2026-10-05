(*---
fsharp_newline_before_multiline_computation_expression = false
---*)
let fetch () =
    async {
        let! data = load ()
        return data
    }
