(*---
fsharp_newline_before_multiline_computation_expression = true
---*)
let fetch () = async {
    let! data = load ()
    return data
}
