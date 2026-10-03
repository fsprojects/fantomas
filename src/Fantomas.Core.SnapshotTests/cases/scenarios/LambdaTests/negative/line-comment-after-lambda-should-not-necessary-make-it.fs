(*---
fsharp_max_function_binding_width = 150
---*)
let a = fun _ -> div [] [] // React.lazy is not compatible with SSR, so just use an empty div
