(*---
fsharp_max_value_binding_width = 90
---*)
let resource = promise {
    return new DisposableAction(fun () -> isDisposed := true)
}
promise {
    use! r = resource
    step1ok := not !isDisposed
}
