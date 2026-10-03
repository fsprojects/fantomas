let resource = promise { return new DisposableAction(fun () -> isDisposed := true) }

promise {
    use! r = resource
    step1ok := not !isDisposed
}
