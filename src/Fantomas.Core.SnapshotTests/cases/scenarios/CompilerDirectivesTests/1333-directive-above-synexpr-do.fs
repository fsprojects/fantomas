(*---
fsharp_newline_between_type_definition_and_members = false
---*)
[<AutoOpen>]
module ReactHookExtensions =
    type React with
        [<Hook>]
        static member useDeferred(operation: Async<'T>, dependencies: obj array) =
            let (deferred, setDeferred) = React.useState(Deferred.HasNotStartedYet)
            let token = React.useCancellationToken()
            let executeOperation = async {
                try
                    do setDeferred(Deferred<'T>.InProgress)
                    let! output = operation
                    do setDeferred(Deferred<'T>.Resolved output)
                with error ->
                    #if DEBUG
                    Browser.Dom.console.log(error)
                    #endif
                    do setDeferred(Deferred<'T>.Failed error)
            }

            React.useEffect((fun () -> Async.StartImmediate(executeOperation, token.current)), dependencies)

            deferred
