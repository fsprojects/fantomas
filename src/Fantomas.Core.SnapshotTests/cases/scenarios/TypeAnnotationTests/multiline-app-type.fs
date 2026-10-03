(*---
max_line_length = 80
---*)
type CancellableTaskResultBuilderBase with

    [<NoEagerConstraintApplication>]
    static member inline BindDynamic< ^TaskLike, 'TResult1, 'TResult2, ^Awaiter, 'TOverall, 'Error
        when ^TaskLike: (member GetAwaiter: unit -> ^Awaiter)
        and ^Awaiter :> ICriticalNotifyCompletion
        and ^Awaiter: (member get_IsCompleted: unit -> bool)
        and ^Awaiter: (member GetResult: unit -> Result<'TResult1, 'Error>)>
        (
            sm:
                byref<
                    ResumableStateMachine<
                        CancellableTaskResultStateMachineData<'TOverall, 'Error>
                >
                >,
            task: CancellationToken -> ^TaskLike,
            continuation:
                ('TResult1 -> CancellableTaskResultCode<'TOverall, 'Error, 'TResult2>)
        ) : bool = true
