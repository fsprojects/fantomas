type Foo() =
    member this.addTaskToScheduler
        (scheduler: IScheduler)
        taskName
        taskCron
        prio
        (task: unit -> unit)
        groupName
        = {
        astContext with
            IsInsideMatchClausePattern = true
    }
