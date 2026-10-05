type Foo() =
    member this.addTaskToScheduler
        (scheduler: IScheduler)
        taskName
        taskCron
        prio
        (task: unit -> unit)
        groupName
        = {
        A = longTypeName
        B = someOtherVariable
        C = ziggyBarX
    }
