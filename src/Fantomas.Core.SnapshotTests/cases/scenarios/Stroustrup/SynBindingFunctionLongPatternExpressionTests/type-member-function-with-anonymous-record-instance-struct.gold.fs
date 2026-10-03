type Foo() =
    member this.addTaskToScheduler
        (scheduler: IScheduler)
        taskName
        taskCron
        prio
        (task: unit -> unit)
        groupName
        = struct {|
        A = longTypeName
        B = someOtherVariable
        C = ziggyBarX
    |}
